(*
 * Copyright (c) 2013-2019 Thomas Gazagnaire <thomas@gazagnaire.org>
 *
 * Permission to use, copy, modify, and distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *)

let src = Logs.Src.create "irmin.pack" ~doc:"Irmin in-memory store"
module Log = (val Logs.src_log src : Logs.LOG)

module type IO = sig
  type t
  val open_file: string -> t
  val blit: string -> srcoff:int -> t -> dstoff:int64 -> int -> unit
end

module Read_only (IO: IO) (K: Irmin.Type.S) (V: Irmin.Type.S) = struct

type key = K.t
  type value = V.t

  type 'a t = {
    pack_file : IO.t;
    mutable pack_offset : int64
  }

  let v _config = Lwt.return {
    pack_file = IO.open_file "test.pack";
    pack_offset = 0L
  }

  let cast t = (t :> [`Read | `Write] t)
  let batch t f = f (cast t)

  let pp_key = Irmin.Type.pp K.t

  let find _t key =
    Log.debug (fun f -> f "find %a" pp_key key);
    failwith "TODO"

  let mem _t key =
    Log.debug (fun f -> f "mem %a" pp_key key);
    failwith "TODO"

end

module type IO_Conf = sig
  val openfile : string -> Unix.file_descr
end

module IO_mapped_file_unix (Conf : IO_Conf) = struct

  (* Since big arrays don't support int64 indices *)
  let chunk_size = 12

  type t = {
    data : Cstruct.t;
    next : t Lazy.t
  }

  (** Map the given file in memory, RW
      May raise [Unix.Unix_error] *)
  let open_file file_name =
    let fd = Conf.openfile file_name in
    let kind, layout, shared = Bigarray.(Char, C_layout, true) in
    let rec map_chunk pos =
      let buffer = Unix.map_file ~pos fd kind layout shared [| chunk_size |] in
      {
        data = Cstruct.of_bigarray (Bigarray.array1_of_genarray buffer);
        next = lazy (map_chunk Int64.(add pos (of_int chunk_size)))
      }
    in
    map_chunk 0L

  let rec blit src ~srcoff chunk ~dstoff len =
    Log.debug (fun f -> f "Blit \"%s\" srcoff=%d dstoff=%Ld len=%d" src srcoff dstoff len);
    if Int64.(compare dstoff (of_int chunk_size)) >= 0
    then (
      Log.debug (fun f -> f "Skip chunk");
      let dstoff = Int64.(sub dstoff (of_int chunk_size)) in
      blit src ~srcoff (Lazy.force chunk.next) ~dstoff len )
    else (
      let dstoff = Int64.to_int dstoff in
      let blit_len = min (chunk_size - dstoff) len in
      Log.debug (fun f -> f "Blitting srcoff=%d dstoff=%l len=%d/%d"
        srcoff dstoff blit_len len);
      Cstruct.blit_from_string src srcoff chunk.data dstoff blit_len;
      if blit_len < len
      then
        let len = len - blit_len in
        let srcoff = srcoff + blit_len in
        blit src ~srcoff (Lazy.force chunk.next) ~dstoff:0L len )

end

module Append_only (IO: IO) (K: Irmin.Type.S) (V: Irmin.Type.S) = struct

  include Read_only(IO)(K)(V)

  let add t key value =
    let value_str = Irmin.Type.encode_bin V.t value in
    Log.debug (fun f -> f "add -> %a, \"%s\"" pp_key key value_str);
    let len = String.length value_str in
    IO.blit value_str ~srcoff:0 t.pack_file ~dstoff:t.pack_offset len;
    t.pack_offset <- Int64.(add t.pack_offset (of_int len));
    Lwt.return_unit

end

let config () = Irmin.Private.Conf.empty

module Make (IO: IO) = Irmin.Make(Irmin.Content_addressable(Append_only(IO)))(Irmin_mem.Atomic_write)

module KV (IO: IO) (C: Irmin.Contents.S) =
  Make(IO)
    (Irmin.Metadata.None)
    (C)
    (Irmin.Path.String_list)
    (Irmin.Branch.String)
    (Irmin.Hash.SHA1)
