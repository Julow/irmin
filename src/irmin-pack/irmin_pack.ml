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
  type fd
  module Infinite_file : sig
    type t
    val of_fd: fd -> t
    val blit: string -> srcoff:int -> t -> dstoff:int64 -> int -> unit
  end
  val open_file: string -> fd
  val get_buffer: fd -> ?pos:int64 -> int -> Cstruct.t
end

module Read_only (IO: IO) (K: Irmin.Type.S) (V: Irmin.Type.S) = struct

type key = K.t
  type value = V.t

  type 'a t = {
    pack_file : IO.Infinite_file.t;
    mutable pack_offset : int64;
    pack_header : Cstruct.t
  }

  let init_pack_file file_name =
    let fd = IO.open_file file_name in
    let header = IO.get_buffer fd 16 in
    Cstruct.(
      LE.set_uint64 header 0 1L;
      LE.set_uint64 header 8 0L
    );
    IO.Infinite_file.of_fd fd, header

  let v _config =
    let pack_file, pack_header = init_pack_file "test.pack" in
    Lwt.return {
      pack_file;
      pack_offset = 16L;
      pack_header
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

module IO_mapped_file_unix (Conf : IO_Conf) : IO = struct

  type fd = Unix.file_descr

  let get_buffer fd ?pos len =
    let kind, layout, shared = Bigarray.(Char, C_layout, true) in
    let buffer = Unix.map_file ?pos fd kind layout shared [| len |] in
    Cstruct.of_bigarray (Bigarray.array1_of_genarray buffer)

  module Infinite_file = struct

    (* Since big arrays don't support int64 indices *)
    let chunk_size = 12

    type t = {
      data : Cstruct.t;
      next : t Lazy.t
    }

    let of_fd fd =
      let rec map_chunk pos =
        {
          data = get_buffer fd ~pos chunk_size;
          next = lazy (map_chunk Int64.(add pos (of_int chunk_size)))
        }
      in
      map_chunk 0L

    let rec blit src ~srcoff chunk ~dstoff len =
      Log.debug (fun f -> f "Blit \"%s\" srcoff=%d dstoff=%Ld len=%d" src srcoff dstoff len);
      if Int64.(compare dstoff (of_int chunk_size)) >= 0
      then (
        let dstoff = Int64.(sub dstoff (of_int chunk_size)) in
        blit src ~srcoff (Lazy.force chunk.next) ~dstoff len )
      else (
        let dstoff = Int64.to_int dstoff in
        let blit_len = min (chunk_size - dstoff) len in
        Cstruct.blit_from_string src srcoff chunk.data dstoff blit_len;
        if blit_len < len
        then
          let len = len - blit_len in
          let srcoff = srcoff + blit_len in
          blit src ~srcoff (Lazy.force chunk.next) ~dstoff:0L len )
  end

  let open_file file_name =
    Conf.openfile file_name

end

module Append_only (IO: IO) (K: Irmin.Type.S) (V: Irmin.Type.S) = struct

  include Read_only(IO)(K)(V)

  let add t key value =
    let value_str = Irmin.Type.encode_bin V.t value in
    Log.warn (fun f -> f "add -> %a, \"%s\" at offset %Ld" pp_key key value_str t.pack_offset);
    let len = String.length value_str in
    IO.Infinite_file.blit value_str ~srcoff:0 t.pack_file ~dstoff:t.pack_offset len;
    t.pack_offset <- Int64.(add t.pack_offset (of_int len));
    Cstruct.LE.set_uint64 t.pack_header 8 t.pack_offset;
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
