(*
 * Copyright (c) 2013-2017 Thomas Gazagnaire <thomas@gazagnaire.org>
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

val config: unit -> Irmin.config


module type IO = sig
  type t
  val open_file: string -> t
  val blit: string -> srcoff:int -> t -> dstoff:int64 -> int -> unit
end

module Append_only (IO: IO): Irmin.APPEND_ONLY_STORE_MAKER
module Make (IO: IO): Irmin.S_MAKER
module KV (IO: IO): Irmin.KV_MAKER

module type IO_Conf = sig
  (** Allow to specify flags and file perms, O_RDWR must be set *)
  val openfile : string -> Unix.file_descr
end

module IO_mapped_file_unix (Conf : IO_Conf): IO
