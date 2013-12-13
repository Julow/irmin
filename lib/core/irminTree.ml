(*
 * Copyright (c) 2013 Louis Gesbert     <louis.gesbert@ocamlpro.com>
 * Copyright (c) 2013 Thomas Gazagnaire <thomas@gazagnaire.org>
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

type ('a, 'b) node = {
  value   : 'a option;
  children: (string * 'b) list;
}

let debug fmt =
  IrminLog.debug "TREE" fmt

module Path = struct

  include IrminBase.List(IrminBase.String)

  exception Unknown of string

  exception Invalid of string

  let name = "path"

  let to_string =
    String.concat "/"

  let of_string path =
    let strings = IrminMisc.split path '/' in
    List.filter ((<>) "") strings

  let pretty = to_string

  let of_pretty = of_string

end

module type STORE = sig
  type key
  type value
  type tree = (key, key) node
  include IrminBase.S with type t := tree
  include IrminStore.A with type key := key
                        and type value := tree
  val empty: tree
  val tree: t -> ?value:value -> (string * tree) list -> key Lwt.t
  val value: t -> tree -> value Lwt.t option
  val children: t -> tree -> (string * tree Lwt.t) list
  val sub: t -> tree -> Path.t -> tree option Lwt.t
  val sub_exn: t -> tree -> Path.t -> tree Lwt.t
  val update: t -> tree -> Path.t -> value -> tree Lwt.t
  val find: t -> tree -> Path.t -> value option Lwt.t
  val find_exn: t -> tree -> Path.t -> value Lwt.t
  val remove: t -> tree -> Path.t -> tree Lwt.t
  val valid: t -> tree -> Path.t -> bool Lwt.t
end

module Tree (A: IrminBase.S) (B: IrminBase.S) = struct

  type t = (A.t, B.t) node

  module XValue = struct
    include IrminBase.Option(A)
    let name = "value"
  end

  module XChildren = struct
    include IrminBase.List(IrminBase.Pair(IrminBase.String)(B))
    let name = "children"
  end

  module XTree = struct
    include IrminBase.Pair(XValue)(XChildren)
    let name = "tree"
  end

  let name = XTree.name

  let compare t1 t2 =
    XTree.compare (t1.value, t1.children) (t2.value, t2.children)

  let equal t1 t2 =
    compare t1 t2 = 0

  let hash t =
    XTree.hash (t.value, t.children)

  let pretty t =
    XTree.pretty (t.value, t.children)

  let to_json t =
    XTree.to_json (t.value, t.children)

  let of_json j =
    let value, children = XTree.of_json j in
    { value; children }

  (* |-----|---------| *)
  (* | 'T' | PAYLOAD | *)
  (* |-----|---------| *)

  let header = "T"

  let sizeof t =
    1 + XTree.sizeof (t.value, t.children)

  let set buf t =
    IrminBuffer.set_string buf header;
    XTree.set buf (t.value, t.children)

  let get buf =
    let h = IrminBuffer.get_string buf 1 in
    if header <> h then None
    else match XTree.get buf with
      | None                   -> None
      | Some (value, children) -> Some { value; children }

  let to_string t =
    XTree.to_string (t.value, t.children)

end

module type MAKER =
  functor (K: IrminKey.BINARY) ->
  functor (V: IrminValue.STORE with type key = K.t) ->
    STORE with type key = K.t
           and type value = V.value

module Make
    (S: IrminStore.A_MAKER)
    (K: IrminKey.BINARY)
    (V: IrminValue.STORE with type key = K.t) =
struct

  open Lwt

  module T = Tree(K)(K)

  module S = S(K)(T)

  type key = K.t

  type value = V.value

  type tree = (K.t, K.t) node

  type path = string list

  type t = {
    v: V.t;
    t: S.t;
  }

  let create () =
    V.create () >>= fun v ->
    S.create () >>= fun t ->
    return { v; t }

  let add t tree =
    S.add t.t tree

  let read t key =
    S.read t.t key

  let read_exn t key =
    S.read_exn t.t key

  let mem t key =
    S.mem t.t key

  module Graph = IrminGraph.Make(K)

  let list t key =
    debug "list %s" (K.pretty key);
    read_exn t key >>= fun _ ->
    let pred k =
      read_exn t k >>= fun r -> return (List.map snd r.children) in
    Graph.closure pred ~min:[] ~max:[key] >>= fun g ->
    return (Graph.vertex g)

  let contents t =
    S.contents t.t

  let empty = {
    value = None;
    children = [];
  }

  let tree t ?value children =
    debug "tree";
    begin match value with
      | None   -> return_none
      | Some v -> V.add t.v v >>= fun k -> return (Some k)
    end
    >>= fun value ->
    Lwt_list.map_p (fun (l, tree) ->
        add t tree >>= fun k ->
        return (l, k)
      ) children
    >>= fun children ->
    let tree = { value; children } in
    add t tree

  let value t tree =
    match tree.value with
    | None   -> None
    | Some k -> Some (V.read_exn t.v k)

  let children t tree =
    List.map (fun (l, k) -> l, read_exn t k) tree.children

  let child t tree label =
    try Some (List.assoc label (children t tree))
    with Not_found -> None

  let sub_exn t tree path =
    let rec aux tree path =
      match path with
    | []    -> return tree
    | h::tl ->
      match child t tree h with
      | None      -> fail Not_found
      | Some tree -> tree >>= fun tree -> aux tree tl in
    aux tree path

  let sub t tree path =
    catch
      (fun () ->
         sub_exn t tree path >>= fun tree ->
         return (Some tree))
      (function Not_found -> return_none | e -> fail e)

  let find_exn t tree path =
    sub t tree path >>= function
    | None      -> fail Not_found
    | Some tree ->
      match value t tree with
      | None   -> fail Not_found
      | Some v -> v

  let find t tree path =
    sub t tree path >>= function
    | None      -> return_none
    | Some tree ->
      match value t tree with
      | None   -> return_none
      | Some v -> v >>= fun v -> return (Some v)

  let valid t tree path =
    sub t tree path >>= function
    | None      -> return false
    | Some tree ->
      match value t tree with
      | None   -> return false
      | Some _ -> return true

  let map_children t children f label =
    let rec aux acc = function
      | [] ->
        f empty >>= fun tree ->
        if tree = empty then return (List.rev acc)
        else
          add t tree >>= fun k ->
          return (List.rev_append acc [label, k])
      | (l, k) as child :: children ->
        if l = label then
          read t k >>= function
          | None      -> fail (K.Invalid (K.pretty k))
          | Some tree ->
            f tree >>= fun tree ->
            if tree = empty then return (List.rev_append acc children)
            else
              add t tree >>= fun k ->
              return (List.rev_append acc ((l, k) :: children))
        else
          aux (child::acc) children
    in
    aux [] children

  let map_subtree t tree path f =
    let rec aux tree = function
      | []      -> return (f tree)
      | h :: tl ->
        map_children t tree.children (fun tree -> aux tree tl) h
        >>= fun children ->
        return { tree with children } in
    aux tree path

  let remove t tree path =
    map_subtree t tree path (fun tree -> { tree with value = None })

  let update t tree path value =
    V.add t.v value >>= fun k  ->
    map_subtree t tree path (fun tree -> { tree with value = Some k })

  include (Tree(K)(K): IrminBase.S with type t := tree)

end