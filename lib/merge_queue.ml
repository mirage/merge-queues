(*
 * Copyright (c) 2014 Benjamin Farinier <benjamin.farinier@ens-lyon.fr>
 * Copyright (c) 2014 Thomas Gazagnaire <thomas@gazagnaire.org>
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

open Lwt
open Irmin.Merge.OP

let list_dedup ?(compare=Pervasives.compare) t =
  let t = List.sort compare t in
  let rec aux acc = function
    | []      -> List.rev acc
    | [x]     -> aux (x :: acc) []
    | x::(y::_ as tl) ->
      match compare x y with
      | 0 -> aux acc tl
      | _ -> aux (x :: acc) tl
  in
  aux [] t

module Log = Log.Make(struct let section = "QUEUE" end)

exception Empty

type error = [ `Corrupted | `Invalid_access ]
exception Error of error

type stats = {
  ops: int;
  reads: int;
  writes: int;
}

let string_of_stats t =
  Printf.sprintf "%i\t%f\t%f%!"
    t.ops
    ((float t.reads)  /. (float t.ops))
    ((float t.writes) /. (float t.ops))

module type S = sig
  include Irmin.Contents.S
  type elt
  val create : unit -> t Lwt.t
  val length : t -> int Lwt.t
  val is_empty : t -> bool Lwt.t
  val push : t -> elt -> t Lwt.t
  val pop_exn : t -> (elt * t) Lwt.t
  val pop : t -> (elt * t) option Lwt.t
  val peek_exn : t -> (elt * t) Lwt.t
  val peek : t -> (elt * t) option Lwt.t
  val dump : t -> string Lwt.t
  val stats : t -> stats
end

module type Config = sig
  val conf: Irmin.config
  val task: string -> Irmin.task
end

module Make
    (AO: Irmin.AO_MAKER)
    (K: Irmin.Hash.S)
    (V: Tc.S0)
    (P: Irmin.Path.S)
    (Config: Config)
= struct

  module Path = P

  module C = struct

    (*
     * Type of index, which are queue accessor.
     * 'push' is the number of push applied to the queue since its creation,
     * 'pop' is the number of pop applied to the queue since its creation,
     * 'top' is the key of the queue top element,
     * 'bottom' is the key of the queue bottom element.
    *)
    type index = {
      push  : int;
      pop   : int;
      top   : K.t;
      bottom: K.t;
    }

    module Index = Tc.Biject
        (Tc.Pair (Tc.Pair(Tc.Int)(Tc.Int))(Tc.Pair (K)(K)))
        (struct
          type t = index
          let to_t ((push, pop), (top, bottom)) = {push; pop; top; bottom}
          let of_t {push; pop; top; bottom} = (push, pop), (top, bottom)
        end)

    (*
     * Type of node, which are elements manipulated by queue operations.
     * 'next' is the optional key of a next element in the queue,
     * 'previous' is the optional key of a previous element in the queue,
     * 'elt' is the optional key of a elt associated to the node.
    *)
    type node = {
      next    : K.t option;
      previous: K.t option;
      elt     : K.t option;
      branch  : index option;
    }

    module KO = Tc.Option (K)
    module Node = Tc.Biject
        (Tc.Pair(Tc.Pair(KO)(KO))(Tc.Pair(KO)(Tc.Option(Index))))
        (struct
          type t = node
          let to_t ((next, previous), (elt, branch)) =
            {next; previous; elt; branch}
          let of_t {next; previous; elt; branch} =
            (next, previous), (elt, branch)
        end)

    (*
     * Type of store elements.
    *)
    type t =
      | Index of Index.t
      | Node  of Node.t
      | Elt   of V.t
    with compare

    let equal_node n1 n2 =
      Node.compare n1 n2 = 0

    let to_json = function
      | Index i -> `O [ "index", Index.to_json i ]
      | Node n  -> `O [ "node" , Node.to_json n ]
      | Elt e   -> `O [ "elt"  , V.to_json e ]

    let of_json = function
      | `O [ "index", j ] -> Index (Index.of_json j)
      | `O [ "node" , j ] -> Node (Node.of_json j)
      | `O [ "elt"  , j ] -> Elt (V.of_json j)
      | j -> Ezjsonm.parse_error j "C.of_json"

    let equal x y = match x, y with
      | Index x, Index y -> Index.equal x y
      | Node x, Node y -> Node.equal x y
      | Elt x, Elt y -> V.equal x y
      | _ -> false

    let hash = Hashtbl.hash

    (* FIXME: slow *)
    let to_string t = Ezjsonm.to_string (to_json t)
    let of_string s = of_json (Ezjsonm.from_string s)
    let write t buf =
      let str = to_string t in
      let len = String.length str in
      Cstruct.blit_from_string str 0 buf 0 len;
      Cstruct.shift buf len
    let read buf =
      Mstruct.get_string buf (Mstruct.length buf)
      |> of_string
    let size_of t =
      let str = to_string t in
      String.length str

  end

  let (incr_read, incr_write, get_read, get_write) =
    let count_read = ref 0 in
    let count_write = ref 0 in
    (
      (fun () -> incr count_read),
      (fun () -> incr count_write),
      (fun () -> !count_read),
      (fun () -> !count_write)
    )

  module Store = struct

    module S = AO(K)(C)

    include S

    let create () =
      create Config.conf Config.task

    let read t k =
      incr_read ();
      S.read t k

    let read_exn t k =
      incr_read ();
      S.read_exn t k

    let read_free t k =
      S.read_exn t k

    let add t v =
      incr_write ();
      S.add t v

  end

  (*
   * Type of a queue.
   * 'index' is the index of the queue in its store,
   * 'root' is the key of the 'empty' element of store.
  *)
  type queue = {
    index: C.Index.t;
    root : K.t;
  }

  module T = Tc.Biject (Tc.Pair (C.Index)(K))
      (struct
        type t = queue
        let to_t (index, root) = {index; root}
        let of_t {index; root} = (index, root)
      end)
  include T

  type elt = V.t

  let empty = {
    C.next = None;
    C.previous = None;
    C.elt = None;
    C.branch = None;
  }

  (*
   * Create a new queue in the store 'store'.
   * 'top' and 'bottom' are pointed on the 'empty' node.
  *)
  let create () =
    Store.create () >>= fun store ->
    Store.add (store "create") (C.Node empty) >>= fun root ->
    let index = {
      C.push = 0;
      C.pop = 0;
      C.top = root;
      C.bottom = root;
    } in
    return { index; root }

  let length t =
    return (t.index.C.push - t.index.C.pop)

  let is_empty t =
    return (t.index.C.push = t.index.C.pop)

  (*
   * Queues are implemented with two lists,
   * the push list, containing pushed elements,
   * and the pop list, containing elements to be poped.
   * 'normalise' flush the push list into the pop one.
  *)
  let normalize q =
    Store.create () >>= fun store ->
    let index = q.index in
    let root = q.root in

    let apply k1 k2 queue old_node k_old_node k_new_node =
      k1 queue old_node k_old_node k_new_node k2
    in

    (*
     * Go through the pop list and call the continuation on the push list,
     * then rebuild it from its last element to its first element.
     * Not tail recursive.
    *)
    let rec from_top queue old_node k_old_node k_new_node k =

      (match old_node.C.next with
       | None -> (
           assert (C.equal_node old_node empty);
           k queue k_old_node k_new_node
         )
       | Some old_next -> (
           Store.read_exn (store "from_top: read") old_next >>= fun old_next ->
           match old_next with
           | C.Index _
           | C.Elt _ -> fail (Error `Corrupted)
           | C.Node old_next -> from_top queue old_next k_old_node k_new_node k
         )
      ) >>= fun new_next ->
      match old_node.C.elt with
      | None -> return new_next
      | Some elt -> (
          Store.add (store "from_top: add") (C.Node new_next) >>= fun new_key_node ->
          let new_node = {
            C.next = Some new_key_node;
            C.previous = None;
            C.elt = Some elt;
            C.branch = None;
          } in return new_node
        )
    in

    (*
     * Go through the push list rebuilding its elements, then call the continuation.
     * Tail recursive.
    *)
    let rec from_bottom queue old_node new_node =

      match old_node.C.branch with
      | Some index -> (
          Store.read_exn (store "from_bottom: old=index") index.C.top >>= fun branch_top ->
          match branch_top with
          | C.Index _
          | C.Elt _ -> fail (Error `Corrupted)
          | C.Node branch_top ->
            Store.read_exn (store "from_bottom: old=node")  index.C.bottom >>= fun branch_bottom ->
            match branch_bottom with
            | C.Index _
            | C.Elt _ ->  fail (Error `Corrupted)
            | C.Node branch_bottom ->
              let root = queue.root in
              let new_queue = {index; root} in
              apply from_top from_bottom new_queue branch_top
                branch_bottom new_node >>= fun node ->
              match old_node.C.previous with
              | None -> return new_node
              | Some old_previous -> (
                  Store.read_exn (store "from_bottom: old=node/previous") old_previous >>= fun old_previous ->
                  match old_previous with
                  | C.Index _
                  | C.Elt _ -> fail (Error `Corrupted)
                  | C.Node old_previous -> from_bottom queue old_previous node
                )
        )
      | None -> (
          match old_node.C.previous with
          | None -> (
              assert (C.equal_node old_node empty);
              return new_node;
            )
          | Some old_previous -> (
              Store.read_exn (store "from_bottom: old=None") old_previous >>= fun old_previous ->
              match old_previous with
              | C.Index _
              | C.Elt _ -> fail (Error `Corrupted)
              | C.Node old_previous -> (
                  Store.add (store "from_bottom: old=None/previous=node") (C.Node new_node) >>= fun key_node ->
                  let new_previous = {
                    C.next = Some key_node;
                    C.previous = None;
                    C.elt = old_node.C.elt;
                    C.branch = None;
                  } in from_bottom queue old_previous new_previous
                )
            )
        )
    in

    Store.read_exn (store "from_bottom") index.C.top >>= fun top_node ->
    match top_node with
    | C.Index _
    | C.Elt _ -> fail (Error `Corrupted)
    | C.Node top_node ->
      Store.read_exn (store "from_bottom: top=node") index.C.bottom >>= fun bottom_node ->
      match bottom_node with
      | C.Index _
      | C.Elt _ -> fail (Error `Corrupted)
      | C.Node bottom_node ->
        apply from_top from_bottom q top_node bottom_node empty >>= fun node ->
        Store.add (store "from_bottom: top=node/bottom=node") (C.Node node) >>= fun key_top ->
        let index = {
          C.push = index.C.push;
          C.pop = index.C.pop;
          C.top = key_top;
          C.bottom = root;
        } in
        return { index; root }

  (*
   * Add a new node in the push list, and move the index on.
   * The new index is NOT added in the store, ie the queue is NOT updated.
  *)
  let push q elt =

    Store.create () >>= fun store ->
    let index = q.index in
    let root = q.root in

    Store.add (store "push 1") (C.Elt elt) >>= fun key_elt ->
    let node = {
      C.next = None;
      C.previous = Some index.C.bottom;
      C.elt = Some key_elt;
      C.branch = None;
    } in
    Store.add (store "push 2") (C.Node node) >>= fun key_node ->
    let index = {
      C.push = index.C.push + 1;
      C.pop = index.C.pop;
      C.top = index.C.top;
      C.bottom = key_node;
    } in
    return { index; root }

  let push_branch q branch =

    Store.create () >>= fun store ->
    let index = q.index in
    let root = q.root in

    let node = {
      C.next = None;
      C.previous = Some index.C.bottom;
      C.elt = None;
      C.branch = Some branch;
    } in
    Store.add (store "push_branch") (C.Node node) >>= fun key_node ->
    let index = {
      C.push = index.C.push;
      C.pop = index.C.pop;
      C.top = index.C.top;
      C.bottom = key_node;
    } in
    return { index; root }

  (*
   * Move the index of the queue to the next element.
   * The new index is NOT added in the store, ie the queue is NOT updated.
   * Return None if the queue is empty.
  *)
  let rec pop q =

    Store.create () >>= fun store ->
    let index = q.index in
    let root = q.root in

    if index.C.push = index.C.pop then
      return None
    else
      Store.read_exn (store "pop 1") index.C.top >>= fun node ->
      match node with
      | C.Index _
      | C.Elt _ -> fail (Error `Corrupted)
      | C.Node node ->
        match node.C.elt with
        | None -> normalize q >>= fun q -> pop q
        | Some elt ->
          Store.read_exn (store "pop 2") elt >>= fun elt ->
          match elt with
          | C.Index _
          | C.Node _ -> fail (Error `Corrupted)
          | C.Elt elt ->
            let key = (match node.C.next with
                | None -> root
                | Some key -> key) in
            let index = {
              C.push = index.C.push;
              C.pop = index.C.pop + 1;
              C.top = key;
              C.bottom = index.C.bottom;
            } in

            return (Some (elt, { index; root }))

  (*
   * Move the index of the queue to the next element.
   * The new index is NOT added in the store, ie the queue is NOT updated.
   * Raise Empty if the queue is empty.
  *)
  let rec pop_exn q =

    Store.create () >>= fun store ->
    let index = q.index in
    let root = q.root in

    if index.C.push = index.C.pop then
      fail Empty
    else
      Store.read_exn (store "pop_exn") index.C.top >>= fun node ->
      match node with
      | C.Index _
      | C.Elt _ -> fail (Error `Corrupted)
      | C.Node node ->
        match node.C.elt with
        | None -> normalize q >>= fun q -> pop_exn q
        | Some elt ->
          Store.read_exn (store "pop_exn") elt >>= fun elt ->
          match elt with
          | C.Index _
          | C.Node _ -> fail (Error `Corrupted)
          | C.Elt elt ->
            let key = (match node.C.next with
                | None -> root
                | Some key -> key) in
            let index = {
              C.push = index.C.push;
              C.pop = index.C.pop + 1;
              C.top = key;
              C.bottom = index.C.bottom;
            } in

            return (elt, { index; root })

  (*
   * Read the elt associated to the top node of the queue.
   * Return None if the queue is empty.
  *)
  let rec peek q =

    Store.create () >>= fun store ->
    let index = q.index in

    if index.C.push = index.C.pop then
      return None
    else
      Store.read_exn (store "peek 1") index.C.top >>= fun node ->
      match node with
      | C.Index _
      | C.Elt _ -> fail (Error `Corrupted)
      | C.Node node ->
        match node.C.elt with
        | None -> normalize q >>= fun q -> peek q
        | Some elt ->
          Store.read_exn (store "peek 2") elt >>= fun elt ->
          match elt with
          | C.Index _
          | C.Node _ -> fail (Error `Corrupted)
          | C.Elt elt ->
            return (Some (elt, q))

  (*
   * Read the elt associated to the top node of the queue.
   * Raise Empty if the queue is empty.
  *)
  let rec peek_exn q =

    Store.create () >>= fun store ->
    let index = q.index in

    if index.C.push = index.C.pop then
      raise Empty
    else
      Store.read_exn (store "peek_exn 1") index.C.top >>= fun node ->
      match node with
      | C.Index _
      | C.Elt _ -> fail (Error `Corrupted)
      | C.Node node ->
        match node.C.elt with
        | None -> normalize q >>= fun q -> peek_exn q
        | Some elt ->
          Store.read_exn (store "peek_exn 2") elt >>= fun elt ->
          match elt with
          | C.Index _
          | C.Node _ -> fail (Error `Corrupted)
          | C.Elt elt ->
            return (elt, q)

  let dump q =

    Store.create () >>= fun store ->

    let rec from_top queue node =
      match node.C.next with
      | None -> (
          match node.C.elt with
          | None -> return (Printf.sprintf (if C.equal_node node empty then "Empty%!"
                                            else "None%!"))
          | Some key ->
            Store.read_free (store "from_top 1")  key >>= fun elt ->
            return (Printf.sprintf "Some %s%!" (C.to_string elt))
        )
      | Some next -> (
          Store.read_free (store "from_top 2") next >>= fun next ->
          match next with
          | C.Index _
          | C.Elt _ -> assert false
          | C.Node next -> (
              from_top queue next >>= fun string ->
              match node.C.elt with
              | None ->return (Printf.sprintf "None -> %s%!" string)
              | Some elt ->
                Store.read_free (store "from_top 3") elt >>= fun elt ->
                return (Printf.sprintf "Some %s -> %s%!" (C.to_string elt) string)
            )
        )
    in

    let rec from_bottom queue node =
      match node.C.previous with
      | None -> (
          match node.C.elt with
          | None -> return (Printf.sprintf (if C.equal_node node empty then "Empty%!"
                                            else "None%!"))
          | Some key ->
            Store.read_free (store "from_bottom 1")  key >>= fun elt ->
            return (Printf.sprintf "Some %s%!" (C.to_string elt))
        )
      | Some previous -> (
          Store.read_free (store "from_bottom 2") previous >>= fun previous ->
          match previous with
          | C.Index _
          | C.Elt _ -> assert false
          | C.Node previous -> (
              from_bottom queue previous >>= fun string ->
              match node.C.elt with
              | None ->return (Printf.sprintf "None -> %s%!" string)
              | Some elt ->
                Store.read_free (store "from_bottom 3") elt >>= fun elt ->
                return (Printf.sprintf "Some %s -> %s%!" (C.to_string elt) string)
            )
        )
    in

    Store.read_free (store "from_bottom 4") q.index.C.top >>= fun top ->
    match top with
    | C.Index _
    | C.Elt _ -> assert false
    | C.Node top ->
      from_top q top >>= fun string_top ->
      Store.read_free (store "from_bottom 5") q.index.C.bottom >>= fun bottom ->
      match bottom with
      | C.Index _
      | C.Elt _ -> assert false
      | C.Node bottom ->
        from_bottom q bottom >>= fun string_bottom ->
        let string =
          Printf.sprintf "push: %i, pop: %i\nfrom top: %s\nfrom bottom: %s\n\n%!"
            q.index.C.push q.index.C.pop string_top string_bottom;
        in return string

  let stats q =
    let ops = q.index.C.push + q.index.C.pop in
    let reads = get_read () in
    let writes = get_write () in
    { ops; reads; writes }

  let merge: Path.t -> t option Irmin.Merge.t =

    let rec clean old q =
      if old.index.C.push > q.index.C.pop
      && q.index.C.push > q.index.C.pop then
        pop_exn q >>= fun (_, q) -> clean old q
      else return q
    in

    let rec equalize old q1 q2 =
      if K.(q1.index.C.top = q2.index.C.top
            && q1.index.C.bottom = q2.index.C.bottom)
      then
        create () >>= fun q2 -> return (q1, q2)
      else (
        if q2.index.C.pop > q1.index.C.pop
        && old.index.C.push > q1.index.C.pop
        && q1.index.C.push > q1.index.C.pop
        then
          pop_exn q1 >>= fun (_, q1) -> equalize old q1 q2
        else clean old q2 >>= fun q2 -> return (q1, q2))
    in

    let merge ~old q1 q2 =
      old () >>= function  (* FIXME *)
      | `Conflict _ | `Ok None -> conflict "merge"
      | `Ok (Some old) ->
        assert K.(q1.root = q2.root && old.root = q1.root);
        let root = q1.root in
        equalize old q1 q2 >>= fun (q1, q2) ->
        (if q2.index.C.push > q2.index.C.pop then
           push_branch q1 q2.index
         else return q1) >>= fun q ->
        let index = {
          C.push = q1.index.C.push + q2.index.C.push - old.index.C.push;
          C.pop = q1.index.C.pop + q2.index.C.pop - old.index.C.push;
          C.top = q.index.C.top;
          C.bottom = q.index.C.bottom;
        } in
        ok {index; root}
    in

    fun _path -> Irmin.Merge.option (module T) merge

  (*
   * Return all the keys (index, node or value) that are accessible.
   * Returned keys may be associated to unreadable value!
   * Should be only use by the GC.
  *)
  let list q list =

    Store.create () >>= fun store ->

    let add list = function
      | None -> list
      | Some opt -> opt :: list
    in

    let rec iter tmp_list res_list = match tmp_list with
      | [] -> return res_list
      | key :: tmp_list ->
        Store.read_exn (store "iter") key >>= fun value ->
        match value with
        | C.Elt _       -> fail (Error `Invalid_access)
        | C.Index index -> iter
                             (index.C.top :: index.C.bottom :: tmp_list)
                             (index.C.top :: index.C.bottom ::res_list)
        | C.Node node ->
          let res_list = add res_list node.C.next in
          let res_list = add res_list node.C.previous in
          let res_list = add res_list node.C.elt in
          let tmp_list = add tmp_list node.C.next in
          let tmp_list = add tmp_list node.C.previous in
          match node.C.branch with
          | None -> iter tmp_list res_list
          | Some index -> iter
                            (index.C.top :: index.C.bottom :: tmp_list)
                            (index.C.top :: index.C.bottom ::res_list)
    in

    iter list (q.root::list) >>= fun list ->
    return (list_dedup list)

end
