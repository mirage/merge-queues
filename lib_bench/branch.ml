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

let (>>=) = Lwt.bind

module Git = Irmin_git.AO(Git.Memory)
module Config = struct
  let conf = Irmin_git.config ()
  let task = Irmin_unix.task
end
module Path = Irmin.Path.String_list
module Queue = Merge_queue.Make(Git)(Irmin.Hash.SHA1)(Tc.Int)(Path)(Config)

type choice = Top | Bot

let () =
  Random.self_init ()

let get_val =
  let c = ref (-1) in
  (fun () -> incr c; !c)

let choose lambda x =
  let p = exp (-. (float(x) /. float(lambda))) in
  if Random.float 1. < p then Top else Bot

let rec clean q =
  Queue.is_empty q >>= fun b ->
  if b then Lwt.return q
  else
    Queue.pop_exn q >>= fun (_, q) ->
    clean q

let rec filling queue lambda count push pop =
  match choose lambda count with
  | Top -> (
      match choose lambda (push - pop) with
      | Top ->(
          Queue.push queue (get_val ()) >>= fun queue ->
          filling queue lambda (count + 1) (push + 1) pop
        )
      | Bot -> (
          Queue.pop_exn queue >>= fun (_, queue) ->
          filling queue lambda count push (pop + 1)
        )
    )
  | Bot -> Lwt.return (queue, count, push, pop)

and branching queue lambda mu nu push pop branch depth =
  building queue lambda mu nu push pop branch (depth + 1) >>= fun q1 ->
  match choose mu branch with
  | Top -> (
      branching queue lambda mu nu push pop (branch + 1) depth >>= fun q2 ->
      let old = Irmin.Merge.promise (Some queue) in
      Queue.merge [] ~old (Some q1) (Some q2) >>= fun res ->
      match res with
      | `Conflict s  -> failwith s
      | `Ok None     -> failwith "none"
      | `Ok (Some q) -> Lwt.return q
    )
  | Bot -> Lwt.return q1

and building queue lambda mu nu push pop branch depth =
  filling queue lambda 0 push pop >>= fun (queue, count, push, pop) ->
  match choose nu depth with
  | Top -> branching queue lambda mu nu push pop branch depth
  | Bot -> Lwt.return queue

let error () =
  Printf.eprintf "usage: queue <lambda> <mu> <nu>\n";
  exit 1

let main () =
  let argc = Array.length Sys.argv in
  if argc < 4  then
    error ()
  else (

    let lambda = int_of_string(Sys.argv.(1)) in
    let mu = int_of_string(Sys.argv.(2)) in
    let nu = int_of_string(Sys.argv.(3)) in

    Queue.create ()                     >>= fun queue ->
    building queue lambda mu nu 0 0 0 0 >>= fun queue ->
    clean queue                         >>= fun queue ->
    Queue.dump queue                    >>= fun string ->
    print_endline string;
    Lwt.return ()
  )

let () =
  Lwt_unix.run (main ())
