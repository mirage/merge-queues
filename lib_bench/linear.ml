open Lwt
open Core_kernel.Std
open Irmin_unix

module Int   = IrminIdent.Int
module Git   = IrminGit.Memory
module Queue = MQueue.Make(Git.AO)(IrminKey.SHA1)(Int)

type action =
  | Push
  | Pop

let () =
  Random.self_init ()

let (incr_count, get_count) =
  let count = ref 0 in
  (
    (fun () -> incr count),
    (fun () -> !count)
  )


let (incr_push, incr_pop, get_push, get_pop) =
  let push = ref 0 in
  let pop = ref 0 in
  (
    (fun () -> incr push),
    (fun () -> incr pop),
    (fun () -> !push),
    (fun () -> !pop)
  )

let choose_action lambda push pop =
  let p = exp (-. (float(push - pop) /. float(lambda))) in
  if Random.float 1. < p then Push else Pop

let rec clean q =
  Queue.is_empty q >>= fun b ->
  if b then return q
  else
    Queue.pop_exn q >>= fun (_, q) ->
    clean q

let rec iter queue lambda max =
  let count = get_count () in
  let push = get_push () in
  let pop = get_pop () in
  if count < max then (
    match choose_action lambda push pop with
    | Push ->
      incr_count ();
      incr_push ();
      Queue.push queue count >>= fun queue ->
      iter queue lambda max
    | Pop ->
      incr_pop ();
      Queue.pop_exn queue >>= fun (_, queue) ->
      iter queue lambda max
  )
  else
    return queue


let error () =
  eprintf "usage: queue <lambda> <number>\n";
  exit 1


let main () =
  let argc = Array.length Sys.argv in
  if argc < 3  then
    error ()
  else (

    let lambda = int_of_string(Sys.argv.(1)) in
    let number = int_of_string(Sys.argv.(2)) in

    Queue.create ()          >>= fun queue ->
    iter queue lambda number >>= fun queue ->
    clean queue              >>= fun queue ->
    let stats = Queue.stats queue in
    print_endline (MQueue.string_of_stats stats);
    return ()
  )


let () =
  Lwt_unix.run (main ())
