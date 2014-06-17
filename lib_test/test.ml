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
open Core_kernel.Std

module Int   = IrminIdent.Int
module Git   = IrminGit.Memory
module Queue = MQueue.Make(Git.AO)(IrminKey.SHA1)(Int)
module Store = Git.Make(IrminKey.SHA1)(Queue)(IrminTag.String)

type choice =
  | Top
  | Bot

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
  if b then return q
  else
    Queue.pop_exn q >>= fun (_, q) ->
    clean q

let assert_queue q_old q1 q2 q_merge =

  let rec prepare_old_merge q_old q1 q2 q_merge q_tmp =
    (*print_endline "prepare_old_merge";*)
    Queue.pop q_tmp >>= fun opt_tmp ->
    Queue.pop q_merge >>= fun opt_merge ->
    match (opt_tmp, opt_merge) with
    | None, None -> (
        Queue.is_empty q1 >>= fun b1 ->
        Queue.is_empty q2 >>= fun b2 ->
        OUnit.assert_bool "prepare_old_merge" (b1 && b2);
        return ()
      )
    | None, Some (a_merge, q_merge') -> (
        prepare_old_q1_q2 q_old q1 q2 q_merge
      )
    | Some (a_tmp, q_tmp'), None -> (
        prepare_old_q1_q2 q_old q1 q2 q_merge
      )
    | Some (a_tmp, q_tmp'), Some (a_merge, q_merge') -> (
        if (a_tmp = a_merge) then
          prepare_old_merge_continue q_old q1 q2 q_merge' q_tmp'
        else
          prepare_old_merge q_old q1 q2 q_merge q_tmp'
      )

  and prepare_old_merge_continue q_old q1 q2 q_merge q_tmp =
    (*print_endline "prepare_old_merge_continue";*)
    Queue.pop q_tmp >>= fun opt_tmp ->
    Queue.pop q_merge >>= fun opt_merge ->
    match (opt_tmp, opt_merge) with
    | None, None -> (
        Queue.is_empty q1 >>= fun b1 ->
        Queue.is_empty q2 >>= fun b2 ->
        OUnit.assert_bool "prepare_old_merge_continue" (b1 && b2);
        return ()
      )
    | None, Some (a_merge, q_merge') -> (
        prepare_old_q1_q2 q_old q1 q2 q_merge
      )
    | Some (a_tmp, q_tmp'), None -> (
        prepare_old_q1_q2 q_old q1 q2 q_merge
      )
    | Some (a_tmp, q_tmp'), Some (a_merge, q_merge') -> (
        if (a_tmp = a_merge) then
          prepare_old_merge_continue q_old q1 q2 q_merge' q_tmp'
        else
          prepare_old_q1_q2 q_old q1 q2 q_merge
      )

  and prepare_old_q1_q2 q_old q1 q2 q_merge =
    (*print_endline "prepare_old_q1_q2";*)
    Queue.pop q_old >>= fun opt_old ->
    Queue.pop q1 >>= fun opt1 ->
    Queue.pop q2 >>= fun opt2 ->
    match (opt_old, opt1, opt2) with
    | None, None, None -> (
        Queue.is_empty q_merge >>= fun b ->
        OUnit.assert_bool "prepare_old_q1_q2 (1)" b;
        return ()
      )
    | None, None, Some (a2, q2') -> (
        compare_q2_merge q2 q_merge
      )
    | None, Some (a1, q1'), None -> (
        compare_q1_merge q1 q_merge
      )
    | None, Some (a1, q1'), Some (a2, q2') -> (
        compare_q1_q2_merge q1 q2 q_merge
      )
    | Some (a_old, q_old'), None, None -> (
        Queue.is_empty q_merge >>= fun b ->
        OUnit.assert_bool "prepare_old_q1_q2 (2)" b;
        return ()
      )
    | Some (a_old, q_old'), None, Some (a2, q2') -> (
        if (a_old = a2) then
          prepare_old_q2 q_old' q1 q2' q_merge
        else
          compare_q2_merge q2 q_merge
      )
    | Some (a_old, q_old'), Some (a1, q1'), None -> (
        if (a_old = a1) then
          prepare_old_q1 q_old' q1' q2 q_merge
        else
          compare_q1_merge q1 q_merge
      )
    | Some (a_old, q_old'), Some (a1, q1'), Some (a2, q2') -> (
        match (a_old = a1, a_old = a2) with
        | false, false -> prepare_old_q1_q2 q_old' q1 q2 q_merge
        | false, true -> prepare_old_q1_q2 q_old' q1 q2' q_merge
        | true, false -> prepare_old_q1_q2 q_old' q1' q2 q_merge
        | true, true -> prepare_old_q1_q2 q_old' q1' q2' q_merge
      )

  and prepare_old_q1 q_old q1 q2 q_merge =
    (*print_endline "prepare_old_q1";*)
    Queue.pop q_old >>= fun opt_old ->
    Queue.pop q1 >>= fun opt1 ->
    match (opt_old, opt1) with
    | None, None -> (
        Queue.is_empty q_merge >>= fun b ->
        OUnit.assert_bool "prepare_old_q1 (1)" b;
        return ()
      )
    | None, Some (a1, q1') -> (
        compare_q1_q2_merge q1 q2 q_merge
      )
    | Some (a_old, q_old'), None -> (
        Queue.is_empty q_merge >>= fun b ->
        OUnit.assert_bool "prepare_old_q1 (2)" b;
        return ()
      )
    | Some (a_old, q_old'), Some (a1, q1') -> (
        OUnit.assert_bool "prepare_old_q1 (3)" (a_old = a1);
        prepare_old_q1 q_old' q1' q2 q_merge
      )

  and prepare_old_q2 q_old q1 q2 q_merge =
    (*print_endline "prepare_old_q2";*)
    Queue.pop q_old >>= fun opt_old ->
    Queue.pop q2 >>= fun opt2 ->
    match (opt_old, opt2) with
    | None, None -> (
        Queue.is_empty q_merge >>= fun b ->
        OUnit.assert_bool "prepare_old_q2 (1)" b;
        return ()
      )
    | None, Some (a2, q2') -> (
        compare_q1_q2_merge q1 q2 q_merge
      )
    | Some (a_old, q_old'), None -> (
        Queue.is_empty q_merge >>= fun b ->
        OUnit.assert_bool "prepare_old_q2 (2)" b;
        return ()
      )
    | Some (a_old, q_old'), Some (a2, q2') -> (
        OUnit.assert_bool "prepare_old_q2 (3)" (a_old = a2);
        prepare_old_q2 q_old' q1 q2' q_merge
      )

  and compare_q1_q2_merge q1 q2 q_merge =
    (*print_endline "compare_q1_q2_merge";*)
    Queue.pop q1 >>= fun opt1 ->
    Queue.pop q2 >>= fun opt2 ->
    Queue.pop q_merge >>= fun opt_merge ->
    match (opt1, opt2, opt_merge) with
    | None, None, None -> return ()
    | None, None, Some (a_merge, q_merge') ->
      OUnit.assert_failure "compare_q1_q2_merge (1)"
    | None, Some (a2, q2'), None ->
      OUnit.assert_failure "compare_q1_q2_merge (2)"
    | None, Some (a2, q2'), Some (a_merge, q_merge') ->
      OUnit.assert_bool "compare_q1_q2_merge (3)" (a2 = a_merge);
      compare_q2_merge q2' q_merge'
    | Some (a1, q1'), None, None ->
      OUnit.assert_failure "compare_q1_q2_merge (4)"
    | Some (a1, q1'), None, Some (a_merge, q_merge') ->
      OUnit.assert_bool "compare_q1_q2_merge" (a1 = a_merge);
      compare_q1_merge q1' q_merge'
    | Some (a1, q1'), Some (a2, q2'), None ->
      OUnit.assert_failure "compare_q1_q2_merge (5)"
    | Some (a1, q1'), Some (a2, q2'), Some (a_merge, q_merge') ->
      OUnit.assert_bool "compare_q1_q2_merge" (a1 = a_merge);
      compare_q1_q2_merge q1' q2 q_merge'

  and compare_q1_merge q1 q_merge =
    (*print_endline "compare_q1_merge";*)
    Queue.pop q1 >>= fun opt1 ->
    Queue.pop q_merge >>= fun opt_merge ->
    match (opt1, opt_merge) with
    | None, None -> return ()
    | None, Some (a_merge, q_merge') ->
      OUnit.assert_failure "compare_q1_merge (1)"
    | Some (a1, q1'), None ->
      OUnit.assert_failure "compare_q1_merge (2)"
    | Some (a1, q1'), Some (a_merge, q_merge') ->
      OUnit.assert_bool "compare_q1_merge (3)" (a1 = a_merge);
      compare_q1_merge q1' q_merge'

  and compare_q2_merge q2 q_merge=
    (*print_endline "compare_q2_merge";*)
    Queue.pop q2 >>= fun opt2 ->
    Queue.pop q_merge >>= fun opt_merge ->
    match (opt2, opt_merge) with
    | None, None -> return ()
    | None, Some (a_merge, q_merge') ->
      OUnit.assert_failure "compare_q2_merge (1)"
    | Some (a2, q2'), None ->
      OUnit.assert_failure "compare_q2_merge (2)"
    | Some (a2, q2'), Some (a_merge, q_merge') ->
      OUnit.assert_bool "compare_q2_merge" (a2 = a_merge);
      compare_q2_merge q2' q_merge'
  in
  prepare_old_merge q_old q1 q2 q_merge q_old

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
  | Bot -> return (queue, count, push, pop)

and branching queue lambda mu nu push pop branch depth =
  building queue lambda mu nu push pop branch (depth + 1) >>= fun q1 ->
  match choose mu branch with
  | Top -> (
      branching queue lambda mu nu push pop (branch + 1) depth >>= fun q2 ->
      let origin = IrminOrigin.create "%i%i%i%i" push pop branch depth in
      IrminMerge.merge Queue.merge ~origin ~old:queue q1 q2 >>= function
      | `Conflict s -> OUnit.assert_failure s
      | `Ok merge_q -> (
          assert_queue queue q1 q2 merge_q >>= fun () ->
          return merge_q
        )
    )
  | Bot -> return q1

and building queue lambda mu nu push pop branch depth =
  filling queue lambda 0 push pop >>= fun (queue, count, push, pop) ->
  match choose nu depth with
  | Top -> branching queue lambda mu nu push pop branch depth
  | Bot -> return queue

let make lambda mu nu =
  Queue.create ()                     >>= fun queue ->
  building queue lambda mu nu 0 0 0 0 >>= fun _ ->
  return ()

let () =
  let suite = [
    `Quick, 10 , 10 , 4;
    `Quick, 100, 10,  3;
    `Slow , 100, 10, 10;
  ] in
  let test_cases =
    List.map ~f:(fun (q, lambda, mu, nu) ->
        Printf.sprintf "lambda=%d mu=%d nu=%d" lambda mu nu,
        q,
        fun () -> Lwt_unix.run (make lambda mu nu)
      ) suite in
  Alcotest.run "mqueue" [ "GIT-MEMORY", test_cases ]
