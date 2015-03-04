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

(** An efficient implementation of mergeable queues. *)

type stats = {
  ops: int;
  reads     : int;
  writes    : int;
}
(** Statistic values. *)

val string_of_stats: stats -> string
(** Pretty-print the stats. *)

exception Empty
(** Empty queue. *)

type error = [ `Corrupted | `Invalid_access ]
(** Internal errors. *)

exception Error of error
(** Internal errors. *)

module type S = sig

  include Irmin.Contents.S
  (** The type of queues. *)

  type elt
  (** The elements of the queues. *)

  val create : unit -> t Lwt.t
  (** Create a new queue. *)

  val length : t -> int Lwt.t
  (** Return the length of the queue [t]. *)

  val is_empty : t -> bool Lwt.t
  (** Return true if the given queue [t] is empty, false
      otherwise. *)

  val push : t -> elt -> t Lwt.t
  (** Returns a queue with adds [value] to the end of
      [t]. Complexity: O(1). *)

  val pop_exn : t -> (elt * t) Lwt.t
  (** Returns the top element and a version of the queue where it is
      removed. Raise [Empty] if the queue is empty. Complexity:
      amortized O(1). *)

  val pop : t -> (elt * t) option Lwt.t
  (** Like pop_exn, but returns result optionally, without
      exception. Complexity: amortized O(1). *)

  val peek_exn : t -> (elt * t) Lwt.t
  (** Returns the top element and a version of the queue updated but
      inchanged. Raises [Empty] if no element is found. Complexity:
      O(1). *)

  val peek : t -> (elt * t) option Lwt.t
  (** Like pop_exn, but returns result optionally, without
      exception. Complexity: O(1). *)

  val dump : t -> string Lwt.t
  (** Dump the contents of the queue. *)

  val stats : t -> stats
  (** Print global statistics on queue operations.  *)

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
    (C: Config)
  : S with type elt = V.t
       and module Path = P
