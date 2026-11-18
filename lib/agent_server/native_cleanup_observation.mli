open! Core
module P = Agent_protocol
module CO = Agent_session.Close_observation

(** Actual native cleanup boundary observations only. This immutable transport
    owns no resource, executor or cleanup permission. *)
type 'a t = ('a, P.Error.t) result CO.t

val return : 'a -> 'a t
val capture : (unit -> ('a, P.Error.t) result) -> 'a t

(** Forward supplied actual carriers. An escaping invocation, or an actual
    negative with no meaningful selected carrier, owns one boundary diagnostic.
    Missing custody/completion evidence is never manufactured here. *)
val capture_observed : (unit -> 'a t) -> 'a t

val is_negative : 'a t -> bool

(** Both operations have already executed. Select the first negative raw result,
    retaining all SAME carriers in order. Positive results have no primary. *)
val combine : unit t -> unit t -> unit t
