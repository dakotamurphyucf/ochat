(** One ordered window and its source-located members. *)
open! Core

module P = Agent_protocol
module Member = Recovery_window_location.Member

val max_members : int

type origin =
  | Root
  | Successor of
      { previous : P.Id.Context_window.t
      ; operation : P.Id.Operation.t
      }
  | New_generation of { previous : P.Id.Context_window.t }
[@@deriving sexp_of]

type state = P.Recovery_query.State.t =
  | Open
  | Sealed
  | Expired
[@@deriving equal, sexp_of]

type t

val create
  :  P.Recovery_window.Scope.t
  -> origin:origin
  -> state:state
  -> Member.t list
  -> (t, P.Error.t) result

val manifest : t -> P.Recovery_window.t
val origin : t -> origin
val state : t -> state

(** Permanently expire a sealed window, retaining exact immutable membership. *)
val expire : t -> (t, P.Error.t) result

val length : t -> int
val has_withdrawals : t -> bool
val at : t -> ordinal:int -> Member.t option
val scope : t -> P.Recovery_window.Scope.t
val id : t -> P.Id.Context_window.t

val fold_members
  :  t
  -> init:'a
  -> f:('a -> Member.t -> ('a, 'error) Result.t)
  -> ('a, 'error) Result.t

val to_jsonaf : t -> Jsonaf.t
val of_jsonaf : P.Session_ref.t -> Jsonaf.t -> (t, P.Error.t) result
val shape : Document_schema.Shape.t
