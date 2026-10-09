(** Durable host run lifecycle. Session liveness and work execution are separate;
    successful finish does not stop a session or terminate unrelated jobs. *)
module Mode : sig
  type t =
    | Single_turn
    | Workflow
  [@@deriving compare, equal, sexp]

  val to_json : t -> Jsonaf.t
  val of_json : Jsonaf.t -> (t, Error.t) result
end

module Terminal : sig
  type t =
    | Completed of Run_result_reference.t option
    | Failed of Error.code option
    | Cancelled
    | Limited
    | Interrupted
  [@@deriving equal, sexp]

  val validate : t -> (unit, Error.t) result
  val to_json : t -> Jsonaf.t
  val of_json : Jsonaf.t -> (t, Error.t) result
end

module Lifecycle : sig
  type t =
    | Admitted
    | Active
    | Waiting of Run_wake.t
    | Terminal of Terminal.t
  [@@deriving equal, sexp]

  val to_json : t -> Jsonaf.t
  val of_json : Jsonaf.t -> (t, Error.t) result
end

type t = private
  { id : Id.Run.t
  ; session : Session_ref.t
  ; principal_id : Id.Principal.t
  ; source : Run_source.t
  ; mode : Mode.t
  ; lifecycle : Lifecycle.t
  ; revision : int64
  ; owned_work : Run_work.t list
  ; relinquished_work : Run_work.t list
  ; terminal_work : Run_work.Terminal.t list
  ; created_at : Timestamp.t
  ; updated_at : Timestamp.t
  }
[@@deriving equal, sexp]

(** Finite record/ownership bounds; duplicates and contradictory ownership fail.
    Decoders and sexp admission share constructor validation. *)
val create
  :  id:Id.Run.t
  -> session:Session_ref.t
  -> principal_id:Id.Principal.t
  -> source:Run_source.t
  -> mode:Mode.t
  -> lifecycle:Lifecycle.t
  -> revision:int64
  -> owned_work:Run_work.t list
  -> relinquished_work:Run_work.t list
  -> terminal_work:Run_work.Terminal.t list
  -> created_at:Timestamp.t
  -> updated_at:Timestamp.t
  -> (t, Error.t) result

val validate : t -> (unit, Error.t) result
val to_json : t -> Jsonaf.t
val of_json : Jsonaf.t -> (t, Error.t) result

(** Immutable identity/source/mode and terminal receipts; revision advances once.
    Prior terminal work/disposition evidence cannot disappear or change, including
    when an underlying job advances to a later attempt. *)
val validate_transition : previous:t -> t -> (unit, Error.t) result
