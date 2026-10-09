(** Current-authority, nonactivating shared run inspection. Full work and receipt
    views require transcript/security scopes plus session visibility and either
    the original run principal or actual session administrator authority. *)
module Request : sig
  type t = private
    { session : Session_ref.t
    ; page : Page.Request.t
    }

  val create : session:Session_ref.t -> page:Page.Request.t -> (t, Error.t) result
  val sexp_of_t : t -> Sexplib0.Sexp.t
  val t_of_sexp : Sexplib0.Sexp.t -> t
  val to_json : t -> Jsonaf.t
  val of_json : Jsonaf.t -> (t, Error.t) result
end

module Lookup_request : sig
  type t =
    { session : Session_ref.t
    ; run_id : Id.Run.t
    }

  val sexp_of_t : t -> Sexplib0.Sexp.t
  val t_of_sexp : Sexplib0.Sexp.t -> t
  val to_json : t -> Jsonaf.t
  val of_json : Jsonaf.t -> (t, Error.t) result
end

module View : sig
  type t = private
    { run : Run.t
    ; result_references : Run_result_reference.t list
    ; pending_action : Run_action.t option
    ; admission_receipt : Run_receipt.t option
    ; terminal_receipt : Run_receipt.t option
    ; session_revision : int64
    ; event_sequence : int64
    }

  (** Correlates every receipt/action with immutable run/source identity. A
      completed terminal result reference remains evidence when its content has
      expired; lookup never rewrites completion to a missing-result failure. *)
  val create
    :  run:Run.t
    -> result_references:Run_result_reference.t list
    -> pending_action:Run_action.t option
    -> admission_receipt:Run_receipt.t option
    -> terminal_receipt:Run_receipt.t option
    -> session_revision:int64
    -> event_sequence:int64
    -> (t, Error.t) result

  val sexp_of_t : t -> Sexplib0.Sexp.t
  val t_of_sexp : Sexplib0.Sexp.t -> t
  val to_json : t -> Jsonaf.t
  val of_json : Jsonaf.t -> (t, Error.t) result
end

module Outcome : sig
  (** Unknown/expired or foreign-principal evidence is unavailable, never a claim
      that the host terminated a run. Session visibility failures remain errors. *)
  type t = private
    | Available of View.t
    | Unavailable of Id.Run.t

  val available : View.t -> t
  val unavailable : Id.Run.t -> t
  val sexp_of_t : t -> Sexplib0.Sexp.t
  val t_of_sexp : Sexplib0.Sexp.t -> t
  val to_json : t -> Jsonaf.t
  val of_json : Jsonaf.t -> (t, Error.t) result
end
