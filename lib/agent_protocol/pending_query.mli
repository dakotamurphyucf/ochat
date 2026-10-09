(** Authorized nonactivating inspection of pending input. The host enforces
    current session/transcript visibility and page bounds before reading. Private
    stored ownership, custody and raw operation errors are never projected. *)
module Request : sig
  type t = private
    { session_id : Id.Session.t
    ; page : Page.Request.t
    }
  [@@deriving sexp]

  val create : session_id:Id.Session.t -> page:Page.Request.t -> (t, Error.t) result
  val to_json : t -> Jsonaf.t
  val of_json : Jsonaf.t -> (t, Error.t) result
end

module Lookup_request : sig
  type t =
    { session_id : Id.Session.t
    ; history_id : History.Id.t
    }
  [@@deriving sexp]

  val to_json : t -> Jsonaf.t
  val of_json : Jsonaf.t -> (t, Error.t) result
end

module Retirement_reason : sig
  type t =
    | Source_reset
    | Source_replaced
    | Canonical_history_retired
  [@@deriving equal, sexp]
end

module Item : sig
  type t = private
    { history : Public_history.t
    ; generation : int
    ; binding : Pending_input.Binding.t
    }
  [@@deriving sexp]

  (** Projection of the known timing and an already authorized public history
      occurrence. Never grants permission to reconstruct canonical input. *)
  val create
    :  history:Public_history.t
    -> generation:int
    -> binding:Pending_input.Binding.t
    -> (t, Error.t) result

  val to_json : t -> Jsonaf.t
  val of_json : Jsonaf.t -> (t, Error.t) result
end

module Outcome : sig
  (** [Adopted.current=None] means the retained adoption is known but no current
      canonical occurrence is available. It never claims the input was rejected.
      [Unavailable] includes expired disposition evidence and unknown identity;
      it is never permission to replay the original submission. *)
  type t = private
    | Pending of Item.t
    | Adopted of
        { history_id : History.Id.t
        ; admitted_content_revision : History.Content_revision.t
        ; current : Public_history.t option
        }
    | Cancelled of History.Id.t
    | Retired of History.Id.t * Retirement_reason.t
    | Unavailable of History.Id.t
  [@@deriving sexp]

  val pending : Item.t -> t

  val adopted
    :  history_id:History.Id.t
    -> admitted_content_revision:History.Content_revision.t
    -> current:Public_history.t option
    -> (t, Error.t) result

  val cancelled : History.Id.t -> t
  val retired : History.Id.t -> reason:Retirement_reason.t -> t
  val unavailable : History.Id.t -> t
  val history_id : t -> History.Id.t
  val to_json : t -> Jsonaf.t
  val of_json : Jsonaf.t -> (t, Error.t) result
end

module View : sig
  type t = private
    { pending_revision : Pending_input.Revision.t
    ; page : Item.t Page.t
    }
  [@@deriving sexp]

  val create
    :  pending_revision:Pending_input.Revision.t
    -> page:Item.t Page.t
    -> (t, Error.t) result

  val to_json : t -> Jsonaf.t
  val of_json : Jsonaf.t -> (t, Error.t) result
end
