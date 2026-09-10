(** Presentation-neutral canonical and moderated transcript projections.
    Entry equality preserves exact JSON payload structure and object field order. *)

type delivery_id = Id.Delivery.t [@@deriving equal, sexp]

module Id : sig
  type t = History_entry.Id.t [@@deriving compare, equal, hash, sexp]

  val of_string : string -> (t, Error.t) result
  val to_string : t -> string
  val to_json : t -> Jsonaf.t
  val of_json : Jsonaf.t -> (t, Error.t) result
end

type role =
  | System
  | User
  | Assistant
  | Tool
[@@deriving compare, equal, sexp]

type kind =
  | Message
  | Reasoning
  | Tool_call
  | Tool_output
  | Other
[@@deriving compare, equal, sexp]

type provenance =
  | Canonical
  | Moderator_inserted
  | Moderator_replaced of Id.t
  | Runtime_notification of delivery_id
  | Runtime_authoring of Authoring_guidance.t
[@@deriving equal, sexp]

type entry =
  { id : Id.t
  ; role : role
  ; kind : kind
  ; payload : Jsonaf.t
  ; provenance : provenance
  ; redacted : bool
  }
[@@deriving equal, sexp]

val entry_to_json : entry -> Jsonaf.t

(** Validate host provenance metadata. This does not decode a provider item or
    claim the original guidance payload is still present; use the presence hook
    after applying effective-history edits to determine that. *)
val validate_entry : entry -> (unit, Error.t) result

val entry_of_json : Jsonaf.t -> (entry, Error.t) result

module Window_request : sig
  type position =
    | Tail of int
    | After of Id.t
    | Before of Id.t
    | Cursor of Page.Cursor.t
  [@@deriving sexp]

  type t =
    { position : position
    ; limit : int
    ; effective : bool
    }
  [@@deriving sexp]

  val to_json : t -> Jsonaf.t
  val of_json : Jsonaf.t -> (t, Error.t) result
end

module Window : sig
  type t =
    { entries : entry list
    ; previous_cursor : Page.Cursor.t option
    ; next_cursor : Page.Cursor.t option
    ; reached_start : bool
    ; reached_end : bool
    ; structurally_complete : bool
    }
  [@@deriving sexp]

  val to_json : t -> Jsonaf.t
  val of_json : Jsonaf.t -> (t, Error.t) result
end
