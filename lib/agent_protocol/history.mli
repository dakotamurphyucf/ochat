(** Presentation-neutral canonical and moderated transcript projections. *)

module Id : sig
  type t = History_entry.Id.t [@@deriving compare, hash, sexp]

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
[@@deriving sexp]

type entry =
  { id : Id.t
  ; role : role
  ; kind : kind
  ; payload : Jsonaf.t
  ; provenance : provenance
  ; redacted : bool
  }
[@@deriving sexp]

val entry_to_json : entry -> Jsonaf.t
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
