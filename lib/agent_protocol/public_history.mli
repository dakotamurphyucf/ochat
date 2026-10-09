(** Read-only history projections. Public views never authorize canonical input
    or a history edit. Full payloads retain their immutable neutral evidence;
    visible and redacted bodies cannot be converted back into canonical entries. *)

module Visible : sig
  type part =
    | Text of string
    | Refusal of string
    | Image of
        { uri : string
        ; detail : string History_entry.Payload.Presence.t
        }
    | Redacted_part of { kind : string }
  [@@deriving equal, sexp_of]

  type t = private
    | Message of
        { form : History_entry.Payload.Semantic.message_form
        ; role : History_entry.Payload.Role.t
        ; content : part list
        ; phase : string History_entry.Payload.Presence.t
        }
    | Reasoning of { readable_summary : string list }
  [@@deriving equal, sexp_of]

  (** Whitelist known readable fields. Arbitrary raw content, annotations,
      logprobs and provider metadata are never copied. Unknown message parts keep
      only their position and structural kind. Other semantic families return
      [None], requiring explicit redaction. *)
  val of_semantic : History_entry.Payload.Semantic.t -> t option

  val header : t -> Transcript.Header.t
  val to_json : t -> Jsonaf.t
  val of_json : Jsonaf.t -> (t, Error.t) result
end

module Redaction : sig
  type t = private { disclosed_header : Transcript.Header.t option }
  [@@deriving equal, sexp_of]

  (** A structural header may be disclosed independently of content. *)
  val create : disclosed_header:Transcript.Header.t option -> t
end

type body =
  | Full of History_entry.Payload.t
  | Visible of Visible.t
  | Redacted of Redaction.t
[@@deriving sexp_of]

type t = private
  { id : History.Id.t
  ; content_revision : History.Content_revision.t
  ; provenance : History.provenance
  ; body : body
  }
[@@deriving sexp_of]

val full
  :  ?content_revision:History.Content_revision.t
  -> History_entry.t
  -> provenance:History.provenance
  -> (t, Error.t) result

val visible
  :  ?content_revision:History.Content_revision.t
  -> History.Id.t
  -> provenance:History.provenance
  -> Visible.t
  -> (t, Error.t) result

val redacted
  :  ?content_revision:History.Content_revision.t
  -> History.Id.t
  -> provenance:History.provenance
  -> Redaction.t
  -> (t, Error.t) result

val header : t -> Transcript.Header.t option
val full_payload : t -> History_entry.Payload.t option

(** Exact full-payload JSON comparison retains field order and numeric spelling. *)
val equal : t -> t -> bool

(** Reject repeated host IDs within a public history sequence. *)
val validate_unique_ids : t list -> (unit, Error.t) result

val to_json : t -> Jsonaf.t
val of_json : Jsonaf.t -> (t, Error.t) result

module Window : sig
  type entry = t

  type t =
    { entries : entry list
    ; previous_cursor : Page.Cursor.t option
    ; next_cursor : Page.Cursor.t option
    ; reached_start : bool
    ; reached_end : bool
    ; structurally_complete : bool
    }
  [@@deriving sexp_of]

  (** Checks complete envelope bounds and unique host identities. *)
  val validate : t -> (unit, Error.t) result

  val to_json : t -> Jsonaf.t
  val of_json : Jsonaf.t -> (t, Error.t) result
end
