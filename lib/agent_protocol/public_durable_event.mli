(** Typed public durable events. Hidden entries preserve sequence slots without
    exposing content. Internal events and their persistence codecs stay intact. *)
module Shared_payload : sig
  type t [@@deriving sexp_of]

  (** Exhaustive admission of the shared, non-history payload alternatives.
      Rejects all history variants and the untyped internal moderator overlay.
      Native children pass the same domain codec admission as received payloads.
      Optional replacement/status fields are projected separately. *)
  val of_internal : Event.Durable.Payload.t -> (t, Error.t) result

  val value : t -> Event.Durable.Payload.t
end

type overlay =
  { effective_history : Public_history.Window.t option
  ; halted : bool
  ; halt_reason : string option
  }
[@@deriving sexp_of]

type payload =
  | History_message_deferred of Public_history.t
  | History_appended of Public_history.t list
  | History_replaced of Public_history.Window.t
  | Moderator_overlay_changed of overlay
  | Shared of Shared_payload.t
[@@deriving sexp_of]

type body =
  | Full of payload
  | Filtered of payload
  | Hidden
[@@deriving sexp_of]

type t = private
  { session_id : Id.Session.t
  ; sequence : int64
  ; revision : int64
  ; timestamp : Timestamp.t
  ; kind : Event.Durable.kind
  ; body : body
  ; extension_status : Extension_status.t list option
  ; replacement_snapshot : Public_snapshot.t option
  }
[@@deriving sexp_of]

(** Copies only the event envelope. Checks kind, counters, payload session
    ownership and replacement anchors. Extension summaries are unique, validated
    and cannot name a future generation of their session update. Hidden events
    must contain neither status nor snapshot extras. *)
val of_internal_envelope
  :  Event.Durable.t
  -> body:body
  -> extension_status:Extension_status.t list option
  -> replacement_snapshot:Public_snapshot.t option
  -> (t, Error.t) result

val to_json : t -> Jsonaf.t
val of_json : Jsonaf.t -> (t, Error.t) result
val to_notification : t -> Envelope.t
