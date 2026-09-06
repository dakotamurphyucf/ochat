open! Core

(** Transport-level HTTP request validation shared by the listener and its
    contract tests. *)

val connection_header : string
val protocol_version_header : string
val require_json_content_type : Piaf.Request.t -> (unit, Agent_protocol.Error.t) result
val require_protocol_version : Piaf.Request.t -> (unit, Agent_protocol.Error.t) result

(** [bearer_token] accepts one nonempty Bearer credential and rejects every
    malformed Authorization spelling without exposing the credential. *)
val bearer_token : Piaf.Request.t -> (string option, unit) result

val request_body
  :  max_body_bytes:int
  -> Piaf.Request.t
  -> (string, Agent_protocol.Error.t) result

(** [event_cursor] rejects conflicting header/query cursors and negative or
    malformed sequences. *)
val event_cursor : Piaf.Request.t -> (int64 option, Agent_protocol.Error.t) result
