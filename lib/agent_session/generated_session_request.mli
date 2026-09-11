open Core

type lifetime =
  | Owned
  | Independent
[@@deriving equal, sexp_of]

(** Inline, captured source only. Parent identity, authority and storage paths are
    supplied by the host, never by this request. Model/reasoning settings belong
    to the generated ChatMD configuration. *)
type t = private
  { bundle : Chatmd_source_bundle.t
  ; tools : string list
  ; start_immediately : bool
  ; lifetime : lifetime
  ; display_name : string option
  ; idempotency_key : Agent_protocol.Idempotency_key.t
  }

type created =
  { session : Agent_protocol.Session.t
  ; parent_session_id : Agent_protocol.Id.Session.t
  ; tools : string list
  }

type service =
  { limits : Chatmd_source_bundle.limits
  ; create :
      Native_tool_invocation.borrowed
      -> t
      -> (created, Agent_protocol.Invocation.tool_error) result
  }

val parameters : Jsonaf.t

val decode
  :  limits:Chatmd_source_bundle.limits
  -> Jsonaf.t
  -> (t, Agent_protocol.Invocation.tool_error) result

val to_json : created -> Jsonaf.t
