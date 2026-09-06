open! Core

(** Versioned already-redacted audit events and mutable filter responses. *)

type t =
  { phase : string
  ; sequence : int64
  ; timestamp : float
  ; session_id : string option
  ; runtime_id : string
  ; manifest_sha256 : string
  ; request_id : string
  ; plan_id : string option
  ; event : string
  ; fields : string String.Map.t
  }

type response =
  | Keep
  | Drop_field of string
  | Replace_fields of string String.Map.t

val of_envelope
  :  secret_filter:Shell_access.Secret_filter.t
  -> Shell_access.Audit.envelope
  -> t

val encode : t -> Chatml.Chatml_lang.value
val decode : Chatml.Chatml_lang.value -> (t, Chatmd_shell_spec.Diagnostic.t) result
val encode_response : response -> Chatml.Chatml_lang.value
val decode_response
  :  Chatml.Chatml_lang.value
  -> (response, Chatmd_shell_spec.Diagnostic.t) result
