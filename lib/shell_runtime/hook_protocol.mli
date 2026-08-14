open! Core

(** Strict one-shot shell-hook-json-v1 values. *)

type version = V1 [@@deriving sexp, compare, equal]

type kind =
  | Before_interceptor
  | After_interceptor
  | Reviewer
  | Effect_analyzer
  | Audit_filter
[@@deriving sexp, compare, equal]

type status =
  | Exited of int
  | Signaled of int
[@@deriving sexp, compare, equal]

type action =
  | Continue
  | Rewrite of string list
  | Respond of
      { status : status
      ; stdout : string
      ; stderr : string
      }
  | Reject of string
  | Replace_result of
      { status : status
      ; stdout : string
      ; stderr : string
      }
  | Defer
  | Approve_once
  | Approve_scope of
      { scope : string
      ; expires_at : float option
      }
  | Deny of string
  | Reviewer_rewrite of string list
  | Add_effects of string list
  | Replace_effects of string list
  | Audit_keep
  | Audit_drop_field of string
  | Audit_replace_fields of string String.Map.t
[@@deriving sexp, compare, equal]

type request =
  { version : version
  ; request_id : string
  ; hook_id : string
  ; kind : kind
  ; payload : Jsonaf.t
  }
[@@deriving sexp]

type response =
  { version : version
  ; request_id : string
  ; action : action
  }
[@@deriving sexp, compare, equal]

(** [encode_request request] returns deterministic UTF-8 JSON. *)
val encode_request : request -> string

(** [decode_response ~kind source] rejects malformed UTF-8, extra JSON values,
    duplicate fields, unknown fields, wrong action families, and NUL argv. *)
val decode_response : kind:kind -> string -> (response, string) result

val kind_to_string : kind -> string

module For_testing : sig
  val canonicalize : Jsonaf.t -> Jsonaf.t
  val validate_no_duplicates : Jsonaf.t -> (unit, string) result
end
