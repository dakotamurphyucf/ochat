(** Pure encoding/validation for the selected, locally owned Responses request
    profile. No credentials, transport, tool execution or history allocation.
    Model/profile capability eligibility and canonical call/result pairing belong
    to host preparation. This module does not grant those capabilities.

    Raw input envelopes remain ordered and unchanged, including opaque reasoning
    and exact call strings. Only supported local item kinds are admitted; remote
    references, hosted tools, provider conversations and compaction are rejected.
    Unknown top-level request/tool options fail closed rather than being forwarded.
    Duplicate object keys, invalid JSON numbers and excessive nesting reject. *)
open! Core

module Truncation_emission : sig
  type t =
    | Explicit_disabled
    | Omit
  [@@deriving equal, sexp_of]
end

module Field : sig
  type 'a t =
    | Absent
    | Null
    | Value of 'a
  [@@deriving equal, sexp_of]
end

module Reasoning : sig
  module Effort : sig
    type t =
      | None
      | Minimal
      | Low
      | Medium
      | High
      | Xhigh
      | Max
    [@@deriving equal, sexp_of]
  end

  module Summary : sig
    type t =
      | Auto
      | Concise
      | Detailed
    [@@deriving equal, sexp_of]
  end

  type t =
    { effort : Effort.t Field.t
    ; summary : Summary.t Field.t
    }
end

module Text : sig
  module Verbosity : sig
    type t =
      | Low
      | Medium
      | High
    [@@deriving equal, sexp_of]
  end

  module Format : sig
    type t =
      | Text
      | Json_object
      | Json_schema of
          { name : string
          ; schema : Jsonaf.t
          ; description : string Field.t
          ; strict : bool Field.t
          }
  end

  type t =
    { format : Format.t Field.t
    ; verbosity : Verbosity.t Field.t
    }
end

module Tool : sig
  type t

  module Custom_format : sig
    type t =
      | Text
      | Grammar of
          { syntax : [ `Lark | `Regex ]
          ; definition : string
          }
  end

  (** These codecs validate shape, names and the selected local-tool options.
      JSON Schema/grammar satisfiability and model support remain preparation and
      provider concerns. Top-level function parameters/strict are required but
      nullable; Absent rejects. Custom format/description/async reject null.
      Async encoding is conditional data support; dispatch eligibility and local
      scheduling remain host-owned and require the separate selected mapping. *)
  val function_
    :  name:string
    -> parameters:Jsonaf.t Field.t
    -> strict:bool Field.t
    -> ?description:string Field.t
    -> ?output_schema:Jsonaf.t Field.t
    -> ?async:bool Field.t
    -> unit
    -> t Or_error.t

  val custom
    :  name:string
    -> ?description:string Field.t
    -> ?format:Custom_format.t Field.t
    -> ?async:bool Field.t
    -> unit
    -> t Or_error.t

  val of_jsonaf : Jsonaf.t -> t Or_error.t
  val jsonaf_of_t : t -> Jsonaf.t
end

module Tool_choice : sig
  module Reference : sig
    type t =
      | Function of string
      | Custom of string
  end

  type mode =
    | Auto
    | Required
  [@@deriving equal, sexp_of]

  type t =
    | None
    | Auto
    | Required
    | Named of Reference.t
    | Allowed of
        { mode : mode
        ; tools : Reference.t list
        }
end

module Cache : sig
  module Retention : sig
    type t =
      | In_memory
      | Hours_24
    [@@deriving equal, sexp_of]
  end

  module Options : sig
    type mode =
      | Implicit
      | Explicit
    [@@deriving equal, sexp_of]

    type t =
      { mode : mode Field.t
      ; ttl : [ `Minutes_30 ] Field.t
      }
  end
end

type t

(** [create] always emits store=false and the requested stream Boolean. Truncation is
    explicitly disabled by default; trusted endpoint policy may omit that field,
    never emit automatic truncation or null. It sends
    the complete supplied input list; no previous_response_id/conversation path.
    Optional arguments are presence-valued: omitted defaults to Absent. Null is
    accepted only for documented nullable fields, not required input/model,
    store/stream, text/text.format, tools/tool_choice or cache options. Scalars/nested policy
    are also checked by [of_jsonaf]. max_output_tokens must be at least16.
    No model-name allowlist, default model, default output format or silent repair.
    include_encrypted_reasoning defaults to true for full local replay without
    interpreting opaque data. Explicit false is for a host-prepared profile that
    does not support reasoning; this codec does not establish that eligibility. *)
val create
  :  model:string
  -> input:Jsonaf.t list
  -> stream:bool
  -> ?truncation_emission:Truncation_emission.t
  -> ?instructions:string Field.t
  -> ?max_output_tokens:int Field.t
  -> ?parallel_tool_calls:bool Field.t
  -> ?temperature:float Field.t
  -> ?top_p:float Field.t
  -> ?reasoning:Reasoning.t Field.t
  -> ?text:Text.t Field.t
  -> ?tools:Tool.t list Field.t
  -> ?tool_choice:Tool_choice.t Field.t
  -> ?prompt_cache_key:string Field.t
  -> ?prompt_cache_retention:Cache.Retention.t Field.t
  -> ?prompt_cache_options:Cache.Options.t Field.t
  -> ?include_encrypted_reasoning:bool
  -> unit
  -> t Or_error.t

(** Validate an independently authored request without losing omission/null/value
    or raw input fields. store=false is required. Unsupported extensions reject;
    truncation may be omitted (the documented disabled default) or disabled, never auto/null.
    failed validation returns an error and does not change the supplied value. *)
val of_jsonaf : Jsonaf.t -> t Or_error.t

val jsonaf_of_t : t -> Jsonaf.t
val to_jsonaf : t -> Jsonaf.t
val stream : t -> bool
val model : t -> string
val input : t -> Jsonaf.t list
val field : t -> string -> Jsonaf.t Field.t
