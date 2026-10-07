open! Core

(** Immutable provider-neutral inference inputs. No I/O, credentials, protocol
    IDs, tool execution, model catalog or ambient defaults. Raw values are private
    request data; observations must use their separate explicit safe projection. *)
module Presence = History_entry.Payload.Presence

module Error : sig
  type t =
    | Json of Document_schema.Error.t
    | Invalid_field of
        { field : string
        ; reason : string
        }
    | Duplicate_setting of string
    | Duplicate_tool_name of string
    | Duplicate_asset_reference of string
    | Duplicate_history_id of History_entry.Id.t
  [@@deriving equal, sexp_of]
end

module Setting : sig
  type provenance =
    | Execution_override
    | Captured_prompt
    | Profile_default
  [@@deriving equal, sexp_of]

  type t

  (** Nonempty UTF8 name and completely validated bounded setting value. Absent
      is omission, Null is explicit; Value `Null rejects rather than silently changing
      native presence. Neither means reset without an adapter field
      policy. Provider-specific key/domain/capability checks belong to preparation.
      The host must exclude credentials/headers; a generic JSON constructor cannot
      establish secrecy. The adapter applies its known setting allowlist before
      authentication/network dispatch. *)
  val create
    :  name:string
    -> value:Jsonaf.t Presence.t
    -> provenance:provenance
    -> limits:Document_schema.Limits.t
    -> (t, Error.t) Result.t

  val name : t -> string
  val value : t -> Jsonaf.t Presence.t
  val provenance : t -> provenance

  (** Update only the owned value/provenance fields of the original admitted JSON.
      Unknown setting members retain their order and values. Value `Null rejects;
      Absent removes only the value member. Original and result admit under limits. *)
  val with_value
    :  t
    -> value:Jsonaf.t Presence.t
    -> provenance:provenance
    -> limits:Document_schema.Limits.t
    -> (t, Error.t) Result.t

  val equal : t -> t -> bool
  val to_json : t -> Jsonaf.t
  val of_json : Jsonaf.t -> limits:Document_schema.Limits.t -> (t, Error.t) Result.t
end

module Target : sig
  type t

  (** Captures actual nonsecret selection and already resolved effective settings.
      No default model/account/endpoint is inferred. Missing account/revision is
      explicitly unavailable, never an invented historical identity. The profile
      revision is host evidence when actually available; model/settings overrides
      cannot change adapter/account. Required labels/model/endpoint are nonempty
      UTF8. Adapter preparation validates endpoint semantics and actual eligibility.
      Duplicate setting names reject; no precedence merge occurs here. The complete
      stored JSON, including unknown fields, is bounded by [limits]. *)
  val create
    :  adapter:string
    -> profile:string
    -> profile_revision:string option
    -> account:string option
    -> endpoint:string
    -> model:string
    -> settings:Setting.t list
    -> limits:Document_schema.Limits.t
    -> (t, Error.t) Result.t

  val adapter : t -> string
  val profile : t -> string
  val profile_revision : t -> string option
  val account : t -> string option
  val endpoint : t -> string
  val model : t -> string
  val settings : t -> Setting.t list
  val equal : t -> t -> bool

  (** Preserve the original unknown target/settings members. These operations
      cannot change adapter/profile/account/endpoint. Original and result admit
      under limits. A model override is explicit; an omitted override inherits. *)
  val with_model
    :  t
    -> model:string
    -> limits:Document_schema.Limits.t
    -> (t, Error.t) Result.t

  (** Update an existing setting by exact name through Setting.with_value, or
      append a new setting. Other entries retain their order and original JSON;
      Absent is omission, not removal/reset of a setting's future metadata. *)
  val with_setting
    :  t
    -> name:string
    -> value:Jsonaf.t Presence.t
    -> provenance:Setting.provenance
    -> limits:Document_schema.Limits.t
    -> (t, Error.t) Result.t

  (** Private storage JSON. Decoding validates the entire original tree and all
      native invariants. Encoding preserves admitted unknown members/presence;
      this is not a public effective-configuration projection or an auth record. *)
  val to_json : t -> Jsonaf.t

  val of_json : Jsonaf.t -> limits:Document_schema.Limits.t -> (t, Error.t) Result.t
  val validate : t -> limits:Document_schema.Limits.t -> (unit, Error.t) Result.t
end

module Asset : sig
  type kind =
    | Image
    | Document of { filename : string option }
  [@@deriving equal, sexp_of]

  type t

  (** Host-resolved immutable bytes for an exact history reference. No remote URL,
      provider file ID, mutable pathname lookup or I/O closure is retained. The
      reference labels the host's mapping, not permission to access a file. Media
      type/reference/optional filename are nonempty UTF8; bytes may be binary and
      must fit the positive [max_bytes]. Encoding/capability compatibility remains
      adapter preparation. No serializer or default body diagnostic is exposed. *)
  val create
    :  reference:string
    -> kind:kind
    -> media_type:string
    -> bytes:string
    -> max_bytes:int
    -> (t, Error.t) Result.t

  val reference : t -> string
  val kind : t -> kind
  val media_type : t -> string
  val bytes : t -> string
  val equal : t -> t -> bool
end

module Tool_spec : sig
  module Custom_format : sig
    type t =
      | Text
      | Grammar of
          { syntax : [ `Lark | `Regex ]
          ; definition : string
          }
    [@@deriving equal, sexp_of]
  end

  type view =
    | Function of
        { parameters : Jsonaf.t Presence.t
        ; strict : bool Presence.t
        }
    | Custom of { format : Custom_format.t Presence.t }

  type t

  (** Names are exact nonempty UTF8; all schema/description/output-schema JSON
      and native fields are admitted under [limits]. Presence is preserved; JSON
      Value `Null rejects (use Null), including parameters/output_schema.
      Function/custom use the existing canonical call families; namespace,
      discovery and provider-native async are excluded by the accepted57 scope.
      JSON Schema/grammar semantics and adapter-specific null support are checked
      at existing host admission/preparation, not treated as tool authority. *)
  val create
    :  name:string
    -> description:string Presence.t
    -> output_schema:Jsonaf.t Presence.t
    -> view:view
    -> limits:Document_schema.Limits.t
    -> (t, Error.t) Result.t

  val name : t -> string
  val description : t -> string Presence.t
  val output_schema : t -> Jsonaf.t Presence.t
  val view : t -> view
  val kind : t -> History_entry.Payload.Call_kind.t
  val equal : t -> t -> bool
end

type t

(** Capture after final effective ordered history, moderator changes, immutable
    assets, selected tools and final authoring guidance. Complete aggregate request
    admission under [limits] includes history IDs/full canonical payloads, target,
    descriptors and base64-size asset bodies. Asset base64 size is checked before
    any base64 allocation. [encoded_bytes] measures this private neutral admission
    representation, not the adapter's actual wire request (which has its own bounds). Canonical Payload admission remains
    independent. Duplicate host IDs/tool names/asset references reject. History is
    never reconstructed from a public/redacted view. Scope belongs to the later
    actual dispatch, not this preparation input. No admission or effects occur. *)
val create
  :  target:Target.t
  -> history:History_entry.t list
  -> tools:Tool_spec.t list
  -> assets:Asset.t list
  -> limits:Document_schema.Limits.t
  -> (t, Error.t) Result.t

val target : t -> Target.t
val history : t -> History_entry.t list
val tools : t -> Tool_spec.t list
val assets : t -> Asset.t list
val encoded_bytes : t -> int
