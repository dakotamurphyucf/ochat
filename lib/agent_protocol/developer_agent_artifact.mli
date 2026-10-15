open! Core

(** Immutable reference to an already selected Agent.run source artifact. Its
    validated revision, manifest digest, and portable root member are data, not
    source, native, actor, provider, or execution authority. The same codec is
    used by Background_request v2 and durable inference-row classification. *)
type t

type artifact = t

val create
  :  revision_id:Id.Prompt_revision.t
  -> manifest_sha256:Chatml_source.Digest.t
  -> root_relative_path:Source.Portable_path.t
  -> t

val revision_id : t -> Id.Prompt_revision.t
val manifest_sha256 : t -> Chatml_source.Digest.t
val root_relative_path : t -> Source.Portable_path.t

(** The existing three-field Background_request v2 representation, with exact
    known keys, duplicate rejection, bounded field count, typed digest and
    portable member. [of_json] inspects this small object only; callers bound
    the surrounding JSON tree before admitting it. *)
val to_json : t -> Jsonaf.t

val of_json : Jsonaf.t -> (t, Protocol_error.t) result

module Saved_tool_request : sig
  type t

  (** Structural historical classification of an immutable Async_tool job
      payload. Require the existing v2 envelope, Tool target/name, exactly one
      matching capability pin, and [agent_artifact] decoded above. Inspect only
      these fixed-size fields; the Store has already bounded the complete job
      payload and does not reparse its executable input or policy. Valid copied
      JSON may classify historically but cannot enter a Worker, source owner,
      native callback, or inference provider. Host execution still re-decodes
      the whole Background_request with its actual policy and physical proof. *)
  val of_payload : Jsonaf.t -> (t, Protocol_error.t) result

  val artifact : t -> artifact
  val selected_name : t -> string
end
