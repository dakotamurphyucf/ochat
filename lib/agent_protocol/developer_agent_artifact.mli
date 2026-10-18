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

module Issuer_source : sig
  (** Pure v3 issuing-child comparison data. The [artifact] is the issuer's
      selected revision, independent of a Tool's optional target artifact.
      This object is never a source lease, Claim or execution grant. *)
  type nonrec artifact = artifact

  type t

  module Tool_contract : sig
    type t =
      | Standard
      | Developer_model_spawn
    [@@deriving equal, sexp_of]
  end

  val create
    :  artifact:artifact
    -> prompt:Id.Prompt_definition.t
    -> workspace:Id.Workspace_definition.t
    -> tool_contract:Tool_contract.t
    -> source_policy:Chatml_source.Digest.t
    -> runtime_policy:Chatml_source.Digest.t
    -> t

  val artifact : t -> artifact
  val prompt : t -> Id.Prompt_definition.t
  val workspace : t -> Id.Workspace_definition.t
  val tool_contract : t -> Tool_contract.t
  val source_policy : t -> Chatml_source.Digest.t
  val runtime_policy : t -> Chatml_source.Digest.t

  (** Six exact known fields, duplicate rejection and typed validation. The
      enclosing bounded Store payload is already admitted before this decoder;
      the finite contract spelling is mapped by its Chat_response consumer. *)
  val to_json : t -> Jsonaf.t

  val of_json : Jsonaf.t -> (t, Protocol_error.t) result
end

module Saved_tool_request : sig
  type t

  (** Structural historical classification of an immutable Async_tool job
      payload. Require a strict v2 or v3 envelope, Tool target/name, exactly
      one matching capability pin, and [agent_artifact] decoded above. A v3
      [issuer_source] is a separate fixed-shape comparison field; it does not
      change the target artifact or grant issuer authority. Inspect only
      these fixed-size fields; the Store has already bounded the complete job
      payload and does not reparse its executable input or policy. Valid copied
      JSON may classify historically but cannot enter a Worker, source owner,
      native callback, or inference provider. Host execution still re-decodes
      the whole Background_request with its actual policy and physical proof. *)
  val of_payload : Jsonaf.t -> (t, Protocol_error.t) result

  val artifact : t -> artifact
  val selected_name : t -> string
end
