(** Immutable causal caller identity. These validated facts are DATA, not proof of
    a live installation, ancestry, permission or admission. Runtime issuers must
    authenticate the actual prepared source and worker separately. *)
module Unmoderated : sig
  type t = private
    { generation : int
    ; prompt_revision_id : Agent_protocol.Id.Prompt_revision.t
    ; configuration_revision : int64
    ; artifact_manifest_sha256 : string
    }
  [@@deriving equal, sexp_of]

  (** Counters are nonnegative; the digest identifies the verified complete
      artifact manifest and must be 64 lowercase hexadecimal characters. *)
  val create
    :  generation:int
    -> prompt_revision_id:Agent_protocol.Id.Prompt_revision.t
    -> configuration_revision:int64
    -> artifact_manifest_sha256:string
    -> (t, Agent_protocol.Error.t) result
end

type t [@@deriving equal, sexp_of]

type view =
  | Installed of Agent_protocol.Run_source.t
  | Unmoderated of Unmoderated.t

(** Validates the actual installed source. No missing-source constructor exists. *)
val installed : Agent_protocol.Run_source.t -> (t, Agent_protocol.Error.t) result

val unmoderated : Unmoderated.t -> t
val view : t -> view
val generation : t -> int
val matches_installed : t -> Agent_protocol.Run_source.t -> bool

(** Known-only closed source envelope. Complete compatible raw extensions belong
    to the enclosing Cause/Stream/Forwarding carrier; they are not owned here. *)
val to_json : t -> Jsonaf.t

val of_json : Jsonaf.t -> (t, Agent_protocol.Error.t) result
val shape : Document_schema.Shape.t
