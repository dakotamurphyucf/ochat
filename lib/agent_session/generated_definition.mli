open Core

(** Scoped generated definitions. Preparation only validates captured bytes and
    compiles; installation only persists an immutable artifact. Neither creates a
    session, initializes scripts, constructs native tools or grants authority. *)
type t

val prepare
  :  ?limits:Chatml_compilation.limits
  -> ?catalog:Chat_response.Authoring_policy.catalog
  -> env:Eio_unix.Stdenv.base
  -> dir:Eio.Fs.dir_ty Eio.Path.t
  -> revision_id:Agent_protocol.Id.Prompt_revision.t
  -> created_at:Agent_protocol.Timestamp.t
  -> current_capabilities:(unit -> Chat_response.Tool_capability.t)
  -> references:Chat_response.Tool_capability.reference list
  -> Chatmd_source_bundle.t
  -> (t, Chatmd_shell_spec.Diagnostic.t list) result

val artifact : t -> Agent_store.Prompt_artifact_store.Artifact.t
val admission : t -> Chat_response.Generated_admission.t
val capability_pins : t -> (string * string) list

(** Retain the validated source, compiled definition and capability selection,
    rebuilding only artifact identity for a durable creation reservation/retry. *)
val with_identity
  :  t
  -> revision_id:Agent_protocol.Id.Prompt_revision.t
  -> created_at:Agent_protocol.Timestamp.t
  -> (t, Chatmd_shell_spec.Diagnostic.t list) result

(** Allocate the admitted plain initial messages without initializing scripts,
    loading resources or copying parent history. Returns the next history ID. *)
val initial_history
  :  t
  -> session_id:Agent_protocol.Id.Session.t
  -> (History_entry.t list * int, Agent_protocol.Error.t) result

(** The creation coordinator must durably reserve the artifact ID/protect it from
    collection before calling this, and recheck parent/delegation authority before
    exposing a child. Repeating installation accepts only an identical verified
    manifest. A conflicting revision cannot overwrite the original artifact. *)
val install
  :  artifact_store:Agent_store.Prompt_artifact_store.t
  -> transaction_id:Agent_protocol.Id.Transaction.t
  -> t
  -> (unit, Chatmd_shell_spec.Diagnostic.t list) result

(** Install using a durable reservation from the same owned data root. Checks the
    current immutable admission, revocation, exact manifest and effective pins
    before writing; advances Artifact_installed only after complete verification.
    A concurrent revocation prevents advancement and leaves any installed bytes
    protected for recovery. Replays use the reservation's original transaction.
    Parent policy admission and child creation remain the coordinator's duty. *)
val install_reserved
  :  delegations:Agent_store.Delegation_store.t
  -> reservation:Agent_store.Delegation_store.record
  -> artifact_store:Agent_store.Prompt_artifact_store.t
  -> t
  -> (Agent_store.Delegation_store.record, Chatmd_shell_spec.Diagnostic.t list) result

(** Verify the exact generated artifact and its full materialized tree without
    compiling, evaluating initializers or resolving parent tools. The expected
    manifest digest must come from the verified private admission. Permits
    inspection of retained stopped children after parent revocation/unavailability;
    this supplies no executable admission or parent authority. *)
val load_artifact
  :  artifact_store:Agent_store.Prompt_artifact_store.t
  -> revision_id:Agent_protocol.Id.Prompt_revision.t
  -> manifest_sha256:string
  -> ( Agent_store.Prompt_artifact_store.Artifact.t
       , Chatmd_shell_spec.Diagnostic.t list )
       result

(** Requires the parent's currently authorized delegable registry and pins from
    the host-owned delegation record. Names alone do not restore a grant. Checks
    every pin, the admission record's expected manifest digest, generated-only
    parser/runtime contract, full source tree, and
    recompiles without initialization. Changed live selection during compilation
    is rejected. Parent generation, revocation, moderator mediation and lifetime
    remain the owning session/delegation service's responsibility. *)
val restore
  :  ?limits:Chatml_compilation.limits
  -> ?source_limits:Chatmd_source_bundle.limits
  -> ?catalog:Chat_response.Authoring_policy.catalog
  -> env:Eio_unix.Stdenv.base
  -> artifact_store:Agent_store.Prompt_artifact_store.t
  -> revision_id:Agent_protocol.Id.Prompt_revision.t
  -> manifest_sha256:string
  -> current_capabilities:(unit -> Chat_response.Tool_capability.t)
  -> pins:(string * string) list
  -> unit
  -> (t, Chatmd_shell_spec.Diagnostic.t list) result
