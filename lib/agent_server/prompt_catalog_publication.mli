open Core

(** Actual host coordinator for source capture, last-successful dependency history
    and prompt-catalog publication. Factory/config pointers remain coordinated by
    Daemon in the same final protected nonyielding phase. No second journal. *)
type t

module History = Agent_session.Prompt_project_baseline_store.Snapshot.History

module Policy : sig
  type t

  val create
    :  source_preparation:Agent_session.Prompt_script_preparation.Policy.t
    -> custody_limits:Chatml_prompt_programs.Limits.t
    -> baseline_limits:Agent_session.Prompt_project_baseline.Limits.t
    -> source_limits:Agent_session.Prompt_source_owner.Limits.t
    -> max_roots:int
    -> t Or_error.t
end

module Mode : sig
  type t =
    | Build
    | Update of
        { definitions : Agent_protocol.Id.Prompt_definition.t list
        ; expected_generation : int
        }
end

module Error : sig
  type t =
    | Busy
    | Closed
    | Foreign_domain
    | Invalid_token
    | Publication_uncertain
    | Invalid_update
    | Stale_generation
    | Unknown_definition of Agent_protocol.Id.Prompt_definition.t
    | History of Agent_session.Prompt_project_baseline_store.Error.t
    | Association of Agent_session.Prompt_project_baseline.Error.t
    | Catalog of Core.Error.t
    | Seal of Agent_session.Prompt_catalog.Seal.Error.t
    | Artifacts of Agent_store.Store_error.t
    | Preparation of Agent_session.Prompt_revision_builder.Diagnostic.t list
end

(** Borrow the exclusively owned actual Session_store and already opened baseline
    store for the host lifetime. A new baseline must come only from the successful
    Session_store.create branch; an existing root always uses open_existing.
    Reads one bounded actual baseline snapshot before returning. Creating this
    coordinator neither opens sources nor retains source authority. *)
val create
  :  store:Agent_store.Session_store.t
  -> catalog:Agent_session.Prompt_catalog.t
  -> artifacts:Agent_store.Prompt_artifact_store.t
  -> baselines:Agent_session.Prompt_project_baseline_store.t
  -> policy:Policy.t
  -> content_limits:Chatmd_prompt_content.Limits.t option
  -> checkpoint:(unit -> unit)
  -> (t, Error.t) Result.t

(** Last installed history, initialized from the admitted startup snapshot.
    Pure, including during pending or uncertain publication; it does not claim
    the current durable generation after an uncertain write. Foreign-domain
    access raises Invalid_argument before reading state. *)
val history : t -> History.t

module Prepared : sig
  type t

  val entries : t -> Agent_session.Prompt_catalog.entry list
  val previous_history : t -> History.t
  val history : t -> History.t
end

(** Reserve this host's complete preparation/publication attempt. Snapshot durable
    history, prepare every actual definition with per-script selected operations
    and its explicit Prompt_definition tool compiler contract (Standard by
    default, Developer_model_spawn only when configured),
    preserve authentic next associations, finalize only actual successful durable
    artifacts, preallocate the bounded next index, and seal the exact catalog
    candidate. An Update validates its expected generation and target definitions
    before source IO; other definitions still Build. Each capture reuses its source
    scope and configured budgets; the caller checkpoint supplies any whole-attempt
    deadline. No old-graph compilation or per-script lock publication.

    Invalid/disabled definitions retain last-successful baseline rows. Unknown
    historical metadata never permits Initial or Update: configured entries become
    unavailable; purely implicit entries may still be published without rewriting
    the unknown index. Prepared tokens retain pending artifacts against pruning.
    Expected failure/exception closes unused candidates and releases reservation;
    originals/cancellation propagate after joined cleanup. [sw] is the actual host
    lifetime, shared with the baseline/source owners: it stays alive through
    publish/commit/retire or discard, not merely until preparation returns.
    [sources] borrows actual operator-admitted anchors/Git grants only until prepare
    returns; its source-host borrow need not span publication. The prepared token
    retains immutable program custody and separately owned original catalog scopes,
    never additional source anchors, Git handles, readers or grants. *)
val prepare
  :  t
  -> sources:Agent_session.Prompt_script_preparation.Host.t
  -> sw:Eio.Switch.t
  -> env:Eio_unix.Stdenv.base
  -> mode:Mode.t
  -> created_at:Agent_protocol.Timestamp.t
  -> checkpoint:(unit -> unit)
  -> Agent_session.Prompt_definition.t list
  -> (Prepared.t, Error.t) Result.t

(** Actual durable baseline publication; never a no-op stand-in for required disk
    writing. Only the explicitly retained Untracked/implicit case has no new index.
    Recheck exact expected generation/bytes through the existing store. Errors after
    writer entry preserve uncertainty and both artifact sets; no automatic retry.
    Overlapping token operations are rejected with Busy before callbacks or IO.
    The caller must immediately enter its coordinated final pointer phase on Ok. *)
val publish : Prepared.t -> checkpoint:(unit -> unit) -> (unit, Error.t) Result.t

(** Infallible for the acknowledged pending token; same-domain, nonyielding,
    preallocated pointer commit only. Caller combines actual Factory/config swaps
    in this SAME protected phase. Misuse returns a preallocated typed error. *)
val commit : Prepared.t -> (unit, Error.t) Result.t

type certainty =
  | Rejected
  | Uncertain
[@@deriving equal, sexp_of]

(** Before commit: abort only a certainly unpublished candidate. An acknowledged or
    uncertain durable write marks the seal/host uncertain rather than rolling back.
    Cleanup may suspend and is protected; original caller exception remains its
    caller's responsibility. All unused resources joined before releasing admission.
    If abort cleanup raises, the token remains nonpublishable and retains admission;
    a later discard retries only joined cleanup.
    Overlapping token operations raise Invalid_argument before mutation or cleanup. *)
val discard : Prepared.t -> certainty

(** After commit: joined retirement outside catalog mutex/final pointer phase.
    Releases this host attempt only after cleanup settles. Idempotent. Overlapping
    token operations raise Invalid_argument before mutation or cleanup. *)
val retire : Prepared.t -> unit

(** Use the SAME host admission to prevent pruning between artifact creation and
    baseline sealing. Adds all last-successful references to actual session/child
    references. Unknown historical index preserves all artifacts (returns zero),
    never infers historical roots from directory order. *)
val prune
  :  t
  -> additional:Agent_protocol.Id.Prompt_revision.t list
  -> checkpoint:(unit -> unit)
  -> (int, Error.t) Result.t

(** Prepare portable child custody using the SAME host admission and actual current
    baseline snapshot. The caller supplies its original live Scope/Capture and
    Startup and the selected child [tool_contract]; no source authority or
    compiler callback is retained. Mode.Build
    preserves known selections and rejects configured Untracked history. This
    operation neither publishes history nor installs/reserves a child artifact.
    Artifact retention through queue/delegation admission remains caller owned. *)
val prepare_child_programs
  :  t
  -> sources:Agent_session.Prompt_script_preparation.Host.t
  -> startup:Chat_response.Startup_compilation.t
  -> tool_contract:Chatml_compilation.Tool_contract.t
  -> invocation:Agent_session.Runtime_script_preparation.Invocation.t
  -> scope:Agent_session.Prompt_source_owner.Scope.t
  -> capture:Agent_session.Prompt_source_owner.Capture.t
  -> env:Eio_unix.Stdenv.base
  -> checkpoint:(unit -> unit)
  -> (Chatml_prompt_programs.t, Error.t) Result.t
