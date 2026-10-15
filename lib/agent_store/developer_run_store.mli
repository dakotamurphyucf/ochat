(** Daemon-owned developer run checkpoint document. No ID, observation, or
    retained record grants host execution or result-read authority. *)
open! Core

module Limits : sig
  type t

  (** Read-only configured bounds. These project the exact immutable limits
      accepted by [create]; they grant no Store handle or admission. *)
  val document : t -> Document_schema.Limits.t

  val commands : t -> Developer_command_limits.t
  val max_runs : t -> int

  (** Every bound is positive and finite. Inference row count is bounded by
      16,384 and encoded aggregate inference bytes by the configured whole
      document cap (itself at most 16 MiB); each Attempt_record also passes
      its canonical 64 KiB/observation/diagnostic admission. The document
      limits use the existing Document_schema JSON byte/depth/field/node
      accounting. [commands] bounds command rows, complete row bytes and
      individual terminal payloads; its byte cap must fit this same document.
      Joint recovery capacity reserves all pending command interruptions and
      all live inference interruptions together. Aggregate recovery limits
      cover directory entries, bytes
      and work before allocation. On disk, an over-limit document is corrupt;
      ordinary admission reports Limit_exceeded before mutation. *)
  val create
    :  max_runs:int
    -> max_epochs_per_run:int
    -> max_jobs_per_run:int
    -> max_invocations_per_run:int
    -> max_inference_attempts_per_run:int
    -> max_inference_bytes_per_run:int
    -> commands:Developer_command_limits.t
    -> max_selected_per_commit:int
    -> max_job_payload_bytes:int
    -> max_document_bytes:int
    -> max_recovery_entries:int
    -> max_recovery_bytes:int
    -> max_work_steps:int
    -> document:Document_schema.Limits.t
    -> t Or_error.t
end

module Error : sig
  type t =
    | Store of Store_error.t
    | Conflict
    (** This Prepared never attempted replacement: its first commit lost the
        before-bytes CAS, or another claim already installed identical
        after-bytes. Only this exact pending token is retired. *)
    | Prewrite_conflict
    | Foreign_or_stale
    | Limit_exceeded
    | Closed
  [@@deriving sexp_of]
end

type t
type handle
type store = t

module Live_directory : sig
  type t

  (** Reserve one provisional directory slot for this exact Store-issued Live
      handle before native resource preparation. This sibling of the durable
      developer-runs namespace is an exclusive generation-named 0700 child
      under the SAME daemon lock and filesystem capability borrowed through
      Store.with_lease. The owner/handle, rather than a caller path or run ID,
      selects it. A second open for the same run is Conflict. No directory
      receipt grants Root/Run_claim/workspace/source authority. Failure or
      cancellation releases only this reservation and removes only a child
      created by this call; Store.close_and_wait rejects while a child is live.

      [open_or_create] performs finite, no-follow startup inspection and
      removal of abandoned children under its existing recovery allowance.
      Unknown or linked child roots/ownership markers and over-limit entries
      fail closed, not as empty inventory. Symbolic links WITHIN an authenticated
      owned child are unlinked without following them.
      Cold terminal recovery never adopts a previous Live directory. *)
  val open_
    :  handle
    -> sw:Eio.Switch.t
    -> checkpoint:(unit -> unit)
    -> (t, Error.t) result

  (** Actual owner-derived paths for Runtime_paths and the existing
      Workspace_resolver Session_dir coordinate. None is an independent native
      filesystem capability or permission to read a workspace. *)
  val runtime_directory : t -> string

  val cache_directory : t -> string
  val response_directory : t -> string
  val workspace_parent_directory : t -> string

  (** Checks original Store/daemon lock, issuing domain and own live slot.
      A copied path or equal owner record does not pass. *)
  val check_live : t -> (unit, Error.t) result

  (** Call only after native/workspace users join and any Session_dir child is
      cleaned through Workspace_cleanup.remove_bounded. Refuses a remaining
      workspace child. Each attempt uses a fresh finite no-follow streaming
      allowance from this Store's immutable recovery limits, retaining this
      token and Store close exclusion on partial/error cleanup. An inner link is
      unlinked as a link. A failed final marker/rmdir tail retains in-memory
      root identity for same-process retry; after restart an unmarked child is
      quarantined. Repeated close after success is harmless. Switch release
      retries actual unfinished work. [checkpoint] preserves cancellation and
      there is no unbounded rmtree tail. *)
  val close_and_wait : t -> checkpoint:(unit -> unit) -> (unit, Error.t) result
end

module Run_claim : sig
  type t

  (** Non-yielding physical claim of one stable run ID in this exact Store.
      [handle] must have been issued by [store]. Registration precedes any
      actor admission read or callback, so a second owner for the same run
      cannot pass an Eio wait and enter; an in-flight same-run [admit_epoch]
      or unresolved Store transaction also excludes acquisition. Other run IDs
      remain independent. Acquire/release stay on the issuing Eio domain and
      perform no Eio operation themselves.
      This checks physical identity, not principal/source authorization or
      latest epoch, which the host verifies after acquisition. The claim is
      distinct from a one-shot worker Claim and is not execution authority. *)
  val acquire : store:store -> handle:handle -> (t, Error.t) result

  (** Non-yielding release of this exact issuer token after host-owned actions,
      workers and uncertain handoffs are joined/reconciled. A pending Store
      transaction, open recovery window, in-flight normal Store operation or
      unsettled issued worker Claim or open Tracking manager for the run
      rejects release. Repeated release is harmless;
      the shared Store and unrelated runs stay open. *)
  val release : t -> (unit, Error.t) result
end

module Checkpoint : sig
  type phase =
    | Active
    | Interrupted
    | Closed
  [@@deriving equal, sexp_of]

  type t

  val owner : t -> Agent_protocol.Execution_owner.developer
  val capture_fingerprint : t -> Chatml_source.Digest.t
  val phase : t -> phase
  val revision : t -> int64
end

module Observation : sig
  type t

  (** Immutable exact file-revision observation, physically tied to its issuing
      store and run. A matching numeric revision alone is not a CAS proof. *)
  val checkpoint : t -> Checkpoint.t
end

module Invocation_observation : sig
  type t

  val revision : t -> int
  val value : t -> Agent_protocol.Host_invocation.t
end

module Invocation_lookup : sig
  type t

  val prior : t -> Invocation_observation.t option
end

(** Exact lookup tokens are tied to an immutable Observation, including an
    observed absence. Neither a numeric revision nor an ID alone is a CAS
    proof. Reads disclose only lower-store data, not service authority. *)
val find_invocation
  :  Observation.t
  -> Agent_protocol.Id.Invocation.t
  -> Invocation_lookup.t

val find_job
  :  Observation.t
  -> Agent_protocol.Id.Job.t
  -> Agent_protocol.Developer_job.t option

module Job_lookup : sig
  type t

  val value : t -> Agent_protocol.Developer_job.t option
end

(** Read-only queued-job projection from one exact latest-Active Observation.
    This value retains the Store-issued lookup and immutable job, but grants no
    claim, effect or result authority. A copied job/ID cannot construct one. *)
module Scheduler_candidate : sig
  type t

  val job : t -> Agent_protocol.Developer_job.t
  val observation : t -> Observation.t
  val lookup : t -> Job_lookup.t
end

(** Exact job presence and bytes tied to a physical Observation. A copied job
    or bare ID is not a claim basis. *)
val find_job_exact : Observation.t -> Agent_protocol.Id.Job.t -> Job_lookup.t

module Claim : sig
  type t

  (** Physical issuer comparison only. This neither consumes claim entry nor
      authorizes a terminal transition; equal owner/job IDs are insufficient. *)
  val issued_by : t -> store -> bool

  val owner : t -> Agent_protocol.Execution_owner.developer
  val job : t -> Agent_protocol.Developer_job.t
  val attempt : t -> int

  (** Verify the actual issuing Store and retained handle are live, the job is
      still in the latest Active epoch, and the caller is on the issuing domain
      BEFORE atomically consuming this token's one-shot entry bit. A foreign or
      stale handle rejects without consuming it; [Ok false] means it entered
      once already. A repeated installed reconciliation returns this SAME token.
      Entry alone is not host permission or capacity authority. [admit] is a
      trusted, non-yielding local issuer-liveness check evaluated inside the
      writer after the Store read and immediately before the atomic entry bit;
      it must not call the Store or perform Eio operations. [on_enter] is a
      trusted non-yielding marker executed immediately after this call wins the
      CAS, before any cancellation point, so the physical scope can distinguish
      its own interrupted entry from a replayed token belonging to another
      already-live callback. *)
  val try_enter
    :  t
    -> handle:handle
    -> admit:(unit -> bool)
    -> on_enter:(unit -> unit)
    -> (bool, Error.t) result

  (** Trusted scope cleanup before any callback starts. This closes a token
      consumed by a Store CAS whose acknowledgement/caller was cancelled before
      callback entry. A token already abandoned or never consumed is harmless.
      The scope alone proves its callback has not begun. *)
  val abandon_before_callback : t -> handle:handle -> (unit, Error.t) result

  (** Read actual latest epoch/phase from the issuing Store on every parent
      effect check. This may yield; callers recheck local proof liveness after
      it returns. An actor's missed epoch-advance notice cannot validate an
      old claim. *)
  val current_for_effect : t -> (bool, Error.t) result
end

module Inference_row : sig
  type t

  (** A bounded, validated projection from this Store's actual document. It
      carries comparison evidence only; it cannot be used to enter a worker,
      allocate another attempt or disclose a provider payload. *)
  val ordinal : t -> int64

  val owner : t -> Agent_protocol.Execution_owner.developer
  val capture_fingerprint : t -> Chatml_source.Digest.t
  val job_id : t -> Agent_protocol.Id.Job.t
  val job_attempt : t -> int
  val context_id : t -> Inference.Observation.Observation_id.t
  val record : t -> Inference.Observation.Attempt_record.t
end

module Prepared : sig
  type t

  (** The complete bounded exact previous/new document bytes, bound to one
      physical issuing handle. This is minted before I/O so an uncertain
      acknowledgement can be reconciled without repeating the effect or
      inventing a commit. *)
end

module Installed : sig
  type t

  (** Physical Store issuer comparison for an exact selected-result
      acknowledgement. Equal run/job IDs or copied DTOs do not suffice. *)
  val issued_by : t -> store -> bool

  val observation : t -> Observation.t
end

type reconciliation =
  | Installed of Installed.t
  | Absent
  | Diverged

(** Terminal bookkeeping for attempts whose process-local worker Claim was lost
    at restart. This is available only during the issuing run's exclusive
    startup window, before its normal worker or native dispatch is exposed.
    It never mints a Claim, enters an effect, chooses a retry, or selects child
    jobs. A current-epoch attempt receives the same terminal-only restriction
    as a historical interrupted attempt. Waiting_completion is an already
    ended dependency state and is deliberately outside this window. *)
module Recovery : sig
  type t

  module Inference_candidate : sig
    type t

    (** Exact persisted Prepared or Running row from this View. The row's
        historical owner, capture, job, attempt, scope and evidence are never
        rewritten to the new startup epoch. *)
    val row : t -> Inference_row.t
  end

  module Inference_pending : sig
    type t

    val row : t -> Inference_row.t
    val after : t -> Inference.Observation.Attempt_record.t
  end

  module Candidate : sig
    type t

    (** Exact persisted Running or Waiting_permission job, including its
        original host/run/principal/epoch/capture/attempt identity. The owner
        is never replaced by the current startup handle's epoch. *)
    val job : t -> Agent_protocol.Developer_job.t
  end

  module View : sig
    type t

    (** One bounded, source-of-truth document observation. A Candidate is tied
        to this observation and cannot be fabricated from a job DTO. *)
    val observation : t -> Observation.t

    val candidates : t -> Candidate.t list
    val inference_candidates : t -> Inference_candidate.t list
  end

  module Pending : sig
    type t

    val job : t -> Agent_protocol.Developer_job.t
    val after : t -> Agent_protocol.Developer_job.State.t
  end

  (** Open once for the exact live Run_claim and handle before any worker/native
      activity under that claim. The Store's run-lifecycle gate marks ordinary
      claim/dispatch preparation and effect entry before they can yield, tracks
      outstanding issued Claims until their exact terminal settlement, and
      excludes opening while such work or a pending Store write exists. Opening
      also excludes later ordinary preparation, commit and Claim entry until
      [finish] or [abort_clean]. The gate never waits on I/O while locked. A
      second/late opening, foreign handle/domain or released claim rejects.
      Run_claim.release rejects throughout an open window, even before a
      Pending is selected. *)
  val open_ : claim:Run_claim.t -> handle:handle -> (t, Error.t) result

  (** Read one bounded current document and return only abandoned physically
      claimed attempts. Each visit and publication is charged to Store work and
      document limits. A fresh View is required after a successful terminal
      commit or a document revision change. No result disclosure follows. *)
  val observe : t -> (View.t, Error.t) result

  (** Reserve an exact terminal-only transition before result bytes or private
      intent are written. The Candidate and View must be from the same exact
      observation, still current at the Store read. [after] must be terminal,
      preserve job attempt/start and contain no retry or child selection. The
      existing pure transition and document encoder produce the one Prepared
      CAS; a mismatch rejects without writing. Persisted Prepared/Running
      inference rows for this job/attempt must first be interrupted through
      this same Recovery window, so the job cannot become terminal while its
      old provider attempt still appears live. A retained Pending must be
      settled before any different selection is prepared. *)
  val prepare_terminal
    :  t
    -> view:View.t
    -> candidate:Candidate.t
    -> after:Agent_protocol.Developer_job.State.t
    -> (Pending.t, Error.t) result

  (** Prepare the exact recovered inference row before interrupting its
      abandoned Model_call job. Prepared proves no application request began
      and becomes Host_interrupted/Definitely_not_submitted. Running is
      conservatively Host_interrupted/Possibly_submitted; its recorded
      observations are retained. A terminal or already-interrupted row is not
      a candidate. This derives the change from the source row and cannot
      manufacture a provider terminal or a new scope. *)
  val prepare_inference_interruption
    :  t
    -> view:View.t
    -> candidate:Inference_candidate.t
    -> (Inference_pending.t, Error.t) result

  (** A retained exact preparation is reported after an uncertain write or
      cancellation. Higher result publication reuses/reconciles it and its
      private selected intent; it must not select replacement bytes. Successful
      installation clears the window's Pending. Proven Absent retains it until
      a retry or [reprepare_same]; Diverged/unknown retains it and forbids a
      different preparation. *)
  val pending : t -> Pending.t option

  val inference_pending : t -> Inference_pending.t option

  (** Use the same Store writer, pending barrier, file/directory durability and
      exact reconciliation as ordinary claimed terminal transitions. [admit]
      is a non-yielding host check immediately before the replace; it must
      confirm the original actor/selection authority for a successful result,
      or the physical owner's right to truthful terminal cleanup. A revoked
      actor after prior I/O cannot publish a new success. Errors/cancellation
      preserve the Pending and original exception/backtrace. *)
  val commit_terminal
    :  t
    -> Pending.t
    -> admit:(unit -> bool)
    -> (Installed.t, Error.t) result

  val reconcile_terminal : t -> Pending.t -> (reconciliation, Error.t) result

  (** After exact reconciliation proves the prior Pending absent, reobserve
      the document and prepare ONLY its same original job/attempt and terminal
      [after] (including identical selected result reference). This handles an
      unrelated revision advancing the file without allowing a new selection.
      Conflict, divergent or uncertain evidence leaves the old Pending in
      custody. A proven same-revision absence may instead retry its Prepared. *)
  val reprepare_same : t -> Pending.t -> (Pending.t, Error.t) result

  (** Same one-shot window, per-run pending barrier, exact Prepared bytes and
      bounded reconciliation as [commit_terminal]. Only one job or inference
      Pending may exist at once. An uncertain write retains the exact physical
      token in process; a restarted process instead reads the authoritative
      persisted row and opens a NEW Recovery window. No callback is replayed.
      Proven Absent can reprepare only this original row/state against a fresh
      View, not choose another interruption or parent. *)
  val commit_inference_interruption
    :  t
    -> Inference_pending.t
    -> (Installed.t, Error.t) result

  val reconcile_inference_interruption
    :  t
    -> Inference_pending.t
    -> (reconciliation, Error.t) result

  val reprepare_inference_same
    :  t
    -> Inference_pending.t
    -> (Inference_pending.t, Error.t) result

  (** Complete the one-shot startup window only after no job/inference Pending
      remains and a fresh bounded read finds no Running/Waiting_permission job
      or Prepared/Running inference row in any retained epoch. Before closing,
      derive Interrupted for every nonterminal command from its exact current
      persisted row, preserving whether Live execution had started. Persist
      these rows in one full-image transition through the existing run writer,
      pending map, combined recovery headroom and exact before/after classifier.
      An uncertain/cancelled replacement retains that exact preparation; a
      repeated [finish] reconciles it, never chooses a new terminal or grants
      Execution_start. A proven Absent may retry only after a fresh exact read;
      Diverged remains blocked. [abort_clean] cannot discard this custody.
      The host must have joined the former physical owner before opening this
      Run_claim/Recovery window; a stale read-only command row alone never
      authorizes interruption. A successful close enables normal claims under
      this Run_claim. No callback may enter a worker between the final read
      and closing the window. *)
  val finish : t -> (unit, Error.t) result

  (** Non-authorizing lifecycle observation for the owning startup adapter. *)
  val is_finished : t -> bool

  (** Release a window only when no terminal was prepared or attempted and no
      selected custody was retained. This supports ordinary validation failure
      before publication. It never discards uncertain Store work; otherwise
      the physical claim remains quarantined for retry/restart. *)
  val abort_clean : t -> (unit, Error.t) result
end

(** One physically registered inference-attempt manager for a live run claim.
    It shares this Store's document writer, bounded codec, run-lifecycle gate
    and single per-run pending barrier. This lower layer never constructs an
    Inference_client.Identity, provider request, fake Session or worker lease.
    Root owns this manager until all inference callbacks have joined and their
    exact tracking writes are settled. All operations use the issuing domain;
    a DTO with matching IDs is not physical authority. *)
module Tracking : sig
  type t

  module Registration : sig
    type t

    (** Minted only after the exact admission write is durably Installed. The
        row accessor is that admitted immutable snapshot; use [observe] for
        a fresh latest record after later revisions. *)
    val row : t -> Inference_row.t

    val scope : t -> Transcript.Scope.t
    val accounting_id : t -> Inference.Observation.Observation_id.t
    val context_id : t -> Inference.Observation.Observation_id.t
  end

  module View : sig
    type t

    (** The exact fresh Store Observation plus the row registered by this
        manager. Matching numeric revisions or copied rows do not suffice. *)
    val observation : t -> Observation.t

    val row : t -> Inference_row.t
  end

  module Pending : sig
    type t
  end

  type receipt =
    | Admitted of Registration.t
    | Updated of View.t

  type reconciliation =
    | Tracking_installed of receipt
    | Tracking_absent
    | Tracking_diverged

  type change =
    | Started
    | Observed of Inference.Observation.t
    | Finished of Inference.Event.Terminal.t
    | Interrupted of
        { reason : Inference.Observation.Attempt_record.interruption
        ; delivery : Inference.Event.Terminal.delivery
        }

  type preparation =
    | Unchanged of View.t
    | Prepared of Pending.t

  type relation =
    | Root
    | Nested of
        { parent : Registration.t
        ; call_entry_id : History_entry.Id.t option
        ; call_alias : string option
        }

  (** Register exactly once for the physical Run_claim, after its one-shot
      Recovery window finished and before exposing any inference callback.
      The Store rejects a second manager even if it has the same handle/claim.
      Registration occurs before a yielding read; foreign Store/handle/domain,
      closed claim, unfinished recovery or a live same-run pending reject. *)
  val open_ : claim:Run_claim.t -> handle:handle -> (t, Error.t) result

  (** A new Prepared row needs an actually entered Claim for this Store, exact
      immutable Model_call job with a positive attempt in durable Running
      state, still held by the issuer's physical worker registry, latest
      Active epoch and capture, and remaining row/byte/ordinal capacity.
      Waiting_permission is an ended callback state, not new-work admission.
      A nested parent is a
      genuinely issued retained registration from this manager and the SAME
      physical entered Claim; it may be Terminal because an admitted tool
      call can execute after the parent provider response completes. The
      parent key/ordinal and supplied call binding are retained in the Scope
      relation. The higher host must prove that binding came from its actual
      admitted call candidate; the Store cannot infer that fact from an
      Attempt_record or caller text. Parent provenance does not grant new
      effect authority: the current Claim/actor/provider checks remain
      mandatory. The
      generated source, ordinal-based attempt, usage and context IDs are
      validated and persisted by the Store, not supplied as authority. This
      prepares exact before/after bytes and reserves the manager's one Pending
      slot. No inference callback may begin until [commit] yields Admitted. *)
  val prepare_admit
    :  t
    -> claim:Claim.t
    -> relation:relation
    -> configuration:Inference.Observation.Configuration.t
    -> (Pending.t, Error.t) result

  (** Separate saved-Agent admission into the SAME manager, writer, Pending
      barrier, row limits and recovery. Require a physically entered Running
      Async_tool Claim whose immutable payload has a structurally valid
      Developer_agent_artifact.Saved_tool_request v2 Tool reference. Version 1,
      Script and general Async_tool requests remain ineligible. This Store
      classification proves neither the referenced artifact's bytes nor
      source/native/actor/provider permission: Root must first hold and keep
      checking its genuine selected saved-source and dispatched native scopes.
      [prepare_admit] above remains strictly Model_call-only. All later changes
      use the exact Registration and physical Claim issued by this manager;
      they cannot select a different job kind or infer authority from a copied
      payload. The remaining preparation/commit contract is identical. *)
  val prepare_admit_saved_agent
    :  t
    -> claim:Claim.t
    -> relation:relation
    -> configuration:Inference.Observation.Configuration.t
    -> (Pending.t, Error.t) result

  (** Re-read the exact registered row through the issuing handle and writer
      lease. This may yield and is never an effect-currentness lease. *)
  val observe : t -> Registration.t -> (View.t, Error.t) result

  (** Validates exact registration/Claim/job/attempt, fresh View, full immutable
      Scope/accounting/configuration and canonical Attempt_record bounds.
      Started is allowed only while the exact issued job remains durable Running
      and its epoch remains latest Active;
      an entered historical claim may record only already-started observations
      and truthful terminal/interruption bookkeeping. Observed uses complete
      authoritative snapshots: higher revisions replace, exact duplicates and
      stale revisions produce Unchanged, same-revision disagreement conflicts.
      A retained terminal may receive a later matching observation but cannot
      reopen or change its terminal evidence. Finished must match the original
      full Scope and its delivery/outcome combination. An invalid change leaves
      the prior row untouched. *)
  val prepare_change
    :  t
    -> claim:Claim.t
    -> registration:Registration.t
    -> previous:View.t
    -> change:change
    -> (preparation, Error.t) result

  (** Both preparation kinds settle through ONE typed operation and the Store's
      existing exact Prepared/pending classifier. [receipt] distinguishes the
      only legal acknowledgement of each kind. [admit] is a trusted
      nonyielding physical host-currentness check at the pre-rename boundary;
      it may reject new work while permitting truthful old-attempt cleanup.
      Store additionally rechecks this manager's exact Run_claim, handle,
      entered worker Claim, job/attempt, retained Pending and CAS bytes; an
      always-true host callback cannot replace those lower checks. It performs
      no I/O or callback replay. Unexpected exceptions and Eio
      cancellation retain this manager's exact Pending and their original
      identity/backtrace. Repeating an already-settled exact Pending returns
      its original receipt without rewriting or minting another registration. *)
  val commit : t -> Pending.t -> admit:(unit -> bool) -> (receipt, Error.t) result

  (** Classify an uncertain attempted write using the exact retained token;
      Installed acknowledges its original bytes, Absent permits
      [reprepare_same] or the narrower fresh-proof
      [retire_absent_admission] when no Attempt/callback was ever admitted, or
      [retire_absent_start] after a failed strict Start before provider entry,
      and Diverged remains quarantined. A proven prewrite
      conflict retires only this never-attempted token; it cannot adopt another
      writer's identical after-bytes. The application callback is never retried. *)
  val reconcile : t -> Pending.t -> (reconciliation, Error.t) result

  val reprepare_same : t -> Pending.t -> (Pending.t, Error.t) result

  (** Exposes the single physical Pending to its owning Root after a lexical
      inference callback unwinds; it remains live until exact settlement. This
      value grants no separate writer and cannot be copied across managers. *)
  val pending : t -> Pending.t option

  (** Retire only a prepared admission that has conclusively never attempted a
      replacement and has never exposed a callback; a change or uncertain
      admission cannot be dropped. The Store's pending slot is released only
      when that exact token is removed. *)
  val abort_unattempted_admission : t -> Pending.t -> (unit, Error.t) result

  (** Retire an admission whose exact Prepared is FRESHLY classified Absent
      under this manager's Store lease, after the admitting callback unwound
      without receiving an Admitted Registration or beginning an Attempt.
      Unlike [abort_unattempted_admission], this can retire a previously
      attempted rename that is now conclusively absent; it does not reprepare
      after the owning Worker has become terminal. The method reclassifies the
      current bytes itself and atomically removes only its exact pending token
      without a yield between proof and retirement. A remembered Absent flag,
      copied Pending, other manager, Installed, unknown or Diverged outcome
      cannot authorize retirement. An installed registration/attempt is never
      dropped. This performs no callback or new document write. *)
  val retire_absent_admission : t -> Pending.t -> (unit, Error.t) result

  (** Retire only this manager's attempted Prepared-to-Running Start mutation
      after the strict on_attempt callback failed before provider entry and its
      physical callback/children have joined. The trusted Root proves that
      lifecycle fact. Under one Store writer lease, this method independently
      checks the exact original issued Registration, prior Prepared row, same
      immutable Running proposal, no receipt and a FRESH Absent classification.
      It removes only that Pending token; the Registration remains issued so
      Root can durably record Interrupted from the unchanged Prepared row.
      Installed, unknown, divergent and stale prewrite outcomes retain custody
      and never authorize dropping a Started record. *)
  val retire_absent_start : t -> Pending.t -> (unit, Error.t) result

  (** Finish after joined callbacks, no Pending, and a fresh bounded document
      read proving every registration issued by this manager is Terminal or
      Interrupted. This unregisters the exact manager and invalidates its
      tokens. It never closes the shared Store or other runs. Run_claim.release
      rejects while the manager is open; a second manager cannot open on this
      SAME claim after finish. Failure retains the manager/claim for bounded
      reconciliation and prevents capacity release by the higher owner. *)
  val finish : t -> (unit, Error.t) result
end

(** Open one singleton facade for this physical Session_store under its existing
    daemon lock. The implementation checks the lock remains live and belongs to
    the expected root/host on every operation; Session_store.close invalidates
    all handles. An additive developer-runs subtree is admitted without changing
    the existing root/session schema. Absence on an older root means empty. A
    bounded startup scan rejects links, unknown authoritative files, duplicate
    run IDs and malformed documents. Recognized incomplete atomic-writer
    temporary files are never treated as authority. No separate daemon lock or
    writer exists. Opening syncs the daemon root after finding or creating the
    subtree, including a retry after an uncertain subtree creation.
    The issuing Store retains the actual Eio Domain_manager from this opening
    environment solely for bounded pure canonical document CPU work. Physical
    Store/Root/actor handles, directory I/O, leases, original callbacks and
    final admission remain on their issuing domain. A joined in-flight read or
    encode cannot outlive [close_and_wait].
    One final versioned document layout has checkpoint, invocation and job
    fields from the outset; the latter may be empty. Unknown versions fail
    closed. There is no intermediate milestone schema or migration. The caller
    retains [sw] until [close_and_wait]. [checkpoint] still runs on every
    charged Store work step on the issuing domain. The Store also yields at a
    finite charged cadence during typed traversal, even if the trusted
    callback only checks cancellation; no yield is inserted between final
    nonyielding before-rename admission and rename entry. *)
val open_or_create
  :  Session_store.t
  -> sw:Eio.Switch.t
  -> limits:Limits.t
  -> checkpoint:(unit -> unit)
  -> (t, Error.t) result

(** Trusted host wiring first authenticates the real principal/run and compares
    [capture_fingerprint] with its actual admitted Chatml_developer_capture.t.
    This lower store persists those coordinates and checks exact current epoch;
    it does not authenticate a caller or admit an effect. Repeating an identical
    admission returns the same run; a changed capture/principal conflicts. A new
    epoch requires an exact prior observation and interrupts the previous epoch;
    its older claimed jobs can later finish bookkeeping without new execution.
    The configured epoch/run quota is reported as [Limit_exceeded]; malformed
    over-limit documents found on disk fail recovery. A live Run_claim for this
    run rejects same/new-epoch admission until the physical owner relinquishes
    it. Repeating the persisted latest owner and capture with [previous = Some
    exact_observation] is also the read-first startup-recovery route: no epoch
    is created, and the resulting handle alone grants no execution. The host
    must finish recovery before reserving a fresh command, then independently
    reauthorize any new effects against current captured source and actor.
    A missing run or changed latest epoch/capture conflicts; same-epoch reissue
    reads the current document revision rather than promising the previous
    revision is unchanged. This route cannot silently recreate a run from an
    observed absence. The exclusion is registered
    before [admit_epoch]'s first yield and
    removed on every exit; a Run_claim cannot slip in while that operation
    waits for I/O. Other runs remain independent. *)
val admit_epoch
  :  t
  -> owner:Agent_protocol.Execution_owner.developer
  -> capture_fingerprint:Chatml_source.Digest.t
  -> previous:Observation.t option
  -> (handle * Observation.t, Error.t) result

val owner : handle -> Agent_protocol.Execution_owner.developer
val capture_fingerprint : handle -> Chatml_source.Digest.t
val issued_by : t -> handle -> bool
val is_closed : t -> bool

(** Read-only exact configured whole-document JSON limits from this physical
    live Store handle. The external Model.spawn actor uses them to re-admit a
    decoded recipe and its complete nonsecret Target envelope before staging
    capacity. This is not admission authority; the Store repeats all current
    job, aggregate payload and document checks under its writer lease. *)
val document_limits : handle -> (Document_schema.Limits.t, Error.t) result

(** Comparison-only exact immutable limits configured on this physical live
    Store. A caller-supplied wider policy cannot replace them. Closed Store
    rejects. No I/O, command reservation or result disclosure follows. *)
val limits : t -> (Limits.t, Error.t) result

(** Physical issuer comparison for host wiring of the one shared blob
    coordinator. IDs, root paths and host labels do not substitute for this. *)
val issued_by_session_store : t -> Session_store.t -> bool

val observe : handle -> (Observation.t, Error.t) result

(** The same bounded physical observation, admitted only while this handle's
    epoch is the document's latest Active epoch in that exact writer read.
    This is a comparison snapshot, not execution authority; a later CAS must
    still validate the same condition under its own writer lease. *)
val observe_latest_active : handle -> (Observation.t, Error.t) result

(** Read one current document through [handle]'s existing Store lease and
    configured byte/work limits, then project its queued jobs for this exact
    Active owner/epoch/capture. Every decoded job visited and candidate
    retained is charged before allocation to the same Store work allowance;
    [checkpoint] runs between bounded steps and propagates exception or
    cancellation unchanged. At most the Store's configured jobs-per-run limit
    can be returned. Large pure canonical decoding follows the same bounded
    Store CPU path as physical claim preparation and commit; typed projection
    yields at the charged cadence on the issuing domain. This does not use the
    full retention inventory or grant a worker Claim. A later claim must
    reread/revalidate the exact observed
    lookup and current owner. No partial candidate list escapes on error. *)
val scheduler_candidates
  :  handle
  -> checkpoint:(unit -> unit)
  -> (Scheduler_candidate.t list, Error.t) result

(** Storage custody for an existing developer run. This is not a worker claim,
    execution grant, current-epoch assertion or result-disclosure grant. A run
    root deliberately covers every retained epoch of that run. *)
module Result_root : sig
  type t

  (** Read the exact authority document through the issuing live Store before
      minting a root. An old-epoch handle is valid when its original epoch and
      capture still occur in that document; latest/Active is not required. *)
  val of_handle : handle -> (t, Error.t) result

  (** Select only an already persisted run. Missing is distinct from corrupt;
      this operation cannot admit a new epoch or create a run file. *)
  val find_existing
    :  store
    -> Agent_protocol.Id.Developer_run.t
    -> (t option, Error.t) result

  (** Observe the persisted latest epoch of this exact existing run for startup
      recovery, before source recapture. The returned physical Observation
      includes its original owner, capture and observed document revision; it
      is comparison evidence only. A foreign or changed root, unresolved Store
      Pending, missing run, closed Store or corrupt document rejects. The read
      uses the Store's bounded lease, charged work and cooperative checkpoint.
      The host may pass [Some observation] to [admit_epoch] with the exact
      Checkpoint owner/capture, then acquire a physical Run_claim and open
      Recovery. [admit_epoch] re-reads the latest epoch and may observe a newer
      same-epoch document revision; Recovery re-reads under the exclusive
      claim. It must not treat this read or the reissued handle as a current
      source, actor or effect grant. *)
  val observe_latest : t -> (Observation.t, Error.t) result

  (** Read an already persisted command key before any source recapture or
      epoch admission. [principal] must match this physical root's retained
      principal; it is comparison data, not a disclosure grant. The host
      authenticates the actor and receipt-read policy before and after this
      yielding operation. A returned row retains its original owner, capture,
      Receipt.Id and phase; its epoch/capture must occur in the validated
      document. [None] is an authoritative key miss only when this run has no
      unresolved Store Pending. Closed/foreign/corrupt or uncertain state
      fails closed. The existing Store lease, document bounds, charged work and
      cooperative checkpoint apply. This lookup neither mints a handle nor
      grants Start, terminal mutation, recapture, or result disclosure. *)
  val find_command_snapshot
    :  t
    -> principal:Agent_protocol.Id.Principal.t
    -> key:Agent_protocol.Idempotency_key.t
    -> (Developer_command_row.t option, Error.t) result

  (** Read one exact historical job through this root's physically issuing
      Store and daemon writer lease. [owner] must match this root's host, run
      and principal; its epoch must occur in the authentic document with the
      exact [capture_fingerprint]. A found job must match the entire owner,
      capture, ID and attempt. [None] means only that this authenticated epoch
      contains no job with [job_id]; a job in another epoch, changed attempt,
      malformed document, closed Store or uncertain read is an error. The
      returned immutable snapshot is comparison data, never a result-read
      grant. This operation uses the Store's existing document byte/work caps
      and cooperative checkpoint, and releases its lease before the caller
      enters Blob_store. Repeat after Blob I/O to reject a changed state; a
      historical epoch need not be the latest Active epoch. *)
  val find_job_snapshot
    :  t
    -> owner:Agent_protocol.Execution_owner.developer
    -> capture_fingerprint:Chatml_source.Digest.t
    -> job_id:Agent_protocol.Id.Job.t
    -> attempt:int
    -> (Agent_protocol.Developer_job.t option, Error.t) result

  (** Return all existing roots in stable run-ID order. The same bounded,
      checkpointed recovery scan validates every name, regular file, document,
      host/run/principal identity and aggregate entry/byte/work limit before
      returning any root. No callback runs under the daemon-root lease. *)
  val existing : store -> (t list, Error.t) result

  (** The same complete validated scan during shared blob retention, borrowing
      the caller's Retention_reader allowance for every run-directory entry and
      document byte as well as this Store's own recovery/work ceilings. The
      reader is rebound only to this Store's leased developer-runs directory;
      it never authorizes a caller-selected path. No root escapes on partial
      error, and no callback runs while leased. *)
  val existing_for_retention
    :  store
    -> reader:Retention_reader.t
    -> (t list, Error.t) result

  (** The same complete Store recovery traversal for result startup. In
      addition to Store's own budget and the shared reader, [checkpoint]
      charges the enclosing result-inventory work allowance before each
      entry, decode/publication step and bounded read chunk. Its exception or
      cancellation propagates unchanged; this function does not manufacture a
      new work budget. *)
  val existing_for_result_inventory
    :  store
    -> reader:Retention_reader.t
    -> checkpoint:(unit -> unit)
    -> (t list, Error.t) result

  val host_id : t -> Agent_protocol.Id.Server.t
  val run_id : t -> Agent_protocol.Id.Developer_run.t
  val principal_id : t -> Agent_protocol.Id.Principal.t

  (** Physical issuer check against the bound Store; coordinates alone are
      insufficient even when another Store names the same native directory. *)
  val issued_by : store -> t -> bool

  (** Borrow the issuing physical daemon root only while [f] runs. Every borrow
      rechecks store lifetime, original lock and exact persisted run identity;
      it never tests whether the run's newest epoch is Active. The callback must
      not reenter this Store or close it, and must not retain [Data_root.t] as
      authority. Blob_store enters its shared coordinator before this lease;
      no Developer_run_store method may call Blob_store while leased. The
      callback's exception, cancellation and backtrace propagate unchanged. *)
  val with_lease
    :  t
    -> f:(root:Data_root.t -> ('a, Store_error.t) result)
    -> ('a, Store_error.t) result

  (** The same authenticated root borrow for private result-intent I/O, using
      the issuing Store's exact Eio filesystem capability rather than a
      caller-supplied [env]. It holds the run/root storage gate but NOT the
      Blob coordinator. The callback may use only the selected private
      namespace and must not reenter this Store, acquire Blob_store, or close
      the owner while leased. The publisher releases this lease before Blob
      staging, which acquires Blob coordination before its own root borrow. *)
  val with_storage_lease
    :  t
    -> f:(env:Eio_unix.Stdenv.base -> root:Data_root.t -> ('a, Store_error.t) result)
    -> ('a, Store_error.t) result

  (** Retention borrow with the same root/lifetime checks. Its document
      revalidation charges the enclosing reader as well as Store recovery
      limits before [f] sees the root; no hidden fresh budget is used. *)
  val with_retention_lease
    :  t
    -> reader:Retention_reader.t
    -> f:(root:Data_root.t -> ('a, Store_error.t) result)
    -> ('a, Store_error.t) result

  (** The same root borrow for result startup, charging every document read,
      decode and bounded chunk to both the shared reader and caller work
      allowance. No callback runs unless the issuing root is still exact. *)
  val with_result_inventory_lease
    :  t
    -> reader:Retention_reader.t
    -> checkpoint:(unit -> unit)
    -> f:(root:Data_root.t -> ('a, Store_error.t) result)
    -> ('a, Store_error.t) result

  module Inventory_rows : sig
    type t

    val epochs : t -> (int * Chatml_source.Digest.t) list
    val jobs : t -> Agent_protocol.Developer_job.t list
  end

  (** Read the authentic persisted run document ONCE for a complete result
      inventory. Historical epoch identity is necessary even for a verified
      empty epoch directory. Each returned job retains its actual owner,
      epoch and capture; both projections are comparison evidence only. The
      Store document ceilings, shared [reader] and [checkpoint] charge read,
      decode, each epoch/job visit and retained-list construction. No worker
      Claim or result authority follows. A Blob must still match one exact
      terminal job, not merely coincide in Blob ID. *)
  val rows_for_inventory
    :  t
    -> reader:Retention_reader.t
    -> checkpoint:(unit -> unit)
    -> (Inventory_rows.t, Store_error.t) result
end

(** A single physically issued, bounded borrow for cross-owner collection.
    It is entered only after the shared Blob coordinator. The callback holds
    this Store's existing developer-root gate from complete run enumeration
    through the caller's final deletion; Store writes, claim release and close
    cannot enter in between. This is not a worker Claim, result disclosure or
    permission to address arbitrary native paths. *)
module Retention : sig
  type t
  type run

  (** Reuse the canonical no-follow Store recovery scan and document codec,
      debiting every directory entry, document byte, retained row and bounded
      processing step to [reader]/[checkpoint] in addition to Store limits.
      A failed scan exposes no partial inventory. The callback must not reenter
      this Store or retain [t] as authority; exceptions/cancellation preserve
      their original identity and release the gate. *)
  val with_inventory
    :  store
    -> reader:Retention_reader.t
    -> checkpoint:(unit -> unit)
    -> f:(t -> ('a, Store_error.t) result)
    -> ('a, Store_error.t) result

  (** Stable, bounded projections of the same authenticated documents read
      once under [with_inventory]. Original bytes are kept so a conservative
      reference scan does not lose unknown or escaped fields. *)
  val runs : t -> run list

  (** The already borrowed canonical Data_root for an at_root view of the same
      Reader allowance. This path is comparison-only and grants no new root. *)
  val root_directory : t -> string

  val result_root : run -> Result_root.t
  val document_bytes : run -> string

  val iter_document_strings
    :  run
    -> checkpoint:(unit -> unit)
    -> f:(string -> unit)
    -> unit

  val epochs : run -> (int * Chatml_source.Digest.t) list
  val jobs : run -> Agent_protocol.Developer_job.t list

  (** Callback-only location for the existing exact intent-file algorithm.
      It checks the still-live borrow; it neither reacquires the same root gate
      nor treats [Data_root.t] alone as an authority after callback return. *)
  val with_root
    :  t
    -> result_root:Result_root.t
    -> f:(env:Eio_unix.Stdenv.base -> root:Data_root.t -> ('a, Store_error.t) result)
    -> ('a, Store_error.t) result
end

(** Comparison boundary for a result selected during this Store's exclusive
    startup Recovery window. These checks do not read or stage result bytes,
    mint a Claim, or commit a terminal. They bind the physical Store/root,
    current one-shot window, exact View/Candidate and original historical
    owner/epoch/capture before higher result custody uses filesystem evidence.
    Every check rereads current Store authority, so call it before and after
    yielding inventory or stage I/O; an earlier [Ok] is never a lease. *)
module Result_recovery_affinity : sig
  (** Every authoritative read uses this action's shared bounded Reader and
      checkpoint. A caller cannot use an earlier uncharged observation to
      bypass currentness after a yielding scan or stage operation. *)
  val check_view
    :  t
    -> root:Result_root.t
    -> window:Recovery.t
    -> view:Recovery.View.t
    -> reader:Retention_reader.t
    -> checkpoint:(unit -> unit)
    -> (unit, Error.t) result

  (** [candidate] must belong to [view] from this physical window. [pending]
      must be that window's CURRENT exact preparation for the candidate's
      job/attempt and byte-identical [after]. A matching DTO or Pending from
      another window/Store does not pass. Successful terminal installation
      clears Pending and makes this operation reject. *)
  val check_pending
    :  t
    -> root:Result_root.t
    -> window:Recovery.t
    -> view:Recovery.View.t
    -> candidate:Recovery.Candidate.t
    -> pending:Recovery.Pending.t
    -> after:Agent_protocol.Developer_job.State.t
    -> reader:Retention_reader.t
    -> checkpoint:(unit -> unit)
    -> (unit, Error.t) result

  (** Exact installed-byte comparison for the original physical window and
      retained Pending. The Store-issued Installed observation must contain
      that Pending's COMPLETE prepared after-bytes. Installed has no separate
      provenance token; this claims only exact persisted identity, not a
      stronger provenance fact. The same reader validates the exact selected
      job in the current document before the marker is acknowledged; unrelated
      later job revisions may advance that document. The original window must
      remain open through acknowledgement. *)
  val check_installed
    :  t
    -> root:Result_root.t
    -> window:Recovery.t
    -> view:Recovery.View.t
    -> candidate:Recovery.Candidate.t
    -> pending:Recovery.Pending.t
    -> after:Agent_protocol.Developer_job.State.t
    -> installed:Installed.t
    -> reader:Retention_reader.t
    -> checkpoint:(unit -> unit)
    -> (unit, Error.t) result
end

(** Read the authoritative latest epoch/phase under the daemon writer. This is
    an epoch-lifecycle check, not permission to execute a job. *)
val is_latest_active : handle -> (bool, Error.t) result

(** Foundation checkpoint-only CAS. The prepared value makes no durability
    claim. A successful commit atomically replaces the file and syncs the
    parent directory before acknowledgement; an interrupted replacement must
    be reconciled. No job is visible through this operation. A new revision
    must be positive, representable and monotone. *)
val prepare_checkpoint
  :  handle
  -> expected:Observation.t
  -> phase:Checkpoint.phase
  -> (Prepared.t, Error.t) result

(** First native CAS may install Dispatching directly from observed absence;
    there is no synthetic persisted Admitted step. If Admitted is already
    present, exact context and entry revision must match. Terminal or already
    Dispatching records reject. The higher actor must confirm durability before
    any callback effect. *)
val prepare_dispatch
  :  handle
  -> expected:Observation.t
  -> lookup:Invocation_lookup.t
  -> dispatched:Agent_protocol.Host_invocation.t
  -> (Prepared.t, Error.t) result

(** Atomically advance this run checkpoint with newly selected Run_root jobs
    from a direct run action, so none has a claimed parent job. A worker may
    later launch from the run scope with a real parent job; that requires the
    separate physical worker-claim path, not this root-action entrypoint. This
    lower layer validates owner/capture/epoch/launch and immutable new queued
    shape; a live root scope and policy belong to the actor. *)
val prepare_root_selected
  :  handle
  -> expected:Observation.t
  -> phase:Checkpoint.phase
  -> selected:Agent_protocol.Developer_job.t list
  -> (Prepared.t, Error.t) result

(** One CAS changes the exact Dispatching invocation to terminal and appends
    only its newly selected child jobs. Native active-parent proof remains an
    actor responsibility. Empty selected preserves the old no-child behavior. *)
val prepare_terminal_selected
  :  handle
  -> expected:Observation.t
  -> lookup:Invocation_lookup.t
  -> terminal:Agent_protocol.Host_invocation.t
  -> selected:Agent_protocol.Developer_job.t list
  -> (Prepared.t, Error.t) result

module External_model_parent : sig
  type t =
    | Run_root
    | Dispatching of Invocation_lookup.t
    | Claimed_model of Claim.t * Job_lookup.t
end

(** Prepare exactly one fresh Queued/attempt-zero Model_call as an immediate
    external effect in the latest Active epoch. [job] carries the versioned
    bounded Developer_model_call envelope, including the complete nonsecret
    selected Target and original recipe JSON. Run_root has no parent job and
    depth zero; Claimed_model has a Run_root launch with the exact physically
    entered Running Model_call parent, attempt and next depth. The Job_lookup
    must come from [expected]; the actual Claim must be entered and retained
    by this physical Store. The Store rechecks its currentness under its writer
    lease. No native invocation is invented. Dispatching must be the physically
    observed lookup from [expected] for the exact same-epoch invocation named
    by the job's launch.
    The current durable invocation must still be Dispatching. Recorded
    parent-job relation is checked structurally here; live callback, actor,
    selected provider and final effect admission belong to the host.

    This distinct Prepared kind uses only the existing per-run pending barrier
    and exact Handoff through [commit_and_settle]/[reconcile_and_settle]. It
    cannot be passed to foundation [commit]/[reconcile] or selected-root/native
    preparation. A copied lookup, job ID or payload never grants execution.
    Stale/foreign/duplicate/malformed proposals return Conflict; admitted
    job/payload/document budget exhaustion returns Limit_exceeded. Persisted
    malformed bytes remain Store errors. No callback is replayed on uncertain
    write; the host retains its exact Prepared/Handoff and capacity permit. *)
val prepare_external_model
  :  handle
  -> expected:Observation.t
  -> parent:External_model_parent.t
  -> job:Agent_protocol.Developer_job.t
  -> (Prepared.t, Error.t) result

(** Bounded claim preparation from an exact queued lookup in the latest
    Active epoch. A trusted host samples [now], proves due/current authority
    and acquires the existing shared capacity lease before this CAS. The
    resulting distinct Prepared changes only this job to Running, increments
    the attempt once, starts it and clears retry wait. Its fresh read and
    canonical encode use the same bounded cooperative CPU path as candidate
    scanning; physical CAS/currentness remain under the original Store writer
    lease. No callback can run yet. *)
val prepare_job_claim
  :  handle
  -> expected:Observation.t
  -> lookup:Job_lookup.t
  -> now:Agent_protocol.Timestamp.t
  -> (Prepared.t, Error.t) result

module Claimed : sig
  type t

  val claim : t -> Claim.t
  val observation : t -> Observation.t
end

type claim_reconciliation =
  | Claimed of Claimed.t
  | Claim_absent
  | Claim_diverged

(** The one document writer and per-run pending barrier durably install Running
    before minting a physical Claim. Unknown commit/cancellation enters no
    callback; exact bounded reconciliation can establish Installed. Repeating a
    settled Prepared returns the same Claim even after unrelated document
    mutations. Two separate preparations of the same queued observation and
    timestamp cannot mint two Claims: only the Prepared that attempted the
    replacement may accept identical installed after-bytes. A proven prewrite
    conflict retires only its own pending token; attempted or uncertain writes
    retain the exact Prepared for recovery. *)
val commit_job_claim : handle -> Prepared.t -> (Claimed.t, Error.t) result

val reconcile_job_claim : handle -> Prepared.t -> (claim_reconciliation, Error.t) result

(** Prepare a same-epoch cancellation of the exact Queued or
    Waiting_completion job in [lookup]. This uses the one versioned document,
    the same writer/pending barrier and the same bounded work ledger as a
    claim; bare IDs and copied job records are not authority. A concurrent
    claim or epoch change cannot be cancelled through this path. No physical
    callback or capacity lease is touched by preparation. *)
val prepare_inactive_cancel
  :  handle
  -> expected:Observation.t
  -> lookup:Job_lookup.t
  -> now:Agent_protocol.Timestamp.t
  -> (Prepared.t, Error.t) result

(** Admit a prepared inactive cancellation under the Store writer. [admit] is
    the trusted owner's final non-yielding currentness/lifetime check just
    before rename entry; it cannot perform Store I/O or Eio operations. An
    Error/cancellation may follow a durable rename, so the caller retains the
    exact Prepared and reconciles before releasing capacity or reporting a
    terminal cancellation. A proven prewrite CAS conflict retires only this
    Prepared and may be retried with a fresh Observation; an attempted or
    uncertain write is never blindly retried. *)
val commit_inactive_cancel
  :  ?admit:(unit -> bool)
  -> handle
  -> Prepared.t
  -> (Installed.t, Error.t) result

val reconcile_inactive_cancel : handle -> Prepared.t -> (reconciliation, Error.t) result

(** Fresh-Observation CAS for the SAME physical Claim/owner/capture/job/attempt
    and immutable launch/payload/retry fields; unrelated document revisions do
    not invalidate claim ownership. Current epoch allows Waiting_permission
    within the live scope, resumption to Running, then Queued retry,
    Waiting_completion or terminal. A stale epoch permits terminal bookkeeping
    only, never another effect/wait/retry. [revoked_after] is selected under the
    same authoritative read, so a concurrent reset cannot publish a proposed
    retry or wait; an already issued Prepared is never rewritten. A
    completed/superseded attempt cannot be overwritten. Waiting_completion later resolves by actor authority, not
    by entering this claim again. Result/artifact custody remains higher-layer. *)
val prepare_claimed_transition
  :  handle
  -> expected:Observation.t
  -> claim:Claim.t
  -> after:Agent_protocol.Developer_job.State.t
  -> revoked_after:Agent_protocol.Developer_job.State.t
  -> (Prepared.t, Error.t) result

(** The same physical Claim/handle/domain, fresh Observation and one-shot
    Prepared path as [prepare_claimed_transition]. In the latest Active epoch,
    prepare one document containing [after] and the exact selected children;
    after an epoch change prepare only terminal [revoked_after] bookkeeping
    and omit all new children. Parent and child rows never use separate writes.
    The caller's physical Worker retains child capacity until the exact
    Prepared is installed, proven absent, or remains quarantined. *)
val prepare_claimed_selected
  :  handle
  -> expected:Observation.t
  -> claim:Claim.t
  -> after:Agent_protocol.Developer_job.State.t
  -> revoked_after:Agent_protocol.Developer_job.State.t
  -> selected:Agent_protocol.Developer_job.t list
  -> (Prepared.t, Error.t) result

module Claimed_selection : sig
  type disposition =
    | Selected_installed
    | Selected_omitted_after_epoch_change
  [@@deriving sexp_of]

  (** Read only after [commit_claimed_transition] or
      [reconcile_claimed_transition] returned this exact Installed result.
      Verify the Prepared and Installed belong to the same issuing Store,
      handle/owner and after-document. Compare every selected immutable child
      against that document: all exact rows yields Selected_installed; no rows
      after an actual epoch change yields Selected_omitted_after_epoch_change;
      partial, foreign or changed rows fail closed. Empty selection is
      vacuously installed. A terminal parent status, copied child ID or
      independently constructed DTO is never evidence of publication. *)
  val installed_disposition
    :  prepared:Prepared.t
    -> installed:Installed.t
    -> (disposition, Error.t) result
end

(** An issued Claim whose callback never entered can be abandoned exactly once
    by the trusted worker owner. This atomically closes its entry gate before
    preparing only Cancelled or Interrupted bookkeeping for the same Running
    attempt. It cannot create a native effect, retry or result. If I/O is
    uncertain, reconcile the exact Prepared without reopening entry. *)
val prepare_unentered_claim_cleanup
  :  handle
  -> expected:Observation.t
  -> claim:Claim.t
  -> status:[ `Cancelled | `Interrupted of string ]
  -> now:Agent_protocol.Timestamp.t
  -> (Prepared.t, Error.t) result

(** A first-commit CAS mismatch before any write returns [Prewrite_conflict]
    and retires only this pending token. An attempted write never takes that
    path, even if a later observation diverges. *)
val commit_claimed_transition
  :  ?admit:(unit -> bool)
  -> handle
  -> Prepared.t
  -> (Installed.t, Error.t) result

val reconcile_claimed_transition
  :  handle
  -> Prepared.t
  -> (reconciliation, Error.t) result

(** Foundation checkpoint-only commit. A developer dispatch or selected-job
    Prepared is rejected; it must use [commit_and_settle]. One serialized writer
    replaces the exact document and syncs file/directory before acknowledgement.
    An Error or cancellation may occur after durability, so [reconcile] checks
    authority before a caller interprets that outcome. The original
    cancellation/exception identity is preserved. *)
val commit : handle -> Prepared.t -> (Installed.t, Error.t) result

(** Foundation checkpoint-only reconciliation; a dispatch or selected-job
    Prepared is rejected. Sync the owned parent directory, then compare the
    bounded authoritative
    file with the prepared exact before/after bytes and revision. An Installed
    response therefore confirms the observed rename is durable even if the
    original replacement was interrupted during directory sync. Diverged means
    another authoritative state exists and requires explicit recovery. *)
val reconcile : handle -> Prepared.t -> (reconciliation, Error.t) result

module Settlement : sig
  type t =
    | Installed of Installed.t
    | Absent
    (** This selected transaction never attempted replacement and lost its
        first CAS, including an identical after-image installed by another
        Prepared. Its exact pending token is retired; provisional capacity
        remains with the caller for bounded fresh-observation preparation. *)
    | Prewrite_conflict
    | Diverged
end

module Handoff : sig
  type t

  (** Trusted host adapter over the exact provisional shared-capacity
      reservations for one prepared transaction. The callback must be
      idempotent, acquire only the capacity mutex and never re-enter this
      store. First use binds this token physically to one Prepared value. *)
  val create : settle:(Settlement.t -> unit) -> t
end

(** One daemon-root lease covers bounded compare/replace, file+directory
    durability, exact reconciliation and protected capacity settlement. The
    settlement callback may repeat until it succeeds; a successful settlement
    is recorded once and a repeat of the same pair returns that historical
    outcome without writing or calling it again. Native/application effects do
    not repeat. Installed publishes, exact Absent aborts and Diverged quarantines.
    A proven prewrite conflict retires only this Prepared's pending token; its
    callback must retain staged capacity so the caller can reprepare the same
    sealed selection from a fresh Observation, with a finite retry bound and
    without replaying the root action. An identical after-image from a
    different never-attempted Prepared is also a prewrite conflict, not an
    adoption of publication authority. An attempted or unknown write remains
    quarantined on divergence. Unknown I/O retains the bounded pending pair;
    a later mutation/reset for the same run is rejected until reconciliation.
    Unexpected exception or
    cancellation preserves its original identity after protected bookkeeping.
    [admit] is a trusted non-yielding host proof checked after temporary-file
    sync and immediately before entering the authoritative rename. False
    prevents publication, settles an exact absent handoff and returns
    [Foreign_or_stale]. The host must bind it to the retained actor, physical
    run scope, current selection and capture; a caller-supplied ID/digest is
    not proof. The check cannot be atomic with an external authorization
    change during the kernel rename. Reconciliation of a write already
    attempted never calls [admit] or replays the action.
    On process restart the host rebuilds capacity from durable selected jobs. *)
val commit_and_settle
  :  handle
  -> Prepared.t
  -> Handoff.t
  -> admit:(unit -> bool)
  -> (Settlement.t, Error.t) result

(** One bounded read/settlement attempt under the same lease, no replay loop.
    Only the retained physical pair is accepted. A known successful settlement
    releases the pending barrier; divergence remains quarantined. *)
val reconcile_and_settle
  :  handle
  -> Prepared.t
  -> Handoff.t
  -> (Settlement.t, Error.t) result

module Command : sig
  module Receipt = Developer_command_receipt
  module Row = Developer_command_row

  module View : sig
    type t

    (** Historical comparison data tied to one exact Store observation. Neither
        a copied row nor a View grants execution or result disclosure. *)
    val row : t -> Row.t
  end

  module Pending : sig
    type t

    (** The immutable selected row in this command's complete after-image.
        Pending remains owned by the existing per-run Store pending map. *)
    val after : t -> Row.t
  end

  module Execution_start : sig
    type t

    (** Consume this process-local acknowledgement once on its issuing domain.
        Only the first uninterrupted successful commit of a fresh Start can
        issue one. Reconciliation, repeat commit and historical reads cannot.
        Consumption is nonyielding and rejects a foreign/closed Store or a
        previously consumed acknowledgement. It is NOT host execution
        authority: the caller must also enter its actual owned Root and apply
        current actor/source/provider checks at every effect boundary. *)
    val consume : t -> (unit, Error.t) result
  end

  type preparation =
    | Unchanged of View.t
    | Prepared of Pending.t

  type installed =
    { view : View.t
    ; execution_start : Execution_start.t option
    }

  type reconciliation =
    | Installed of View.t
    | Absent
    | Diverged

  (** Read a retained key across epochs of this same stable principal/run.
      Return its original owner/capture/ID/phase. This may yield; the host
      authenticates historical access before and after this operation. A
      command-key miss is never proof that a Pending write did not happen.
      An unresolved same-run mutation rejects ordinary observation. *)
  val observe
    :  handle
    -> key:Agent_protocol.Idempotency_key.t
    -> (View.t option, Error.t) result

  (** Return this handle's exact command Pending from the existing Store map.
      A different kind of pending mutation is Conflict, not absent. Lookup is
      nonyielding and does not release or settle custody. *)
  val pending : handle -> (Pending.t option, Error.t) result

  (** Under the sole writer, require latest Active original epoch/capture and
      prove absence before allocating an ID. An existing identical
      kind/request/capture/epoch returns its persisted View; changed identity
      conflicts. Register the complete immutable Pending before releasing the
      writer or attempting replacement. No new work follows from Unchanged:
      only pure Check/Fixture in the current original Reserved epoch may be
      recomputed, while a stored Start is terminal-only. *)
  val prepare_reservation
    :  handle
    -> key:Agent_protocol.Idempotency_key.t
    -> kind:Receipt.Kind.t
    -> request_fingerprint:Chatml_source.Digest.t
    -> (preparation, Error.t) result

  (** Validate the exact View and immutable row under the writer. Wait/Start
      require Live_run and latest Active original epoch. Finish may settle a
      historical row but cannot append, reopen, change identity, or turn an
      unstarted Live command into success. Only an exact identical terminal
      returns Unchanged. Earlier-epoch pure work may be interrupted, never
      silently recomputed under its old key. All changes share the canonical
      row validator and existing full-document Prepared encoder. *)
  val prepare_change
    :  handle
    -> previous:View.t
    -> change:Row.Change.t
    -> (preparation, Error.t) result

  (** Commit only the exact Store-owned Pending through the existing writer
      and before/after image classifier. New reservation, wait and Start
      recheck original latest Active owner/capture plus trusted nonyielding
      [admit] immediately before rename, after all waits. Finish still calls
      [admit], allowing the host's narrower terminal-bookkeeping policy.
      Successful repeated commit returns the same View with no fresh Start.
      If the first Start acknowledgement is interrupted or uncertain, even an
      installed after-image is observation only: never issue execution_start
      from reconciliation. Exceptions/cancellation preserve identity and raw
      backtrace while retaining exact Pending custody. An unknown/diverged
      write blocks any different mutation; selected terminal bytes never get
      replaced by a recomputed result. *)
  val commit : handle -> Pending.t -> admit:(unit -> bool) -> (installed, Error.t) result

  (** One fresh full-image classification under the writer for an unsettled
      Pending; an already settled exact token returns its Installed observation.
      Never repeat an effect or mint Execution_start. Installed settles this exact Pending.
      Absent and Diverged retain it. Unknown I/O preserves the original token;
      neither a cached Absent nor a key miss releases the pending barrier. *)
  val reconcile : handle -> Pending.t -> (reconciliation, Error.t) result

  (** Recheck exact before-image absence under the writer and replace only
      this Pending with a fresh preparation of the SAME immutable row/change.
      Current-epoch admission is repeated for reserve/wait/Start. A formerly
      unknown Start can be retried only after this proves its barrier absent;
      an installed Start can never be made executable again. No terminal
      result is selected again and no runtime callback is invoked. *)
  val reprepare_same : handle -> Pending.t -> (Pending.t, Error.t) result

  (** Rebase ONLY an exact physically retained Pending whose first write never
      began and whose fresh before/after classifier proves Prewrite_conflict
      from another same-run document writer. Re-read the current document,
      prove the original command row/owner/phase is unchanged (or the reserved
      key is still absent), and rebuild this SAME immutable action and selected
      terminal through the existing bounded full-image encoder. Replace only
      that Pending in the existing map. Unknown, attempted, installed, Absent
      and Diverged writes cannot rebase; no callback or result is reselected,
      and a Start acknowledgement is never minted by this preparation. *)
  val reprepare_after_prewrite_conflict
    :  handle
    -> Pending.t
    -> (Pending.t, Error.t) result

  (** Retire only a reserve/wait/Start whose complete before-image is freshly
      proved present under the writer. This allows cancellation or revocation
      to release an uninstalled admission without requiring renewed live
      authority. An installed/unknown/diverged write rejects, and terminal
      selections cannot be discarded through this operation. There is no
      execution permission to revoke: a settled Start is no longer Pending.
      Cached classification never suffices. *)
  val retire_absent_admission : handle -> Pending.t -> (unit, Error.t) result
end

(** Stop admission, join owned operations and invalidate all handles. An active
    Run_claim rejects explicit close before any state change; the daemon must
    close/join/release all per-run owners first. This check never waits for a
    callback while holding the writer lease. No runtime
    callback is replayed by closing. A physically pending handoff belongs to
    this store lifetime and cannot be acknowledged after close; its exact
    durable document remains for fresh recovery. The host actor must retire
    this run's unclaimed reservations and reconcile durable rows in the same
    daemon-wide capacity ledger before admitting work on a reopened store.
    Claimed worker leases remain charged until their actual cleanup. This
    storage operation itself makes no capacity-recovery claim. *)
val close_and_wait : t -> (unit, Error.t) result
