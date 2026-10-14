module Bundle = Agent_protocol.Authoring_bundle.Bundle
module Captured = Chatml_source
module Build = Chatml_project_build
module Contract = Chatml.Chatml_compiler_contract

(** Pure validation of an exact uploaded Project closure. A manifest is routing
    and selection intent, not source custody, Git provenance or permission to
    publish, read or execute. The publisher separately binds successful values
    to an authenticated live sealed upload and current host policy. *)

module Limits : sig
  type t

  (** One positive bounded allowance across the full uploaded snapshot and all
      selected Project validations, including multiple ChatMD script Projects.
      Retained input charges the exact full snapshot once and each completed
      Build's Prepared input separately, even when strings share storage. *)
  val create
    :  max_work_steps:int
    -> max_retained_input_bytes:int
    -> document:Document_schema.Limits.t
    -> t Core.Or_error.t
end

module Error : sig
  type problem =
    | Snapshot_mismatch
    | Missing_or_surplus_member
    | Wrong_project_basis
    | Missing_selected_script
    | Derived_entry_mismatch
    | Previous_lock_mismatch
    | Unexpected_previous_build
    | Result_mismatch
    | Work_limit
    | Input_limit
    | Accounting_overflow
    | Attempt_closed
    | Attempt_busy
    | Builder of Build.Error.t
    | Project_declaration of Chatml_project.Error.t

  type t

  val problem : t -> problem
end

module Validated : sig
  (** Immutable selected Project evidence. It contains no sealed upload claim,
      host root, source-read grant, native acquisition or runtime authority. *)
  type t

  val manifest_fingerprint : t -> Captured.Digest.t
  val root_source : t -> Captured.Id.t
  val build : t -> Build.t
  val input_fingerprint : t -> Captured.Digest.t
  val selected_input_sources : t -> Captured.t list
end

module Attempt : sig
  (** A single mutable, domain-confined validation ledger. It is not safe for
      concurrent calls. Every error or checkpoint exception invalidates it;
      attempts cannot retry a failed nested Project with a fresh allowance.
      It retains a typed index of the full exact snapshot once, so per-script
      validation does not rescan the whole upload. No checkpoint is retained. *)
  type t

  module Stage : sig
    type t

    (** Borrowed only during [run_stage]. Every operation checks that the token
        is still live and consumes the same attempt ledger as Project validation.
        [checkpoint] consumes one work step, then invokes the callback supplied
        to [run_stage]. Its exception and backtrace propagate unchanged. *)
    val checkpoint : t -> unit

    (** Charge retained parser/conversion input before copying it. Negative
        counts and exhausted allowance become [Budget_error]. *)
    val debit_input : t -> bytes:int -> unit

    (** Exact already-admitted source lookup; consumes one work step. No source
        acquisition or authority is implied by a successful lookup. *)
    val source : t -> Captured.Id.t -> Captured.t option
  end

  type 'stage_error stage_failure =
    | Stage_error of 'stage_error
    | Budget_error of Error.t

  (** Borrow the one mutable budget for parser/conversion work. Rejects concurrent
      or reentrant use and revokes the Stage token when the callback returns or
      raises. A callback error, budget error or exception closes the attempt;
      no phase may refill its allowance. Only the module's own budget exceptions
      become [Budget_error]. Cancellation and other callback exceptions retain
      their original identity/backtrace. The token must not escape the callback;
      attempts to use an escaped token raise without a budget debit and
      permanently invalidate the owning attempt. *)
  val run_stage
    :  t
    -> checkpoint:(unit -> unit)
    -> f:(Stage.t -> ('a, 'stage_error) result)
    -> ('a, 'stage_error stage_failure) result

  (** Validate one exact duplicate-free source per manifest member, with no
      surplus source, using qualified ID, computed digest and byte length. A
      top-level Project's source subset must cover the entire upload. Document
      member/project union coverage remains the document adapter's duty. The
      result proves in-memory coherence only, not the origin of the store claim.
      At most 1024 sources and 512 MiB of exact bytes are admitted by the wire
      manifest; this function rechecks the whole supplied list. *)
  val admit
    :  Bundle.Manifest.t
    -> sources:Captured.t list
    -> limits:Limits.t
    -> checkpoint:(unit -> unit)
    -> (t, Error.t) result

  (** Select only the manifest's top-level Project. *)
  val validate_top_project
    :  t
    -> previous:Build.t option
    -> contract:Contract.t
    -> checkpoint:(unit -> unit)
    -> (Validated.t, Error.t) result

  (** Select one manifest script's Project by its exact typed selector.
      [derived_entry] is mandatory exactly for Derived_script, and its source
      ID/digest/length must match both the Project entry and script body. The
      document adapter independently proves that the selector belongs to a
      genuine parsed declaration and these bytes come from its actual binding;
      this Project adapter cannot grant that authored provenance. *)
  val validate_selected_script
    :  t
    -> selector:Bundle.Manifest.Script.Selector.t
    -> derived_entry:Captured.t option
    -> previous:Build.t option
    -> contract:Contract.t
    -> checkpoint:(unit -> unit)
    -> (Validated.t, Error.t) result

  (** Rebind the FINAL published lock and exact graph captures with an
      independently selected current compiler contract. This never needs the
      original Compose previous Build and never composes a new lock. Both
      variants retain this Attempt's one snapshot index, input/work ledger,
      selected source and declaration/route/result reconciliation. The lower
      [Build.restore] receives the remaining allowance; its returned work is
      debited once and its Prepared input is charged even when strings share
      storage. A graph mismatch closes the Attempt. *)
  val restore_published_top
    :  t
    -> lock_source:Captured.t
    -> sources:Captured.t list
    -> contract:Contract.t
    -> checkpoint:(unit -> unit)
    -> (Validated.t, Error.t) result

  val restore_published_selected_script
    :  t
    -> selector:Bundle.Manifest.Script.Selector.t
    -> derived_entry:Captured.t option
    -> lock_source:Captured.t
    -> sources:Captured.t list
    -> contract:Contract.t
    -> checkpoint:(unit -> unit)
    -> (Validated.t, Error.t) result

  val manifest_fingerprint : t -> Captured.Digest.t
end

(** Both validators honor the manifest's mode. Exact_lock selects its captured
    lock from the Project subset and calls [Build.restore], never composition as
    a fallback. Compose compares the full indicated prior lock ID/digest/length
    with a separately host-admitted previous Build, then calls
    [Build.Effective.compose]. A missing previous Build selects the existing
    Local-only gate. The root declaration's actual [Chatml_project.select]
    result reconciles entry and default or explicit environment with the Build;
    all routes, selected source revisions, generated/exact lock, and authentic
    compiler contract are compared before returning evidence.

    Adapter scans, comparisons and declaration selection debit the outer work
    ledger. Each lower Build receives only the remaining allowance; its returned
    [Build.Accounting.work_steps] is debited exactly once. Its checkpoint is for
    cancellation/cooperation and is not independently charged a second time.
    Success debits actual Prepared input before publishing Validated. Exceptions
    retain their original identity and backtrace while invalidating Attempt. *)
