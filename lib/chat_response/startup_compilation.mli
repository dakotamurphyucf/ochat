open Core

(** One same-domain compilation invocation and lexical checkpoint view. Views
    share one invocation deadline/failure; selected-source views own an independent
    accounting ledger. Runtime
    activation is separate from preparation and never polls this deadline. *)
type t

module Ledger : sig
  (** Borrowed exact-text/canonical-graph accounting and original compiler policy.
      A lexical loan remembers every parent view and its owning ledger. Any
      closed parent invalidates the loan, without charging an ancestor's elapsed
      preparation time. The owner closes after joined callbacks; borrowers never
      clear counters or change ceilings. Never transfer across domains. This
      contains no source, Root or execution authority. *)
  type t
end

(** Borrow accounting while this exact view and its owners remain live. This
    checks domain/lifetime and accounting failure, not elapsed preparation time.
    Hosts normally extract it at completed preparation before activation. *)
val ledger : t -> (Ledger.t, Chatml_compilation.error) Result.t

(** Start ONE independently admitted preparation invocation using the original
    shared ledger. The trusted host calls once per decoded Model request, before
    bounded member lookup, and forwards this context through acquisition,
    installation, role restoration and resource preparation. Compiler limits
    must equal the ledger's original policy; no phase can refresh its deadline.
    The clock remains the owner's original monotonic clock. Accounting capacity
    and sticky ledger rejection are shared; preparation deadline/failure is local
    to this invocation. The supplied checkpoint enforces the actual parent
    runtime cancellation/deadline, never an ancestor's expired preparation clock.
    Original callback exceptions, Timeout and cancellation retain identity and
    backtrace. This brackets lifetime only: neither a timer around all [f] nor a
    final-return checkpoint may charge activation. Closing this invocation does
    not close its borrowed ledger. Every lexical parent must remain live, but an
    ancestor's elapsed preparation clock is never polled by this new invocation. *)
val with_invocation
  :  Ledger.t
  -> limits:Chatml_compilation.limits
  -> checkpoint:(unit -> unit)
  -> map_error:(Chatml_compilation.error -> 'error)
  -> f:(t -> ('a, 'error) Result.t)
  -> ('a, 'error) Result.t

(** Lend ONE selected-source acquisition view with its own accounting ledger,
    preserving the existing source-phase allowances independently of the parent
    restoration/resource ledger. Default distinct text/help and canonical-graph
    input ceilings are each 8 MiB; negative ceilings reject before [f]. The view
    shares the SAME invocation deadline/failure. Optional [limits] preserves
    the source's independent compiler policy; absence uses the parent's policy.
    Its original per-input source ceiling is retained. One source-local wall cap
    starts at entry and remains bounded by the inherited deadline: effective time
    is the minimum of both, with any nested source caps retained. This never
    refreshes an allowance for each compiler phase. Source timeout rejects the
    current invocation, while source accounting exhaustion affects its ledger only.
    It composes the original callback with source work/capture checks and never
    refreshes time. The selected-source adapter enters once for source capture,
    installation and replay, before returning to the outer request for role and
    resource preparation. This is not a general fresh allowance per phase.

    Closing invalidates this view and its source ledger only. Every descendant
    view or extracted ledger loan rejects if ANY lexical parent closes. The
    original parent and sibling owners keep their own lifetimes. Cancellation,
    callback Timeout and unexpected exceptions retain identity and raw backtrace;
    only compiler policy rejection uses [map_error]. Domain/lifetime checks
    precede all ledger access. No callback, view or loan escapes [f]. *)
val with_source_accounting
  :  t
  -> ?limits:Chatml_compilation.limits
  -> ?max_distinct_text_bytes:int
  -> ?max_distinct_graph_input_bytes:int
  -> checkpoint:(unit -> unit)
  -> map_error:(Chatml_compilation.error -> 'error)
  -> f:(t -> ('a, 'error) Result.t)
  -> unit
  -> ('a, 'error) Result.t

(** Borrow an existing invocation, or start one before validation. Default whole
    invocation ceilings are the existing distinct text/help 8 MiB and distinct
    canonical graph-input 8 MiB limits. Explicit smaller ceilings support hosts
    with tighter policy. Original checkpoint exceptions/backtraces propagate,
    including Timeout; the context rejects use after the callback returns.
    A borrowed context requires the same original limits; it cannot weaken a
    caller's smaller policy. This scopes lifetime, without timing the whole
    callback/activation. Foreign-domain use rejects before accessing the ledger. *)
val with_context
  :  ?startup:t
  -> ?max_distinct_text_bytes:int
  -> ?max_distinct_graph_input_bytes:int
  -> limits:Chatml_compilation.limits
  -> env:Eio_unix.Stdenv.base
  -> checkpoint:(unit -> unit)
  -> map_error:(Chatml_compilation.error -> 'error)
  -> f:(t -> ('a, 'error) Result.t)
  -> unit
  -> ('a, 'error) Result.t

(** Check current caller cancellation before/after the original callback, then
    the absolute deadline. Policy rejection is handled by [within]/[with_context].
    Never call from another domain or wrap this callback in catch-all mapping. *)
val checkpoint : t -> unit

(** Exact source/schema/canonical-help bytes deduplicate across preparation
    phases. Candidate length is checked before chunked hashing; digest buckets
    compare exact bytes with polling. No copy of the text is retained. *)
val admit_text : t -> string -> (unit, Chatml_compilation.error) Result.t

(** Canonical source+metadata input deduplicates by complete Prepared fingerprint.
    Different scopes/contracts count separately even with shared physical bodies.
    This does not replace custody's per-program logical accounting or tool checks. *)
val admit_prepared
  :  t
  -> Chatml_prepared_program.t
  -> (unit, Chatml_compilation.error) Result.t

(** Time remaining on the original deadline, retaining the original per-input
    source ceiling. Expiration rejects; it never grants a fresh/clamped budget. *)
val remaining_limits : t -> (Chatml_compilation.limits, Chatml_compilation.error) Result.t

(** Time one actual host preparation/preflight phase using the original remaining
    allowance. Do not include activation. Compiler calls inside remain joined by
    their existing worker on cancellation; original failures propagate unchanged. *)
val within
  :  t
  -> map_error:(Chatml_compilation.error -> 'error)
  -> f:(unit -> ('a, 'error) Result.t)
  -> ('a, 'error) Result.t
