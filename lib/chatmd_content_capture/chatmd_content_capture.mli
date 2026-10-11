open Core
module Content = Chatmd_prompt_content
module Parsed = Prompt.Chat_markdown.Parsed
module Range = Chatmd_parser_input.Range

(** Lower native adapter, independent of Agent_session/Builder. Scope supplies its
    SAME retained Local and actual Capture contexts; no Eio path/root getter or
    artifact-filename/native-path reopening. *)
module Error : sig
  type t

  val range : t -> Range.t option
  val sexp_of_t : t -> Sexp.t
end

module Accounting : sig
  type t

  val occurrences : t -> int
  val input_bytes : t -> int
  val output_bytes : t -> int
  val nodes : t -> int

  (** Complete conservative logical stage-work receipt: native discovery/read
      stage units plus final Content.accounting work, which already includes all
      Rendered work. Neither CPU/syscall timing nor allocated filesystem blocks. *)
  val work_steps : t -> int
end

type t

val content : t -> Content.t

(** Original immutable aggregate ceilings for downstream reconciliation. *)
val limits : t -> Content.Limits.t

val accounting : t -> Accounting.t

(** Preflight registering contexts, input/occurrence counts and portable declaring
    paths before native reads. Count every actual slot, including dynamic slots;
    visit each captured slot once without deduplicating its input allowance.
    Local member is resolved solely from the authentic declaring Input.path and
    src. Builtin finds the exact embedded ID; unknown/reserved skills rejects.
    No remote read, decompression, fallback or widened authority.

    Before each native read reserve [allowance + 2*components + 6] conservative
    stage units (matching the existing local-capture model: one-byte partial reads
    plus EOF and descriptor traversal). Clamp allowance to remaining input AND
    work capacity before effects. On successful read replace this reservation
    with [observed_bytes + 2*components + 6]; failed read aborts the invocation.
    This charges conservative observed stage units, not a native syscall counter.
    Conversion receives remaining aggregate input/output/node/work ceilings.
    Capture reconciles Rendered receipts once and receives remaining work after
    native/selection stages; its own metadata/projection allocations remain bounded.
    Any error aborts this attempt; no retry, publication or retained callback.
    Borrowed checkpoint/cancellation exception identities propagate unchanged.
    Local's actual joined operation owner remains responsible for native cleanup. *)
val run
  :  Chatml_local_source.t
  -> parsed_contexts:Parsed.t list
  -> limits:Content.Limits.t
  -> checkpoint:(unit -> unit)
  -> (t, Error.t) Result.t
