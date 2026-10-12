open Core
module Content = Chatmd_prompt_content
module Capture = Chatmd_content_capture
module Parsed = Prompt.Chat_markdown.Parsed
module Storage = Agent_store.Prompt_program_bundle

(** Structural/replay owner. No acquisition, callback or native root survives. *)
module Error : sig
  type t

  val range : t -> Chatmd_parser_input.Range.t option
  val sexp_of_t : t -> Sexp.t
end

type t = Content.t

(** Inspect authentic parser-owned slots without allocation or acquisition. Poll
    once per visited occurrence; the caller owns parser admission bounds. Legacy
    elements without an owned parser cannot acquire this guarantee retrospectively.
    Borrowed checkpoint exceptions propagate unchanged. *)
val requires_captured_documents : Parsed.t -> checkpoint:(unit -> unit) -> bool

val admit
  :  Storage.t
  -> programs:Chatml_prompt_programs.t
  -> limits:Content.Limits.t
  -> checkpoint:(unit -> unit)
  -> (t, Error.t) Result.t

module Prepared : sig
  type t

  val content : t -> Content.t

  (** Complete native capture work plus subsequent structural reconciliation and
      validation projection work. Debited once against supplied whole ceiling;
      native input/output/node observations are not charged again by validation.
      Each registering Root's activation occurrence/output allowance contributes
      to one separate aggregate validation ledger, despite physical dedup. *)
  val work_steps : t -> int
end

(** Supplied capture must match the actual original contexts and already admitted
    parser/program inventory. Its complete work receipt is consumed first; every
    structural/validation operation receives remaining work. Reject captured or
    builtin slots when supplied=None, without lookup/read; all-Dynamic contexts
    may create complete immutable zero-render custody. Context/input/occurrence
    count preflight precedes lists/maps/copies. The caller supplies explicit limits,
    ordinarily Capture.limits from the same completed acquisition. No new defaults.
    A failed stage aborts; borrowed checkpoint exceptions propagate unchanged. *)
val prepare
  :  parsed_contexts:Parsed.t list
  -> programs:Chatml_prompt_programs.t
  -> supplied:Capture.t option
  -> limits:Content.Limits.t
  -> checkpoint:(unit -> unit)
  -> (Prepared.t, Error.t) Result.t

module Restored : sig
  type t

  val content : t -> Content.t
  val materialized : t -> Content.Materialized.t
  val work_steps : t -> int
end

(** Admit the complete inactive/active inventory, then materialize one actual
    selected Root using the remaining SAME work allowance. Requires explicit
    host policy; never infer limits from untrusted metadata or read originals. *)
val restore
  :  Storage.t
  -> programs:Chatml_prompt_programs.t
  -> parsed:Parsed.t
  -> limits:Content.Limits.t
  -> checkpoint:(unit -> unit)
  -> (Restored.t, Error.t) Result.t

(** Fresh artifact-root replay from already admitted content. The original
    immutable limits remain authoritative; materialization consumes whole work
    minus [Prepared.work_steps]. The returned work includes both phases. This
    selected activation has its own occurrence/output ceiling; input/conversion
    observations are not charged again. No acquisition or storage readmission.
    Actual root/input/flags are reconciled; checkpoint exceptions propagate. *)
val replay
  :  Prepared.t
  -> parsed:Parsed.t
  -> checkpoint:(unit -> unit)
  -> (Restored.t, Error.t) Result.t

val fingerprint
  :  t
  -> checkpoint:(unit -> unit)
  -> (string, Document_schema.Error.t) Result.t
