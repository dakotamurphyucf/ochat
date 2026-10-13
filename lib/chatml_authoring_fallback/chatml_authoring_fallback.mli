open Core

(** Captured-byte ChatML lexical/template completion and authentic selected
    compiler-contract name assistance. No parser recovery, checked binding or
    type claim, source read, native resolver, Eio, initializer or host grant.
    The service authenticates actor/context/source/editor revision/selected
    Build and rechecks currentness before publication. *)
module Captured = Chatml_source

module Build = Chatml_project_build
module Contract = Chatml.Chatml_compiler_contract
module Highlight = Language_highlighting.Highlight_snapshot
module Wire = Agent_protocol.Authoring_assistance

module Limits : sig
  type t

  (** Positive host ceilings. The [Highlight_snapshot] scan has one fixed source,
      identity, span, issue, nesting and work bound. Contract admission has a
      separate work and pre-allocation identity-byte bound. One non-resetting
      projection ledger covers source comparisons, at most one mapped-point
      and one mapped-range callback, name filtering, ranges and output. The
      total request bound is the SUM of those three fixed caps, with separate
      retained label/insert and result-byte ceilings. No phase gets a fresh
      allowance per suggestion. The captured source itself is host-admitted. *)
  val create
    :  lexical:Highlight.Limits.t
    -> max_contract_identity_bytes:int
    -> max_contract_work_steps:int
    -> max_projection_work_steps:int
    -> max_output_bytes:int
    -> t Or_error.t
end

module Error : sig
  type t =
    | Stale_request_source
    | Stale_selected_build
    | Contract_mismatch
    | Invalid_projection
    | Contract_identity_limit
    | Contract_work_limit
    | Projection_work_limit
    | Output_byte_limit
    | Lexical_admission of Highlight.Error.t
    | Wire of Agent_protocol.Error.t

  (** Finite source/path-free service code, never raw source/contract text. *)
  val sexp_of_t : t -> Sexp.t
end

module Selected_contract : sig
  type t

  (** Host supplies its actual selected Build and Contract, never a client
      digest. Standard contracts are rederived with [Contract.for_target];
      Native uses only an independently host-admitted actual native Contract.
      [Contract.input_bytes] charges before conversion, then the authentic
      fingerprint must match BOTH Prepared.compiler_contract and the required
      environment contract. Retained names from this one static surface are
      bounded by [Limits] and never unioned across targets. Required host
      entrypoints contribute to the fingerprint but are not expression-global
      suggestions. The value is comparison evidence, not authority. *)
  val admit
    :  Build.t
    -> selected_contract:Contract.t
    -> limits:Limits.t
    -> checkpoint:(unit -> unit)
    -> (t, Error.t) Result.t

  val prepared : t -> Chatml_prepared_program.t
end

module Site_map : sig
  (** Trusted host-composed map for one selected embedded script, built from
      the genuine parser-bound extraction map by the ChatMD adapter. The core
      validates every returned exact compiled/authored source owner, digest,
      range and cursor containment. [None] yields Source_map_unavailable. Each
      callback is invoked at most once and its checkpoints debit the one
      projection ledger. No client DTO can supply a callback; it grants no
      source authority and host custody/currentness checks remain mandatory. *)
  type t =
    { compiled_point :
        int -> checkpoint:(unit -> unit) -> (Captured.Location.t option, Error.t) Result.t
    ; authored_replacement :
        Captured.Location.t
        -> checkpoint:(unit -> unit)
        -> (Captured.Location.t option, Error.t) Result.t
    }
end

module Origin : sig
  type t

  (** Exact directly editable ChatML capture, including no-Build standalone
      source. Wire Request.source must match ID/digest/length; editor revision
      and actor/context currentness remain host-owned. *)
  val chatml : Captured.t -> t

  (** Selected compiled script plus its separately admitted current authored
      document source. A Script originating in an imported parser input cannot
      project onto the root editable source by path or matching text. *)
  val mapped : compiled:Captured.t -> authored:Captured.t -> map:Site_map.t -> t
end

(** Produce [Template + Lexical] syntax items from the tolerant bounded ChatML
    scan. Static spellings are checked against the production lexer; strings,
    comments and unknown/unscanned cursor state do not gain guessed facts.
    With a matching selected Build, [Reference + Selected_catalog] items come
    only from that exact contract's expression globals/modules. They may be
    shadowed and never claim a visible semantic binding. Invalid UTF-8 and
    malformed source bytes are not rewritten or rejected by this producer.
    ASCII identifier prefix filtering and full-token replacement use exact byte
    offsets, including a zero-width cursor at EOF; mapped replacements require
    BOTH a unique identity authored→compiled point and a fully exact reverse
    compiled→authored range. All successful items use current AUTHORED wire
    Request.source and pass Wire.Assistance.Result.create. No selected Build
    means Complete is relative to lexical evidence alone; an expected but
    unusable selected catalog yields zero-item Catalog_unavailable, never an
    apparent certified empty set. A failed [Selected_contract.admit] is NOT
    passed as [None]; the host converts that expected absence into zero-item
    Catalog_unavailable (or a protocol resource error for a hard limit) after
    its currentness gate. Work/item/result truncation retains only a
    truthful prefix. Checkpoint exceptions/cancellation preserve identity. *)
val produce
  :  Origin.t
  -> request:Wire.Assistance.Request.t
  -> selected:Selected_contract.t option
  -> limits:Limits.t
  -> checkpoint:(unit -> unit)
  -> (Wire.Assistance.Result.t, Error.t) Result.t
