open! Core
module P = Agent_protocol
module A = P.Authoring.Analysis
module Ledger = Chatml_authoring_context
module Semantics = Chatml_semantics
module Captured = Chatml_source

(** PRIVATE immutable comparison index for current parser-confirmed prefix
    results. It owns one latest row per context, up to the explicit host-only
    provisional-result cap. This is separate from checked-cache
    [Policy.max_results]; both capacities are bounded independently, and each
    Provisional uses that same joined compiler's input/fact/edge limits.
    A label identifies one owner-local publication incarnation only, never a
    source, actor, selected contract or execution grant. *)
type t

type row

module Document : sig
  type t

  (** The selected Script, its exact parser-input/source associations and the
      old-row-to-current-declaration proof are admitted together. This is
      comparison evidence for the one published provisional row, never a new
      source-read or selection grant. *)
  val create
    :  script:Chatmd_authoring_analysis.Script.t
    -> input_sources:(Chatmd_parser_input.t * Captured.t) list
    -> selection:Authoring_document_selection_rebind.t
    -> checkpoint:(unit -> unit)
    -> t Or_error.t

  val script : t -> Chatmd_authoring_analysis.Script.t
  val input_sources : t -> (Chatmd_parser_input.t * Captured.t) list
  val selection : t -> Authoring_document_selection_rebind.t
end

module Error : sig
  type t =
    | Retention_limit
    | Identity_exhausted
    | Foreign_source
    | Evidence_mismatch
end

val create : owner_nonce:P.Id.Transaction.t -> max_rows:int -> t Or_error.t
val same_head : t -> t -> bool
val retained_count : t -> int

(** Construct the wire Provisional and next immutable index together outside
    the Core gate. Validate full Prepared fingerprint/contract/environment,
    revised prefix graph fingerprint, selected native source ID/digest/bytes,
    parser-confirmed prefix length and exact request context/source/cursor.
    The caller supplies only an already admitted current full wire Source;
    [authored_request], if present, must match the actual Prefix.Request and
    Script_input target. Its native mapping is proved separately by Script.
    Replace this context's earlier row or return Retention_limit for a new
    context at capacity; no partial publication occurs on any error. *)
val prepare
  :  t
  -> identity:Ledger.Identity.t
  -> capture:A.Capture.t
  -> request:A.Prefix.Request.t
  -> provisional:Semantics.Provisional.t
  -> full_source:Captured.t
  -> full_wire_source:P.Authoring.Source.t
  -> compiled_cursor_byte:int
  -> authored_request:A.Prefix.Authored_request.t option
  -> document:Document.t option
  -> checkpoint:(unit -> unit)
  -> (t * A.Prefix.provisional, Error.t) result

(** Resolve ONLY the exact owner-local published incarnation, capture,
    compiled full/prefix source, authored echo and graph. Equal client bytes
    cannot recreate a retired row. The host must still reauthorize actor,
    context revision/source, selected Build/contract and current root. *)
val current : t -> identity:Ledger.Identity.t -> A.Prefix.provisional -> row option

val retire : t -> identity:Ledger.Identity.t -> t
val transition : t -> contexts:Ledger.t -> t
val semantics : row -> Semantics.Provisional.t
val full_source : row -> Captured.t
val capture : row -> A.Capture.t
val wire : row -> A.Prefix.provisional

(** Exactly the selected Script and freshly associated parser inputs admitted
    for this row, if it came from Document Prefix. No owner capability or
    native read path is retained. *)
val document : row -> Document.t option
