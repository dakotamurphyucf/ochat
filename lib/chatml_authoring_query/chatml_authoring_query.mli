open Core

(** Pure, read-only projection of actual immutable ChatML semantic facts into
    [Agent_protocol.Authoring_analysis.Query] payloads. This library neither checks source text
    nor owns a context, client, parser, worker or publication gate. The host
    admits the current captured closure and request evidence, authorizes the
    actor, and rechecks currentness before publishing the enclosing response.
    A payload or numeric symbol ID is never a source or host authority. *)
module Captured = Chatml_source

module Semantics = Chatml_semantics
module Wire = Agent_protocol.Authoring_analysis

module Limits : sig
  type t

  (** Positive ceilings, with member depth at most wire depth 6 and member
      edges at most the wire binding-tree node limit 2048. [max_items] is an
      aggregate count of emitted Symbol/Reference/Binding/Configured_module/
      Syntax primary facts and standalone Type results. Embedded signatures
      and inferred-type displays instead consume the shared display session,
      work and label-byte ledgers; they do not debit a second item. Each page,
      including nested member pages, is
      also restricted by the wire request's [max_items] and protocol ceiling;
      enclosing selection/page containers are not double-debited. One
      aggregate work ledger covers source
      inventory comparisons, each Snapshot query callback, symbol/member/export
      traversal, name/path bytes, site callbacks and all type displays. Member
      edges/depth bound recursive public views; bounded native candidate and
      configured-module root slots are reserved before optional nested member
      expansion, so a large earlier module cannot hide later root entries.
      Every emitted item and label
      byte is charged before construction. [type_display] supplies the limits
      for one invocation-owned [Chatml_type_display.Session] shared by all
      roots in a request; each render receives
      the current remaining aggregate label-byte allowance through its narrow
      dynamic cap, in addition to its fixed structural and output caps. An
      exhausted projection reports a truthful
      page/display truncation or typed error; it never emits an unbounded tail. *)
  val create
    :  max_items:int
    -> max_member_depth:int
    -> max_member_edges:int
    -> max_label_bytes:int
    -> max_work_steps:int
    -> type_display:Chatml_type_display.Limits.t
    -> t Or_error.t
end

module Error : sig
  type t =
    | Unsupported_kind
    | Evidence_mismatch
    | Stale_source
    | Source_unavailable
    | Foreign_symbol
    | Invalid_site_projection
    | Work_limit
    | Label_byte_limit
    | Member_edge_limit
    | Projection_limit
    | Query of Semantics.Snapshot.Error.t
    | Display of Chatml_type_display.Error.t
    | Wire of Agent_protocol.Error.t

  (** Finite code for a service boundary; never prints raw source/path text. *)
  val sexp_of_t : t -> Sexp.t
end

module Evidence : sig
  (** A Complete snapshot is certified only against the host's matching
      Capture/Snapshot_id/validated graph receipt. A provisional value retains
      both the full input fingerprint and distinct exact prefix source owner.
      This variant cannot promote Failed/Partial data into semantic facts. *)
  type t =
    | Complete of Semantics.Snapshot.t
    | Provisional of
        { provisional : Semantics.Provisional.t
        ; full_source : Captured.t
        }
end

module Prefix_origin : sig
  (** Request-local proof that the actual selected Provisional source is the
      exact byte prefix of the separately admitted full captured source. The
      full source comes from the current host-selected Build/ChatMD Script;
      this proof does not itself authorize that custody. Construction verifies
      selected ID, full and prefix digests and lengths, end bound, and exact
      prefix bytes once using the supplied aggregate checkpoint. It does not
      retain a global cache or compare the entire prefix for every site. *)
  type t

  val create
    :  Semantics.Provisional.t
    -> full_source:Captured.t
    -> checkpoint:(unit -> unit)
    -> (t, Error.t) Result.t

  (** Map a native provisional location within the proved prefix to the same
      byte range on the full source for
      [Chatmd_authoring_analysis.Projection.of_location].
      Return None for other native compiled sources. A selected prefix location
      with foreign digest, invalid span or range beyond the prefix is an error.
      This returned full location is ONLY input to authored projection; the
      public Wire.Site always retains its original prefix [native] location. *)
  val full_location
    :  t
    -> Captured.Location.t
    -> checkpoint:(unit -> unit)
    -> (Captured.Location.t option, Error.t) Result.t
end

module Site_projection : sig
  (** Host-composed pure callback for one exact compiler-owned location. It
      resolves the corresponding CURRENT Wire.Source revision and may use the
      bound ChatMD extraction map. For a selected script's provisional prefix,
      it may call [Prefix_origin.full_location] and then the Script projection's generic
      full-location projection, retaining the prefix as native; without that
      witness it leaves authored projection absent. Authored_exact requires
      separately admitted authored-input custody. External/approximate remain
      typed and never become a fabricated exact editor selection. The callback
      polls the passed aggregate checkpoint for its work and retains nothing.

      The producer validates returned Site.native Source ID/digest/byte length
      and half-open range against the input location and its captured inventory.
      The host additionally validates editor revision/current custody. Callback
      errors are typed; cancellation/unexpected exceptions keep their identity. *)
  type t =
    Captured.Location.t -> checkpoint:(unit -> unit) -> (Wire.Site.t, Error.t) Result.t
end

(** Query Certified/Provisional semantic facts only. The request's Capture
    input fingerprint, required/selected contracts, environment, source ID,
    digest and length must match the actual immutable Snapshot or Provisional
    prefix. Its byte offset is a zero-width captured Point; EOF is allowed.
    The host separately compares Snapshot_id, graph receipt, target/context,
    editor revision and actor. Failed analysis cannot be passed here.

    Use actual Snapshot selection/fact/page APIs; preserve Ambiguous, Missing,
    Unavailable, available-empty and truncation independently. Candidate/member
    signatures use actual selected PUBLIC Binding.scheme; Symbol.scheme remains
    a distinct principal signature. Configured modules, lexical modules, values,
    type aliases and type parameters retain their separate namespaces and
    shadowing. Snapshot Symbol_id.index is read only after the same-owner
    Snapshot.symbol lookup; its integer is stable only within one host-owned
    Snapshot_id. Type display uses one shared Session. Display Node/Edge/Depth/
    Work truncation maps to Wire Work_limit, Byte to Wire Byte_limit.
    Configured-module keys use a length-framed escaped namespace/path spelling
    for display and same-response comparison only; they are not a portable
    source ID, cross-snapshot key or authority token.

    [Syntax_at] is rejected here: the wire Query permits it only with Syntax_only.
    The result is a payload that has already passed the Wire.Query.create
    envelope check; the host wraps it after its final currentness check.
    Every expected rejection is typed, while
    checkpoint exceptions propagate unchanged. *)
val project
  :  Evidence.t
  -> request:Wire.Query.Request.t
  -> sites:Site_projection.t
  -> limits:Limits.t
  -> checkpoint:(unit -> unit)
  -> (Wire.Query.payload, Error.t) Result.t

(** Same-run Failed/Partial syntax only. The request must carry Syntax_only
    evidence for the same full input Capture and exact captured source. It
    projects parser-emitted Site roles/names only, not certified references,
    bindings, type roots, lexical visibility or a guessed parse recovery table.
    A failed parse with no actual sites yields Syntax_result(Unavailable
    No_syntax), not available-empty. The host uses authoring Assistance for
    separately labelled lexical/selected-contract fallback suggestions. *)
val syntax_at
  :  Semantics.Partial.t
  -> request:Wire.Query.Request.t
  -> sites:Site_projection.t
  -> limits:Limits.t
  -> checkpoint:(unit -> unit)
  -> (Wire.Query.payload, Error.t) Result.t
