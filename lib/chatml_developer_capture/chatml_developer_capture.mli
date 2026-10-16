open Core

(** Immutable, portable input for one developer check or deterministic test.
    Construction verifies exact selected graph and provenance consistency; it
    does not authorize a root, bundle, editor, execution, or live capability.
    OCH-266's host obtains/adopts the underlying build and rechecks its owner. *)

module Json_bounds : sig
  type t

  (** Positive host policy capped by the existing Tool_schema ceiling of
      1 MiB, 128 levels and 100,000 nodes. Traversal checks bytes/depth/nodes
      before JSON encoding or hashing. String escaping is charged before
      allocation; a conservative refusal is permitted. *)
  val create : max_bytes:int -> max_depth:int -> max_nodes:int -> t Or_error.t

  (** Conservative encoded-byte upper bound with callback polling. Numbers
      must have a finite, non-underflowing ChatML float import and at most
      4,096 lexical bytes. Caller exceptions, including [Exit], propagate
      unchanged. *)
  val measure : t -> Jsonaf.t -> checkpoint:(unit -> unit) -> int Or_error.t
end

module Origin : sig
  module Error : sig
    type t =
      | Invalid_provenance
      | Identity_limit
    [@@deriving sexp_of]
  end

  (** Provenance describes where the host observed the selected bytes. It is
      data for receipts, never a capability or a promise that a local file was
      saved. All graph digests must equal the admitted build's fingerprint.
      Local revision and upload receipt labels are nonempty and capped at 256
      bytes before retention. *)
  type t

  val local_saved : graph:Chatml_source.Digest.t -> source_revision:string -> t Or_error.t

  (** [versions] are exact OCH-278 open versions. Each must belong to [context]
      and [generation], and match the selected graph source digest. The host
      supplies the actual current context snapshot and verifies this list is
      complete for its changed sources. *)
  val editor_snapshot
    :  graph:Chatml_source.Digest.t
    -> context:Chatml_authoring_context.Identity.t
    -> generation:Chatml_authoring_context.Generation.t
    -> versions:Chatml_authoring_context.Version.t list
    -> max_versions:int
    -> checkpoint:(unit -> unit)
    -> t Or_error.t

  (** [receipt_id] identifies an OCH-263/264 host-admitted immutable upload.
      This label is not a receipt decoder or grant; the host binds its actual
      principal-owned receipt and graph before calling [create]. *)
  val uploaded_bundle : graph:Chatml_source.Digest.t -> receipt_id:string -> t Or_error.t

  (** Bounded comparison metadata for a host-proven current ChatMD script.
      The host first proves actual Parsed/declaration membership and the
      declaring parser input's exact captured source; this constructor cannot
      grant that parser, context or source authority. [root_source] is the
      current document root, [declaring_source] is that exact root or genuine
      import, and [script_source] is the admitted inline fragment or external
      captured body. Ranges are half-open BYTE offsets in the declaring
      input's effective bytes, not invented authored offsets or a
      Ledger.Version for a generated fragment. [input_effective_bytes] and
      the range must be ordered/nonnegative; source identity fields and
      [script_id] are charged before canonical origin encoding under
      [max_identity_bytes]. Same Source.Id with different digest/length rejects.
      [create_prepared] additionally requires the selected script ID to be
      Prepared.entry, then compares its digest/length with the exact Prepared
      inventory under the caller's checkpoint. No field grants a native read,
      project selection, compiler target or effect. *)
  val document_script
    :  graph:Chatml_source.Digest.t
    -> context:Chatml_authoring_context.Identity.t
    -> generation:Chatml_authoring_context.Generation.t
    -> root_source:Chatml_source.t
    -> declaring_source:Chatml_source.t
    -> declaring_input_digest:Chatml_source.Digest.t
    -> input_effective_bytes:int
    -> declaration_start_byte:int
    -> declaration_end_byte:int
    -> script_id:string
    -> script_source:Chatml_source.t
    -> max_identity_bytes:int
    -> checkpoint:(unit -> unit)
    -> (t, Error.t) result

  val graph : t -> Chatml_source.Digest.t
end

module Fixture : sig
  module Response : sig
    type t =
      | Return of Jsonaf.t
      | Fail of string
    [@@deriving sexp_of]
  end

  module Step : sig
    type t

    (** One ordered Tool.call expectation. Preflight both schemas, expected
        input, and returned JSON/error under [json_bounds] before encoding/
        hashing; compile the existing bounded schemas and validate expected
        input and Return output against them both before and after ChatML number
        conversion; a precision-changing value that leaves its schema rejects at
        admission. Runtime matching checks the input
        schema and exact JSON structure after normalizing expected numbers through
        ChatML Json.t's finite float representation (for example 1 and 1.0 match).
        Nonfinite/out-of-range number imports and numeric lexemes above 4,096
        bytes reject before capture. [tool_name] and Fail strings are additionally
        capped at 256 and 16 KiB bytes respectively. No live tool implementation
        is created. *)
    val create
      :  tool_name:string
      -> input_schema:Jsonaf.t
      -> output_schema:Jsonaf.t
      -> expected_input:Jsonaf.t
      -> response:Response.t
      -> json_bounds:Json_bounds.t
      -> checkpoint:(unit -> unit)
      -> t Or_error.t

    val tool_name : t -> string
    val expected_input : t -> Jsonaf.t
    val matches_input : t -> Jsonaf.t -> bool
    val response : t -> Response.t
  end

  type t

  (** Every listed step is required in order; an empty list is valid for a pure
      program, and any Tool.call then fails closed. Reject over-limit count,
      aggregate canonical bytes, and integer overflow before retaining the
      list or computing its digest. The host selects positive finite limits;
    this type has no mutable cursor. [max_steps] cannot exceed 4,096 and
    [max_bytes] cannot exceed 16 MiB. *)
  val create
    :  steps:Step.t list
    -> max_steps:int
    -> max_bytes:int
    -> checkpoint:(unit -> unit)
    -> t Or_error.t

  val steps : t -> Step.t list
  val digest : t -> Chatml_source.Digest.t
end

type t

module Error : sig
  type t =
    | Invalid_origin
    | Stale_source of Chatml_source.Id.t
    | Duplicate_source of Chatml_source.Id.t
    | Incompatible_contract
    | Unsupported_target
    | Invalid_entrypoint
    | Input_limit
  [@@deriving sexp_of]
end

(** Freeze exact admitted Prepared graph, selected Standard One_off_v1 or Tool_v1
    standalone contract, entry, bounded JSON input, and optional finite fixture set into one
    fingerprint. The graph/source/origin checks are identical to [create].
    A host can use an authentic [Prepared.single_captured] without fabricating
    a Project Build. Origin labels remain comparison data; the host separately
    proves their physical source/actor/currentness association. *)
val create_prepared
  :  prepared:Chatml_prepared_program.t
  -> origin:Origin.t
  -> target:Chatml.Chatml_compiler_contract.Target.t
  -> entrypoint:string
  -> input:Jsonaf.t
  -> fixtures:Fixture.t option
  -> json_bounds:Json_bounds.t
  -> checkpoint:(unit -> unit)
  -> (t, Error.t) Result.t

(** Freeze exact admitted build, selected Standard standalone contract, entry,
    bounded JSON input, and optional finite fixture set into one fingerprint.
    The source graph fingerprint already binds contract/environment/provenance
    and every source ID/digest. This capture fingerprint additionally binds the
    origin label, entrypoint, input and fixture digest. It never rewrites source
    or selects a filesystem path. Tool_v1's two-argument run role also needs
    exact host-owned inert context/schema selection before fixture execution.
    Other targets fail explicitly before compilation. A fixture capture cannot
    silently become Live. This wrapper delegates to
    [create_prepared ~prepared:(Build.prepared build)] with no weaker checks. *)
val create
  :  build:Chatml_project_build.t
  -> origin:Origin.t
  -> target:Chatml.Chatml_compiler_contract.Target.t
  -> entrypoint:string
  -> input:Jsonaf.t
  -> fixtures:Fixture.t option
  -> json_bounds:Json_bounds.t
  -> checkpoint:(unit -> unit)
  -> (t, Error.t) Result.t

val prepared : t -> Chatml_prepared_program.t
val origin : t -> Origin.t
val target : t -> Chatml.Chatml_compiler_contract.Target.t
val entrypoint : t -> string
val input : t -> Jsonaf.t
val fixtures : t -> Fixture.t option
val graph_fingerprint : t -> Chatml_source.Digest.t
val fingerprint : t -> Chatml_source.Digest.t
