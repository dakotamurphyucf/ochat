(** Installed source material for the authoring corpus. Human reference documents
    are embedded at build time from docs-src, and signatures come from the current
    compiler's surface tables. Access performs no file, network, provider or tool
    operation and needs no repository checkout.

    This is source material, not a qualified topic corpus or a model-facing service.
    Documents retain their qualification/legacy notes. Hosts must still select the
    actual execution surface and effective capabilities, assemble audited topics,
    bind the installed runtime identity and enforce reference budgets/policy. *)

type document = private
  { path : string
  ; sha256 : string
  ; text : string
  }

type t

type grammar_production = private
  { id : string
  ; contract_sha256 : string
  }

(** Every regular production in the compiled ChatML parser, including structural
    delimiters and explicit rejection branches. Stable IDs describe lhs/rhs;
    contracts also hash the semantic action. Generated parser numbers/locations
    are excluded. This does not inventory lexer rules or inference semantics. *)
val grammar : t -> grammar_production list

(** Materialize the embedded documents and compiler-owned signature inventory.
    Fails on invalid or ambiguous source identities rather than omitting material.
    No compiler builtin implementation is invoked. *)
val installed : unit -> (t, string) result

val format_version : int

(** Digest of the format version, sorted document paths/content hashes and all
    separate signature inventories and parser production contracts. It changes
    when those sources change; it is
    not the runtime build identity, an execution grant or a completeness claim. *)
val identity : t -> string

val documents : t -> document list

(** Exact docs-src-relative lookup, with no path normalization, filesystem
    fallback or fetching a newer version. *)
val document : t -> path:string -> (document, string) result

val surface_ids : t -> string list

(** Exact compiler surface lookup. Never returns the union of multiple surfaces.
    The retrieval host, not a submitted model claim, chooses the permitted target. *)
val signatures
  :  t
  -> surface_id:string
  -> (Chatml.Chatml_surface_inventory.t, string) result

(** Structural source manifest: format, bundle identity, document hashes/byte
    lengths, grammar production contracts and separate surface hashes. Excludes
    document bodies and does not
    mark unaudited topics or packages complete. *)
val manifest : t -> Jsonaf.t
