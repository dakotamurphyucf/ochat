(** Source IDs and canonical locators for one physically admitted state. *)
open! Core

module P = Agent_protocol

module Source : sig
  type basis = private
    { owner : P.Session_ref.t
    ; generation : int
    ; revision : int64
    }
  [@@deriving equal, sexp_of]

  type archive_kind =
    | Summary
    | Rollover
  [@@deriving equal, sexp_of]

  type archive = private
    { kind : archive_kind
    ; basis : basis
    ; operation : P.Id.Operation.t
    ; digest : Chatml_source.Digest.t
    }
  [@@deriving equal, sexp_of]

  type t =
    | Live of basis
    | Archive of archive
  [@@deriving equal, sexp_of]

  val basis
    :  owner:P.Session_ref.t
    -> generation:int
    -> revision:int64
    -> (basis, P.Error.t) result

  val archive
    :  basis
    -> kind:archive_kind
    -> operation:P.Id.Operation.t
    -> digest:Chatml_source.Digest.t
    -> t

  val coordinates : t -> basis
  val owner_shape : Document_schema.Shape.t
end

module Member : sig
  type provenance =
    | Observed
    | Carried_from of P.Id.Context_window.t
    | Derived_summary of P.Id.Operation.t
  [@@deriving sexp_of]

  type t

  val equal : t -> t -> bool

  val create
    :  P.Recovery_window.Citation.t
    -> source:Source.t
    -> source_ordinal:int
    -> provenance:provenance
    -> (t, P.Error.t) result

  (** A committed withdrawal preserves the original citation and position. Its
      former source is evidence only and must never be dereferenced for recovery. *)
  val withdraw : t -> t

  val is_withdrawn : t -> bool
  val citation : t -> P.Recovery_window.Citation.t
  val source : t -> Source.t
  val source_ordinal : t -> int
  val provenance : t -> provenance
  val matches : t -> P.History.entry -> bool
  val same_version : t -> t -> bool
  val to_jsonaf : t -> Jsonaf.t
  val of_jsonaf : P.Recovery_window.Scope.t -> Jsonaf.t -> (t, P.Error.t) result
  val shape : Document_schema.Shape.t
end
