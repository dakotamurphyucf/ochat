(** Design-only OCH-49 review; no implementation or installed storage API. *)
open! Core

module Json : sig
  type t = Jsonaf.t

  type limits =
    { max_bytes : int
    ; max_depth : int
    ; max_fields : int
    }

  type error =
    | Too_large
    | Too_deep
    | Duplicate_key of string
    | Malformed of string

  (** Bounded parse preserves presence/values/order/extras and rejects duplicate
      keys before lookup; no current-domain decoder precedes this boundary. *)
  val decode : limits:limits -> string -> (t, error) Result.t
end

module Document : sig
  type t
  type kind
  type version

  type error =
    | Invalid_envelope of string
    | Unsupported_format of string
    | Unsupported_kind of string

  (** Logical family: format/schema_version/kind/payload plus optional non-null
      extensions. Current v1 starts forward conversion compatibility; frame,
      protocol, app and provider replay versions are independent. Wide counters
      use validated decimal strings, not floating-point JSON numbers. *)
  val inspect : Json.t -> (t, error) Result.t

  val kind : t -> kind
  val version : t -> version
  val json : t -> Json.t
end

module Verified_record : sig
  (** Only produced after stored-version frame/chain/anchor checks. Original
      immutable bytes/digest remain available; upgraded JSON cannot redefine an
      existing transaction hash. Existing Frame v1 stays separately versioned. *)
  type t

  val stored_bytes : t -> string
  val stored_digest : t -> string
  val document : t -> Document.t
end

module Current_document : sig
  type t

  val document : t -> Document.t
end

module Conversion_error : sig
  type t =
    | Unsupported_version of int
    | Missing_conversion of
        { kind : string
        ; from_version : int
        ; to_version : int
        }
    | Missing_information of
        { path : string
        ; reason : string
        }
    | Unknown_required_semantics of string
    | Invalid_field of
        { path : string
        ; reason : string
        }
    | Limit_exceeded
end

module Conversion : sig
  type t
  type step

  (** Pure deterministic bounded generic-data conversions. Steps retain old field
      meanings, not old OCaml/SDK types. Null and absence differ; unknown extensions
      survive unless required semantics prevent load. No I/O or execution. *)

  (** Per-kind target versions, validated positive and unique. Families need not
      evolve together. Upgrade proves this document reached its kind's target;
      Domain_codec also checks that kind/version matches its own contract. *)
  val create
    :  targets:(Document.kind * int) list
    -> max_steps:int
    -> steps:step list
    -> (t, Conversion_error.t) Result.t

  val upgrade : t -> Document.t -> (Current_document.t, Conversion_error.t) Result.t
end

module Extension_carrier : sig
  (** Uninterpreted data stays beside validated domain values across edits/writes;
      neither a second mutable authority nor automatically executable semantics. *)
  type 'a t

  (** New authored data intentionally has no inherited extensions. Never use
      this constructor when editing a restored document. *)
  val of_authored_value : 'a -> 'a t

  (** Functional edit retaining uninterpreted fields/template and their paths;
      encode subsequently validates known data and rejects ownership conflicts. *)
  val with_value : 'a t -> 'a -> 'a t

  val value : 'a t -> 'a
  val extensions : 'a t -> Json.t
end

module Domain_codec : sig
  type 'a t

  type error =
    | Wrong_kind
    | Invalid_domain of string
    | Required_extension_unknown of string
    | Extension_conflict of { path : string }

  (** Current-domain validation follows inspection/conversion. Reuse existing
      identity/counter/authority validators, never [%of_sexp: Current_state.t]
      or a current bin reader before conversion. *)
  val decode : 'a t -> Current_document.t -> ('a, error) Result.t

  (** Validate known data, merge with preserved unknown fields at their original
      paths and emit current kind/version. A preserved field conflicting with a
      now-owned field returns Extension_conflict; never overwrite/discard it.
      Pure kind conversion must explicitly resolve intentional promotions. *)
  val encode : 'a t -> 'a Extension_carrier.t -> (Current_document.t, error) Result.t
end

module Load_error : sig
  type t =
    | Framing of string
    | Integrity of string
    | Document of Document.error
    | Conversion of Conversion_error.t
    | Domain of Domain_codec.error
    | Unsupported_beta_format
end

module Restore : sig
  type t

  (** Caller retains existing Eio readers/verification/commit owners. This pure
      boundary handles snapshots/deltas/events/audits/archives/moderator history
      before current runtime construction. Failure preserves bytes/pointers; no
      jobs/tools/providers, repair or destructive reset occurs here. *)
  val decode
    :  t
    -> verified:Verified_record.t
    -> codec:'a Domain_codec.t
    -> ('a Extension_carrier.t, Load_error.t) Result.t
end
