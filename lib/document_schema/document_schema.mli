(** Pure, provider-independent durable named-field documents. No runtime types,
    I/O, tool execution or derived binary layouts participate in this boundary. *)
open! Core

module Error : sig
  type t =
    | Invalid_configuration of string
    | Limit_exceeded of string
    | Malformed of string
    | Duplicate_key of string list
    | Unsupported_beta_format
    | Unsupported_format of string
    | Invalid_field of
        { path : string list
        ; reason : string
        }
    | Unsupported_kind of string
    | Unsupported_version of
        { kind : string
        ; version : int
        ; target : int
        }
    | Missing_conversion of
        { kind : string
        ; version : int
        }
    | Wrong_kind of
        { expected : string
        ; actual : string
        }
    | Wrong_version of
        { expected : int
        ; actual : int
        }
    | Required_semantics_unknown of string
    | Extension_conflict of string list
  [@@deriving sexp, equal]
end

module Limits : sig
  type t [@@deriving equal]

  (** All bounds are positive; depth is at most 256. Fields counts all object
      members, nodes counts all values, bytes includes JSON string encoding. *)
  val create
    :  max_bytes:int
    -> max_depth:int
    -> max_fields:int
    -> max_nodes:int
    -> (t, Error.t) Result.t

  val default : t
  val max_bytes : t -> int
end

module Json : sig
  type t = Jsonaf.t

  type presence =
    | Absent
    | Null
    | Value of t

  val decode : limits:Limits.t -> string -> (t, Error.t) Result.t

  (** Counts the exact compact JSON byte size, including escaping, without
      materializing encoded output. Rejects before a byte-limit addition would
      exceed the bound, including for in-memory strings and object keys. *)
  val validate : limits:Limits.t -> t -> (unit, Error.t) Result.t

  (** The same validation and error ordering as [validate], returning its exact
      compact encoded-byte count without materializing encoded output. *)
  val validate_and_measure : limits:Limits.t -> t -> (int, Error.t) Result.t

  (** Lookup on a validated tree preserves absence separately from explicit null. *)
  val field : t -> name:string -> presence

  (** Named object fields compare without key order; arrays retain order,
      numbers retain their lexemes and absent fields differ from null. Callers
      validate raw Jsonaf trees before these queries; internal codec use is bounded. *)
  val equal : t -> t -> bool
end

module Document : sig
  type t

  val format : string

  (** Positive per-kind version, nonempty kind, object payload. Optional
      [extensions] must be an object; [required_semantics] is a unique string
      list. Unknown envelope fields and their order are retained. *)
  val inspect : limits:Limits.t -> Json.t -> (t, Error.t) Result.t

  val decode : limits:Limits.t -> string -> (t, Error.t) Result.t

  val create
    :  limits:Limits.t
    -> kind:string
    -> version:int
    -> payload:Json.t
    -> (t, Error.t) Result.t

  val kind : t -> string
  val version : t -> int
  val payload : t -> Json.t
  val required_semantics : t -> string list
  val json : t -> Json.t
  val to_string : t -> string

  (** Recheck this immutable document under the requested limits. Admission
      records the limits that validated its exact JSON. Equal or componentwise
      looser limits reuse that proof; any stricter component triggers complete
      JSON validation with the ordinary error ordering. No shared cache. *)
  val validate : t -> limits:Limits.t -> (unit, Error.t) Result.t

  (** Ordered scalar edits beneath the payload. Paths must be nonempty existing
      object members, with scalar old and new leaves. The original complete
      document must already satisfy [limits]; shrinking cannot repair an
      oversized input. Every replacement receives full scalar JSON validation.
      All object keys/order, untouched unknown fields and envelope metadata are
      preserved. Node/field counts and depth remain unchanged. If every edit's
      compact escaped scalar size is nonincreasing, the original admission proof
      establishes complete final bounds; any growth uses full final inspection.
      Duplicate paths apply sequentially; growth remains sticky even if a later
      edit shrinks the same leaf. Empty edits still validate the original.
      Missing paths, scalar ancestors
      and container leaves return [Invalid_field]. Domain validation remains the
      caller's responsibility. No input is mutated. *)
  val replace_payload_scalars
    :  t
    -> limits:Limits.t
    -> updates:(string list * Json.t) list
    -> (t, Error.t) Result.t
end

module Conversion : sig
  module Operation : sig
    (** Paths name object fields. Rename/move fail on a present destination,
        defaults apply only to absent fields, explicit null survives. Removing
        or relocating information is an explicit versioned schema decision. *)
    type t =
      | Rename of
          { parent : string list
          ; src : string
          ; dst : string
          }
      | Move of
          { src : string list
          ; dst : string list
          }
      | Default of
          { path : string list
          ; value : Json.t
          }
      | Remove of string list
  end

  module Step : sig
    type t

    (** One adjacent version transition. Operations execute in declared order. *)
    val create
      :  kind:string
      -> from_version:int
      -> operations:Operation.t list
      -> (t, Error.t) Result.t

    (** Structural conversions not expressible as field operations. The callback
        must be pure, deterministic and bounded; the engine bounds invocation
        count and validates its output, but cannot sandbox arbitrary OCaml code.
        It receives only payload, preserving the family envelope automatically. *)
    val of_function
      :  kind:string
      -> from_version:int
      -> f:(Json.t -> (Json.t, Error.t) Result.t)
      -> (t, Error.t) Result.t
  end

  type t

  (** Unique positive targets and transitions; all transitions must belong to a
      target kind and precede its target. Every supported chain is contiguous.
      Limits apply before and after each operation or callback. Conversion never
      constructs current runtime types. Callbacks obey the pure contract above. *)
  val create
    :  limits:Limits.t
    -> targets:(string * int) list
    -> max_steps:int
    -> max_operations:int
    -> steps:Step.t list
    -> (t, Error.t) Result.t

  val upgrade : t -> Document.t -> (Document.t, Error.t) Result.t
end

module Shape : sig
  (** Shape declares field ownership, independent of runtime representations.
      [Value] owns the entire subtree. Object fields are unique. Arrays with
      identity use a unique, required string field owned by the element shape.
      Arrays without identity reject edits when index-associated unknown data
      would otherwise be ambiguously reassigned. *)
  type t

  val value : t

  (** Allows explicit null while retaining structured ownership for non-null
      values. Replacing a container containing unknown fields with null fails. *)
  val nullable : t -> t

  val object_ : (string * t) list -> (t, Error.t) Result.t

  (** Selects a case's object ownership using a required string discriminator.
      Each case must own that discriminator as a value. Unknown tags fail
      closed; changing cases with retained unknown data is a conflict. *)
  val tagged_object : discriminator:string -> (string * t) list -> (t, Error.t) Result.t

  (** Empty identity strings are allowed only when explicitly requested, for
      dictionaries whose keys are arbitrary strings rather than domain IDs. *)
  val array
    :  ?allow_empty_identity:bool
    -> t
    -> identity_field:string option
    -> (t, Error.t) Result.t
end

module Extension_carrier : sig
  type 'a t

  val of_authored_value : 'a -> 'a t

  (** Replaces the typed projection without changing the original template or
      ownership metadata. A unit projection can retain preservation context
      alongside a domain record without a recursive carrier. *)
  val with_value : 'a t -> 'b -> 'b t

  val value : 'a t -> 'a

  (** Original converted document template, including field presence and unknown
      paths. New authored values have no template. *)
  val template : 'a t -> Document.t option
end

module Domain_codec : sig
  type 'a t

  (** Callbacks are pure. Decode validates current domain IDs, order, counters
      and relationships. Encode returns only owned fields, validates authored or
      edited domain values, and explicitly represents null/absence policy.
      Known projection is decoded again before merge. Unexpected exceptions
      propagate; callbacks return typed errors for ordinary invalid domains. *)
  val create
    :  limits:Limits.t
    -> kind:string
    -> version:int
    -> shape:Shape.t
    -> supported_semantics:string list
    -> decode:(Json.t -> ('a, Error.t) Result.t)
    -> encode:('a -> (Json.t, Error.t) Result.t)
    -> ('a t, Error.t) Result.t

  (** Explicit original-domain validation contract for complete typed serializers.
      [validate] checks the original authored or edited value before [encode] is
      called, including invariants that serialization could normalize away.
      [encode] must faithfully and completely represent every validated value;
      codec owners verify this with domain roundtrip tests. JSON bounds, owned
      shape, unexpected fields, extension merge and final document bounds are
      still checked. Encoding does not decode the projection again. Decoding
      always uses the ordinary domain-validating [decode] callback. Callbacks are
      pure and unexpected exceptions propagate, as with [create]. *)
  val create_validated
    :  limits:Limits.t
    -> kind:string
    -> version:int
    -> shape:Shape.t
    -> supported_semantics:string list
    -> validate:('a -> (unit, Error.t) Result.t)
    -> decode:(Json.t -> ('a, Error.t) Result.t)
    -> encode:('a -> (Json.t, Error.t) Result.t)
    -> ('a t, Error.t) Result.t

  (** Kind, exact target version and required semantics checked before decode. *)
  val decode : 'a t -> Document.t -> ('a Extension_carrier.t, Error.t) Result.t

  (** Retains unknown fields at original paths; ownership changes, deleted
      containers, changed array identity policies with unknown data, or ambiguous
      array changes return [Extension_conflict]. *)
  val encode : 'a t -> 'a Extension_carrier.t -> (Document.t, Error.t) Result.t

  (** Transfer compatible extension ownership into [incoming]'s current value.
      Both carriers must belong to this codec. The previous carrier is first
      updated to the incoming value and encoded, so unsafe deletion/ownership
      changes fail before adoption. Unknown fields from either side survive;
      different values at the same unknown path conflict. Array alignment uses
      the common current projection, never positions in an older template.
      No input carrier or document is mutated. *)
  val adopt
    :  'a t
    -> previous:'a Extension_carrier.t
    -> incoming:'a Extension_carrier.t
    -> ('a Extension_carrier.t, Error.t) Result.t
end
