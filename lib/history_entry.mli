open! Core

module Id : sig
  type t [@@deriving compare, hash]

  include Binable.S with type t := t
  include Sexpable.S with type t := t

  val jsonaf_of_t : t -> Jsonaf.t
  val t_of_jsonaf : Jsonaf.t -> t

  (** [create ~namespace ~sequence] creates an application-owned history ID.

      [namespace] must be nonempty and [sequence] must be nonnegative. Provider
      item IDs and tool [call_id] values are unrelated payload metadata. *)
  val create : namespace:string -> sequence:int -> (t, string) result

  (** [of_string encoded] decodes the canonical length-prefixed representation. *)
  val of_string : string -> (t, string) result

  (** [to_string t] encodes [t] as
      ["<namespace-byte-length>:<namespace>:<sequence>"]. *)
  val to_string : t -> string

  val namespace : t -> string
  val sequence : t -> int
  val equal : t -> t -> bool
end

module Allocator : sig
  type t

  (** [create ~namespace ~next_sequence] creates a concurrency-safe allocator.

      [next_sequence] is the next unused sequence. Restoring persisted state
      creates a new allocator; a live allocator cannot move backwards. *)
  val create : namespace:string -> next_sequence:int -> (t, string) result

  (** [create_bounded] creates an allocator that cannot advance beyond the
      already committed exclusive high-water mark. *)
  val create_bounded
    :  namespace:string
    -> next_sequence:int
    -> limit_exclusive:int
    -> (t, string) result

  val namespace : t -> string
  val next_sequence : t -> int
  val allocate : t -> (Id.t, string) result

  (** [reserve t ~count] atomically reserves [count] consecutive IDs.

      A failed reservation does not advance [t]. *)
  val reserve : t -> count:int -> (Id.t list, string) result
end

module Payload : sig
  module Presence : sig
    type 'a t =
      | Absent
      | Null
      | Value of 'a
    [@@deriving equal, sexp_of]
  end

  module Origin : sig
    type t

    (** Explicitly unavailable provenance; no invented adapter/endpoint/model. *)
    val unavailable : t

    (** Nonempty known fields; account/profile/model only when actually known.
        Identity is a nonsecret compatibility reference, never authority. *)
    val create
      :  adapter:string
      -> provider:string
      -> account:string option
      -> endpoint:string
      -> profile:string option
      -> model:string option
      -> replay_version:int
      -> (t, string) Result.t

    val is_available : t -> bool
    val model : t -> string option
    val provider : t -> string option
    val profile : t -> string option

    (** Known adapter/account/endpoint/replay-version equality only. This omits
        logical provider/profile and model and grants no replay permission;
        callers must separately qualify those fields under trusted policy. *)
    val same_replay_transport_context : t -> t -> bool

    (** Known adapter/provider/account/endpoint/profile/replay version equality;
        model is deliberately excluded. Unavailable provenance never matches. *)
    val same_replay_context : t -> t -> bool

    val to_json : t -> Jsonaf.t
    val of_json : Jsonaf.t -> (t, string) Result.t
  end

  module Role : sig
    type t =
      | System
      | Developer
      | User
      | Assistant
      | Tool
    [@@deriving equal, sexp_of]
  end

  module Call_kind : sig
    type t =
      | Function
      | Custom
    [@@deriving equal, sexp_of]
  end

  module Metadata : sig
    type t =
      { item_id : string Presence.t
      ; response_id : string Presence.t
      ; call_id : string Presence.t
      ; status : string Presence.t
      }

    val empty : t
  end

  module Content : sig
    type t =
      | Text of
          { text : string
          ; annotations : Jsonaf.t list
          ; logprobs : Jsonaf.t Presence.t
          }
      | Refusal of string
      | Image of
          { uri : string
          ; detail : string Presence.t
          }
      | Unknown of
          { kind : string
          ; raw : Jsonaf.t
          }

    (* URI/reference does not establish admission or immutable asset ownership. *)
  end

  module Output : sig
    type t =
      | Text of string
      | Content of Content.t list
    [@@deriving sexp_of]

    val to_json : t -> Jsonaf.t

    (** Validates the complete JSON tree before decoding neutral output. *)
    val of_json : Jsonaf.t -> limits:Document_schema.Limits.t -> (t, string) Result.t
  end

  module Call_relation : sig
    type t =
      | Bound of Id.t
      | Unresolved
    [@@deriving equal, sexp_of]

    (** Bound is supplied only when the host knows the occurrence. Unresolved
        preserves existing standalone/authored/orphan outputs. Provider call ID
        remains metadata and never becomes an invented host ID. *)
  end

  module Semantic : sig
    type message_form =
      | Input
      | Output
    [@@deriving equal, sexp_of]

    type view =
      | Message of
          { form : message_form
          ; role : Role.t
          ; content : Content.t list
          ; phase : string Presence.t
          }
      | Call of
          { kind : Call_kind.t
          ; name : string
          ; namespace : string Presence.t
          ; input_bytes : string
          ; async : bool Presence.t
          }
      | Result of
          { relation : Call_relation.t
          ; kind : Call_kind.t
          ; output : Output.t
          }
      | Reasoning of { readable_summary : string list }
      | Unknown of { provider_kind : string }

    type t

    (** Validates neutral invariants only. Exact call strings are never parsed. *)
    val create : view -> metadata:Metadata.t -> (t, string) Result.t

    val view : t -> view
    val metadata : t -> Metadata.t
  end

  type representation =
    | Authored
    | Captured of
        { origin : Origin.t
        ; raw : Jsonaf.t
        }
    | Reconstructed of
        { provider : string
        ; raw : Jsonaf.t
        }

  (** Reconstructed is explicitly the lossy known-field serialization of a
      provider DTO, not an actual wire capture. It is retained only for exact
      legacy runtime projection/known provider fields and grants no opaque replay.
      Actual captures originate in OCH50 Wire and retain arbitrary unknown raw.
      All representations are immutable. Editing semantic content creates Authored,
      unless an adapter recaptures a new actual envelope under its own contract. *)

  type t [@@deriving bin_io, sexp]

  val authored : Semantic.t -> t
  val captured : Semantic.t -> origin:Origin.t -> raw:Jsonaf.t -> (t, string) Result.t

  val reconstructed
    :  Semantic.t
    -> provider:string
    -> raw:Jsonaf.t
    -> (t, string) Result.t

  val semantic : t -> Semantic.t
  val representation : t -> representation
  val to_json : t -> Jsonaf.t

  (** Bounded, duplicate-aware neutral validation; no provider decode.
      Retains full immutable JSON tree or a carrier for unknown neutral fields.
      Canonical load is not evidence that raw matches the semantic projection:
      replay must adapter-decode/reconcile raw before use. *)
  val of_json : Jsonaf.t -> (t, string) Result.t

  val validate : t -> (unit, string) Result.t
end

type t [@@deriving bin_io, sexp]
type entry = t

val create : allocator:Allocator.t -> Payload.t -> (t, string) result
val create_with_id : id:Id.t -> Payload.t -> t
val id : t -> Id.t
val payload : t -> Payload.t

(** Replacing semantic content invalidates capture unless an actual adapter
    supplies a new capture. The host ID remains unchanged. *)
val with_payload : t -> Payload.t -> t

(** Validates neutral payloads, duplicate IDs and allocator high-water marks.
    Foreign namespaces remain permitted; no provider decoder participates. *)
val validate : allocator:Allocator.t -> t list -> (unit, string) result

(** Checks explicit retained host bindings for order, call family and shared
    provider metadata. Unresolved outputs and archived/missing calls stay legal. *)
val validate_relations : t list -> (unit, string) result

(** Removes a bound host pair, or the nearest matching provider metadata pair
    for explicitly unresolved existing standalone results. *)
val remove_with_tool_pair : t list -> entry_id:Id.t -> (t list, string) result

module Id_source : sig
  type t

  (** [of_allocator] preserves the existing standalone allocator behavior. *)
  val of_allocator : Allocator.t -> t

  (** [create] installs an application-owned allocator and collection
      validator. The allocator must reserve durable identity before returning
      an ID when externally visible streams require that invariant. *)
  val create
    :  namespace:string
    -> allocate:(unit -> (Id.t, string) result)
    -> validate:(entry list -> (unit, string) result)
    -> t

  val namespace : t -> string
  val allocate : t -> (Id.t, string) result
  val validate : t -> entry list -> (unit, string) result
end
