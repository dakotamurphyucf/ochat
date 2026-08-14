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

  val namespace : t -> string
  val next_sequence : t -> int
  val allocate : t -> (Id.t, string) result

  (** [reserve t ~count] atomically reserves [count] consecutive IDs.

      A failed reservation does not advance [t]. *)
  val reserve : t -> count:int -> (Id.t list, string) result
end

type t [@@deriving bin_io, sexp]

val create : allocator:Allocator.t -> Openai.Responses.Item.t -> (t, string) result
val create_with_id : id:Id.t -> Openai.Responses.Item.t -> t
val id : t -> Id.t
val item : t -> Openai.Responses.Item.t

(** [with_item t item] replaces the provider payload while preserving [t]'s ID. *)
val with_item : t -> Openai.Responses.Item.t -> t

val items : t list -> Openai.Responses.Item.t list

(** [validate ~allocator entries] rejects duplicate IDs and entries in the
    allocator's namespace whose sequence is not below its next unused
    sequence. Entries from other namespaces are permitted.

    Call collection validation while no concurrent allocation or collection
    mutation is in progress. Explicitly constructed/imported entries must be
    validated before becoming canonical history. *)
val validate : allocator:Allocator.t -> t list -> (unit, string) result
