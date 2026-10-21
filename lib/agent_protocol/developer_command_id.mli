(** Portable comparison identity for an existing persisted Developer command.
    This preserves the Store receipt ID's historical wire format exactly:
    any nonempty string of at most 256 bytes. It is deliberately not a
    prefixed {!Id} value. Decoding an ID never creates or controls a command.

    Agent_store.Developer_command_receipt.Id aliases this type and delegates
    validation here, adapting only its existing Document_schema error channel.
    Persisted bytes and equality remain unchanged. *)

type t [@@deriving compare, equal, sexp_of]

val of_string : string -> (t, Error.t) result
val to_string : t -> string
val to_json : t -> Jsonaf.t
val of_json : Jsonaf.t -> (t, Error.t) result
