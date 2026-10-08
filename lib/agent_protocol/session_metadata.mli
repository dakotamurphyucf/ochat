(** Organization metadata confers no execution authority. Stored values retain
    historical strings under their document bounds; label keys are unique.
    Authored patches separately bound names to 1024 bytes, 128 label operations,
    keys to 256 bytes and values to 4096 bytes and reject malformed text. *)
module Values : sig
  type t = private
    { display_name : string option
    ; labels : (string * string) list
    }
  [@@deriving equal, sexp]

  val create
    :  display_name:string option
    -> labels:(string * string) list
    -> (t, Error.t) Result.t
end

module Patch : sig
  type name_change =
    | Keep
    | Set of string
    | Clear
  [@@deriving equal, sexp]

  type t [@@deriving sexp]

  (** Duplicate keys and overlapping set/remove operations are invalid. *)
  val create
    :  name:name_change
    -> set_labels:(string * string) list
    -> remove_labels:string list
    -> (t, Error.t) Result.t

  val apply : t -> Values.t -> (Values.t, Error.t) Result.t
  val to_json : t -> Jsonaf.t
  val of_json : Jsonaf.t -> (t, Error.t) Result.t
end

module Request : sig
  type t =
    { session_id : Id.Session.t
    ; attachment_id : Id.Attachment.t
    ; expected_metadata_revision : int64
    ; patch : Patch.t
    ; idempotency_key : Idempotency_key.t
    }
  [@@deriving sexp]

  val to_json : t -> Jsonaf.t
  val of_json : Jsonaf.t -> (t, Error.t) Result.t
end
