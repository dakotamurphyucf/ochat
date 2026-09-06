(** Opaque cursor pagination shared by list methods. *)

module Cursor : sig
  type t [@@deriving compare, equal, sexp]

  (** [of_string encoded] validates a nonempty opaque cursor. *)
  val of_string : string -> (t, Error.t) result

  (** [to_string t] returns the opaque cursor bytes. *)
  val to_string : t -> string

  (** [of_json json] decodes a cursor string. *)
  val of_json : Jsonaf.t -> (t, Error.t) result

  (** [to_json t] encodes a cursor string. *)
  val to_json : t -> Jsonaf.t
end

module Request : sig
  type t =
    { limit : int
    ; cursor : Cursor.t option
    }
  [@@deriving sexp]

  (** [create ~limit ?cursor ()] creates a page request with a positive limit. *)
  val create : limit:int -> ?cursor:Cursor.t -> unit -> (t, Error.t) result

  (** [to_fields t] encodes request fields for embedding in method parameters. *)
  val to_fields : t -> (string * Jsonaf.t) list

  (** [of_fields fields] decodes [limit] and optional [cursor]. *)
  val of_fields : Json_codec.fields -> (t, Error.t) result

  (** [to_json t] encodes a page request object. *)
  val to_json : t -> Jsonaf.t

  (** [of_json json] decodes a page request object. *)
  val of_json : Jsonaf.t -> (t, Error.t) result
end

type 'a t =
  { items : 'a list
  ; next_cursor : Cursor.t option
  }
[@@deriving sexp]

(** [to_json encode_item t] encodes a page response. *)
val to_json : ('a -> Jsonaf.t) -> 'a t -> Jsonaf.t

(** [of_json decode_item json] decodes a page response. *)
val of_json : (Jsonaf.t -> ('a, Error.t) result) -> Jsonaf.t -> ('a t, Error.t) result
