type t

(** [create ()] creates a stateful UTF-8 byte-stream decoder. *)
val create : unit -> t

(** [add t bytes] consumes [bytes] and returns complete valid UTF-8 decoded
    since this call began.

    Incomplete trailing sequences remain buffered in [t]. Malformed sequences
    are replaced with U+FFFD.

    @raise Invalid_argument if [t] has been finished. *)
val add : t -> string -> string

(** [finish t] ends the byte stream and returns remaining decoded UTF-8.

    An incomplete trailing sequence is replaced with U+FFFD.

    @raise Invalid_argument if [t] has already been finished. *)
val finish : t -> string
