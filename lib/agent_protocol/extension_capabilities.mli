(** Versioned host qualification, separate from record-codec support. Presence of
    this metadata does not enable any model-visible tool or authorize effects. *)

type host =
  | Daemon
  | Embedded_durable
  | Embedded_transient
  | Direct
[@@deriving compare, equal, sexp]

type journal_flush =
  | Synced
  | Buffered
  | Memory
[@@deriving compare, equal, sexp]

type t = private
  { host : host
  ; journal_flush : journal_flush
  ; available_features : string list
  }
[@@deriving sexp]

val known_features : string list

val create
  :  host:host
  -> journal_flush:journal_flush
  -> available_features:string list
  -> (t, Error.t) result

(** Filter only the extension namespace; preserve unrelated protocol features.
    Host options cannot advertise an extension that has not been qualified. *)
val filter_available : t -> string list -> string list

val to_json : t -> Jsonaf.t
val of_json : Jsonaf.t -> (t, Error.t) result
