(** Best-effort transient display state. It is not a terminal result, canonical
    history or a durable replay stream. Channels retain bounded text suffixes. *)
type channel =
  | Assistant
  | Reasoning
  | Stdout
  | Stderr
  | Activity
[@@deriving compare, equal, sexp]

type item =
  { channel : channel
  ; text : string
  ; truncated : bool
  }
[@@deriving sexp]

type t =
  { sequence : int
  ; channels : item list
  }
[@@deriving sexp]

val max_update_bytes : int
val max_channel_bytes : int
val max_updates : int
val to_json : t -> Jsonaf.t
val of_json : Jsonaf.t -> (t, Error.t) result
