(** Plain text with one highlighted literal occurrence. Offsets and lengths count
    UTF-8 bytes, relative to [text], and both ends are Unicode scalar boundaries.
    At most 2048 bytes; no HTML or terminal rendering interpretation is implied.
    Truncation flags describe omitted source text, never an incomplete search. *)
type t [@@deriving equal, sexp_of]

val create
  :  text:string
  -> highlight_start:int
  -> highlight_length:int
  -> truncated_before:bool
  -> truncated_after:bool
  -> (t, Error.t) result

val text : t -> string
val highlight_start : t -> int
val highlight_length : t -> int
val truncated_before : t -> bool
val truncated_after : t -> bool
val to_json : t -> Jsonaf.t
val of_json : Jsonaf.t -> (t, Error.t) result
