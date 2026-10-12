open Core

(** One private invocation ledger shared by DOM, conversion and output owners. *)
module Limits : sig
  type t

  val create
    :  max_input_bytes:int
    -> max_output_bytes:int
    -> max_work_steps:int
    -> max_nodes:int
    -> max_depth:int
    -> t Or_error.t
end

module Error : sig
  type t =
    | Input_limit
    | Output_limit
    | Work_limit
    | Node_limit
    | Depth_limit
    | Invalid_html
  [@@deriving equal, sexp_of]
end

module Accounting : sig
  type t

  val input_bytes : t -> int
  val output_bytes : t -> int
  val work_steps : t -> int
  val nodes : t -> int
  val depth : t -> int
end

type t

(** Unbounded context preserves ordinary legacy wrappers; bounded run owns only
    its own private admission escape. No callback exception is translated. *)
val legacy : unit -> t

val run
  :  Limits.t
  -> input_bytes:int
  -> checkpoint:(unit -> unit)
  -> f:(t -> 'a)
  -> ('a * Accounting.t, Error.t) Result.t

val is_bounded : t -> bool
val poll : t -> unit
val work : t -> int -> unit
val node : t -> unit
val depth : t -> int -> unit
val output_bytes : t -> int -> unit
val reject_html : t -> unit

(** Bounded string operations charge before allocation, polling byte scans. *)
val scan : t -> string -> unit

val concat : t -> sep:string -> string list -> string
val strip : t -> string -> string
val soup_nodes : t -> 'a Soup.nodes -> 'a Soup.node list
val soup_texts : t -> 'a Soup.node -> string list
val soup_attribute : t -> string -> Soup.element Soup.node -> string option
val soup_delete : t -> 'a Soup.node -> unit
val nodes : t -> int -> unit
val map : t -> 'a list -> f:('a -> 'b) -> 'b list
val mapi : t -> 'a list -> f:(int -> 'a -> 'b) -> 'b list
val filter : t -> 'a list -> f:('a -> bool) -> 'a list
val filter_map : t -> 'a list -> f:('a -> 'b option) -> 'b list
val concat_map : t -> 'a list -> f:('a -> 'b list) -> 'b list
val init : t -> int -> f:(int -> 'a) -> 'a list
val split : t -> string -> on:char -> string list
val split_lines : t -> string -> string list
val lowercase : t -> string -> string
val lstrip : t -> string -> string
val rstrip : t -> string -> string
val reject_output : t -> unit
val enter : t -> unit
val leave : t -> unit
val is_substring : t -> string -> substring:string -> bool
val iter : t -> 'a list -> f:('a -> unit) -> unit
val for_all : t -> 'a list -> f:('a -> bool) -> bool
val exists : t -> 'a list -> f:('a -> bool) -> bool
val find : t -> 'a list -> f:('a -> bool) -> 'a option
val find_map : t -> 'a list -> f:('a -> 'b option) -> 'b option
val count : t -> 'a list -> f:('a -> bool) -> int
val length : t -> 'a list -> int
val rev : t -> 'a list -> 'a list
val rev_append : t -> 'a list -> 'a list -> 'a list
val append : t -> 'a list -> 'a list -> 'a list
val zip_exn : t -> 'a list -> 'b list -> ('a * 'b) list
