open Core

(** Bytes-only local document conversion. No acquisition, filesystem/network I/O,
    subprocess, implicit decompression or installed-asset lookup. *)
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

module Mode : sig
  type t =
    | Verbatim
    | Strip_html
    | Html_to_markdown
  [@@deriving equal, sexp_of]
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

module Output : sig
  type t

  val contents : t -> string
  val accounting : t -> Accounting.t
end

(** Input admission precedes parser/projection allocation. HTML uses the same
    parser and conversion/rendering semantics as the ordinary document owners.
    Verbatim returns exact bytes without HTML parsing; Strip_html trims individual
    text nodes, drops empty nodes and joins with LF. Flag precedence belongs to
    the caller (strip before markdown).

    Nodes count conservative reservations for DOM and conversion/output
    representation nodes, not OCaml words. Depth bounds admitted DOM and recursive
    conversion/rendering structure. Work counts visited nodes and scanned/emitted
    bytes, including repeated traversal; it is not CPU time or a heap-byte quota.
    Output measurement precedes full encoded string allocation; escaping and
    indentation expansions are included. Structural/working representations are
    independently bounded by nodes/work/input, not claimed to fit output bytes.

    Polling is cooperative between bounded batches and parser signals. A Markup
    parser step, RE2 query or admitted string/container primitive is indivisible
    and runs on admitted finite input; no
    preemption or wall-clock bound is promised. Borrowed checkpoint exceptions and
    unexpected failures propagate unchanged. Only this invocation's admission
    failures and understood HTML parse errors become Error. No callback/DOM/budget
    retained in the immutable Output. Limits are explicit and never reset between
    parsing, conversion and rendering. *)
val convert
  :  Mode.t
  -> contents:string
  -> limits:Limits.t
  -> checkpoint:(unit -> unit)
  -> (Output.t, Error.t) Result.t
