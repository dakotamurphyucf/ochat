open Core
module B = Content_budget
module Input = Chatmd_parser_input
module Parsed = Prompt.Chat_markdown.Parsed
module Occurrence = Prompt.Chat_markdown.Document_occurrence
module R = Content_rendered

module Kind : sig
  type t =
    | Dynamic
    | Captured of
        { provenance : R.Provenance.t
        ; input_digest : string
        ; input_bytes : int
        ; output_digest : string
        ; contents : string
        }
end

(** Stored occurrence facts, including Dynamic slots. Input references always
    identify supplied immutable parser revisions, never native paths. *)
type t

val create
  :  root:Input.t
  -> range:Input.Range.t
  -> ordinal:int
  -> src_digest:string
  -> src_bytes:int
  -> local:bool
  -> cleanup_html:bool
  -> markdown:bool
  -> kind:Kind.t
  -> budget:B.t
  -> (t, B.Error.t) Result.t

val key : t -> string
val root : t -> Input.t
val input : t -> Input.t
val range : t -> Input.Range.t
val ordinal : t -> int
val src_digest : t -> string
val src_bytes : t -> int
val local : t -> bool
val cleanup_html : t -> bool
val markdown : t -> bool
val kind : t -> Kind.t

val matches
  :  t
  -> root:Input.t
  -> Occurrence.t
  -> budget:B.t
  -> (bool, B.Error.t) Result.t

val equal : t -> t -> budget:B.t -> (bool, B.Error.t) Result.t
val input_key : Input.t -> budget:B.t -> (string, B.Error.t) Result.t
val input_equal : Input.t -> Input.t -> budget:B.t -> (bool, B.Error.t) Result.t
