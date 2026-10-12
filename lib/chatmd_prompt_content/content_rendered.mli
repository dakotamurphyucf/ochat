open Core
module B = Content_budget
module Input = Chatmd_parser_input
module Parsed = Prompt.Chat_markdown.Parsed
module Occurrence = Prompt.Chat_markdown.Document_occurrence
module Member = Source.Portable_path

module Source : sig
  type t =
    | Local of
        { member : Member.t
        ; contents : string
        }
    | Builtin of Builtin_instructions.Asset.t
end

module Provenance : sig
  type t =
    | Local of Member.t
    | Builtin of string
end

(** Actual occurrence and conversion result; native read authority remains external. *)
type t

val create
  :  Parsed.t
  -> occurrence:Occurrence.t
  -> source:Source.t
  -> limits:B.Limits.t
  -> checkpoint:(unit -> unit)
  -> (t, B.Error.t) Result.t

val root : t -> Input.t
val occurrence : t -> Occurrence.t
val provenance : t -> Provenance.t
val input_digest : t -> string
val output_digest : t -> string
val input_bytes : t -> int
val contents : t -> string
val accounting : t -> B.Accounting.t
