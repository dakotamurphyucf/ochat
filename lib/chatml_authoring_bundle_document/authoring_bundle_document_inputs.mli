(** Exact, in-memory association between explicit uploaded document members and
    the generated parser's portable member paths. The borrowed Stage owns every
    lookup, comparison, and retained parser input charge. *)

module Bundle = Agent_protocol.Authoring_bundle.Bundle
module Captured = Chatml_source
module Stage = Chatml_authoring_bundle_project.Attempt.Stage

type t

val create : Bundle.Manifest.Chatmd_document.t -> stage:Stage.t -> (t, unit) result
val root_file : t -> string
val bundle_sources : t -> (string * string) list
val source_id : t -> Source.Portable_path.t -> Captured.Id.t option
val input_source_id : t -> Chatmd_parser_input.t -> stage:Stage.t -> Captured.Id.t option
val source_at : t -> Source.Portable_path.t -> stage:Stage.t -> Captured.t option

val covers_reads_or_roots
  :  t
  -> read_members:string list
  -> selected_roots:Captured.Id.t list
  -> stage:Stage.t
  -> bool
