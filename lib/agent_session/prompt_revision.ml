open! Core

type t =
  { definition : Prompt_definition.t
  ; artifact : Agent_store.Prompt_artifact_store.Artifact.t
  ; materialized_tree : Eio.Fs.dir_ty Eio.Path.t
  ; elements : Prompt.Chat_markdown.top_level_elements list
  }

let create ~definition ~artifact ~materialized_tree ~elements =
  { definition; artifact; materialized_tree; elements }
;;

let id t = t.artifact.revision_id
let definition t = t.definition
let artifact t = t.artifact
let materialized_tree t = t.materialized_tree
let elements t = t.elements
let root_relative_path t = t.artifact.root_relative_path
