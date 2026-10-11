open! Core

type t =
  { definition : Prompt_definition.t
  ; artifact : Agent_store.Prompt_artifact_store.Artifact.t
  ; materialized_tree : Eio.Fs.dir_ty Eio.Path.t
  ; parser_context : Prompt.Chat_markdown.Parsed.t option
  ; programs : Chatml_prompt_programs.t option
  ; elements : Prompt.Chat_markdown.top_level_elements list
  }

let create ~definition ~artifact ~materialized_tree ~elements =
  { definition
  ; artifact
  ; materialized_tree
  ; elements
  ; parser_context = None
  ; programs = None
  }
;;

let with_custody t ~parser_context ~programs ~checkpoint =
  let root = Chatml_prompt_programs.root_input programs in
  let bundle_matches =
    match t.artifact.program_bundle with
    | None -> false
    | Some bundle ->
      let supplied = Chatml_prompt_programs.to_storage programs in
      Agent_store.Prompt_program_bundle.Manifest.equal
        (Agent_store.Prompt_program_bundle.manifest bundle)
        (Agent_store.Prompt_program_bundle.manifest supplied)
      && List.equal
           (fun left right ->
              checkpoint ();
              let module Payload = Agent_store.Prompt_program_bundle.Payload in
              let a = Payload.contents left
              and b = Payload.contents right in
              if
                (not (String.equal (Payload.sha256 left) (Payload.sha256 right)))
                || String.length a <> String.length b
              then false
              else (
                let same = ref true
                and offset = ref 0 in
                while !same && !offset < String.length a do
                  if !offset mod 4096 = 0 then checkpoint ();
                  same := Char.equal a.[!offset] b.[!offset];
                  incr offset
                done;
                !same))
           (Agent_store.Prompt_program_bundle.payloads bundle)
           (Agent_store.Prompt_program_bundle.payloads supplied)
  in
  checkpoint ();
  if
    (not bundle_matches)
    || (not (String.equal (Chatmd_parser_input.path root) t.artifact.root_relative_path))
    || (not
          (Chatmd_parser_input.authored_matches root t.artifact.root_chatmd ~checkpoint))
    || not (phys_equal t.elements (Prompt.Chat_markdown.Parsed.elements parser_context))
  then Or_error.error_string "Parser custody does not belong to this artifact/elements"
  else (
    let%map.Or_error () =
      Chatml_prompt_programs.validate_selection
        programs
        ~parsed:parser_context
        ~limits:(Prompt_program_custody.limits ())
        ~checkpoint
      |> Result.map_error ~f:(fun _ -> Error.of_string "Invalid active parser custody")
    in
    { t with parser_context = Some parser_context; programs = Some programs })
;;

let id t = t.artifact.revision_id
let definition t = t.definition
let artifact t = t.artifact
let materialized_tree t = t.materialized_tree
let elements t = t.elements
let root_relative_path t = t.artifact.root_relative_path
let parser_context t = t.parser_context
let programs t = t.programs
