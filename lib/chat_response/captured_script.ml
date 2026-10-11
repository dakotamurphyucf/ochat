open Core
module Custody = Chatml_prompt_programs
module Restored = Custody.Restored
module Contract = Chatml.Chatml_compiler_contract
module Prepared = Chatml_prepared_program

module Declaration = struct
  type t =
    | Extension of Chatmd_shell_spec.Extension_spec.script
    | Shell of Chatmd_shell_spec.Chatmd_script_spec.t
    | Moderator of Prompt.Chat_markdown.script
end

type t =
  { prepared : Prepared.t
  ; required_tools : string list
  }

let failure code message = Error Chatml_compilation.{ code; message; diagnostic = None }

let select inventory ~declaration ~contract ~checkpoint =
  let id, source_text, source_sha256, project_selection =
    match declaration with
    | Declaration.Extension script ->
      ( script.id
      , Chatmd_shell_spec.Extension_spec.script_text script
      , script.source_sha256
      , script.project_selection )
    | Shell script ->
      ( script.id
      , Chatmd_shell_spec.Chatmd_script_spec.source_text script
      , script.source_sha256
      , script.project_selection )
    | Moderator script ->
      let source_text =
        match script.source with
        | Inline text | Src { source_text = text; _ } -> text
      in
      ( script.id
      , source_text
      , Chatml_source.Digest.(of_contents source_text ~checkpoint |> to_hex)
      , script.project_selection )
  in
  let digest = Contract.fingerprint contract ~checkpoint in
  let matches =
    List.filter (Restored.programs inventory) ~f:(fun program ->
      checkpoint ();
      String.equal (Custody.Binding.script_id (Restored.Program.binding program)) id
      && Chatml_source.Digest.equal (Restored.Program.compiler_contract program) digest)
  in
  let open Result.Let_syntax in
  let%bind program =
    match matches with
    | [ program ] -> Ok program
    | [] -> failure "chatml.missing_program" "captured script graph is unavailable"
    | _ -> failure "chatml.ambiguous_program" "multiple captured graphs match the script"
  in
  let binding = Restored.Program.binding program in
  let%bind () =
    if
      Option.equal
        Chatmd_shell_spec.Script_project.equal
        project_selection
        (Prompt.Chat_markdown.Declaration.project_selection
           (Restored.Program.declaration program))
    then Ok ()
    else
      failure
        "chatml.project_mismatch"
        "captured declaration has a different project or environment selection"
  in
  let%bind () =
    if
      String.equal
        (Chatml_source.Digest.to_hex (Custody.Binding.script_digest binding))
        source_sha256
    then Ok ()
    else failure "chatml.source_mismatch" "captured graph belongs to a different script"
  in
  let prepared = Restored.Program.prepared program in
  let%bind () =
    match Prepared.find_source prepared (Prepared.entry prepared) with
    | None -> failure "chatml.source_mismatch" "captured entry source is missing"
    | Some entry ->
      let actual = Chatml_source.contents entry in
      let expected = source_text in
      let length = String.length expected in
      let equal = ref (Int.equal length (String.length actual)) in
      let offset = ref 0 in
      while !equal && !offset < length do
        checkpoint ();
        let stop = !offset + Int.min 4096 (length - !offset) in
        while !equal && !offset < stop do
          equal := Char.equal actual.[!offset] expected.[!offset];
          incr offset
        done
      done;
      if !equal
      then Ok ()
      else
        failure "chatml.source_mismatch" "captured entry bytes disagree with declaration"
  in
  let%bind required_tools =
    match project_selection, Restored.Program.input program with
    | None, Provided _ -> Ok []
    | Some _, Selected build ->
      Ok (Chatml_project.Lock.required_tools (Chatml_project_build.lock build))
    | Some _, Provided _ | None, Selected _ ->
      failure "chatml.project_mismatch" "captured graph does not match project selection"
  in
  Ok { prepared; required_tools }
;;

let prepared t = t.prepared
let fingerprint t = Prepared.fingerprint t.prepared
let input_bytes t = Prepared.Accounting.input_bytes (Prepared.accounting t.prepared)

let admit_tools t ~capabilities ~checkpoint =
  checkpoint ();
  Tool_capability.select capabilities ~names:t.required_tools
  |> Result.map ~f:(fun _ -> ())
  |> Result.map_error ~f:(fun error ->
    Chatml_compilation.{ code = error.code; message = error.message; diagnostic = None })
;;
