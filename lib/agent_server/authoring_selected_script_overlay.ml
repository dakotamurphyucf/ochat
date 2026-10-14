open! Core
module Selection = Authoring_script_selection
module Build = Chatml_project_build
module Prepared = Chatml_prepared_program
module Captured = Chatml_source
module Markdown = Prompt.Chat_markdown

module Error = struct
  type t =
    | Invalid_selected_entry
    | Body_limit
    | Lock_identity_unavailable
    | Recomposition of Build.Error.t
end

let prepare selected ~declaration ~max_body_bytes ~limits ~checkpoint =
  let open Result.Let_syntax in
  let body = Markdown.Declaration.script_body declaration in
  if max_body_bytes <= 0 || String.length body > max_body_bytes
  then Error Error.Body_limit
  else (
    let build = Selection.Selected.build selected in
    let prepared = Build.prepared build in
    let entry = Prepared.entry prepared in
    let body_digest = Captured.Digest.of_contents body ~checkpoint in
    let%bind original_entry =
      Prepared.find_source prepared entry
      |> Result.of_option ~error:Error.Invalid_selected_entry
    in
    if
      Int.equal (Captured.byte_length original_entry) (String.length body)
      && Captured.Digest.equal (Captured.digest original_entry) body_digest
    then Ok build
    else (
      let%bind edited_entry =
        Captured.create
          entry
          ~contents:body
          ~expected_digest:(Some body_digest)
          ~max_bytes:(Some max_body_bytes)
          ~checkpoint
        |> Result.map_error ~f:(fun _ -> Error.Invalid_selected_entry)
      in
      let sources =
        List.map (Prepared.sources prepared) ~f:(fun source ->
          checkpoint ();
          if Captured.Id.equal (Captured.id source) entry then edited_entry else source)
      in
      let%bind lock_id =
        Authoring_project_overlay.fresh_lock_id_from_sources
          ~sources
          ~prior_lock:(Captured.id (Build.lock_source build))
          ~checkpoint
        |> Result.map_error ~f:(function
          | Authoring_project_overlay.Error.Lock_identity_unavailable ->
            Error.Lock_identity_unavailable
          | Context _ | No_selected_contract | Work_limit | Invalid_overlay _ ->
            Error.Lock_identity_unavailable)
      in
      Build.Effective.recompose
        build
        ~sources
        ~lock_id
        ~contract:(Selection.Selected.contract selected)
        ~limits
        ~checkpoint
      |> Result.map_error ~f:(fun error -> Error.Recomposition error)))
;;
