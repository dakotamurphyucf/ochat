open! Core
module Selection = Authoring_script_selection
module Build = Chatml_project_build
module Markdown = Prompt.Chat_markdown
module Pure = Authoring_entry_recomposition

module Error = struct
  type t =
    | Invalid_selected_entry
    | Body_limit
    | Lock_identity_unavailable
    | Recomposition of Build.Error.t
end

let recompose_entry build ~contract ~contents ~max_body_bytes ~limits ~checkpoint =
  Pure.recompose_entry build ~contract ~contents ~max_body_bytes ~limits ~checkpoint
  |> Result.map_error ~f:(function
    | Pure.Error.Invalid_selected_entry -> Error.Invalid_selected_entry
    | Body_limit -> Error.Body_limit
    | Lock_identity_unavailable -> Error.Lock_identity_unavailable
    | Recomposition error -> Error.Recomposition error)
;;

let prepare selected ~declaration ~max_body_bytes ~limits ~checkpoint =
  recompose_entry
    (Selection.Selected.build selected)
    ~contract:(Selection.Selected.contract selected)
    ~contents:(Markdown.Declaration.script_body declaration)
    ~max_body_bytes
    ~limits
    ~checkpoint
;;
