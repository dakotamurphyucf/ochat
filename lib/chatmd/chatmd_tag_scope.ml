open! Core
open! Chatmd_ast

module Error = struct
  type t = Depth_limit [@@deriving equal, sexp_of]
end

module Root_hint = struct
  type t =
    | Shell_access
    | Tool of
        { shell : bool
        ; read_file : bool
        ; extension : bool
        }
  [@@deriving sexp_of]
end

type t =
  { depth : int
  ; tags : Chatmd_ast.tag list
  ; structured_roots : (int * Root_hint.t) list
  ; extension_scope_depths : int list
  }

let root = { depth = 0; tags = []; structured_roots = []; extension_scope_depths = [] }
let depth t = t.depth
let top t = List.hd t.tags

let nearest_structured_root t ~checkpoint =
  match t.structured_roots with
  | [] -> None
  | (root_depth, hint) :: _ ->
    let rec collect remaining tags acc =
      if Int.equal remaining 0
      then acc
      else (
        checkpoint ();
        match tags with
        | [] -> acc
        | tag :: tags -> collect (remaining - 1) tags (tag :: acc))
    in
    Some (hint, collect (t.depth - root_depth) t.tags [])
;;

let recognize t ~name =
  match tag_of_string_opt name with
  | Some (Shell_element _) when List.is_empty t.structured_roots -> None
  | Some Uses when List.is_empty t.extension_scope_depths -> None
  | Some (Authoring_context | Authoring_help) when not (Int.equal t.depth 0) -> None
  | tag -> tag
;;

let first_attribute attributes name ~checkpoint =
  List.find_map attributes ~f:(fun (candidate, value) ->
    checkpoint ();
    if String.equal candidate name then Some value else None)
;;

let is_extension_tool attributes ~checkpoint =
  List.exists attributes ~f:(fun (name, value) ->
    checkpoint ();
    String.equal name "type"
    && Option.value_map value ~default:false ~f:(function
      | "moderator" | "chatml" -> true
      | _ -> false))
;;

let structured_root_hint tag attributes ~checkpoint =
  match tag with
  | Shell_access -> Some Root_hint.Shell_access
  | Tool ->
    let shell =
      Option.value_map
        (first_attribute attributes "type" ~checkpoint)
        ~default:false
        ~f:(Option.value_map ~default:false ~f:(String.equal "shell"))
    in
    let read_file =
      Option.value_map
        (first_attribute attributes "name" ~checkpoint)
        ~default:false
        ~f:
          (Option.value_map ~default:false ~f:(function
             | "read_file" | "get_contents" -> true
             | _ -> false))
    in
    let extension = is_extension_tool attributes ~checkpoint in
    Option.some_if
      (shell || read_file || extension)
      (Root_hint.Tool { shell; read_file; extension })
  | Msg
  | User
  | Agent
  | Assistant
  | System
  | Developer
  | Script
  | Doc
  | Img
  | Import
  | Config
  | Reasoning
  | Summary
  | Tool_call
  | Tool_response
  | Moderator_runtime
  | Uses
  | Authoring_context
  | Shell_element _
  | Authoring_help -> None
;;

let enter t ~tag ~attributes ~max_depth ~checkpoint =
  if max_depth <= 0 || t.depth >= max_depth
  then Error Error.Depth_limit
  else (
    let depth = t.depth + 1 in
    let extension_scope_depths =
      if Chatmd_ast.tag_equal tag Tool && is_extension_tool attributes ~checkpoint
      then depth :: t.extension_scope_depths
      else t.extension_scope_depths
    in
    let structured_roots =
      match structured_root_hint tag attributes ~checkpoint with
      | Some hint -> (depth, hint) :: t.structured_roots
      | None -> t.structured_roots
    in
    Ok { depth; tags = tag :: t.tags; structured_roots; extension_scope_depths })
;;

let leave t =
  if Int.equal t.depth 0
  then t
  else (
    let drop_current = function
      | depth :: rest when Int.equal depth t.depth -> rest
      | depths -> depths
    in
    let structured_roots =
      match t.structured_roots with
      | (depth, _) :: roots when Int.equal depth t.depth -> roots
      | roots -> roots
    in
    { depth = t.depth - 1
    ; tags = List.tl t.tags |> Option.value ~default:[]
    ; structured_roots
    ; extension_scope_depths = drop_current t.extension_scope_depths
    })
;;
