open Core
module Project = Chatml_project
module Build = Chatml_project_build
module Captured = Chatml_source

module Root = struct
  type t =
    | Local of
        { locator : string
        ; project : Source.Portable_path.t
        }
    | Git of
        { repository : string
        ; pin : Project.Git_pin.t
        ; package_root : Source.Portable_path.t option
        ; project : Source.Portable_path.t
        }
end

module Entry = struct
  type t =
    | Declared
    | Captured_fragment of Captured.t
    | Captured_file of
        { source : Captured.t
        ; root_member : Source.Portable_path.t option
        }
end

(** Metadata admission does not validate a declaration or grant authority.
    Captured/previous-lock bytes are immutable inputs and separately bounded by
    composition. The authentic compiler contract is never reconstructed by name. *)
type t =
  { root : Root.t
  ; operation : Build.Operation.t
  ; selection : Project.Selection.request
  ; entry : Entry.t
  ; contract : Chatml.Chatml_compiler_contract.t
  ; lock_id : Captured.Id.t
  }

let create_with_entry
      root
      ~operation
      ~selection
      ~entry
      ~contract
      ~lock_id
      ~max_request_bytes
  =
  let root_labels =
    match root with
    | Root.Local { locator; project } ->
      [ locator; Source.Portable_path.to_string project ]
    | Git { repository; pin; package_root; project } ->
      [ repository; Project.Git_pin.commit pin; Source.Portable_path.to_string project ]
      @ Option.to_list (Option.map package_root ~f:Source.Portable_path.to_string)
  in
  let selected_labels =
    match selection with
    | Project.Selection.Entry { name; environment } -> name :: Option.to_list environment
    | Library { environment } -> Option.to_list environment
  in
  let member_labels =
    match entry with
    | Entry.Declared | Captured_fragment _ -> []
    | Captured_file { source = _; root_member } ->
      Option.to_list (Option.map root_member ~f:Source.Portable_path.to_string)
  in
  let labels =
    (Captured.Id.package_namespace lock_id
     :: Captured.Id.relative_path lock_id
     :: root_labels)
    @ selected_labels
    @ member_labels
  in
  let rec fits remaining = function
    | [] -> true
    | label :: rest ->
      let bytes = String.length label in
      bytes > 0 && bytes <= remaining && fits (remaining - bytes) rest
  in
  let valid_entry =
    match selection, entry with
    | Project.Selection.Entry _, Entry.Declared
    | Library _, (Captured_fragment _ | Captured_file _) -> true
    | Entry _, (Captured_fragment _ | Captured_file _) | Library _, Declared -> false
  in
  if max_request_bytes < 0 || (not valid_entry) || not (fits max_request_bytes labels)
  then Or_error.error_string "invalid or excessive project preparation request metadata"
  else Ok { root; operation; selection; entry; contract; lock_id }
;;

let create root ~operation ~selection ~entry ~contract ~lock_id ~max_request_bytes =
  let entry =
    match entry with
    | Build.Entry.Declared -> Entry.Declared
    | Captured source -> Captured_fragment source
  in
  create_with_entry
    root
    ~operation
    ~selection
    ~entry
    ~contract
    ~lock_id
    ~max_request_bytes
;;
