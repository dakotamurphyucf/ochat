open Core

type ids =
  { added : string list
  ; removed : string list
  ; changed : string list
  }
[@@deriving compare, equal, sexp]

type t =
  { server_changed : bool
  ; workspaces : ids
  ; prompts : ids
  ; permission_profiles : ids
  ; manifest_grants : ids
  }
[@@deriving compare, equal, sexp]

let ids ~id ~equal previous current =
  let previous =
    String.Map.of_alist_exn (List.map previous ~f:(fun value -> id value, value))
  in
  let current =
    String.Map.of_alist_exn (List.map current ~f:(fun value -> id value, value))
  in
  let added =
    Map.keys current |> List.filter ~f:(fun key -> not (Map.mem previous key))
  in
  let removed =
    Map.keys previous |> List.filter ~f:(fun key -> not (Map.mem current key))
  in
  let changed =
    Map.to_alist current
    |> List.filter_map ~f:(fun (key, value) ->
      match Map.find previous key with
      | Some old when not (equal old value) -> Some key
      | Some _ | None -> None)
  in
  { added; removed; changed }
;;

let between ~(previous : Config.t) ~(current : Config.t) =
  { server_changed = not (Config.Server.equal previous.server current.server)
  ; workspaces =
      ids
        ~id:(fun value -> value.Config.Workspace.id)
        ~equal:Config.Workspace.equal
        previous.workspaces
        current.workspaces
  ; prompts =
      ids
        ~id:(fun value -> value.Config.Prompt.id)
        ~equal:Config.Prompt.equal
        previous.prompts
        current.prompts
  ; permission_profiles =
      ids
        ~id:(fun value -> value.Config.Permission_profile.id)
        ~equal:Config.Permission_profile.equal
        previous.permission_profiles
        current.permission_profiles
  ; manifest_grants =
      ids
        ~id:(fun value -> value.Config.Manifest_grant.id)
        ~equal:Config.Manifest_grant.equal
        previous.manifest_grants
        current.manifest_grants
  }
;;

let ids_empty ids =
  List.is_empty ids.added && List.is_empty ids.removed && List.is_empty ids.changed
;;

let is_empty t =
  (not t.server_changed)
  && ids_empty t.workspaces
  && ids_empty t.prompts
  && ids_empty t.permission_profiles
  && ids_empty t.manifest_grants
;;
