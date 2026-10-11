open Core
module Project = Chatml_project
module Path = Source.Portable_path
module Error = Chatml_catalog_error

let compare_string left right ~checkpoint =
  let length = Int.min (String.length left) (String.length right) in
  let rec loop offset =
    if offset mod 4096 = 0 then checkpoint ();
    if offset = length
    then Int.compare (String.length left) (String.length right)
    else (
      let comparison = Char.compare left.[offset] right.[offset] in
      if comparison = 0 then loop (offset + 1) else comparison)
  in
  loop 0
;;

let directory_key directory =
  match Project.Source_dir.directory directory with
  | Root -> "."
  | Relative path -> Path.to_string path
;;

let prefix value = String.prefix value (Int.min 256 (String.length value))

let valid_name name =
  (not (String.is_empty name))
  && Char.(name.[0] >= 'A' && name.[0] <= 'Z')
  && String.for_all name ~f:(function
    | 'A' .. 'Z' | 'a' .. 'z' | '0' .. '9' | '_' -> true
    | _ -> false)
;;

module Observation = struct
  type kind =
    | Implementation
    | Interface

  type t =
    { affinity : unit ref
    ; kind : kind
    ; name : string
    ; path : Path.t
    ; declared_at : Project.Declared_at.t
    }
end

module Member = struct
  type requirement =
    | Required
    | Optional
  [@@deriving equal, sexp_of]

  type t =
    { path : Path.t
    ; requirement : requirement
    ; declared_at : Project.Declared_at.t
    }

  let path t = t.path
  let requirement t = t.requirement
  let declared_at t = t.declared_at
end

module Implementation = struct
  type interface =
    | Absent
    | Optional of Path.t
    | Required of Path.t

  type t =
    { name : string
    ; source : Path.t
    ; interface : interface
    ; declared_at : Project.Declared_at.t
    }

  let name t = t.name
  let source t = t.source
  let interface t = t.interface
  let declared_at t = t.declared_at
end

module Discovery = struct
  type t =
    { project : Project.t
    ; selection : Project.Selection.t
    ; origin : Project.Declared_at.t
    ; affinity : unit ref
    ; overrides : string array
    ; exclusions : String.Set.t
    ; directories : Project.Declared_at.t String.Map.t
    }

  let create project ~request ~checkpoint =
    let origin = Project.declared_at project in
    let open Result.Let_syntax in
    let%map selection =
      Project.select project ~request ~checkpoint
      |> Result.map_error ~f:(fun failure -> Error.create origin (Declaration failure))
    in
    let overrides =
      List.map (Project.module_overrides project) ~f:(fun value ->
        checkpoint ();
        Project.Module_override.name value)
      |> List.sort ~compare:(fun left right -> compare_string left right ~checkpoint)
      |> Array.of_list
    in
    let exclusions =
      List.fold
        (Project.exclusions project)
        ~init:String.Set.empty
        ~f:(fun set (path, _) ->
          checkpoint ();
          Set.add set (Path.to_string path))
    in
    let directories =
      List.fold
        (Project.source_dirs project)
        ~init:String.Map.empty
        ~f:(fun map directory ->
          checkpoint ();
          Map.set
            map
            ~key:(directory_key directory)
            ~data:(Project.Source_dir.declared_at directory))
    in
    { project; selection; origin; affinity = ref (); overrides; exclusions; directories }
  ;;

  let overridden t name =
    let rec find low high =
      if low >= high
      then false
      else (
        let middle = low + ((high - low) / 2) in
        let comparison = String.compare name t.overrides.(middle) in
        if comparison = 0
        then true
        else if comparison < 0
        then find low middle
        else find (middle + 1) high)
    in
    find 0 (Array.length t.overrides)
  ;;

  let observe t ~directory ~name =
    let parent = directory_key directory in
    match Map.find t.directories parent with
    | None -> Error (Error.create t.origin Undeclared_directory)
    | Some declared_at ->
      let invalid_path value =
        Error
          (Error.create
             declared_at
             (Invalid_candidate_path { path_prefix = prefix value }))
      in
      if String.length name > 1024
      then invalid_path name
      else if
        String.is_empty name
        || String.equal name "."
        || String.equal name ".."
        || String.exists name ~f:(function
          | '/' | '\000' -> true
          | _ -> false)
      then invalid_path name
      else (
        let selected =
          match String.chop_suffix name ~suffix:".chatml" with
          | Some basename -> Some (Observation.Implementation, basename)
          | None ->
            Option.map (String.chop_suffix name ~suffix:".chatmli") ~f:(fun basename ->
              Observation.Interface, basename)
        in
        match selected with
        | None -> Ok None
        | Some (kind, basename) ->
          let member = if String.equal parent "." then name else parent ^ "/" ^ name in
          if Set.mem t.exclusions member
          then Ok None
          else (
            let module_name =
              if String.is_empty basename
              then basename
              else
                String.of_char (Char.uppercase basename.[0])
                ^ String.drop_prefix basename 1
            in
            if not (valid_name module_name)
            then
              Error
                (Error.create
                   declared_at
                   (Invalid_candidate_name { basename_prefix = prefix basename }))
            else if overridden t module_name
            then Ok None
            else (
              match Path.create member with
              | Error _ -> invalid_path member
              | Ok path ->
                Ok
                  (Some
                     { Observation.affinity = t.affinity
                     ; kind
                     ; name = module_name
                     ; path
                     ; declared_at
                     }))))
  ;;
end

type t =
  { discovery : Discovery.t
  ; implementations : Implementation.t list
  ; members : Member.t list
  }

let project t = t.discovery.project
let selection t = t.discovery.selection
let origin t = t.discovery.origin
let implementations t = t.implementations
let members t = t.members

let companion discovery source declared_at =
  let path = Path.to_string source in
  match String.chop_suffix path ~suffix:".chatml" with
  | None -> Ok Implementation.Absent
  | Some stem ->
    let selected = stem ^ ".chatmli" in
    if Set.mem discovery.Discovery.exclusions selected
    then Ok Implementation.Absent
    else (
      match Path.create selected with
      | Ok path -> Ok (Implementation.Optional path)
      | Error _ ->
        Error
          (Error.create
             declared_at
             (Invalid_candidate_path { path_prefix = prefix selected })))
;;

let conventional discovery observations ~checkpoint =
  let open Result.Let_syntax in
  let observations =
    List.sort observations ~compare:(fun left right ->
      checkpoint ();
      Path.compare left.Observation.path right.Observation.path)
  in
  let%bind by_name =
    List.fold_result observations ~init:String.Map.empty ~f:(fun map observation ->
      checkpoint ();
      match observation.Observation.kind with
      | Interface -> Ok map
      | Implementation ->
        (match Map.find map observation.name with
         | Some previous ->
           Error
             (Error.create
                previous.Implementation.declared_at
                ~related:observation.declared_at
                (Ambiguous_module
                   { name = observation.name
                   ; first = previous.source
                   ; second = observation.path
                   }))
         | None ->
           let%map interface =
             companion discovery observation.path observation.declared_at
           in
           let implementation =
             { Implementation.name = observation.name
             ; source = observation.path
             ; interface
             ; declared_at = observation.declared_at
             }
           in
           Map.set map ~key:observation.name ~data:implementation))
  in
  let paths =
    Map.fold by_name ~init:String.Set.empty ~f:(fun ~key:_ ~data:value set ->
      checkpoint ();
      Set.add set (Path.to_string value.Implementation.source))
  in
  let%map () =
    List.fold_result observations ~init:() ~f:(fun () observation ->
      checkpoint ();
      match observation.Observation.kind with
      | Implementation -> Ok ()
      | Interface ->
        let path = Path.to_string observation.path in
        let expected = String.drop_suffix path 8 ^ ".chatml" in
        if Set.mem paths expected
        then Ok ()
        else
          Error (Error.create observation.declared_at (Orphan_interface observation.path)))
  in
  Map.fold by_name ~init:[] ~f:(fun ~key:_ ~data values ->
    checkpoint ();
    data :: values)
  |> List.rev
;;

let configured discovery ~checkpoint =
  List.map (Project.module_overrides discovery.Discovery.project) ~f:(fun value ->
    checkpoint ();
    let declared_at = Project.Module_override.declared_at value in
    let source = Project.Module_override.implementation value in
    let interface =
      match Project.Module_override.interface_selection value with
      | Explicit path -> Ok (Implementation.Required path)
      | Companion -> companion discovery source declared_at
    in
    Result.map interface ~f:(fun interface ->
      { Implementation.name = Project.Module_override.name value
      ; source
      ; interface
      ; declared_at
      }))
  |> Result.all
;;

let create discovery ~observations ~checkpoint =
  let open Result.Let_syntax in
  let%bind () =
    List.fold_result observations ~init:() ~f:(fun () observation ->
      checkpoint ();
      if phys_equal discovery.Discovery.affinity observation.Observation.affinity
      then Ok ()
      else Error (Error.create discovery.origin Foreign_observation))
  in
  let%bind conventional = conventional discovery observations ~checkpoint in
  let%bind configured = configured discovery ~checkpoint in
  let implementations =
    List.sort (conventional @ configured) ~compare:(fun left right ->
      compare_string left.Implementation.name right.Implementation.name ~checkpoint)
  in
  let add map path requirement declared_at =
    let key = Path.to_string path in
    match Map.find map key with
    | Some previous when Member.equal_requirement previous.Member.requirement Required ->
      map
    | Some _ | None -> Map.set map ~key ~data:{ Member.path; requirement; declared_at }
  in
  let members =
    List.fold implementations ~init:String.Map.empty ~f:(fun map value ->
      checkpoint ();
      let map = add map value.Implementation.source Member.Required value.declared_at in
      match value.interface with
      | Absent -> map
      | Optional path -> add map path Member.Optional value.declared_at
      | Required path -> add map path Member.Required value.declared_at)
  in
  let members =
    match Project.Selection.entry discovery.selection with
    | None -> members
    | Some entry ->
      add
        members
        (Project.Entry.source entry)
        Member.Required
        (Project.Entry.declared_at entry)
  in
  let members =
    Map.fold members ~init:[] ~f:(fun ~key:_ ~data values ->
      checkpoint ();
      data :: values)
    |> List.rev
  in
  Ok { discovery; implementations; members }
;;
