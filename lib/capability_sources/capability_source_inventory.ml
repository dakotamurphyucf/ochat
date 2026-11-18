open! Core
module Path = Source.Portable_path
module Capture = Chatml_source
module Project = Chatml_project
module Schema = Document_schema

module Error = struct
  type code =
    | Invalid_id
    | Invalid_limit
    | Limit_exceeded
    | Invalid_inventory
    | Unknown_resource
    | Unknown_member
  [@@deriving equal, sexp_of]

  type t =
    { code : code
    ; message : string
    }
  [@@deriving sexp_of]

  let code t = t.code
  let message t = t.message
  let create code message = { code; message = String.prefix message 2048 }
end

let invalid message = Error (Error.create Invalid_inventory message)

module Id = struct
  type t = string [@@deriving equal, compare, sexp_of]

  let of_string value =
    let first c = Char.(c >= 'a' && c <= 'z') in
    let rest c = first c || Char.is_digit c || Char.equal c '_' || Char.equal c '-' in
    if String.is_empty value || String.length value > 64
    then Error (Error.create Invalid_id "resource ID must contain 1..64 bytes")
    else if not (first value.[0] && String.for_all value ~f:rest)
    then Error (Error.create Invalid_id "resource ID must match [a-z][a-z0-9_-]*")
    else Ok value
  ;;

  let to_string t = t
end

module Limits = struct
  type t =
    { max_resources : int
    ; max_members : int
    ; max_member_bytes : int
    ; max_total_bytes : int
    ; max_work_steps : int
    }

  let create
        ~max_resources
        ~max_members
        ~max_member_bytes
        ~max_total_bytes
        ~max_work_steps
    =
    if
      List.exists
        [ max_resources; max_members; max_member_bytes; max_total_bytes; max_work_steps ]
        ~f:(fun n -> n <= 0)
    then Error (Error.create Invalid_limit "inventory limits must be positive")
    else
      Ok { max_resources; max_members; max_member_bytes; max_total_bytes; max_work_steps }
  ;;

  let default =
    { max_resources = 16
    ; max_members = 1024
    ; max_member_bytes = 1_048_576
    ; max_total_bytes = 4_194_304
    ; max_work_steps = 1_000_000
    }
  ;;
end

module Member = struct
  type t =
    { path : Path.t
    ; source : Capture.t
    }

  let path t = t.path
  let contents t = Capture.contents t.source
  let digest t = Capture.digest t.source
  let byte_length t = Capture.byte_length t.source
end

module Project_summary = struct
  type t =
    { path : Path.t
    ; namespace : string
    ; required_tools : string list
    ; environments : (string * Project.Compiler_requirement.t) list
    }

  let path t = t.path
  let namespace t = t.namespace
  let required_tools t = t.required_tools
  let environments t = t.environments
end

let string value = `String value
let strings values = `Array (List.map values ~f:string)
let digest_json digest = string (Capture.Digest.to_hex digest)

let option_json f = function
  | None -> `Null
  | Some value -> f value
;;

module Resource = struct
  type t =
    { id : Id.t
    ; title : string
    ; entry : Path.t
    ; digest : Capture.Digest.t
    ; members : Member.t list
    ; projects : Project_summary.t list
    ; byte_length : int
    ; manifest : Jsonaf.t
    }

  let id t = t.id
  let title t = t.title
  let entry t = t.entry
  let digest t = t.digest
  let members t = t.members
  let projects t = t.projects
  let byte_length t = t.byte_length
  let manifest t = t.manifest

  let member t path =
    match List.find t.members ~f:(fun member -> Path.equal member.Member.path path) with
    | Some value -> Ok value
    | None ->
      Error
        (Error.create Unknown_member "member is not in the selected installed resource")
  ;;
end

type raw =
  { id : string
  ; title : string
  ; entry : string
  ; members : (string * string) list
  }

type t = Resource.t list

exception Exhausted

let limit () =
  Error (Error.create Limit_exceeded "installed source inventory limit exceeded")
;;

let schema_error error = Sexp.to_string_hum (Schema.Error.sexp_of_t error)
let capture_error error = Core.Error.to_string_hum error

let project_summary (member : Member.t) ~checkpoint =
  let open Result.Let_syntax in
  let%bind declaration =
    Project.decode member.source ~limits:Schema.Limits.default ~checkpoint
    |> Result.map_error ~f:(fun error ->
      Error.create Invalid_inventory (schema_error (Project.Error.details error)))
  in
  let%bind names =
    Project.Environment_names.list declaration ~max_names:128 ~checkpoint
    |> Result.map_error ~f:(fun _ ->
      Error.create Invalid_inventory "invalid project environment inventory")
  in
  let%map environments =
    List.map names ~f:(fun name ->
      checkpoint ();
      Project.select
        declaration
        ~request:(Library { environment = Some name })
        ~checkpoint
      |> Result.map ~f:(fun selected ->
        ( name
        , Project.Scope_spec.compiler_requirement (Project.Selection.environment selected)
        ))
      |> Result.map_error ~f:(fun error ->
        Error.create Invalid_inventory (schema_error (Project.Error.details error))))
    |> Result.all
  in
  { Project_summary.path = member.path
  ; namespace = Project.namespace declaration
  ; required_tools = List.map (Project.required_tools declaration) ~f:fst
  ; environments
  }
;;

let project_json (project : Project_summary.t) =
  let environment (name, requirement) =
    `Object
      [ "name", string name
      ; ( "compiler_family"
        , option_json string (Project.Compiler_requirement.compiler_family requirement) )
      ; ( "allowed_targets"
        , option_json strings (Project.Compiler_requirement.allowed_targets requirement) )
      ; ( "exact_contract"
        , option_json
            digest_json
            (Project.Compiler_requirement.exact_contract requirement) )
      ]
  in
  `Object
    [ "path", string (Path.to_string project.path)
    ; "namespace", string project.namespace
    ; "required_tools", strings project.required_tools
    ; "environments", `Array (List.map project.environments ~f:environment)
    ]
;;

let member_json (member : Member.t) =
  `Object
    [ "path", string (Path.to_string member.path)
    ; "sha256", digest_json (Member.digest member)
    ; "bytes", `Number (Int.to_string (Member.byte_length member))
    ]
;;

let admit raws ~(limits : Limits.t) ~checkpoint:caller_checkpoint =
  let work = ref 0 in
  let bytes = ref 0 in
  let count = ref 0 in
  let checkpoint () =
    caller_checkpoint ();
    if !work >= limits.max_work_steps then raise Exhausted;
    Int.incr work
  in
  let charge amount =
    checkpoint ();
    if amount > limits.max_total_bytes - !bytes then raise Exhausted;
    bytes := !bytes + amount
  in
  let admit_resource (raw : raw) =
    let open Result.Let_syntax in
    checkpoint ();
    let%bind id = Id.of_string raw.id in
    if String.is_empty raw.title || String.length raw.title > 1024
    then invalid "resource title must contain 1..1024 bytes"
    else (
      charge (String.length raw.title + String.length raw.id);
      let%bind entry =
        Path.create raw.entry
        |> Result.map_error ~f:(fun _ ->
          Error.create Invalid_inventory "invalid resource entry path")
      in
      let%bind members =
        List.map raw.members ~f:(fun (name, contents) ->
          checkpoint ();
          if !count >= limits.max_members then raise Exhausted;
          Int.incr count;
          if String.length contents > limits.max_member_bytes then raise Exhausted;
          charge (String.length name);
          charge (String.length contents);
          let%bind path =
            Path.create name
            |> Result.map_error ~f:(fun _ ->
              Error.create Invalid_inventory "invalid resource member path")
          in
          let%bind source_id =
            Capture.Id.create ~package_namespace:raw.id ~relative_path:name
            |> Result.map_error ~f:(fun error ->
              Error.create Invalid_inventory (capture_error error))
          in
          let%map source =
            Capture.create
              source_id
              ~contents
              ~expected_digest:None
              ~max_bytes:(Some limits.max_member_bytes)
              ~checkpoint
            |> Result.map_error ~f:(fun error ->
              Error.create Invalid_inventory (capture_error error))
          in
          { Member.path; source })
        |> Result.all
      in
      let members =
        List.sort members ~compare:(fun a b -> Path.compare a.Member.path b.Member.path)
      in
      match Path.first_collision (List.map members ~f:Member.path) ~checkpoint with
      | Some _ ->
        invalid
          "resource has duplicate, case-colliding or prefix-conflicting member paths"
      | None ->
        if
          not (List.exists members ~f:(fun member -> Path.equal member.Member.path entry))
        then invalid "resource entry is not included"
        else (
          let%bind projects =
            List.filter_map members ~f:(fun member ->
              checkpoint ();
              if
                String.equal
                  (Filename.basename (Path.to_string member.Member.path))
                  "chatml.project.json"
              then Some (project_summary member ~checkpoint)
              else None)
            |> Result.all
          in
          let identity =
            `Object
              [ "format_version", `Number "1"
              ; "id", string id
              ; "entry", string raw.entry
              ; "members", `Array (List.map members ~f:member_json)
              ]
          in
          let%bind size =
            Schema.Json.validate_and_measure_with_checkpoint
              ~limits:Schema.Limits.default
              ~checkpoint
              identity
            |> Result.map_error ~f:(fun error ->
              Error.create Invalid_inventory (schema_error error))
          in
          charge size;
          let digest =
            Capture.Digest.of_contents (Jsonaf.to_string identity) ~checkpoint
          in
          let manifest =
            `Object
              [ "format_version", `Number "1"
              ; "id", string id
              ; "title", string raw.title
              ; "entry", string raw.entry
              ; "sha256", digest_json digest
              ; "members", `Array (List.map members ~f:member_json)
              ; "projects", `Array (List.map projects ~f:project_json)
              ]
          in
          let%map size =
            Schema.Json.validate_and_measure_with_checkpoint
              ~limits:Schema.Limits.default
              ~checkpoint
              manifest
            |> Result.map_error ~f:(fun error ->
              Error.create Invalid_inventory (schema_error error))
          in
          charge size;
          { Resource.id
          ; title = raw.title
          ; entry
          ; digest
          ; members
          ; projects
          ; byte_length = List.sum (module Int) members ~f:Member.byte_length
          ; manifest
          }))
  in
  try
    checkpoint ();
    if List.length raws > limits.max_resources
    then limit ()
    else
      let open Result.Let_syntax in
      let%bind resources = List.map raws ~f:admit_resource |> Result.all in
      match List.find_a_dup (List.map resources ~f:Resource.id) ~compare:Id.compare with
      | Some _ -> invalid "installed resource IDs are duplicated"
      | None ->
        Ok
          (List.sort resources ~compare:(fun a b ->
             Id.compare a.Resource.id b.Resource.id))
  with
  | Exhausted -> limit ()
;;

let list t ~max_items ~checkpoint =
  checkpoint ();
  if max_items < 0
  then Error (Error.create Invalid_limit "resource list limit must be nonnegative")
  else if List.length t > max_items
  then limit ()
  else (
    List.iter t ~f:(fun _ -> checkpoint ());
    Ok t)
;;

let find t id =
  match List.find t ~f:(fun resource -> Id.equal resource.Resource.id id) with
  | Some resource -> Ok resource
  | None -> Error (Error.create Unknown_resource "resource is not installed")
;;
