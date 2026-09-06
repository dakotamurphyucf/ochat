open! Core
module S = Chatmd_shell_spec.Shell_spec

type t =
  { values : string array
  ; secrets : string list
  }

type error =
  { code : string
  ; message : string
  }
[@@deriving sexp, compare, equal]

exception Environment_error of error

let fail code message = raise_notrace (Environment_error { code; message })

let split_entry entry =
  match String.lsplit2 entry ~on:'=' with
  | Some pair -> pair
  | None -> entry, ""
;;

let process_map host =
  Array.to_list host.Host.process_environment
  |> List.map ~f:split_entry
  |> String.Map.of_alist_reduce ~f:(fun _latest latest -> latest)
;;

let find_process_value host name = Map.find (process_map host) name

let unsafe_prefixes =
  [ "DYLD_"; "LD_"; "PYTHON"; "RUBY"; "PERL"; "GIT_"; "BASH_"; "ZDOTDIR" ]
;;

let has_unsafe_prefix name =
  List.exists unsafe_prefixes ~f:(fun prefix -> String.is_prefix name ~prefix)
;;

let unsafe_names = String.Set.of_list [ "ENV"; "BASH_ENV"; "SHELLOPTS"; "CDPATH"; "IFS" ]
let is_safe_name name = (not (has_unsafe_prefix name)) && not (Set.mem unsafe_names name)

let initial_values host = function
  | S.None_ | Selected -> String.Map.empty
  | Safe | All_sanitized -> Map.filter_keys (process_map host) ~f:is_safe_name
  | Raw -> process_map host
;;

let require_value host name required =
  match find_process_value host name with
  | Some value -> Some value
  | None when not required -> None
  | None ->
    fail "shell.environment_missing" ("required environment source is missing: " ^ name)
;;

let path_value host source path =
  match Host.resolve_path host ~source path with
  | Ok path -> Eio.Path.native_exn path
  | Error error -> fail error.code error.message
;;

let update_path values position entry =
  let current = Map.find values "PATH" |> Option.value ~default:"" in
  let value =
    match position with
    | S.Prepend -> if String.is_empty current then entry else entry ^ ":" ^ current
    | Append_path -> if String.is_empty current then entry else current ^ ":" ^ entry
  in
  Map.set values ~key:"PATH" ~data:value
;;

let apply_operation host source (values, secrets) = function
  | S.Set_env { name; value } ->
    if String.mem value '\000'
    then fail "shell.environment_nul" ("environment value contains NUL: " ^ name);
    Map.set values ~key:name ~data:value, secrets
  | Pass_env { name; required; secret } ->
    (match require_value host name required with
     | None -> values, secrets
     | Some value ->
       Map.set values ~key:name ~data:value, if secret then value :: secrets else secrets)
  | Unset_env name -> Map.remove values name, secrets
  | Unset_prefix prefix ->
    Map.filter_keys values ~f:(fun name -> not (String.is_prefix name ~prefix)), secrets
  | Path { position; path } ->
    update_path values position (path_value host source path), secrets
  | Path_env { name; suffix; position; required } ->
    (match require_value host name required with
     | None -> values, secrets
     | Some base ->
       let entry = Option.value_map suffix ~default:base ~f:(Filename.concat base) in
       update_path values position entry, secrets)
;;

let inherit_value = function
  | S.Set value -> value
  | Inherit | Clear ->
    fail "shell.environment_unresolved" "environment inheritance is unresolved"
;;

let to_array values =
  Map.to_alist values
  |> List.map ~f:(fun (name, value) -> name ^ "=" ^ value)
  |> Array.of_list
;;

let create_exn host ~source specification =
  let values = initial_values host (inherit_value specification.S.inherit_) in
  let values, secrets =
    List.fold specification.operations ~init:(values, []) ~f:(apply_operation host source)
  in
  { values = to_array values; secrets = List.rev secrets }
;;

let create host ~source specification =
  try Ok (create_exn host ~source specification) with
  | Environment_error error -> Error error
;;

let load_file host source path optional strip =
  let path =
    match Host.resolve_path host ~source path with
    | Ok path -> path
    | Error error -> fail error.code error.message
  in
  match Eio.Path.kind ~follow:true path with
  | `Not_found when optional -> None
  | `Not_found -> fail "shell.secret_file_missing" "required secret file is missing"
  | _ ->
    let value = Eio.Path.load path in
    Some (if strip then String.strip value else value)
;;

let load_secret host source = function
  | S.From_env { name; optional } -> require_value host name (not optional)
  | From_file { path; optional; strip } -> load_file host source path optional strip
  | Literal value -> Some value
;;

let load_secrets_exn host ~source specification =
  List.filter_map specification.S.sources ~f:(load_secret host source)
;;

let load_secrets host ~source specification =
  try Ok (load_secrets_exn host ~source specification) with
  | Environment_error error -> Error error
  | exn ->
    Error
      { code = "shell.secret_load_failed"
      ; message = "failed to load configured secret: " ^ Exn.to_string exn
      }
;;
