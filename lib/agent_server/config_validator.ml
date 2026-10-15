open Core

type context =
  { env : Eio_unix.Stdenv.base
  ; source_file : string
  ; source_directory : string
  }

let diagnostic (context : context) ~code ~path ~message ~remediation =
  Config.Diagnostic.
    { code; config_path = path; source_file = context.source_file; message; remediation }
;;

let fail (context : context) ~code ~path ~message ~remediation =
  Error [ diagnostic context ~code ~path ~message ~remediation ]
;;

let shape_error (context : context) path expected =
  fail
    context
    ~code:"config.shape"
    ~path
    ~message:("expected " ^ expected)
    ~remediation:"Use the documented version 1 S-expression shape."
;;

let atom (context : context) path = function
  | Sexp.Atom value -> Ok value
  | _ -> shape_error context path "an atom or quoted string"
;;

let list (context : context) path = function
  | Sexp.List values -> Ok values
  | _ -> shape_error context path "a list"
;;

let record (context : context) path sexp =
  let open Result.Let_syntax in
  let%bind fields = list context path sexp in
  let parse_field = function
    | Sexp.List [ Sexp.Atom name; value ] -> Ok (name, value)
    | _ -> shape_error context path "record fields of the form (name value)"
  in
  let%bind fields = Result.all (List.map fields ~f:parse_field) in
  match
    List.find_a_dup fields ~compare:(fun (left, _) (right, _) ->
      String.compare left right)
  with
  | None -> Ok fields
  | Some (name, _) ->
    fail
      context
      ~code:"config.duplicate_field"
      ~path:(path ^ "." ^ name)
      ~message:("duplicate field " ^ name)
      ~remediation:"Remove the duplicate field."
;;

let field fields name = List.Assoc.find fields name ~equal:String.equal

let required context path fields name =
  match field fields name with
  | Some value -> Ok value
  | None ->
    fail
      context
      ~code:"config.missing_field"
      ~path:(path ^ "." ^ name)
      ~message:("missing required field " ^ name)
      ~remediation:("Add the " ^ name ^ " field.")
;;

let ensure_allowed context path fields allowed =
  match
    List.find fields ~f:(fun (name, _) -> not (List.mem allowed name ~equal:String.equal))
  with
  | None -> Ok ()
  | Some (name, _) ->
    fail
      context
      ~code:"config.unknown_field"
      ~path:(path ^ "." ^ name)
      ~message:("unknown field " ^ name)
      ~remediation:"Remove the field or update the configuration schema version."
;;

let parse_bool context path sexp =
  let open Result.Let_syntax in
  let%bind value = atom context path sexp in
  match value with
  | "true" -> Ok true
  | "false" -> Ok false
  | _ -> shape_error context path "true or false"
;;

let parse_int context path sexp =
  let open Result.Let_syntax in
  let%bind value = atom context path sexp in
  match Int.of_string value with
  | value -> Ok value
  | exception _ -> shape_error context path "an integer"
;;

let optional_value context path fields name parse ~default =
  match field fields name with
  | None -> Ok default
  | Some value -> parse context (path ^ "." ^ name) value
;;

let identifier context path sexp =
  let open Result.Let_syntax in
  let%bind value = atom context path sexp in
  let valid_character = function
    | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '_' | '-' | '.' -> true
    | _ -> false
  in
  if String.is_empty value || not (String.for_all value ~f:valid_character)
  then
    fail
      context
      ~code:"config.invalid_identifier"
      ~path
      ~message:("invalid identifier " ^ value)
      ~remediation:"Use letters, digits, '.', '_', or '-' in a nonempty identifier."
  else Ok value
;;

let sha256 context path sexp =
  let open Result.Let_syntax in
  let%bind value = atom context path sexp in
  let value = String.lowercase value in
  let is_hex = function
    | '0' .. '9' | 'a' .. 'f' -> true
    | _ -> false
  in
  if String.length value = 64 && String.for_all value ~f:is_hex
  then Ok value
  else
    fail
      context
      ~code:"config.invalid_sha256"
      ~path
      ~message:"SHA-256 digest must contain exactly 64 hexadecimal characters"
      ~remediation:"Pin the lowercase hexadecimal SHA-256 digest of the exact artifact."
;;

let principal_id context path sexp =
  let open Result.Let_syntax in
  let%bind value = atom context path sexp in
  match Agent_protocol.Id.Principal.of_string value with
  | Ok id -> Ok (Agent_protocol.Id.Principal.to_string id)
  | Error error ->
    fail
      context
      ~code:"config.invalid_principal_id"
      ~path
      ~message:error.message
      ~remediation:"Use a valid opaque principal ID beginning with pri_."
;;

let parse_list_values context path sexp parse =
  let open Result.Let_syntax in
  let%bind values = list context path sexp in
  Result.all (List.map values ~f:(parse context (path ^ "[]")))
;;

let require_nonempty context path values =
  if List.is_empty values
  then
    fail
      context
      ~code:"config.empty_list"
      ~path
      ~message:"list must contain at least one value"
      ~remediation:"Add at least one explicit value."
  else Ok values
;;

let require_unique context path values =
  match List.find_a_dup values ~compare:String.compare with
  | None -> Ok values
  | Some value ->
    fail
      context
      ~code:"config.duplicate_value"
      ~path
      ~message:("duplicate list value " ^ value)
      ~remediation:"Remove duplicate values from the list."
;;

let normalize_absolute path =
  let components = String.split path ~on:'/' in
  let normalized =
    List.fold components ~init:[] ~f:(fun stack component ->
      match component, stack with
      | ("" | "."), _ -> stack
      | "..", _ :: rest -> rest
      | "..", [] -> []
      | component, _ -> component :: stack)
    |> List.rev
  in
  "/" ^ String.concat ~sep:"/" normalized
;;

let expand_home value =
  match Sys.getenv "HOME" with
  | Some home when String.equal value "~" -> home
  | Some home when String.is_prefix value ~prefix:"~/" ->
    Filename.concat home (String.drop_prefix value 2)
  | _ -> value
;;

let resolve_path (context : context) path sexp =
  let open Result.Let_syntax in
  let%map value = atom context path sexp in
  let value = expand_home value in
  let absolute =
    if Filename.is_absolute value
    then value
    else Filename.concat context.source_directory value
  in
  normalize_absolute absolute
;;

let positive context path value =
  if value > 0
  then Ok value
  else
    fail
      context
      ~code:"config.range"
      ~path
      ~message:"value must be positive"
      ~remediation:"Set a value greater than zero."
;;

let http_header_name context path sexp =
  let open Result.Let_syntax in
  let%bind value = atom context path sexp in
  let valid = function
    | 'a' .. 'z'
    | 'A' .. 'Z'
    | '0' .. '9'
    | '!'
    | '#'
    | '$'
    | '%'
    | '&'
    | '\''
    | '*'
    | '+'
    | '-'
    | '.'
    | '^'
    | '_'
    | '`'
    | '|'
    | '~' -> true
    | _ -> false
  in
  if String.is_empty value || not (String.for_all value ~f:valid)
  then
    fail
      context
      ~code:"config.http_header"
      ~path
      ~message:"HTTP header name is invalid"
      ~remediation:"Use a nonempty RFC token header name."
  else Ok (String.lowercase value)
;;

let parse_reverse_proxy context path sexp =
  let open Result.Let_syntax in
  let%bind fields = record context path sexp in
  let%bind () =
    ensure_allowed
      context
      path
      fields
      [ "trusted_addresses"; "principal_header"; "scopes_header" ]
  in
  let%bind trusted_addresses =
    required context path fields "trusted_addresses"
    >>= fun value ->
    parse_list_values context (path ^ ".trusted_addresses") value atom
    >>= require_nonempty context (path ^ ".trusted_addresses")
    >>= require_unique context (path ^ ".trusted_addresses")
  in
  let%bind principal_header =
    optional_value
      context
      path
      fields
      "principal_header"
      http_header_name
      ~default:"x-ochat-principal-id"
  in
  let%map scopes_header =
    optional_value
      context
      path
      fields
      "scopes_header"
      http_header_name
      ~default:"x-ochat-scopes"
  in
  Config.Server.{ trusted_addresses; principal_header; scopes_header }
;;

let parse_http context path sexp =
  let open Result.Let_syntax in
  let%bind fields = record context path sexp in
  let%bind () =
    ensure_allowed
      context
      path
      fields
      [ "enabled"
      ; "address"
      ; "port"
      ; "require_auth"
      ; "static_tokens_file"
      ; "oauth_validator"
      ; "reverse_proxy"
      ; "max_connections"
      ; "idle_connection_timeout_ms"
      ]
  in
  let%bind enabled =
    optional_value context path fields "enabled" parse_bool ~default:false
  in
  let%bind address =
    optional_value context path fields "address" atom ~default:"127.0.0.1"
  in
  let%bind port = optional_value context path fields "port" parse_int ~default:8787 in
  let%bind require_auth =
    optional_value context path fields "require_auth" parse_bool ~default:true
  in
  let%bind static_tokens_file =
    match field fields "static_tokens_file" with
    | None -> Ok None
    | Some value ->
      Result.map
        (resolve_path context (path ^ ".static_tokens_file") value)
        ~f:Option.some
  in
  let%bind oauth_validator =
    match field fields "oauth_validator" with
    | None -> Ok None
    | Some value ->
      Result.map (identifier context (path ^ ".oauth_validator") value) ~f:Option.some
  in
  let%bind reverse_proxy =
    match field fields "reverse_proxy" with
    | None -> Ok None
    | Some value ->
      Result.map
        (parse_reverse_proxy context (path ^ ".reverse_proxy") value)
        ~f:Option.some
  in
  let%bind max_connections =
    optional_value context path fields "max_connections" parse_int ~default:1_024
    >>= positive context (path ^ ".max_connections")
  in
  let%bind idle_connection_timeout_ms =
    optional_value
      context
      path
      fields
      "idle_connection_timeout_ms"
      parse_int
      ~default:300_000
    >>= positive context (path ^ ".idle_connection_timeout_ms")
  in
  if port < 1 || port > 65535
  then
    fail
      context
      ~code:"config.port"
      ~path:(path ^ ".port")
      ~message:"HTTP port must be between 1 and 65535"
      ~remediation:"Choose a valid TCP port."
  else
    Ok
      Config.Server.
        { enabled
        ; address
        ; port
        ; require_auth
        ; static_tokens_file
        ; oauth_validator
        ; reverse_proxy
        ; max_connections
        ; idle_connection_timeout_ms
        }
;;

let parse_retention context path sexp =
  let open Result.Let_syntax in
  let%bind fields = record context path sexp in
  let%bind () =
    ensure_allowed
      context
      path
      fields
      [ "completed_stream_ms"; "response_artifact_ms"; "max_events_per_session" ]
  in
  let%bind completed_stream_ms =
    optional_value context path fields "completed_stream_ms" parse_int ~default:3_600_000
  in
  let%bind max_events_per_session =
    optional_value context path fields "max_events_per_session" parse_int ~default:100_000
  in
  let%bind response_artifact_ms =
    optional_value
      context
      path
      fields
      "response_artifact_ms"
      parse_int
      ~default:completed_stream_ms
  in
  let%bind completed_stream_ms =
    positive context (path ^ ".completed_stream_ms") completed_stream_ms
  in
  let%bind response_artifact_ms =
    positive context (path ^ ".response_artifact_ms") response_artifact_ms
  in
  let%map max_events_per_session =
    positive context (path ^ ".max_events_per_session") max_events_per_session
  in
  Config.Server.{ completed_stream_ms; response_artifact_ms; max_events_per_session }
;;

let parse_flush context path sexp =
  let open Result.Let_syntax in
  let%bind value = atom context path sexp in
  match value with
  | "each" -> Ok Config.Server.Each
  | "interval" -> Ok Interval
  | "unsafe_buffered" -> Ok Unsafe_buffered
  | _ -> shape_error context path "each, interval, or unsafe_buffered"
;;

let parse_durability context path sexp =
  let open Result.Let_syntax in
  let%bind fields = record context path sexp in
  let allowed =
    [ "journal_flush"; "journal_flush_ms"; "snapshot_every_events"; "snapshot_every_ms" ]
  in
  let%bind () = ensure_allowed context path fields allowed in
  let%bind journal_flush =
    optional_value
      context
      path
      fields
      "journal_flush"
      parse_flush
      ~default:Config.Server.Interval
  in
  let%bind journal_flush_ms =
    optional_value context path fields "journal_flush_ms" parse_int ~default:50
  in
  let%bind snapshot_every_events =
    optional_value context path fields "snapshot_every_events" parse_int ~default:100
  in
  let%bind snapshot_every_ms =
    optional_value context path fields "snapshot_every_ms" parse_int ~default:5_000
  in
  let%bind journal_flush_ms =
    positive context (path ^ ".journal_flush_ms") journal_flush_ms
  in
  let%bind snapshot_every_events =
    positive context (path ^ ".snapshot_every_events") snapshot_every_events
  in
  let%map snapshot_every_ms =
    positive context (path ^ ".snapshot_every_ms") snapshot_every_ms
  in
  Config.Server.
    { journal_flush; journal_flush_ms; snapshot_every_events; snapshot_every_ms }
;;

let parse_job_limits context path sexp =
  let open Result.Let_syntax in
  let%bind fields = record context path sexp in
  let allowed =
    [ "daemon_total"
    ; "per_principal"
    ; "per_prompt"
    ; "per_workspace"
    ; "per_session"
    ; "per_kind"
    ; "max_nested_depth"
    ]
  in
  let%bind () = ensure_allowed context path fields allowed in
  let parse_positive name default =
    let%bind value = optional_value context path fields name parse_int ~default in
    positive context (path ^ "." ^ name) value
  in
  let%bind daemon_total = parse_positive "daemon_total" 16 in
  let%bind per_principal = parse_positive "per_principal" 8 in
  let%bind per_prompt = parse_positive "per_prompt" 8 in
  let%bind per_workspace = parse_positive "per_workspace" 8 in
  let%bind per_session = parse_positive "per_session" 4 in
  let%bind per_kind = parse_positive "per_kind" 16 in
  let%bind max_nested_depth =
    optional_value context path fields "max_nested_depth" parse_int ~default:8
  in
  if max_nested_depth < 0
  then
    fail
      context
      ~code:"config.nonnegative"
      ~path:(path ^ ".max_nested_depth")
      ~message:"maximum nested job depth must be nonnegative"
      ~remediation:"Set max_nested_depth to zero or a positive integer."
  else
    Ok
      Config.Server.
        { daemon_total
        ; per_principal
        ; per_prompt
        ; per_workspace
        ; per_session
        ; per_kind
        ; max_nested_depth
        }
;;

let parse_authoring_packages context path sexp =
  let module F = Chat_response.Authoring_package_file in
  let open Result.Let_syntax in
  let invalid message =
    fail
      context
      ~code:"config.authoring_packages"
      ~path
      ~message
      ~remediation:"Use valid version-1 authoring package files, then restart the server."
  in
  let%bind values = list context path sexp in
  let%bind () =
    match List.length values <= 128 with
    | true -> Ok ()
    | false -> invalid "at most 128 authoring package files may be configured"
  in
  let%bind paths = List.map values ~f:(resolve_path context path) |> Result.all in
  let%bind paths = require_unique context path paths in
  F.load_many ~env:context.env ~paths
  |> Result.map_error ~f:(fun message ->
    [ diagnostic
        context
        ~code:"config.authoring_packages"
        ~path
        ~message
        ~remediation:"Fix the authoring package files and validate again."
    ])
;;

let parse_authoring_budget context path sexp =
  let module V = Chat_response.Authoring_validation in
  let open Result.Let_syntax in
  let%bind fields = record context path sexp in
  let%bind () =
    ensure_allowed
      context
      path
      fields
      [ "default_tokens"; "max_tokens"; "preload_tokens" ]
  in
  let%bind default_tokens =
    optional_value
      context
      path
      fields
      "default_tokens"
      parse_int
      ~default:V.default_context_budget.default_tokens
  in
  let%bind max_tokens =
    optional_value
      context
      path
      fields
      "max_tokens"
      parse_int
      ~default:V.default_context_budget.max_tokens
  in
  let%bind preload_tokens =
    optional_value
      context
      path
      fields
      "preload_tokens"
      parse_int
      ~default:V.default_context_budget.preload_tokens
  in
  V.context_budget ~default_tokens ~max_tokens ~preload_tokens
  |> Result.map_error ~f:(fun message ->
    [ diagnostic
        context
        ~code:"config.authoring_budget"
        ~path
        ~message
        ~remediation:
          "Use positive token estimates up to 1000000 and default_tokens <= max_tokens."
    ])
;;

let parse_session_helper context path sexp =
  let open Result.Let_syntax in
  let%bind fields = record context path sexp in
  let%bind () =
    ensure_allowed
      context
      path
      fields
      [ "tool_name"
      ; "executable"
      ; "executable_sha256"
      ; "arguments"
      ; "operations"
      ; "read_roots"
      ; "environment"
      ; "private_paths"
      ; "max_request_bytes"
      ; "max_response_bytes"
      ; "max_requests"
      ]
  in
  let required_value name parse =
    required context path fields name >>= parse context (path ^ "." ^ name)
  in
  let strings context path value = parse_list_values context path value atom in
  let paths context path value = parse_list_values context path value resolve_path in
  let%bind tool_name = required_value "tool_name" identifier in
  let%bind executable = required_value "executable" resolve_path in
  let%bind executable_sha256 = required_value "executable_sha256" sha256 in
  let%bind arguments =
    optional_value context path fields "arguments" strings ~default:[]
  in
  let%bind operations =
    required_value "operations" strings
    >>= require_nonempty context (path ^ ".operations")
    >>= require_unique context (path ^ ".operations")
  in
  let%bind () =
    match
      List.for_all operations ~f:(fun name ->
        Result.is_ok (Agent_session.Session_management.operation_of_json (`String name)))
    with
    | true -> Ok ()
    | false ->
      shape_error
        context
        (path ^ ".operations")
        "create, send, read, status, wait, stop, reference or validate"
  in
  let%bind read_roots =
    required_value "read_roots" paths >>= require_nonempty context (path ^ ".read_roots")
  in
  let%bind environment = required_value "environment" strings in
  let%bind () =
    let names = List.map environment ~f:(fun entry -> String.lsplit2 entry ~on:'=') in
    match
      List.for_all names ~f:(function
        | Some (name, _) -> not (String.is_empty name)
        | None -> false)
      && List.for_all environment ~f:(fun entry -> not (String.contains entry '\000'))
    with
    | false ->
      shape_error
        context
        (path ^ ".environment")
        "exact NAME=value strings without NUL bytes"
    | true ->
      List.filter_map names ~f:(Option.map ~f:fst)
      |> require_unique context (path ^ ".environment")
      |> Result.map ~f:(fun _ -> ())
  in
  let%bind private_paths =
    optional_value context path fields "private_paths" paths ~default:[]
  in
  let limits = Shell_access.Request_channel.default_limits in
  let limit name default =
    optional_value context path fields name parse_int ~default
    >>= positive context (path ^ "." ^ name)
  in
  let%bind max_request_bytes = limit "max_request_bytes" limits.max_request_bytes in
  let%bind max_response_bytes = limit "max_response_bytes" limits.max_response_bytes in
  let%map max_requests = limit "max_requests" limits.max_requests in
  Session_helper_policy.
    { tool_name
    ; executable
    ; executable_sha256
    ; arguments
    ; operations
    ; read_roots
    ; environment
    ; private_paths
    ; max_request_bytes
    ; max_response_bytes
    ; max_requests
    }
;;

let project_source_error context path =
  fail
    context
    ~code:"config.project_sources"
    ~path
    ~message:"Invalid or excessive administrative project source declaration"
    ~remediation:"Use bounded explicit source roots and exact qualified Git grants."
;;

let bounded_project_values context path sexp ~maximum =
  let open Result.Let_syntax in
  let%bind values = list context path sexp in
  let rec check remaining = function
    | [] -> Ok values
    | _ :: rest ->
      if remaining = 0
      then project_source_error context path
      else check (remaining - 1) rest
  in
  check maximum values
;;

let project_source_record context path sexp ~maximum =
  let open Result.Let_syntax in
  let%bind values = bounded_project_values context path sexp ~maximum in
  let%bind () =
    List.fold_result values ~init:() ~f:(fun () -> function
      | Sexp.List [ Sexp.Atom name; _ ] when String.length name <= 64 -> Ok ()
      | _ -> project_source_error context path)
  in
  record context path sexp
;;

let project_source_atom context path sexp =
  let open Result.Let_syntax in
  let%bind value = atom context path sexp in
  if String.length value > Project_source_configuration.maximum_path_bytes
  then project_source_error context path
  else Ok value
;;

let parse_project_repository context path = function
  | Sexp.List [ Sexp.Atom "local"; value ] ->
    project_source_atom context path value
    |> Result.map ~f:(fun path -> Project_source_configuration.Repository.Local path)
  | Sexp.List [ Sexp.Atom "https"; sexp ] ->
    let open Result.Let_syntax in
    let%bind fields = project_source_record context path sexp ~maximum:2 in
    let%bind () =
      ensure_allowed context path fields [ "repository"; "authorization_header_file" ]
    in
    let%bind repository =
      required context path fields "repository"
      >>= project_source_atom context (path ^ ".repository")
    in
    let%map authorization_header_file =
      match field fields "authorization_header_file" with
      | None | Some (Sexp.List []) -> Ok None
      | Some value ->
        project_source_atom context (path ^ ".authorization_header_file") value
        |> Result.map ~f:Option.some
    in
    Project_source_configuration.Repository.Https
      { repository; authorization_header_file }
  | _ -> shape_error context path "(local path) or (https ((repository url) ...))"
;;

let parse_project_git context path sexp =
  let open Result.Let_syntax in
  let%bind fields = project_source_record context path sexp ~maximum:4 in
  let%bind () =
    ensure_allowed
      context
      path
      fields
      [ "helper_directory"; "ca_bundle"; "memory_bytes_per_process"; "repositories" ]
  in
  let%bind helper_directory =
    required context path fields "helper_directory"
    >>= project_source_atom context (path ^ ".helper_directory")
  in
  let%bind ca_bundle =
    match field fields "ca_bundle" with
    | None | Some (Sexp.List []) -> Ok None
    | Some value ->
      project_source_atom context (path ^ ".ca_bundle") value |> Result.map ~f:Option.some
  in
  let%bind memory_sexp = required context path fields "memory_bytes_per_process" in
  let%bind memory_text = atom context (path ^ ".memory_bytes_per_process") memory_sexp in
  let%bind memory_bytes_per_process =
    if String.length memory_text > 32
    then project_source_error context path
    else parse_int context (path ^ ".memory_bytes_per_process") memory_sexp
  in
  let%map repositories =
    match field fields "repositories" with
    | None -> Ok []
    | Some values ->
      let%bind values =
        bounded_project_values
          context
          (path ^ ".repositories")
          values
          ~maximum:Project_source_configuration.maximum_git_grants
      in
      List.map values ~f:(parse_project_repository context (path ^ ".repositories[]"))
      |> Result.all
  in
  Project_source_configuration.Git_deployment.
    { helper_directory; ca_bundle; memory_bytes_per_process; repositories }
;;

let resolve_project_path context path value =
  let bound = Project_source_configuration.maximum_path_bytes in
  let selected_home =
    if String.equal value "~" || String.is_prefix value ~prefix:"~/"
    then Sys.getenv "HOME"
    else None
  in
  let home_bytes = Option.value_map selected_home ~default:0 ~f:String.length in
  if home_bytes > bound
  then project_source_error context path
  else (
    let expanded =
      match selected_home with
      | None -> String.length value
      | Some _ when String.equal value "~" -> home_bytes
      | Some _ -> home_bytes + 1 + String.length value
    in
    let absolute =
      match selected_home with
      | Some home -> Filename.is_absolute home
      | None -> Filename.is_absolute value
    in
    let base = if absolute then 0 else String.length context.source_directory in
    if
      expanded > bound
      || base > bound - expanded
      || ((not absolute) && base = bound - expanded)
    then project_source_error context path
    else resolve_path context path (Sexp.Atom value))
;;

let parse_project_sources context path sexp =
  match sexp with
  | Sexp.List [] -> Ok None
  | _ ->
    let module C = Project_source_configuration in
    let open Result.Let_syntax in
    let%bind fields = project_source_record context path sexp ~maximum:2 in
    let%bind () = ensure_allowed context path fields [ "external_roots"; "git" ] in
    let%bind external_roots =
      match field fields "external_roots" with
      | None -> Ok []
      | Some values ->
        let%bind values =
          bounded_project_values
            context
            (path ^ ".external_roots")
            values
            ~maximum:C.maximum_external_roots
        in
        List.map values ~f:(project_source_atom context (path ^ ".external_roots[]"))
        |> Result.all
    in
    let%bind git =
      match field fields "git" with
      | None | Some (Sexp.List []) -> Ok None
      | Some value ->
        parse_project_git context (path ^ ".git") value |> Result.map ~f:Option.some
    in
    let raw = C.{ external_roots; git } in
    let%bind () =
      C.validate_raw raw
      |> Result.map_error ~f:(fun _ ->
        [ diagnostic
            context
            ~code:"config.project_sources"
            ~path
            ~message:"Project source bounds are invalid"
            ~remediation:"Use bounded positive settings and exact administrative paths."
        ])
    in
    let resolve label value = resolve_project_path context (path ^ label) value in
    let%bind external_roots =
      List.map external_roots ~f:(resolve ".external_roots[]") |> Result.all
    in
    let%bind git =
      match git with
      | None -> Ok None
      | Some git ->
        let%bind helper_directory =
          resolve ".git.helper_directory" git.helper_directory
        in
        let%bind ca_bundle =
          match git.ca_bundle with
          | None -> Ok None
          | Some path -> resolve ".git.ca_bundle" path |> Result.map ~f:Option.some
        in
        let%map repositories =
          List.fold_result git.repositories ~init:[] ~f:(fun acc -> function
            | C.Repository.Local value ->
              let%map value = resolve ".git.repositories.local" value in
              C.Repository.Local value :: acc
            | Https { repository; authorization_header_file } ->
              let%map authorization_header_file =
                match authorization_header_file with
                | None -> Ok None
                | Some value ->
                  resolve ".git.repositories.authorization_header_file" value
                  |> Result.map ~f:Option.some
              in
              C.Repository.Https { repository; authorization_header_file } :: acc)
        in
        Some
          C.Git_deployment.
            { git with helper_directory; ca_bundle; repositories = List.rev repositories }
    in
    let configuration = C.{ external_roots; git } in
    let%map () =
      C.validate configuration
      |> Result.map_error ~f:(fun _ ->
        [ diagnostic
            context
            ~code:"config.project_sources"
            ~path
            ~message:"Project source grants are invalid"
            ~remediation:
              "Use distinct exact grants and a positive configured Git memory cap."
        ])
    in
    Some configuration
;;

let parse_authoring_project_access context path sexp ~project_sources =
  let module Access = Authoring_project_access_configuration in
  let open Result.Let_syntax in
  let invalid path =
    fail
      context
      ~code:"config.authoring_project_access"
      ~path
      ~message:"invalid authoring project access"
      ~remediation:
        "Use exact configured roots, authenticated principals and Standard targets."
  in
  let bounded_rows path sexp maximum =
    let%bind rows = list context path sexp in
    if List.length rows > maximum then invalid path else Ok rows
  in
  let parse_grant sexp =
    let path = path ^ ".read_grants[]" in
    let%bind fields = record context path sexp in
    let%bind () =
      ensure_allowed context path fields [ "root"; "principal_id"; "authentication_kind" ]
    in
    let%bind root =
      required context path fields "root"
      >>= atom context (path ^ ".root")
      >>= resolve_project_path context (path ^ ".root")
    in
    let%bind principal_id =
      required context path fields "principal_id"
      >>= atom context (path ^ ".principal_id")
      >>= fun id ->
      match Agent_protocol.Id.Principal.of_string id with
      | Ok principal_id -> Ok principal_id
      | Error _ -> invalid (path ^ ".principal_id")
    in
    let%map authentication_kind =
      required context path fields "authentication_kind"
      >>= atom context (path ^ ".authentication_kind")
    in
    Access.Read_grant.{ root; principal_id; authentication_kind }
  in
  let parse_request sexp =
    let request_path = path ^ ".selections[].request" in
    let%bind name, fields =
      match sexp with
      | Sexp.List [ Sexp.Atom name; fields ] -> Ok (name, fields)
      | _ -> shape_error context request_path "(library fields) or (entry fields)"
    in
    let%bind fields = record context request_path fields in
    let%bind () = ensure_allowed context request_path fields [ "name"; "environment" ] in
    let%bind environment =
      match field fields "environment" with
      | None -> Ok None
      | Some value ->
        atom context (request_path ^ ".environment") value |> Result.map ~f:Option.some
    in
    match name with
    | "library" ->
      if Option.is_some (field fields "name")
      then invalid request_path
      else Ok (Access.Selection.Library { environment })
    | "entry" ->
      let%map name =
        required context request_path fields "name"
        >>= atom context (request_path ^ ".name")
      in
      Access.Selection.Entry { name; environment }
    | _ -> invalid request_path
  in
  let parse_selection sexp =
    let row_path = path ^ ".selections[]" in
    let%bind fields = record context row_path sexp in
    let%bind () =
      ensure_allowed context row_path fields [ "root"; "project"; "request"; "target" ]
    in
    let%bind root =
      required context row_path fields "root"
      >>= atom context (row_path ^ ".root")
      >>= resolve_project_path context (row_path ^ ".root")
    in
    let%bind project =
      required context row_path fields "project" >>= atom context (row_path ^ ".project")
    in
    let%bind request = required context row_path fields "request" >>= parse_request in
    let%map target =
      let%bind spelling =
        required context row_path fields "target" >>= atom context (row_path ^ ".target")
      in
      match spelling with
      | "one_off_v1" -> Ok Chatml.Chatml_compiler_contract.Target.One_off_v1
      | "tool_v1" -> Ok Tool_v1
      | "developer_tool_v1" -> Ok Developer_tool_v1
      | "moderator_v1" -> Ok Moderator_v1
      | "delegated_moderator_v1" -> Ok Delegated_moderator_v1
      | _ -> invalid (row_path ^ ".target")
    in
    Access.Selection.{ root; project; request; target }
  in
  let%bind sources =
    match project_sources with
    | None -> invalid path
    | Some sources -> Ok sources
  in
  let%bind fields = record context path sexp in
  let%bind () = ensure_allowed context path fields [ "read_grants"; "selections" ] in
  let%bind grants =
    required context path fields "read_grants"
    >>= fun rows ->
    bounded_rows (path ^ ".read_grants") rows 256
    >>= fun rows -> List.map rows ~f:parse_grant |> Result.all
  in
  let%bind selections =
    required context path fields "selections"
    >>= fun rows ->
    bounded_rows (path ^ ".selections") rows 256
    >>= fun rows -> List.map rows ~f:parse_selection |> Result.all
  in
  let result = Access.{ read_grants = grants; selections } in
  let%map () =
    Access.validate result ~project_sources:sources
    |> Result.map_error ~f:(fun _ ->
      [ diagnostic
          context
          ~code:"config.authoring_project_access"
          ~path
          ~message:"authoring read grants or Project selections are invalid"
          ~remediation:
            "Use bounded distinct exact principal/root grants and Project targets."
      ])
  in
  result
;;

let parse_authoring_upload_access context path sexp =
  let module Access = Authoring_upload_access_configuration in
  let open Result.Let_syntax in
  let invalid path =
    fail
      context
      ~code:"config.authoring_upload_access"
      ~path
      ~message:"invalid authoring upload access"
      ~remediation:
        "Use bounded exact authenticated principals, independent operation grants and \
         Standard selections."
  in
  let bounded_rows path sexp maximum =
    let%bind rows = list context path sexp in
    if List.length rows > maximum then invalid path else Ok rows
  in
  let principal fields row_path =
    let%bind spelling =
      required context row_path fields "principal_id"
      >>= atom context (row_path ^ ".principal_id")
    in
    match Agent_protocol.Id.Principal.of_string spelling with
    | Ok principal_id -> Ok principal_id
    | Error _ -> invalid (row_path ^ ".principal_id")
  in
  let parse_grant sexp =
    let row_path = path ^ ".grants[]" in
    let%bind fields = record context row_path sexp in
    let%bind () =
      ensure_allowed
        context
        row_path
        fields
        [ "principal_id"; "authentication_kind"; "operation" ]
    in
    let%bind principal_id = principal fields row_path in
    let%bind authentication_kind =
      required context row_path fields "authentication_kind"
      >>= atom context (row_path ^ ".authentication_kind")
    in
    let%map operation =
      let%bind spelling =
        required context row_path fields "operation"
        >>= atom context (row_path ^ ".operation")
      in
      match spelling with
      | "upload_write" -> Ok Access.Grant.Upload_write
      | "publish" -> Ok Publish
      | "bundle_read" -> Ok Bundle_read
      | "rebind" -> Ok Rebind
      | _ -> invalid (row_path ^ ".operation")
    in
    Access.Grant.{ principal_id; authentication_kind; operation }
  in
  let parse_request row_path sexp =
    let request_path = row_path ^ ".request" in
    let%bind name, fields =
      match sexp with
      | Sexp.List [ Sexp.Atom name; fields ] -> Ok (name, fields)
      | _ -> shape_error context request_path "(library fields) or (entry fields)"
    in
    let%bind fields = record context request_path fields in
    let%bind () = ensure_allowed context request_path fields [ "name"; "environment" ] in
    let%bind environment =
      match field fields "environment" with
      | None -> Ok None
      | Some value ->
        atom context (request_path ^ ".environment") value |> Result.map ~f:Option.some
    in
    match name with
    | "library" ->
      if Option.is_some (field fields "name")
      then invalid request_path
      else Ok (Authoring_project_access_configuration.Selection.Library { environment })
    | "entry" ->
      let%map name =
        required context request_path fields "name"
        >>= atom context (request_path ^ ".name")
      in
      Authoring_project_access_configuration.Selection.Entry { name; environment }
    | _ -> invalid request_path
  in
  let parse_selection sexp =
    let row_path = path ^ ".selections[]" in
    let%bind fields = record context row_path sexp in
    let%bind () =
      ensure_allowed
        context
        row_path
        fields
        [ "principal_id"
        ; "authentication_kind"
        ; "request"
        ; "selected_environment"
        ; "target"
        ]
    in
    let%bind principal_id = principal fields row_path in
    let%bind authentication_kind =
      required context row_path fields "authentication_kind"
      >>= atom context (row_path ^ ".authentication_kind")
    in
    let%bind request =
      required context row_path fields "request" >>= parse_request row_path
    in
    let%bind selected_environment =
      required context row_path fields "selected_environment"
      >>= atom context (row_path ^ ".selected_environment")
    in
    let%map target =
      let%bind spelling =
        required context row_path fields "target" >>= atom context (row_path ^ ".target")
      in
      match spelling with
      | "one_off_v1" -> Ok Chatml.Chatml_compiler_contract.Target.One_off_v1
      | "tool_v1" -> Ok Tool_v1
      | "developer_tool_v1" -> Ok Developer_tool_v1
      | "moderator_v1" -> Ok Moderator_v1
      | "delegated_moderator_v1" -> Ok Delegated_moderator_v1
      | _ -> invalid (row_path ^ ".target")
    in
    Access.Selection.
      { principal_id; authentication_kind; request; selected_environment; target }
  in
  let%bind fields = record context path sexp in
  let%bind () = ensure_allowed context path fields [ "mode"; "grants"; "selections" ] in
  let%bind mode =
    let%bind spelling =
      required context path fields "mode" >>= atom context (path ^ ".mode")
    in
    match spelling with
    | "upload_only" -> Ok Access.Upload_only
    | "augment_core" -> Ok Augment_core
    | _ -> invalid (path ^ ".mode")
  in
  let%bind grants =
    required context path fields "grants"
    >>= fun rows ->
    bounded_rows (path ^ ".grants") rows 256
    >>= fun rows -> List.map rows ~f:parse_grant |> Result.all
  in
  let%bind selections =
    required context path fields "selections"
    >>= fun rows ->
    bounded_rows (path ^ ".selections") rows 256
    >>= fun rows -> List.map rows ~f:parse_selection |> Result.all
  in
  let result = Access.{ mode; grants; selections } in
  let%map () =
    Access.validate result
    |> Result.map_error ~f:(fun _ ->
      [ diagnostic
          context
          ~code:"config.authoring_upload_access"
          ~path
          ~message:"authoring upload grants or selections are invalid"
          ~remediation:
            "Use bounded distinct principal/operation grants and Standard targets."
      ])
  in
  result
;;

let parse_server context sexp =
  let path = "server" in
  let open Result.Let_syntax in
  let%bind fields = record context path sexp in
  let allowed =
    [ "data_dir"
    ; "project_sources"
    ; "authoring_project_access"
    ; "authoring_upload_access"
    ; "session_helpers"
    ; "authoring_packages"
    ; "authoring_budget"
    ; "unix_socket"
    ; "http"
    ; "shutdown_grace_ms"
    ; "max_attachments_per_session"
    ; "subscriber_queue_capacity"
    ; "event_retention"
    ; "durability"
    ; "job_limits"
    ; "unsafe_allow_unauthenticated_remote_http"
    ]
  in
  let%bind () = ensure_allowed context path fields allowed in
  let%bind data_dir =
    required context path fields "data_dir" >>= resolve_path context "server.data_dir"
  in
  let%bind project_sources =
    optional_value
      context
      path
      fields
      "project_sources"
      parse_project_sources
      ~default:None
  in
  let%bind authoring_project_access =
    match field fields "authoring_project_access" with
    | None -> Ok None
    | Some value ->
      parse_authoring_project_access
        context
        "server.authoring_project_access"
        value
        ~project_sources
      |> Result.map ~f:Option.some
  in
  let%bind authoring_upload_access =
    match field fields "authoring_upload_access" with
    | None -> Ok None
    | Some value ->
      parse_authoring_upload_access context "server.authoring_upload_access" value
      |> Result.map ~f:Option.some
  in
  let%bind session_helpers =
    optional_value
      context
      path
      fields
      "session_helpers"
      (fun context path value ->
         parse_list_values context path value parse_session_helper)
      ~default:[]
  in
  let%bind authoring_packages =
    optional_value
      context
      path
      fields
      "authoring_packages"
      parse_authoring_packages
      ~default:[]
  in
  let%bind authoring_budget =
    match field fields "authoring_budget" with
    | None -> Ok None
    | Some value ->
      parse_authoring_budget context "server.authoring_budget" value
      |> Result.map ~f:Option.some
  in
  let%bind unix_socket =
    required context path fields "unix_socket"
    >>= resolve_path context "server.unix_socket"
  in
  let%bind http =
    optional_value
      context
      path
      fields
      "http"
      parse_http
      ~default:
        Config.Server.
          { enabled = false
          ; address = "127.0.0.1"
          ; port = 8787
          ; require_auth = true
          ; static_tokens_file = None
          ; oauth_validator = None
          ; reverse_proxy = None
          ; max_connections = 1_024
          ; idle_connection_timeout_ms = 300_000
          }
  in
  let%bind shutdown_grace_ms =
    optional_value context path fields "shutdown_grace_ms" parse_int ~default:30_000
  in
  let%bind shutdown_grace_ms =
    positive context "server.shutdown_grace_ms" shutdown_grace_ms
  in
  let%bind max_attachments_per_session =
    optional_value
      context
      path
      fields
      "max_attachments_per_session"
      parse_int
      ~default:1_024
    >>= positive context "server.max_attachments_per_session"
  in
  let%bind subscriber_queue_capacity =
    optional_value context path fields "subscriber_queue_capacity" parse_int ~default:512
    >>= positive context "server.subscriber_queue_capacity"
  in
  let%bind event_retention =
    optional_value
      context
      path
      fields
      "event_retention"
      parse_retention
      ~default:
        Config.Server.
          { completed_stream_ms = 3_600_000
          ; response_artifact_ms = 3_600_000
          ; max_events_per_session = 100_000
          }
  in
  let%bind durability =
    optional_value
      context
      path
      fields
      "durability"
      parse_durability
      ~default:
        Config.Server.
          { journal_flush = Interval
          ; journal_flush_ms = 50
          ; snapshot_every_events = 100
          ; snapshot_every_ms = 5_000
          }
  in
  let%bind job_limits =
    optional_value
      context
      path
      fields
      "job_limits"
      parse_job_limits
      ~default:
        Config.Server.
          { daemon_total = 16
          ; per_principal = 8
          ; per_prompt = 8
          ; per_workspace = 8
          ; per_session = 4
          ; per_kind = 16
          ; max_nested_depth = 8
          }
  in
  let%bind unsafe_allow_unauthenticated_remote_http =
    optional_value
      context
      path
      fields
      "unsafe_allow_unauthenticated_remote_http"
      parse_bool
      ~default:false
  in
  let%map _ =
    Session_helper_policy.grants
      ~env:context.env
      ~protected_paths:
        ([ context.source_file; data_dir; unix_socket ]
         @ Option.to_list http.static_tokens_file)
      session_helpers
    |> Result.map_error ~f:(fun message ->
      [ diagnostic
          context
          ~code:"config.session_helpers"
          ~path:"server.session_helpers"
          ~message
          ~remediation:
            "Use distinct helper tools and dedicated public read directories outside \
             host state and credentials."
      ])
  in
  Config.Server.
    { data_dir
    ; project_sources
    ; authoring_project_access
    ; authoring_upload_access
    ; session_helpers
    ; authoring_packages
    ; authoring_budget
    ; unix_socket
    ; http
    ; shutdown_grace_ms
    ; max_attachments_per_session
    ; subscriber_queue_capacity
    ; event_retention
    ; durability
    ; job_limits
    ; unsafe_allow_unauthenticated_remote_http
    }
;;

let parse_temporary_location context path sexp =
  let open Result.Let_syntax in
  let%bind value = atom context path sexp in
  match value with
  | "system_tmp" -> Ok Config.Workspace.System_tmp
  | "session_dir" -> Ok Session_dir
  | _ -> shape_error context path "system_tmp or session_dir"
;;

let parse_cleanup context path sexp =
  let open Result.Let_syntax in
  let%bind value = atom context path sexp in
  match value with
  | "on_session_stop" -> Ok Config.Workspace.On_session_stop
  | "on_session_delete" -> Ok On_session_delete
  | "retain" -> Ok Retain
  | _ -> shape_error context path "on_session_stop, on_session_delete, or retain"
;;

let parse_workspace_source context path = function
  | Sexp.List [ Sexp.Atom "physical"; source ] ->
    Result.map
      (resolve_path context (path ^ ".physical") source)
      ~f:(fun path -> Config.Workspace.Physical path)
  | Sexp.List [ Sexp.Atom "temporary"; options ] ->
    let open Result.Let_syntax in
    let%bind fields = record context (path ^ ".temporary") options in
    let%bind () =
      ensure_allowed context path fields [ "location"; "cleanup"; "managed_root" ]
    in
    let%bind location =
      required context path fields "location"
      >>= parse_temporary_location context (path ^ ".temporary.location")
    in
    let%bind cleanup =
      optional_value
        context
        path
        fields
        "cleanup"
        parse_cleanup
        ~default:On_session_delete
    in
    let%map managed_root =
      match field fields "managed_root" with
      | None -> Ok None
      | Some value ->
        Result.map (resolve_path context (path ^ ".managed_root") value) ~f:Option.some
    in
    Config.Workspace.Temporary { location; cleanup; managed_root }
  | _ -> shape_error context path "(physical PATH) or (temporary (...))"
;;

let parse_workspace_access context path sexp =
  let open Result.Let_syntax in
  let%bind value = atom context path sexp in
  match value with
  | "read_only" -> Ok Config.Workspace.Read_only
  | "shared_write" -> Ok Shared_write
  | "exclusive" -> Ok Exclusive
  | _ -> shape_error context path "read_only, shared_write, or exclusive"
;;

let parse_overflow context path sexp =
  let open Result.Let_syntax in
  let%bind value = atom context path sexp in
  match value with
  | "reject" -> Ok Config.Workspace.Reject
  | "queue" -> Ok Queue
  | _ -> shape_error context path "reject or queue"
;;

let parse_prompt_limit context index sexp =
  let path = sprintf "workspaces[].prompt_limits[%d]" index in
  let open Result.Let_syntax in
  let%bind fields = record context path sexp in
  let%bind () =
    ensure_allowed context path fields [ "prompt"; "max_root_agents"; "overflow" ]
  in
  let%bind prompt =
    required context path fields "prompt" >>= identifier context (path ^ ".prompt")
  in
  let%bind max_root_agents =
    required context path fields "max_root_agents"
    >>= parse_int context (path ^ ".max_root_agents")
  in
  let%bind max_root_agents =
    positive context (path ^ ".max_root_agents") max_root_agents
  in
  let%map overflow =
    optional_value context path fields "overflow" parse_overflow ~default:Reject
  in
  Config.Workspace.{ prompt; max_root_agents; overflow }
;;

let parse_prompt_limits context path fields =
  match field fields "prompt_limits" with
  | None -> Ok []
  | Some value ->
    let open Result.Let_syntax in
    let%bind values = list context (path ^ ".prompt_limits") value in
    Result.all (List.mapi values ~f:(parse_prompt_limit context))
;;

let parse_workspace context index sexp =
  let path = sprintf "workspaces[%d]" index in
  let open Result.Let_syntax in
  let%bind fields = record context path sexp in
  let allowed = [ "id"; "source"; "access"; "conflict_domain"; "prompt_limits" ] in
  let%bind () = ensure_allowed context path fields allowed in
  let%bind id = required context path fields "id" >>= identifier context (path ^ ".id") in
  let%bind source =
    required context path fields "source"
    >>= parse_workspace_source context (path ^ ".source")
  in
  let%bind access =
    required context path fields "access"
    >>= parse_workspace_access context (path ^ ".access")
  in
  let%bind conflict_domain =
    match field fields "conflict_domain" with
    | None -> Ok None
    | Some value ->
      Result.map (atom context (path ^ ".conflict_domain") value) ~f:Option.some
  in
  let%map prompt_limits = parse_prompt_limits context path fields in
  Config.Workspace.{ id; source; access; conflict_domain; prompt_limits }
;;

let parse_prompt context index sexp =
  let path = sprintf "prompts[%d]" index in
  let open Result.Let_syntax in
  let%bind fields = record context path sexp in
  let allowed =
    [ "id"
    ; "path"
    ; "description"
    ; "allowed_workspaces"
    ; "permission_profile"
    ; "runtime_policy"
    ; "tool_contract"
    ; "enabled"
    ]
  in
  let%bind () = ensure_allowed context path fields allowed in
  let%bind id = required context path fields "id" >>= identifier context (path ^ ".id") in
  let%bind prompt_path =
    required context path fields "path" >>= resolve_path context (path ^ ".path")
  in
  let%bind description =
    match field fields "description" with
    | None -> Ok None
    | Some value -> Result.map (atom context (path ^ ".description") value) ~f:Option.some
  in
  let%bind allowed_workspaces =
    let%bind value = required context path fields "allowed_workspaces" in
    let%bind values = list context (path ^ ".allowed_workspaces") value in
    Result.all (List.map values ~f:(identifier context (path ^ ".allowed_workspaces[]")))
  in
  let%bind permission_profile =
    required context path fields "permission_profile"
    >>= identifier context (path ^ ".permission_profile")
  in
  let%bind runtime_policy =
    match field fields "runtime_policy" with
    | None -> Ok None
    | Some value ->
      Result.map (identifier context (path ^ ".runtime_policy") value) ~f:Option.some
  in
  let%bind tool_contract =
    match field fields "tool_contract" with
    | None -> Ok Chatml_compilation.Tool_contract.Standard
    | Some value ->
      let%bind value = atom context (path ^ ".tool_contract") value in
      (match value with
       | "standard" -> Ok Chatml_compilation.Tool_contract.Standard
       | "developer_model_spawn" ->
         Ok Chatml_compilation.Tool_contract.Developer_model_spawn
       | _ ->
         shape_error context (path ^ ".tool_contract") "standard or developer_model_spawn")
  in
  let%map enabled =
    optional_value context path fields "enabled" parse_bool ~default:true
  in
  Config.Prompt.
    { id
    ; path = prompt_path
    ; description
    ; allowed_workspaces
    ; permission_profile
    ; runtime_policy
    ; tool_contract
    ; enabled
    }
;;

let parse_tool_default context path sexp
  : (Config.Permission_profile.tool_default, Config.Diagnostic.t list) result
  =
  let open Result.Let_syntax in
  let%bind value = atom context path sexp in
  match value with
  | "ask" -> Ok Config.Permission_profile.Ask
  | "policy" -> Ok (Policy : Config.Permission_profile.tool_default)
  | "allow" -> Ok (Allow : Config.Permission_profile.tool_default)
  | "deny" -> Ok (Deny : Config.Permission_profile.tool_default)
  | _ -> shape_error context path "ask, policy, allow, or deny"
;;

let parse_fallback context path sexp
  : (Config.Permission_profile.approval_fallback, Config.Diagnostic.t list) result
  =
  match sexp with
  | Sexp.Atom "deny" ->
    Ok (Config.Permission_profile.Deny : Config.Permission_profile.approval_fallback)
  | Sexp.Atom "allow" -> Ok (Allow : Config.Permission_profile.approval_fallback)
  | Sexp.Atom "allow_if_policy" ->
    Ok (Allow_if_policy : Config.Permission_profile.approval_fallback)
  | Sexp.List [ Sexp.Atom "model_reviewer"; value ] ->
    Result.map
      (identifier context (path ^ ".model_reviewer") value)
      ~f:(fun id -> Config.Permission_profile.Model_reviewer id)
  | Sexp.List [ Sexp.Atom "external_reviewer"; value ] ->
    Result.map
      (identifier context (path ^ ".external_reviewer") value)
      ~f:(fun id -> Config.Permission_profile.External_reviewer id)
  | _ ->
    shape_error
      context
      path
      "deny, allow_if_policy, (model_reviewer ID), or (external_reviewer ID)"
;;

let parse_manifest_authorization context path sexp
  : (Config.Permission_profile.manifest_authorization, Config.Diagnostic.t list) result
  =
  let open Result.Let_syntax in
  let%bind value = atom context path sexp in
  match value with
  | "require_grant" -> Ok Config.Permission_profile.Require_grant
  | "assume_authorized" ->
    Ok (Assume_authorized : Config.Permission_profile.manifest_authorization)
  | "deny" -> Ok (Deny : Config.Permission_profile.manifest_authorization)
  | _ -> shape_error context path "require_grant, assume_authorized, or deny"
;;

let parse_timeout context path fields =
  match field fields "approval_timeout_ms", field fields "approval_timeout" with
  | Some _, Some _ ->
    fail
      context
      ~code:"config.conflicting_fields"
      ~path
      ~message:"approval_timeout and approval_timeout_ms are mutually exclusive"
      ~remediation:"Use only approval_timeout_ms, or approval_timeout none."
  | Some value, None ->
    Result.bind
      (parse_int context (path ^ ".approval_timeout_ms") value)
      ~f:(fun value ->
        if value < 0
        then shape_error context path "a nonnegative timeout"
        else Ok (Some value))
  | None, Some value ->
    Result.bind
      (atom context (path ^ ".approval_timeout") value)
      ~f:(function
        | "none" -> Ok None
        | _ -> shape_error context path "approval_timeout none")
  | None, None -> Ok None
;;

let parse_permission_profile context index sexp =
  let path = sprintf "permission_profiles[%d]" index in
  let open Result.Let_syntax in
  let%bind fields = record context path sexp in
  let allowed =
    [ "id"
    ; "tool_default"
    ; "approval_timeout"
    ; "approval_timeout_ms"
    ; "approval_fallback"
    ; "manifest_authorization"
    ]
  in
  let%bind () = ensure_allowed context path fields allowed in
  let%bind id = required context path fields "id" >>= identifier context (path ^ ".id") in
  let%bind tool_default =
    required context path fields "tool_default"
    >>= parse_tool_default context (path ^ ".tool_default")
  in
  let%bind approval_timeout_ms = parse_timeout context path fields in
  let%bind approval_fallback =
    optional_value
      context
      path
      fields
      "approval_fallback"
      parse_fallback
      ~default:
        (Config.Permission_profile.Deny : Config.Permission_profile.approval_fallback)
  in
  let%map manifest_authorization =
    required context path fields "manifest_authorization"
    >>= parse_manifest_authorization context (path ^ ".manifest_authorization")
  in
  Config.Permission_profile.
    { id; tool_default; approval_timeout_ms; approval_fallback; manifest_authorization }
;;

let parse_manifest_grant context index sexp =
  let path = sprintf "manifest_grants[%d]" index in
  let open Result.Let_syntax in
  let%bind fields = record context path sexp in
  let allowed =
    [ "id"; "prompt"; "workspaces"; "manifest_sha256"; "source_sha256"; "principals" ]
  in
  let%bind () = ensure_allowed context path fields allowed in
  let%bind id = required context path fields "id" >>= identifier context (path ^ ".id") in
  let%bind prompt =
    required context path fields "prompt" >>= identifier context (path ^ ".prompt")
  in
  let%bind workspaces =
    required context path fields "workspaces"
    >>= fun value -> parse_list_values context (path ^ ".workspaces") value identifier
  in
  let%bind workspaces = require_nonempty context (path ^ ".workspaces") workspaces in
  let%bind workspaces = require_unique context (path ^ ".workspaces") workspaces in
  let%bind manifest_sha256 =
    required context path fields "manifest_sha256"
    >>= sha256 context (path ^ ".manifest_sha256")
  in
  let%bind source_sha256 =
    required context path fields "source_sha256"
    >>= sha256 context (path ^ ".source_sha256")
  in
  let%bind principals =
    match field fields "principals" with
    | None -> Ok []
    | Some value -> parse_list_values context (path ^ ".principals") value principal_id
  in
  let%map principals = require_unique context (path ^ ".principals") principals in
  Config.Manifest_grant.
    { id; prompt; workspaces; manifest_sha256; source_sha256; principals }
;;

let top_fields context forms =
  let parse = function
    | Sexp.List [ Sexp.Atom name; value ] -> Ok (name, value)
    | _ -> shape_error context "$" "top-level forms of the shape (name value)"
  in
  Result.bind
    (Result.all (List.map forms ~f:parse))
    ~f:(fun fields ->
      match
        List.find_a_dup fields ~compare:(fun (left, _) (right, _) ->
          String.compare left right)
      with
      | None -> Ok fields
      | Some (name, _) ->
        fail
          context
          ~code:"config.duplicate_section"
          ~path:name
          ~message:("duplicate top-level section " ^ name)
          ~remediation:"Keep exactly one section with this name.")
;;

let parse_section_list context top name parse =
  let open Result.Let_syntax in
  let%bind value = required context "$" top name in
  let%bind values = list context name value in
  Result.all (List.mapi values ~f:(parse context))
;;

let parse_optional_section_list context top name parse =
  match field top name with
  | None -> Ok []
  | Some value ->
    let open Result.Let_syntax in
    let%bind values = list context name value in
    Result.all (List.mapi values ~f:(parse context))
;;

let duplicate_id values get_id =
  List.find_a_dup values ~compare:(fun left right ->
    String.compare (get_id left) (get_id right))
  |> Option.map ~f:get_id
;;

let validate_duplicates context workspaces prompts profiles grants =
  match
    ( duplicate_id workspaces (fun value -> value.Config.Workspace.id)
    , duplicate_id prompts (fun value -> value.Config.Prompt.id)
    , duplicate_id profiles (fun value -> value.Config.Permission_profile.id)
    , duplicate_id grants (fun value -> value.Config.Manifest_grant.id) )
  with
  | Some id, _, _, _ ->
    fail
      context
      ~code:"config.duplicate_id"
      ~path:"workspaces"
      ~message:("duplicate workspace ID " ^ id)
      ~remediation:"Use unique workspace IDs."
  | _, Some id, _, _ ->
    fail
      context
      ~code:"config.duplicate_id"
      ~path:"prompts"
      ~message:("duplicate prompt ID " ^ id)
      ~remediation:"Use unique prompt IDs."
  | _, _, Some id, _ ->
    fail
      context
      ~code:"config.duplicate_id"
      ~path:"permission_profiles"
      ~message:("duplicate permission profile ID " ^ id)
      ~remediation:"Use unique permission profile IDs."
  | _, _, _, Some id ->
    fail
      context
      ~code:"config.duplicate_id"
      ~path:"manifest_grants"
      ~message:("duplicate manifest grant ID " ^ id)
      ~remediation:"Use unique manifest grant IDs."
  | None, None, None, None -> Ok ()
;;

let validate_grant_references context workspaces prompts grants =
  let workspace_ids =
    String.Set.of_list (List.map workspaces ~f:(fun value -> value.Config.Workspace.id))
  in
  let prompt_by_id =
    String.Map.of_alist_exn
      (List.map prompts ~f:(fun value -> value.Config.Prompt.id, value))
  in
  let invalid =
    List.find_map grants ~f:(fun grant ->
      match Map.find prompt_by_id grant.Config.Manifest_grant.prompt with
      | None -> Some (grant.id, "missing prompt " ^ grant.prompt)
      | Some prompt ->
        List.find_map grant.workspaces ~f:(fun workspace ->
          if not (Set.mem workspace_ids workspace)
          then Some (grant.id, "missing workspace " ^ workspace)
          else if not (List.mem prompt.allowed_workspaces workspace ~equal:String.equal)
          then Some (grant.id, "workspace is not allowed by prompt " ^ workspace)
          else None))
  in
  match invalid with
  | None -> Ok ()
  | Some (grant, message) ->
    fail
      context
      ~code:"config.missing_reference"
      ~path:"manifest_grants"
      ~message:(sprintf "manifest grant %s references %s" grant message)
      ~remediation:"Reference an existing prompt and one of its allowed workspaces."
;;

let validate_references context workspaces prompts profiles grants =
  let workspace_ids =
    String.Set.of_list (List.map workspaces ~f:(fun value -> value.Config.Workspace.id))
  in
  let prompt_ids =
    String.Set.of_list (List.map prompts ~f:(fun value -> value.Config.Prompt.id))
  in
  let profile_ids =
    String.Set.of_list
      (List.map profiles ~f:(fun value -> value.Config.Permission_profile.id))
  in
  let missing_prompt_workspace =
    List.find_map prompts ~f:(fun prompt ->
      List.find prompt.allowed_workspaces ~f:(fun id -> not (Set.mem workspace_ids id))
      |> Option.map ~f:(fun id -> prompt.id, id))
  in
  let missing_profile =
    List.find prompts ~f:(fun prompt ->
      not (Set.mem profile_ids prompt.permission_profile))
  in
  let missing_limit_prompt =
    List.find_map workspaces ~f:(fun workspace ->
      List.find workspace.prompt_limits ~f:(fun limit ->
        not (Set.mem prompt_ids limit.prompt))
      |> Option.map ~f:(fun limit -> workspace.id, limit.prompt))
  in
  match missing_prompt_workspace, missing_profile, missing_limit_prompt with
  | Some (prompt, workspace), _, _ ->
    fail
      context
      ~code:"config.missing_reference"
      ~path:"prompts"
      ~message:(sprintf "prompt %s references missing workspace %s" prompt workspace)
      ~remediation:"Define the workspace or remove it from allowed_workspaces."
  | _, Some prompt, _ ->
    fail
      context
      ~code:"config.missing_reference"
      ~path:"prompts"
      ~message:
        (sprintf
           "prompt %s references missing permission profile %s"
           prompt.id
           prompt.permission_profile)
      ~remediation:"Define the permission profile or update the prompt."
  | _, _, Some (workspace, prompt) ->
    fail
      context
      ~code:"config.missing_reference"
      ~path:"workspaces"
      ~message:
        (sprintf "workspace %s limit references missing prompt %s" workspace prompt)
      ~remediation:"Define the prompt or remove the limit."
  | None, None, None -> validate_grant_references context workspaces prompts grants
;;

let validate_workspace_paths context workspaces =
  match
    List.find_map workspaces ~f:(fun workspace ->
      match workspace.Config.Workspace.source with
      | Physical path
        when not (Eio.Path.is_directory Eio.Path.(Eio.Stdenv.fs context.env / path)) ->
        Some (workspace.id, path)
      | Physical _ | Temporary _ -> None)
  with
  | None -> Ok ()
  | Some (id, path) ->
    fail
      context
      ~code:"config.workspace_unavailable"
      ~path:"workspaces"
      ~message:(sprintf "physical workspace %s is unavailable: %s" id path)
      ~remediation:"Create the directory or correct the configured path."
;;

let validate_prompt_file context prompt =
  let path = prompt.Config.Prompt.path in
  let file = Eio.Path.(Eio.Stdenv.fs context.env / path) in
  if not (Eio.Path.is_file file)
  then
    fail
      context
      ~code:"config.prompt_unavailable"
      ~path:"prompts"
      ~message:(sprintf "prompt %s is unavailable: %s" prompt.id path)
      ~remediation:"Create the prompt file or correct its path."
  else (
    try
      let directory = Eio.Path.(Eio.Stdenv.fs context.env / Filename.dirname path) in
      let max_input_bytes = 8 * 1024 * 1024 in
      let loader =
        Source_loader.confined_filesystem ~root:directory
        |> Source_loader.with_read_limit ~max_bytes:(fun _ -> max_input_bytes)
      in
      let source = Filename.basename path in
      let contents =
        Source_loader.root loader ~file:source
        |> Result.bind ~f:(Source_loader.read loader)
        |> Result.ok_or_failwith
      in
      let limits =
        Prompt.Chat_markdown.Limits.create
          ~max_input_bytes
          ~max_parse_calls:1024
          ~max_tokens:100_000
          ~max_depth:128
        |> Or_error.ok_exn
      in
      ignore
        (Prompt.Chat_markdown.prepare_chat_inputs
           ~source
           ~source_loader:loader
           ~dir:directory
           ~preprocess:Meta_prompting.Preprocessor.preprocess
           ~limits
           ~checkpoint:Eio.Fiber.check
           contents
         : Prompt.Chat_markdown.Parsed.t);
      Ok ()
    with
    | (Eio.Cancel.Cancelled _ | Eio.Time.Timeout) as exn -> raise exn
    | exn ->
      fail
        context
        ~code:"config.prompt_invalid"
        ~path:"prompts"
        ~message:(sprintf "prompt %s failed preflight: %s" prompt.id (Exn.to_string exn))
        ~remediation:
          "Fix ChatMD parsing, imports, scripts, or declarations before reloading.")
;;

let validate_http context server =
  let local =
    List.mem
      [ "127.0.0.1"; "::1"; "localhost" ]
      server.Config.Server.http.address
      ~equal:String.equal
  in
  if
    server.http.enabled
    && (not local)
    && (not server.http.require_auth)
    && not server.unsafe_allow_unauthenticated_remote_http
  then
    fail
      context
      ~code:"config.unsafe_listener"
      ~path:"server.http"
      ~message:"remote HTTP listener requires authentication"
      ~remediation:
        "Enable require_auth or explicitly enable the unsafe development override."
  else if server.http.enabled && server.http.require_auth
  then (
    match
      ( server.http.static_tokens_file
      , server.http.oauth_validator
      , server.http.reverse_proxy )
    with
    | None, None, None ->
      fail
        context
        ~code:"config.http_auth_missing"
        ~path:"server.http"
        ~message:"authenticated HTTP requires at least one authenticator"
        ~remediation:"Configure static_tokens_file, oauth_validator, or reverse_proxy."
    | Some path, _, _ ->
      Authenticator.validate_static_file ~env:context.env ~path
      |> Result.map_error ~f:(fun message ->
        [ Config.Diagnostic.
            { code = "config.http_auth_invalid"
            ; config_path = "server.http.static_tokens_file"
            ; source_file = context.source_file
            ; message
            ; remediation = "Fix the static bearer-token file and validate again."
            }
        ])
    | None, (Some _ | None), Some _ | None, Some _, None -> Ok ())
  else Ok ()
;;

let validate ~env raw =
  let context =
    { env
    ; source_file = raw.Config_parser.Raw_config.source_file
    ; source_directory = Filename.dirname raw.source_file
    }
  in
  let open Result.Let_syntax in
  let%bind top = top_fields context raw.forms in
  let%bind () =
    ensure_allowed
      context
      "$"
      top
      [ "version"
      ; "server"
      ; "workspaces"
      ; "prompts"
      ; "permission_profiles"
      ; "manifest_grants"
      ]
  in
  let%bind version = required context "$" top "version" >>= parse_int context "version" in
  if version <> Config.current_version
  then
    fail
      context
      ~code:"config.version"
      ~path:"version"
      ~message:(sprintf "unsupported configuration version %d" version)
      ~remediation:(sprintf "Use configuration version %d." Config.current_version)
  else (
    let%bind server = required context "$" top "server" >>= parse_server context in
    let%bind workspaces = parse_section_list context top "workspaces" parse_workspace in
    let%bind prompts = parse_section_list context top "prompts" parse_prompt in
    let%bind permission_profiles =
      parse_section_list context top "permission_profiles" parse_permission_profile
    in
    let%bind manifest_grants =
      parse_optional_section_list context top "manifest_grants" parse_manifest_grant
    in
    let%bind () =
      validate_duplicates context workspaces prompts permission_profiles manifest_grants
    in
    let%bind () =
      validate_references context workspaces prompts permission_profiles manifest_grants
    in
    let%bind () = validate_workspace_paths context workspaces in
    let%bind () = Result.all_unit (List.map prompts ~f:(validate_prompt_file context)) in
    let%map () = validate_http context server in
    Config.
      { version
      ; source_file = raw.source_file
      ; server
      ; workspaces
      ; prompts
      ; permission_profiles
      ; manifest_grants
      })
;;
