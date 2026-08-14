open! Core
module A = Chatmd_attributes
module Ast = Chatmd_ast
module D = Chatmd_shell_spec.Diagnostic
module E = Chatmd_shell_spec.Shell_element
module S = Chatmd_shell_spec.Shell_spec
module Source_ref = Chatmd_shell_spec.Source_ref
module Tool = Chatmd_shell_spec.Shell_tool_spec

exception Parse_error of D.t

let raise_error diagnostic = raise_notrace (Parse_error diagnostic)
let fail source path code message = raise_error (D.error ~source ~path ~code message)

let tag_name = function
  | Ast.Shell_access -> "shell_access"
  | Ast.Shell_element element -> E.to_string element
  | Ast.Tool -> "tool"
  | tag -> Sexp.to_string (Ast.sexp_of_tag tag)
;;

let element_exn source path = function
  | Ast.Element (tag, attributes, children) -> tag, attributes, children
  | Ast.Text text ->
    fail source path "shell.unexpected_text" ("unexpected text: " ^ String.strip text)
;;

let element_children source path children =
  List.filter children ~f:(function
    | Ast.Text text when String.for_all text ~f:Char.is_whitespace -> false
    | Ast.Text _ -> fail source path "shell.unexpected_text" "unexpected text"
    | Ast.Element _ -> true)
;;

let attributes source path allowed values =
  match A.create ~source ~path ~allowed values with
  | Ok attributes -> attributes
  | Error diagnostic -> raise_error diagnostic
;;

let optional attributes name =
  match A.optional attributes name with
  | Ok value -> value
  | Error diagnostic -> raise_error diagnostic
;;

let required attributes name =
  match A.required attributes name with
  | Ok value -> value
  | Error diagnostic -> raise_error diagnostic
;;

let no_children source path children =
  if not (List.is_empty (element_children source path children))
  then fail source path "shell.unexpected_child" "element must be empty"
;;

let enum source path name value choices =
  match List.Assoc.find choices value ~equal:String.equal with
  | Some value -> value
  | None ->
    fail source (path @ [ name ]) "shell.invalid_attribute" ("invalid value: " ^ value)
;;

let bool source path name value =
  enum source path name value [ "true", true; "false", false ]
;;

let bool_setting source path attributes name =
  Option.value_map (optional attributes name) ~default:S.Inherit ~f:(fun value ->
    S.Set (bool source path name value))
;;

let bool_value source path attributes name ~default =
  Option.value_map (optional attributes name) ~default ~f:(bool source path name)
;;

let merge source path attributes =
  Option.value_map (optional attributes "merge") ~default:S.Append ~f:(fun value ->
    enum source path "merge" value [ "append", S.Append; "replace", S.Replace ])
;;

let path source path name value =
  match Chatmd_shell_spec.Path_expr.parse value with
  | Ok value -> value
  | Error message -> fail source (path @ [ name ]) "shell.invalid_path" message
;;

let path_option source path_ attributes name =
  Option.map (optional attributes name) ~f:(path source path_ name)
;;

let shell_element_exn source path node =
  let tag, attributes, children = element_exn source path node in
  match tag with
  | Ast.Shell_element element -> element, attributes, children
  | _ -> fail source path "shell.unknown_element" ("unexpected element: " ^ tag_name tag)
;;

let parse_capability_root source node : S.capability_root =
  let element, raw_attributes, children =
    shell_element_exn source [ "capabilities" ] node
  in
  let name = E.to_string element in
  let path_ = [ "capabilities"; name ] in
  let attributes =
    attributes source path_ [ "path"; "path_env"; "optional" ] raw_attributes
  in
  no_children source path_ children;
  match optional attributes "path", optional attributes "path_env" with
  | Some value, None -> S.Path (path source path_ "path" value)
  | None, Some name ->
    S.Path_env
      { name; optional = bool_value source path_ attributes "optional" ~default:false }
  | _ ->
    fail source path_ "shell.invalid_root" "exactly one of path or path_env is required"
;;

let capability_children source children =
  List.partition_map (element_children source [ "capabilities" ] children) ~f:(fun node ->
    let element, _, _ = shell_element_exn source [ "capabilities" ] node in
    match element with
    | E.Read -> First (parse_capability_root source node)
    | E.Write -> Second (parse_capability_root source node)
    | _ -> fail source [ "capabilities" ] "shell.unknown_element" (E.to_string element))
;;

let parse_capabilities source raw_attributes children =
  let path_ = [ "capabilities" ] in
  let allowed =
    [ "sandbox"
    ; "network"
    ; "child_processes"
    ; "arbitrary_code"
    ; "privilege_change"
    ; "merge"
    ]
  in
  let attributes = attributes source path_ allowed raw_attributes in
  let read, write = capability_children source children in
  let sandbox =
    Option.value_map (optional attributes "sandbox") ~default:S.Inherit ~f:(fun value ->
      S.Set
        (enum
           source
           path_
           "sandbox"
           value
           [ "required", S.Required
           ; "preferred", S.Preferred
           ; "direct_unsafe", S.Direct_unsafe
           ]))
  in
  { S.sandbox
  ; network = bool_setting source path_ attributes "network"
  ; child_processes = bool_setting source path_ attributes "child_processes"
  ; arbitrary_code = bool_setting source path_ attributes "arbitrary_code"
  ; privilege_change = bool_setting source path_ attributes "privilege_change"
  ; read
  ; write
  ; merge = merge source path_ attributes
  }
;;

let parse_executable source raw_attributes children =
  let path_ = [ "resolver"; "executable" ] in
  let allowed = [ "id"; "path"; "sha256"; "trusted"; "override" ] in
  let attributes = attributes source path_ allowed raw_attributes in
  no_children source path_ children;
  { S.id = required attributes "id"
  ; path = path source path_ "path" (required attributes "path")
  ; sha256 = optional attributes "sha256"
  ; trusted = bool_value source path_ attributes "trusted" ~default:false
  ; override = bool_value source path_ attributes "override" ~default:false
  }
;;

let parse_resolver_child source node =
  let element, raw_attributes, children = shell_element_exn source [ "resolver" ] node in
  let name = E.to_string element in
  let path_ = [ "resolver"; name ] in
  match element with
  | E.Search_path | E.Trusted_root ->
    let attributes = attributes source path_ [ "path" ] raw_attributes in
    no_children source path_ children;
    let value = path source path_ "path" (required attributes "path") in
    if E.equal element E.Search_path then `Search value else `Trusted value
  | E.Executable -> `Executable (parse_executable source raw_attributes children)
  | _ -> fail source path_ "shell.unknown_element" name
;;

let parse_resolver source raw_attributes children =
  let path_ = [ "resolver" ] in
  let attributes =
    attributes source path_ [ "allow_relative_search_path"; "merge" ] raw_attributes
  in
  let children =
    element_children source path_ children |> List.map ~f:(parse_resolver_child source)
  in
  { S.search_path =
      List.filter_map children ~f:(function
        | `Search value -> Some value
        | _ -> None)
  ; trusted_root =
      List.filter_map children ~f:(function
        | `Trusted value -> Some value
        | _ -> None)
  ; executables =
      List.filter_map children ~f:(function
        | `Executable value -> Some value
        | _ -> None)
  ; allow_relative_search_path =
      bool_setting source path_ attributes "allow_relative_search_path"
  ; merge = merge source path_ attributes
  }
;;

let validate_env_name source path_ name =
  if String.is_empty name || String.mem name '=' || String.mem name '\000'
  then fail source path_ "shell.invalid_env_name" "invalid environment name"
;;

let parse_env_path source raw_attributes children =
  let path_ = [ "environment"; "path" ] in
  let attributes = attributes source path_ [ "prepend"; "append" ] raw_attributes in
  no_children source path_ children;
  match optional attributes "prepend", optional attributes "append" with
  | Some value, None ->
    S.Path { position = S.Prepend; path = path source path_ "prepend" value }
  | None, Some value ->
    S.Path { position = S.Append_path; path = path source path_ "append" value }
  | _ ->
    fail
      source
      path_
      "shell.invalid_path_operation"
      "exactly one of prepend or append is required"
;;

let parse_path_env source raw_attributes children =
  let path_ = [ "environment"; "path_env" ] in
  let allowed = [ "name"; "suffix"; "position"; "required" ] in
  let attributes = attributes source path_ allowed raw_attributes in
  no_children source path_ children;
  let name = required attributes "name" in
  validate_env_name source path_ name;
  let position =
    enum
      source
      path_
      "position"
      (Option.value (optional attributes "position") ~default:"append")
      [ "prepend", S.Prepend; "append", S.Append_path ]
  in
  S.Path_env
    { name
    ; suffix = optional attributes "suffix"
    ; position
    ; required = bool_value source path_ attributes "required" ~default:false
    }
;;

let parse_environment_child source node =
  let element, raw_attributes, children =
    shell_element_exn source [ "environment" ] node
  in
  let path_ = [ "environment"; E.to_string element ] in
  let empty allowed = attributes source path_ allowed raw_attributes in
  match element with
  | E.Set ->
    let attributes = empty [ "name"; "value" ] in
    no_children source path_ children;
    let name = required attributes "name" in
    validate_env_name source path_ name;
    S.Set_env { name; value = required attributes "value" }
  | E.Pass ->
    let attributes = empty [ "name"; "required"; "secret" ] in
    no_children source path_ children;
    let name = required attributes "name" in
    validate_env_name source path_ name;
    S.Pass_env
      { name
      ; required = bool_value source path_ attributes "required" ~default:false
      ; secret = bool_value source path_ attributes "secret" ~default:false
      }
  | E.Unset ->
    let attributes = empty [ "name" ] in
    no_children source path_ children;
    let name = required attributes "name" in
    validate_env_name source path_ name;
    S.Unset_env name
  | E.Unset_prefix ->
    let attributes = empty [ "value" ] in
    no_children source path_ children;
    S.Unset_prefix (required attributes "value")
  | E.Path -> parse_env_path source raw_attributes children
  | E.Path_env -> parse_path_env source raw_attributes children
  | _ -> fail source path_ "shell.unknown_element" (E.to_string element)
;;

let parse_environment source raw_attributes children =
  let path_ = [ "environment" ] in
  let attributes = attributes source path_ [ "inherit"; "merge" ] raw_attributes in
  let inherit_ =
    Option.value_map (optional attributes "inherit") ~default:S.Inherit ~f:(fun value ->
      S.Set
        (enum
           source
           path_
           "inherit"
           value
           [ "none", S.None_
           ; "safe", S.Safe
           ; "selected", S.Selected
           ; "all_sanitized", S.All_sanitized
           ; "raw", S.Raw
           ]))
  in
  { S.inherit_
  ; operations =
      element_children source path_ children
      |> List.map ~f:(parse_environment_child source)
  ; merge = merge source path_ attributes
  }
;;

let duration_setting source path_ attributes name =
  match optional attributes name with
  | None -> S.Inherit
  | Some "none" -> S.Clear
  | Some value ->
    (match Chatmd_shell_spec.Duration.parse value with
     | Ok value -> S.Set (Some value)
     | Error message -> fail source (path_ @ [ name ]) "shell.invalid_duration" message)
;;

let bytes_setting source path_ attributes name =
  match optional attributes name with
  | None -> S.Inherit
  | Some value ->
    (match Chatmd_shell_spec.Duration.parse_bytes value with
     | Ok value -> S.Set value
     | Error message -> fail source (path_ @ [ name ]) "shell.invalid_bytes" message)
;;

let int_setting source path_ attributes name =
  match optional attributes name with
  | None -> S.Inherit
  | Some value ->
    (match Int.of_string_opt value with
     | Some value when value >= 0 -> S.Set value
     | _ ->
       fail
         source
         (path_ @ [ name ])
         "shell.invalid_integer"
         "expected a non-negative integer")
;;

let parse_limits source raw_attributes children =
  let path_ = [ "limits" ] in
  let allowed =
    [ "wall_time"
    ; "idle_time"
    ; "max_stdin"
    ; "stdout"
    ; "stderr"
    ; "total_output"
    ; "cpu_time"
    ; "memory"
    ; "file_size"
    ; "open_files"
    ]
  in
  let attributes = attributes source path_ allowed raw_attributes in
  no_children source path_ children;
  { S.wall_time = duration_setting source path_ attributes "wall_time"
  ; idle_time = duration_setting source path_ attributes "idle_time"
  ; max_stdin = bytes_setting source path_ attributes "max_stdin"
  ; stdout = bytes_setting source path_ attributes "stdout"
  ; stderr = bytes_setting source path_ attributes "stderr"
  ; total_output = bytes_setting source path_ attributes "total_output"
  ; cpu_time = duration_setting source path_ attributes "cpu_time"
  ; memory = bytes_setting source path_ attributes "memory"
  ; file_size = bytes_setting source path_ attributes "file_size"
  ; open_files = int_setting source path_ attributes "open_files"
  }
;;

let platform source path_ attributes =
  enum
    source
    path_
    "when"
    (Option.value (optional attributes "when") ~default:"any")
    [ "macos", S.Macos; "linux", S.Linux; "windows", S.Windows; "any", S.Any ]
;;

let parse_seatbelt source raw_attributes children =
  let path_ = [ "backends"; "seatbelt" ] in
  let allowed = [ "id"; "when"; "executable"; "allow_system_reads" ] in
  let attributes = attributes source path_ allowed raw_attributes in
  no_children source path_ children;
  S.Seatbelt
    { id = optional attributes "id"
    ; when_ = platform source path_ attributes
    ; executable = path_option source path_ attributes "executable"
    ; allow_system_reads =
        bool_value source path_ attributes "allow_system_reads" ~default:true
    }
;;

let parse_bubblewrap source raw_attributes children =
  let path_ = [ "backends"; "bubblewrap" ] in
  let allowed = [ "id"; "when"; "executable"; "private_tmp"; "proc"; "dev" ] in
  let attributes = attributes source path_ allowed raw_attributes in
  no_children source path_ children;
  S.Bubblewrap
    { id = optional attributes "id"
    ; when_ = platform source path_ attributes
    ; executable = path_option source path_ attributes "executable"
    ; private_tmp = bool_value source path_ attributes "private_tmp" ~default:true
    ; proc = bool_value source path_ attributes "proc" ~default:true
    ; dev = Option.value (optional attributes "dev") ~default:"minimal"
    }
;;

let parse_backend_atom source node =
  let element, raw_attributes, children = shell_element_exn source [ "backends" ] node in
  let path_ = [ "backends"; E.to_string element ] in
  no_children source path_ children;
  match element with
  | E.Arg ->
    let attributes = attributes source path_ [ "value" ] raw_attributes in
    S.Literal_atom (required attributes "value")
  | Cwd_value ->
    ignore (attributes source path_ [] raw_attributes : A.t);
    Cwd_atom
  | Target_executable ->
    ignore (attributes source path_ [] raw_attributes : A.t);
    Target_executable_atom
  | Command_argv ->
    ignore (attributes source path_ [] raw_attributes : A.t);
    Command_argv_atom
  | Read_roots | Write_roots ->
    let attributes = attributes source path_ [ "flag" ] raw_attributes in
    let repeated = S.{ flag = required attributes "flag" } in
    if E.equal element E.Read_roots
    then Read_roots_atom repeated
    else Write_roots_atom repeated
  | Network_flag ->
    let attributes = attributes source path_ [ "value" ] raw_attributes in
    Network_flag_atom (required attributes "value")
  | Resource_limit_args ->
    ignore (attributes source path_ [] raw_attributes : A.t);
    Resource_limit_args_atom
  | _ -> fail source path_ "shell.unknown_element" (E.to_string element)
;;

let parse_external_backend source raw_attributes children =
  let path_ = [ "backends"; "external_backend" ] in
  let allowed = [ "id"; "when"; "executable"; "sha256"; "confinement" ] in
  let attributes = attributes source path_ allowed raw_attributes in
  let confinement =
    enum
      source
      path_
      "confinement"
      (Option.value (optional attributes "confinement") ~default:"none")
      [ "verified", S.Verified_confinement
      ; "declared", S.Declared_confinement
      ; "none", S.No_confinement
      ]
  in
  S.External
    { id = optional attributes "id"
    ; when_ = platform source path_ attributes
    ; executable = path source path_ "executable" (required attributes "executable")
    ; sha256 = optional attributes "sha256"
    ; confinement
    ; atoms = element_children source path_ children |> List.map ~f:(parse_backend_atom source)
    }
;;

let parse_backend source node =
  let element, raw_attributes, children = shell_element_exn source [ "backends" ] node in
  match element with
  | E.Seatbelt -> parse_seatbelt source raw_attributes children
  | E.Bubblewrap -> parse_bubblewrap source raw_attributes children
  | E.Direct ->
    let path_ = [ "backends"; "direct" ] in
    let attributes = attributes source path_ [ "id"; "when" ] raw_attributes in
    no_children source path_ children;
    S.Direct { id = optional attributes "id"; when_ = platform source path_ attributes }
  | E.External_backend -> parse_external_backend source raw_attributes children
  | _ -> fail source [ "backends" ] "shell.unknown_element" (E.to_string element)
;;

let parse_backends source raw_attributes children =
  let path_ = [ "backends" ] in
  let allowed = [ "accept_declared_confinement"; "merge" ] in
  let attributes = attributes source path_ allowed raw_attributes in
  { S.values =
      element_children source path_ children |> List.map ~f:(parse_backend source)
  ; accept_declared_confinement =
      bool_value source path_ attributes "accept_declared_confinement" ~default:false
  ; merge = merge source path_ attributes
  }
;;

let parse_effect source path_ attributes =
  enum
    source
    path_
    "name"
    (required attributes "name")
    [ "read_path", S.Read_path
    ; "write_path", S.Write_path
    ; "network", S.Network
    ; "child_processes", S.Child_processes
    ; "arbitrary_code", S.Arbitrary_code
    ; "privilege_change", S.Privilege_change
    ; "unknown", S.Unknown
    ]
;;

let lifecycle source path_ attributes ~default =
  enum
    source
    path_
    "lifecycle"
    (Option.value (optional attributes "lifecycle") ~default)
    [ "invocation", S.Invocation; "session", S.Session; "runtime", S.Runtime ]
;;

let hook_failure source path_ attributes ~default =
  enum
    source
    path_
    "failure"
    (Option.value (optional attributes "failure") ~default)
    [ "deny", S.Deny_failure
    ; "error", S.Error_failure
    ; "no_match", S.No_match_failure
    ; "unknown", S.Unknown_failure
    ; "keep", S.Keep_failure
    ]
;;

let optional_duration source path_ attributes name =
  Option.map (optional attributes name) ~f:(fun value ->
    match Chatmd_shell_spec.Duration.parse value with
    | Ok value -> value
    | Error message -> fail source (path_ @ [ name ]) "shell.invalid_duration" message)
;;

let optional_bytes source path_ attributes name =
  Option.map (optional attributes name) ~f:(fun value ->
    match Chatmd_shell_spec.Duration.parse_bytes value with
    | Ok value -> value
    | Error message -> fail source (path_ @ [ name ]) "shell.invalid_bytes" message)
;;

let executable_hook source path_ attributes =
  let protocol =
    enum
      source
      path_
      "protocol"
      (Option.value (optional attributes "protocol") ~default:"shell-hook-json-v1")
      [ "shell-hook-json-v1", S.Shell_hook_json_v1 ]
  in
  S.
    { executable = path source path_ "executable" (required attributes "executable")
    ; sha256 = optional attributes "sha256"
    ; runtime = Source_ref.qualify source (required attributes "runtime")
    ; protocol
    ; timeout = optional_duration source path_ attributes "timeout"
    ; max_input = optional_bytes source path_ attributes "max_input"
    ; max_output = optional_bytes source path_ attributes "max_output"
    }
;;

let csv source path_ value =
  let values = String.split value ~on:',' |> List.map ~f:String.strip in
  if List.exists values ~f:String.is_empty
  then
    fail source path_ "shell.invalid_list" "comma-separated list contains an empty item";
  values
;;

let value_matcher source element raw_attributes children constructor =
  let name = E.to_string element in
  let path_ = [ "policy"; name ] in
  let attributes = attributes source path_ [ "value" ] raw_attributes in
  no_children source path_ children;
  constructor (required attributes "value")
;;

let rec parse_matcher source node =
  let element, raw_attributes, children = shell_element_exn source [ "policy" ] node in
  let empty name =
    let path_ = [ "policy"; name ] in
    ignore (attributes source path_ [] raw_attributes : A.t);
    no_children source path_ children
  in
  match element with
  | E.Any_command ->
    empty "any_command";
    S.Any_command
  | E.Program ->
    value_matcher source element raw_attributes children (fun value -> S.Program value)
  | E.Basename ->
    value_matcher source element raw_attributes children (fun value -> S.Basename value)
  | E.Resolved_path ->
    value_matcher source element raw_attributes children (fun value ->
      S.Resolved_path (path source [ "policy"; "resolved_path" ] "value" value))
  | E.Trusted_executable ->
    empty "trusted_executable";
    S.Trusted_executable
  | E.Program_regex ->
    value_matcher source element raw_attributes children (fun value ->
      S.Program_regex value)
  | E.Argument ->
    value_matcher source element raw_attributes children (fun value -> S.Argument value)
  | E.Argument_contains ->
    value_matcher source element raw_attributes children (fun value ->
      S.Argument_contains value)
  | E.Argv_prefix -> parse_argv_prefix source raw_attributes children
  | E.Effect -> parse_effect_matcher source raw_attributes children
  | E.No_unknown_effects ->
    empty "no_unknown_effects";
    S.No_unknown_effects
  | E.Raw_shell ->
    empty "raw_shell";
    S.Raw_shell_request
  | E.Chatml_match -> parse_chatml_matcher source raw_attributes children
  | E.All -> S.All (parse_matcher_list source "all" raw_attributes children)
  | E.Any -> S.Any (parse_matcher_list source "any" raw_attributes children)
  | E.Not -> S.Not (parse_single_matcher source raw_attributes children)
  | _ -> fail source [ "policy" ] "shell.unknown_matcher" (E.to_string element)

and parse_argv_prefix source raw_attributes children =
  let path_ = [ "policy"; "argv_prefix" ] in
  let attributes = attributes source path_ [ "values" ] raw_attributes in
  no_children source path_ children;
  S.Argv_prefix (csv source path_ (required attributes "values"))

and parse_effect_matcher source raw_attributes children =
  let path_ = [ "policy"; "effect" ] in
  let attributes = attributes source path_ [ "name"; "under" ] raw_attributes in
  no_children source path_ children;
  S.Effect
    { kind = parse_effect source path_ attributes
    ; under = path_option source path_ attributes "under"
    }

and parse_matcher_list source name raw_attributes children =
  let path_ = [ "policy"; name ] in
  ignore (attributes source path_ [] raw_attributes : A.t);
  let values =
    element_children source path_ children |> List.map ~f:(parse_matcher source)
  in
  if List.is_empty values
  then fail source path_ "shell.empty_matcher" "matcher requires children";
  values

and parse_single_matcher source raw_attributes children =
  let path_ = [ "policy"; "not" ] in
  ignore (attributes source path_ [] raw_attributes : A.t);
  match element_children source path_ children with
  | [ child ] -> parse_matcher source child
  | _ -> fail source path_ "shell.matcher_arity" "not requires exactly one matcher"

and parse_chatml_matcher source raw_attributes children =
  let path_ = [ "policy"; "chatml_match" ] in
  let attributes =
    attributes source path_ [ "script"; "function"; "failure" ] raw_attributes
  in
  no_children source path_ children;
  S.Chatml_match
    { script = Source_ref.qualify source (required attributes "script")
    ; function_ = Option.value (optional attributes "function") ~default:"match_command"
    ; failure =
        (match optional attributes "failure" with
         | None -> S.Conservative_failure
         | Some _ -> hook_failure source path_ attributes ~default:"no_match")
    }
;;

let parse_rule source node =
  let element, raw_attributes, children = shell_element_exn source [ "policy" ] node in
  if not (E.equal element E.Rule)
  then fail source [ "policy" ] "shell.unknown_element" (E.to_string element);
  let path_ = [ "policy"; "rule" ] in
  let attributes =
    attributes source path_ [ "id"; "action"; "override" ] raw_attributes
  in
  let matcher =
    match element_children source path_ children with
    | [ child ] -> parse_matcher source child
    | _ ->
      fail source path_ "shell.rule_matcher_arity" "rule requires exactly one matcher"
  in
  { S.id = required attributes "id"
  ; action =
      enum
        source
        path_
        "action"
        (required attributes "action")
        [ "allow", S.Allow; "ask", S.Ask; "deny", S.Deny ]
  ; matcher
  ; override = bool_value source path_ attributes "override" ~default:false
  }
;;

let parse_policy source raw_attributes children =
  let path_ = [ "policy" ] in
  let attributes = attributes source path_ [ "default"; "merge" ] raw_attributes in
  let default =
    Option.value_map (optional attributes "default") ~default:S.Inherit ~f:(fun value ->
      S.Set
        (enum
           source
           path_
           "default"
           value
           [ "allow", S.Allow; "ask", S.Ask; "deny", S.Deny ]))
  in
  { S.default
  ; rules = element_children source path_ children |> List.map ~f:(parse_rule source)
  ; merge = merge source path_ attributes
  }
;;

let approval_scopes source path_ attributes =
  Option.value_map (optional attributes "scopes") ~default:[] ~f:(fun value ->
    csv source (path_ @ [ "scopes" ]) value
    |> List.map ~f:(fun value ->
      enum
        source
        path_
        "scopes"
        value
        [ "once", S.Once
        ; "exact_session", S.Exact_session
        ; "prefix_session", S.Prefix_session
        ; "durable_exact", S.Durable_exact
        ]))
;;

let parse_approvals source raw_attributes children =
  let path_ = [ "approvals" ] in
  let allowed = [ "provider"; "unavailable"; "scopes"; "durable" ] in
  let attributes = attributes source path_ allowed raw_attributes in
  no_children source path_ children;
  { S.provider =
      Option.value_map
        (optional attributes "provider")
        ~default:S.Inherit
        ~f:(fun value ->
          S.Set (enum source path_ "provider" value [ "ui", S.Ui; "none", S.No_provider ]))
  ; unavailable =
      Option.value_map
        (optional attributes "unavailable")
        ~default:S.Inherit
        ~f:(fun value ->
          S.Set
            (enum
               source
               path_
               "unavailable"
               value
               [ "deny", S.Deny_unavailable; "error", S.Error_unavailable ]))
  ; scopes = approval_scopes source path_ attributes
  ; durable = bool_setting source path_ attributes "durable"
  }
;;

let parse_reviewer source node =
  let element, raw_attributes, children = shell_element_exn source [ "reviewers" ] node in
  if not (E.equal element E.Reviewer)
  then fail source [ "reviewers" ] "shell.unknown_element" (E.to_string element);
  let path_ = [ "reviewers"; "reviewer" ] in
  let allowed =
    [ "id"; "kind"; "script"; "agent"; "model"; "lifecycle"; "failure"
    ; "executable"; "sha256"; "runtime"; "protocol"; "timeout"; "max_input"
    ; "max_output"
    ]
  in
  let attributes = attributes source path_ allowed raw_attributes in
  no_children source path_ children;
  let id = required attributes "id" in
  match required attributes "kind" with
  | "ui" -> S.Ui_reviewer { id }
  | "chatml" ->
    S.Chatml_reviewer
      { id
      ; script = Source_ref.qualify source (required attributes "script")
      ; lifecycle = lifecycle source path_ attributes ~default:"session"
      ; failure = hook_failure source path_ attributes ~default:"deny"
      }
  | "model" ->
    S.Model_reviewer
      { id
      ; agent = required attributes "agent"
      ; model = optional attributes "model"
      ; failure = hook_failure source path_ attributes ~default:"deny"
      }
  | "executable" ->
    S.Executable_reviewer
      { id
      ; hook = executable_hook source path_ attributes
      ; failure = hook_failure source path_ attributes ~default:"deny"
      }
  | value -> fail source (path_ @ [ "kind" ]) "shell.invalid_attribute" value
;;

let parse_reviewers source raw_attributes children : S.reviewers =
  let path_ = [ "reviewers" ] in
  let attributes = attributes source path_ [ "strategy"; "merge" ] raw_attributes in
  let strategy =
    Option.value (optional attributes "strategy") ~default:"first_terminal"
  in
  if not (String.equal strategy "first_terminal")
  then
    fail source path_ "shell.invalid_attribute" "reviewer strategy must be first_terminal";
  { S.values =
      element_children source path_ children |> List.map ~f:(parse_reviewer source)
  ; merge = merge source path_ attributes
  }
;;

let parse_match_child source children =
  match element_children source [ "interceptors"; "interceptor" ] children with
  | [] -> None
  | [ node ] ->
    let element, raw_attributes, children =
      shell_element_exn source [ "interceptors"; "interceptor" ] node
    in
    if not (E.equal element E.Match)
    then fail source [ "interceptors" ] "shell.unknown_element" (E.to_string element);
    ignore (attributes source [ "interceptors"; "match" ] [] raw_attributes : A.t);
    (match element_children source [ "interceptors"; "match" ] children with
     | [ matcher ] -> Some (parse_matcher source matcher)
     | _ ->
       fail
         source
         [ "interceptors"; "match" ]
         "shell.matcher_arity"
         "match requires exactly one matcher")
  | _ ->
    fail
      source
      [ "interceptors"; "interceptor" ]
      "shell.matcher_arity"
      "interceptor accepts at most one match"
;;

let parse_interceptor source node =
  let element, raw_attributes, children =
    shell_element_exn source [ "interceptors" ] node
  in
  if not (E.equal element E.Interceptor)
  then fail source [ "interceptors" ] "shell.unknown_element" (E.to_string element);
  let path_ = [ "interceptors"; "interceptor" ] in
  let allowed =
    [ "id"; "phase"; "script"; "lifecycle"; "failure"; "executable"; "sha256"
    ; "runtime"; "protocol"; "timeout"; "max_input"; "max_output"
    ]
  in
  let attributes = attributes source path_ allowed raw_attributes in
  let phase =
    enum
      source
      path_
      "phase"
      (required attributes "phase")
      [ "before", S.Before; "after", S.After ]
  in
  let matcher = parse_match_child source children in
  if S.equal_interceptor_phase phase S.After && Option.is_some matcher
  then
    fail
      source
      path_
      "shell.after_interceptor_match"
      "after interceptors cannot declare match in version 1";
  let extension =
    match optional attributes "script", optional attributes "executable" with
    | Some script, None ->
      S.Chatml_extension
        { script = Source_ref.qualify source script
        ; lifecycle = lifecycle source path_ attributes ~default:"session"
        }
    | None, Some _ -> Executable_extension (executable_hook source path_ attributes)
    | _ ->
      fail source path_ "shell.invalid_extension" "exactly one of script or executable is required"
  in
  { S.id = required attributes "id"
  ; phase
  ; extension
  ; matcher
  ; failure = hook_failure source path_ attributes ~default:"deny"
  }
;;

let parse_interceptors source raw_attributes children : S.interceptors =
  let path_ = [ "interceptors" ] in
  let attributes = attributes source path_ [ "merge" ] raw_attributes in
  { S.values =
      element_children source path_ children |> List.map ~f:(parse_interceptor source)
  ; merge = merge source path_ attributes
  }
;;

let parse_analyzer source node =
  let element, raw_attributes, children =
    shell_element_exn source [ "effect_analysis" ] node
  in
  if not (E.equal element E.Analyzer)
  then fail source [ "effect_analysis" ] "shell.unknown_element" (E.to_string element);
  let path_ = [ "effect_analysis"; "analyzer" ] in
  let allowed =
    [ "id"; "kind"; "script"; "lifecycle"; "replace"; "failure"; "executable"
    ; "sha256"; "runtime"; "protocol"; "timeout"; "max_input"; "max_output"
    ]
  in
  let attributes = attributes source path_ allowed raw_attributes in
  no_children source path_ children;
  let kind = Option.value (optional attributes "kind") ~default:"chatml" in
  let extension =
    match kind with
    | "chatml" ->
      S.Chatml_extension
        { script = Source_ref.qualify source (required attributes "script")
        ; lifecycle = lifecycle source path_ attributes ~default:"invocation"
        }
    | "executable" -> Executable_extension (executable_hook source path_ attributes)
    | _ -> fail source (path_ @ [ "kind" ]) "shell.invalid_attribute" kind
  in
  { S.id = required attributes "id"
  ; extension
  ; replace = bool_value source path_ attributes "replace" ~default:false
  ; failure = hook_failure source path_ attributes ~default:"unknown"
  }
;;

let parse_effect_analysis source raw_attributes children =
  let path_ = [ "effect_analysis" ] in
  let attributes = attributes source path_ [ "merge" ] raw_attributes in
  { S.analyzers =
      element_children source path_ children |> List.map ~f:(parse_analyzer source)
  ; merge = merge source path_ attributes
  }
;;

let parse_secret source node =
  let element, raw_attributes, children = shell_element_exn source [ "secrets" ] node in
  let path_ = [ "secrets"; E.to_string element ] in
  match element with
  | E.From_env ->
    let attributes = attributes source path_ [ "name"; "optional" ] raw_attributes in
    no_children source path_ children;
    S.From_env
      { name = required attributes "name"
      ; optional = bool_value source path_ attributes "optional" ~default:false
      }
  | E.From_file ->
    let attributes =
      attributes source path_ [ "path"; "optional"; "strip" ] raw_attributes
    in
    no_children source path_ children;
    S.From_file
      { path = path source path_ "path" (required attributes "path")
      ; optional = bool_value source path_ attributes "optional" ~default:false
      ; strip = bool_value source path_ attributes "strip" ~default:true
      }
  | E.Literal ->
    let attributes = attributes source path_ [ "value" ] raw_attributes in
    no_children source path_ children;
    S.Literal (required attributes "value")
  | _ -> fail source path_ "shell.unknown_element" (E.to_string element)
;;

let parse_secrets source raw_attributes children =
  let path_ = [ "secrets" ] in
  let attributes = attributes source path_ [ "replacement"; "merge" ] raw_attributes in
  { S.replacement =
      Option.value_map
        (optional attributes "replacement")
        ~default:S.Inherit
        ~f:(fun value -> S.Set value)
  ; sources = element_children source path_ children |> List.map ~f:(parse_secret source)
  ; merge = merge source path_ attributes
  }
;;

let parse_audit source raw_attributes children =
  let path_ = [ "audit" ] in
  let allowed = [ "format"; "path"; "content"; "failure" ] in
  let audit_attributes = attributes source path_ allowed raw_attributes in
  let children = element_children source path_ children in
  let format =
    enum
      source
      path_
      "format"
      (required audit_attributes "format")
      [ "none", S.No_audit; "stderr", S.Stderr; "jsonl", S.Jsonl; "session", S.Session ]
  in
  let path = path_option source path_ audit_attributes "path" in
  if S.equal_audit_format format S.Jsonl && Option.is_none path
  then fail source path_ "shell.audit_path_required" "jsonl audit requires path";
  let filter =
    match children with
    | [] -> None
    | [ child ] ->
      let element, raw_attributes, children = shell_element_exn source path_ child in
      if not (E.equal element E.Filter)
      then fail source path_ "shell.unknown_element" (E.to_string element);
      let filter_path = [ "audit"; "filter" ] in
      let attributes =
        attributes source filter_path
          [ "script"; "lifecycle"; "executable"; "sha256"; "runtime"; "protocol"
          ; "timeout"; "max_input"; "max_output"
          ] raw_attributes
      in
      no_children source [ "audit"; "filter" ] children;
      (match optional attributes "script", optional attributes "executable" with
       | Some script, None ->
         Some
           (S.Chatml_extension
              { script = Source_ref.qualify source script
              ; lifecycle = lifecycle source filter_path attributes ~default:"session"
              })
       | None, Some _ -> Some (Executable_extension (executable_hook source filter_path attributes))
       | _ ->
         fail source filter_path "shell.invalid_extension" "exactly one of script or executable is required")
    | _ -> fail source path_ "shell.duplicate_section" "audit accepts at most one filter"
  in
  { S.format
  ; path
  ; content =
      enum
        source
        path_
        "content"
        (Option.value (optional audit_attributes "content") ~default:"redacted")
        [ "metadata", S.Metadata; "redacted", S.Redacted; "full", S.Full ]
  ; failure =
      enum
        source
        path_
        "failure"
        (Option.value (optional audit_attributes "failure") ~default:"continue")
        [ "continue", S.Continue; "deny_start", S.Deny_start; "terminate", S.Terminate ]
  ; filter
  }
;;

type sections =
  { capabilities : S.capabilities option
  ; resolver : S.resolver option
  ; environment : S.environment option
  ; limits : S.limits option
  ; backends : S.backends option
  ; policy : S.policy option
  ; approvals : S.approvals option
  ; reviewers : S.reviewers option
  ; interceptors : S.interceptors option
  ; effect_analysis : S.effect_analysis option
  ; secrets : S.secrets option
  ; audit : S.audit option
  }

let empty_sections =
  { capabilities = None
  ; resolver = None
  ; environment = None
  ; limits = None
  ; backends = None
  ; policy = None
  ; approvals = None
  ; reviewers = None
  ; interceptors = None
  ; effect_analysis = None
  ; secrets = None
  ; audit = None
  }
;;

let duplicate_section source name =
  fail
    source
    [ "shell_access"; name ]
    "shell.duplicate_section"
    ("duplicate section: " ^ name)
;;

let add_section source sections node =
  let element, raw_attributes, children =
    shell_element_exn source [ "shell_access" ] node
  in
  let duplicate value =
    if Option.is_some value then duplicate_section source (E.to_string element)
  in
  match element with
  | E.Capabilities ->
    duplicate sections.capabilities;
    { sections with
      capabilities = Some (parse_capabilities source raw_attributes children)
    }
  | E.Resolver ->
    duplicate sections.resolver;
    { sections with resolver = Some (parse_resolver source raw_attributes children) }
  | E.Environment ->
    duplicate sections.environment;
    { sections with
      environment = Some (parse_environment source raw_attributes children)
    }
  | E.Limits ->
    duplicate sections.limits;
    { sections with limits = Some (parse_limits source raw_attributes children) }
  | E.Backends ->
    duplicate sections.backends;
    { sections with backends = Some (parse_backends source raw_attributes children) }
  | E.Policy ->
    duplicate sections.policy;
    { sections with policy = Some (parse_policy source raw_attributes children) }
  | E.Approvals ->
    duplicate sections.approvals;
    { sections with approvals = Some (parse_approvals source raw_attributes children) }
  | E.Reviewers ->
    duplicate sections.reviewers;
    { sections with reviewers = Some (parse_reviewers source raw_attributes children) }
  | E.Interceptors ->
    duplicate sections.interceptors;
    { sections with
      interceptors = Some (parse_interceptors source raw_attributes children)
    }
  | E.Effect_analysis ->
    duplicate sections.effect_analysis;
    { sections with
      effect_analysis = Some (parse_effect_analysis source raw_attributes children)
    }
  | E.Secrets ->
    duplicate sections.secrets;
    { sections with secrets = Some (parse_secrets source raw_attributes children) }
  | E.Audit ->
    duplicate sections.audit;
    { sections with audit = Some (parse_audit source raw_attributes children) }
  | _ -> fail source [ "shell_access" ] "shell.unknown_element" (E.to_string element)
;;

let parse_runtime_exn source node =
  let tag, raw_attributes, children = element_exn source [ "shell_access" ] node in
  if not (Ast.tag_equal tag Ast.Shell_access)
  then fail source [ "shell_access" ] "shell.invalid_root" "expected shell_access";
  let path_ = [ "shell_access" ] in
  let attributes =
    attributes source path_ [ "id"; "extends"; "cwd"; "pipefail" ] raw_attributes
  in
  let id =
    let id = Source_ref.qualify source (required attributes "id") in
    match S.Runtime_id.of_string ~source id with
    | Ok id -> id
    | Error diagnostic -> raise_error diagnostic
  in
  let sections =
    element_children source path_ children
    |> List.fold ~init:empty_sections ~f:(add_section source)
  in
  { S.id
  ; extends = Option.map (optional attributes "extends") ~f:(Source_ref.qualify source)
  ; requested_profile = None
  ; resolved_profile = None
  ; cwd =
      Option.value_map (optional attributes "cwd") ~default:S.Inherit ~f:(fun value ->
        S.Set (path source path_ "cwd" value))
  ; pipefail = bool_setting source path_ attributes "pipefail"
  ; capabilities = sections.capabilities
  ; resolver = sections.resolver
  ; environment = sections.environment
  ; limits = sections.limits
  ; backends = sections.backends
  ; policy = sections.policy
  ; approvals = sections.approvals
  ; reviewers = sections.reviewers
  ; interceptors = sections.interceptors
  ; effect_analysis = sections.effect_analysis
  ; secrets = sections.secrets
  ; audit = sections.audit
  ; source
  }
;;

let tool_argument source node =
  let element, raw_attributes, children =
    shell_element_exn source [ "tool"; "command" ] node
  in
  let path_ = [ "tool"; "command"; E.to_string element ] in
  match element with
  | E.Arg ->
    let attributes = attributes source path_ [ "value" ] raw_attributes in
    no_children source path_ children;
    Tool.Literal (required attributes "value")
  | E.Secret_arg ->
    let attributes = attributes source path_ [ "env"; "prefix" ] raw_attributes in
    no_children source path_ children;
    Tool.Secret_env
      { name = required attributes "env"
      ; prefix = Option.value (optional attributes "prefix") ~default:""
      }
  | E.Path_arg ->
    let attributes = attributes source path_ [ "base"; "path" ] raw_attributes in
    no_children source path_ children;
    let path_value = required attributes "path" in
    let value =
      Option.value_map (optional attributes "base") ~default:path_value ~f:(fun base ->
        sprintf "${%s}/%s" base path_value)
    in
    Tool.Path (path source path_ "path" value)
  | _ -> fail source path_ "shell.unknown_element" (E.to_string element)
;;

let parse_command source raw_attributes children =
  let path_ = [ "tool"; "command" ] in
  let attributes =
    attributes source path_ [ "program"; "executable_ref" ] raw_attributes
  in
  let program = optional attributes "program" in
  let executable_ref = optional attributes "executable_ref" in
  if Option.is_some program && Option.is_some executable_ref
  then
    fail
      source
      path_
      "shell.tool_conflicting_command"
      "program and executable_ref conflict";
  if Option.is_none program && Option.is_none executable_ref
  then
    fail
      source
      path_
      "shell.tool_missing_command"
      "command requires program or executable_ref";
  Tool.Argv
    { program
    ; executable_ref
    ; arguments =
        element_children source path_ children |> List.map ~f:(tool_argument source)
    }
;;

let parse_arguments source raw_attributes children =
  let path_ = [ "tool"; "arguments" ] in
  let allowed = [ "mode"; "min_count"; "max_count"; "max_item_bytes" ] in
  let attributes = attributes source path_ allowed raw_attributes in
  no_children source path_ children;
  let integer name =
    Option.map (optional attributes name) ~f:(fun value ->
      match Int.of_string_opt value with
      | Some value when value >= 0 -> value
      | _ ->
        fail
          source
          (path_ @ [ name ])
          "shell.invalid_integer"
          "expected a non-negative integer")
  in
  { Tool.mode =
      enum
        source
        path_
        "mode"
        (required attributes "mode")
        [ "none", Tool.No_arguments
        ; "optional", Tool.Optional_arguments
        ; "required", Tool.Required_arguments
        ]
  ; min_count = integer "min_count"
  ; max_count = integer "max_count"
  ; max_item_bytes = integer "max_item_bytes"
  }
;;

let tool_children source children =
  List.fold
    (element_children source [ "tool" ] children)
    ~init:(None, None)
    ~f:(fun (command, arguments) node ->
      let element, raw_attributes, children = shell_element_exn source [ "tool" ] node in
      match element with
      | E.Command ->
        if Option.is_some command
        then fail source [ "tool" ] "shell.duplicate_section" "duplicate command"
        else Some (parse_command source raw_attributes children), arguments
      | E.Arguments ->
        if Option.is_some arguments
        then fail source [ "tool" ] "shell.duplicate_section" "duplicate arguments"
        else command, Some (parse_arguments source raw_attributes children)
      | _ -> fail source [ "tool" ] "shell.unknown_element" (E.to_string element))
;;

let parse_tool_exn source node =
  let tag, raw_attributes, children = element_exn source [ "tool" ] node in
  if not (Ast.tag_equal tag Ast.Tool)
  then fail source [ "tool" ] "shell.invalid_root" "expected tool";
  let allowed =
    [ "name"
    ; "type"
    ; "mode"
    ; "runtime"
    ; "description"
    ; "command"
    ; "executable_ref"
    ; "executable"
    ; "script"
    ; "interpreter"
    ; "arguments_before_script"
    ; "fixed_arguments"
    ; "verification"
    ; "max_source_bytes"
    ; "stdin"
    ; "rationale"
    ; "result"
    ; "stream"
    ; "nonzero"
    ]
  in
  let attributes = attributes source [ "tool" ] allowed raw_attributes in
  let command, arguments = tool_children source children in
  match
    Tool.parse_declaration
      ~source
      ~attributes:(A.to_values attributes)
      ~command
      ~arguments
  with
  | Ok tool -> { tool with runtime = Source_ref.qualify source tool.runtime }
  | Error (diagnostic :: _) -> raise_error diagnostic
  | Error [] -> fail source [ "tool" ] "shell.tool_invalid" "invalid shell tool"
;;

let result f =
  try Ok (f ()) with
  | Parse_error diagnostic -> Error [ diagnostic ]
;;

let parse_runtime ~source node = result (fun () -> parse_runtime_exn source node)
let parse_tool ~source node = result (fun () -> parse_tool_exn source node)
