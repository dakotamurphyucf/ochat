open! Core
module E = Chatmd_shell_spec.Shell_element
module S = Chatmd_shell_spec.Shell_spec
module Tool = Chatmd_shell_spec.Shell_tool_spec

module Enum = struct
  let bool = [ "true", true; "false", false ]
  let merge = [ "append", S.Append; "replace", S.Replace ]

  let sandbox =
    [ "required", S.Required; "preferred", S.Preferred; "direct_unsafe", S.Direct_unsafe ]
  ;;

  let path_position = [ "prepend", S.Prepend; "append", S.Append_path ]

  let environment_inherit =
    [ "none", S.None_
    ; "safe", S.Safe
    ; "selected", S.Selected
    ; "all_sanitized", S.All_sanitized
    ; "raw", S.Raw
    ]
  ;;

  let platform =
    [ "macos", S.Macos; "linux", S.Linux; "windows", S.Windows; "any", S.Any ]
  ;;

  let confinement =
    [ "verified", S.Verified_confinement
    ; "declared", S.Declared_confinement
    ; "none", S.No_confinement
    ]
  ;;

  let process_effect =
    [ "read_path", S.Read_path
    ; "write_path", S.Write_path
    ; "network", S.Network
    ; "child_processes", S.Child_processes
    ; "arbitrary_code", S.Arbitrary_code
    ; "privilege_change", S.Privilege_change
    ; "unknown", S.Unknown
    ]
  ;;

  let lifecycle =
    [ "invocation", S.Invocation; "session", S.Session; "runtime", S.Runtime ]
  ;;

  let hook_failure =
    [ "deny", S.Deny_failure
    ; "error", S.Error_failure
    ; "no_match", S.No_match_failure
    ; "unknown", S.Unknown_failure
    ; "keep", S.Keep_failure
    ]
  ;;

  let hook_protocol = [ "shell-hook-json-v1", S.Shell_hook_json_v1 ]
  let policy_action = [ "allow", S.Allow; "ask", S.Ask; "deny", S.Deny ]

  let approval_scope =
    [ "once", S.Once
    ; "exact_session", S.Exact_session
    ; "prefix_session", S.Prefix_session
    ; "durable_exact", S.Durable_exact
    ]
  ;;

  let approval_provider = [ "ui", S.Ui; "none", S.No_provider ]
  let approval_unavailable = [ "deny", S.Deny_unavailable; "error", S.Error_unavailable ]
  let interceptor_phase = [ "before", S.Before; "after", S.After ]

  let audit_format =
    [ "none", S.No_audit; "stderr", S.Stderr; "jsonl", S.Jsonl; "session", S.Session ]
  ;;

  let audit_content = [ "metadata", S.Metadata; "redacted", S.Redacted; "full", S.Full ]

  let audit_failure =
    [ "continue", S.Continue; "deny_start", S.Deny_start; "terminate", S.Terminate ]
  ;;

  let tool_arguments_mode =
    [ "none", Tool.No_arguments
    ; "optional", Tool.Optional_arguments
    ; "required", Tool.Required_arguments
    ]
  ;;
end

module Value_choices = struct
  type t =
    | Closed of string list
    | Comma_separated_tokens of string list
    | Freeform
  [@@deriving sexp_of]
end

module Reviewer_kind = struct
  type t =
    | Ui
    | Chatml
    | Model
    | Executable

  let spellings = [ "ui", Ui; "chatml", Chatml; "model", Model; "executable", Executable ]
  let names = List.map spellings ~f:fst
  let of_string value = List.Assoc.find spellings value ~equal:String.equal
end

module Analyzer_kind = struct
  type t =
    | Chatml
    | Executable

  let spellings = [ "chatml", Chatml; "executable", Executable ]
  let names = List.map spellings ~f:fst
  let of_string value = List.Assoc.find spellings value ~equal:String.equal
end

let reviewer_strategy = "first_terminal"

module Root = struct
  type t =
    | Shell_access
    | Tool_shell
    | Tool_read_file
  [@@deriving compare, equal, sexp_of]
end

module Context = struct
  type t =
    { root : Root.t
    ; ancestors : E.t list
    ; element : E.t
    }
  [@@deriving sexp_of]

  let root t = t.root
  let ancestors t = t.ancestors
  let element t = t.element

  let of_scope scope ~element ~checkpoint =
    match Chatmd_tag_scope.nearest_structured_root scope ~checkpoint with
    | None -> None
    | Some (hint, tags) ->
      let root =
        match hint with
        | Chatmd_tag_scope.Root_hint.Shell_access -> Some Root.Shell_access
        | Tool { extension = true; _ } -> None
        | Tool { shell = true; _ } -> Some Tool_shell
        | Tool { read_file = true; _ } -> Some Tool_read_file
        | Tool { shell = false; read_file = false; extension = false } -> None
      in
      Option.bind root ~f:(fun root ->
        let rec shell_tags tags acc =
          match tags with
          | [] -> Some (List.rev acc)
          | Chatmd_ast.Shell_element tag :: rest ->
            checkpoint ();
            shell_tags rest (tag :: acc)
          | _ :: _ -> None
        in
        Option.map (shell_tags tags []) ~f:(fun ancestors -> { root; ancestors; element }))
  ;;
end

type entry =
  { root : Root.t
  ; path : string list
  ; names : string list
  }

let shell path names = { root = Root.Shell_access; path; names }
let tool path names = { root = Root.Tool_shell; path; names }
let read_file path names = { root = Root.Tool_read_file; path; names }

(* These are the exact existing allowlists of the strict declaration parsers.
   Their call sites consume this table. Diagnostic paths are parser context
   keys; authoring Context keeps typed actual ancestors and normalizes below. *)
let entries =
  [ shell [ "shell_access" ] [ "id"; "extends"; "cwd"; "pipefail" ]
  ; shell
      [ "capabilities" ]
      [ "sandbox"
      ; "network"
      ; "child_processes"
      ; "arbitrary_code"
      ; "privilege_change"
      ; "merge"
      ]
  ; shell [ "capabilities"; "read" ] [ "path"; "path_env"; "optional" ]
  ; shell [ "capabilities"; "write" ] [ "path"; "path_env"; "optional" ]
  ; shell [ "resolver" ] [ "allow_relative_search_path"; "merge" ]
  ; shell [ "resolver"; "search_path" ] [ "path" ]
  ; shell [ "resolver"; "trusted_root" ] [ "path" ]
  ; shell [ "resolver"; "executable" ] [ "id"; "path"; "sha256"; "trusted"; "override" ]
  ; shell [ "environment" ] [ "inherit"; "merge" ]
  ; shell [ "environment"; "set" ] [ "name"; "value" ]
  ; shell [ "environment"; "pass" ] [ "name"; "required"; "secret" ]
  ; shell [ "environment"; "unset" ] [ "name" ]
  ; shell [ "environment"; "unset_prefix" ] [ "value" ]
  ; shell [ "environment"; "path" ] [ "prepend"; "append" ]
  ; shell [ "environment"; "path_env" ] [ "name"; "suffix"; "position"; "required" ]
  ; shell
      [ "limits" ]
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
  ; shell [ "backends" ] [ "accept_declared_confinement"; "merge" ]
  ; shell [ "backends"; "seatbelt" ] [ "id"; "when"; "executable"; "allow_system_reads" ]
  ; shell
      [ "backends"; "bubblewrap" ]
      [ "id"; "when"; "executable"; "private_tmp"; "proc"; "dev" ]
  ; shell [ "backends"; "direct" ] [ "id"; "when" ]
  ; shell
      [ "backends"; "external_backend" ]
      [ "id"; "when"; "executable"; "sha256"; "confinement" ]
  ; shell [ "backends"; "arg" ] [ "value" ]
  ; shell [ "backends"; "cwd_value" ] []
  ; shell [ "backends"; "target_executable" ] []
  ; shell [ "backends"; "command_argv" ] []
  ; shell [ "backends"; "read_roots" ] [ "flag" ]
  ; shell [ "backends"; "write_roots" ] [ "flag" ]
  ; shell [ "backends"; "network_flag" ] [ "value" ]
  ; shell [ "backends"; "resource_limit_args" ] []
  ; shell [ "policy" ] [ "default"; "merge" ]
  ; shell [ "policy"; "rule" ] [ "id"; "action"; "override" ]
  ; shell [ "policy"; "any_command" ] []
  ; shell [ "policy"; "program" ] [ "value" ]
  ; shell [ "policy"; "basename" ] [ "value" ]
  ; shell [ "policy"; "resolved_path" ] [ "value" ]
  ; shell [ "policy"; "trusted_executable" ] []
  ; shell [ "policy"; "program_regex" ] [ "value" ]
  ; shell [ "policy"; "argument" ] [ "value" ]
  ; shell [ "policy"; "argument_contains" ] [ "value" ]
  ; shell [ "policy"; "argv_prefix" ] [ "values" ]
  ; shell [ "policy"; "effect" ] [ "name"; "under" ]
  ; shell [ "policy"; "no_unknown_effects" ] []
  ; shell [ "policy"; "raw_shell" ] []
  ; shell [ "policy"; "chatml_match" ] [ "script"; "function"; "failure" ]
  ; shell [ "policy"; "all" ] []
  ; shell [ "policy"; "any" ] []
  ; shell [ "policy"; "not" ] []
  ; shell [ "approvals" ] [ "provider"; "unavailable"; "scopes"; "durable" ]
  ; shell [ "reviewers" ] [ "strategy"; "merge" ]
  ; shell
      [ "reviewers"; "reviewer" ]
      [ "id"
      ; "kind"
      ; "script"
      ; "agent"
      ; "model"
      ; "lifecycle"
      ; "failure"
      ; "executable"
      ; "sha256"
      ; "runtime"
      ; "protocol"
      ; "timeout"
      ; "max_input"
      ; "max_output"
      ]
  ; shell [ "interceptors" ] [ "merge" ]
  ; shell
      [ "interceptors"; "interceptor" ]
      [ "id"
      ; "phase"
      ; "script"
      ; "lifecycle"
      ; "failure"
      ; "executable"
      ; "sha256"
      ; "runtime"
      ; "protocol"
      ; "timeout"
      ; "max_input"
      ; "max_output"
      ]
  ; shell [ "interceptors"; "match" ] []
  ; shell [ "effect_analysis" ] [ "merge" ]
  ; shell
      [ "effect_analysis"; "analyzer" ]
      [ "id"
      ; "kind"
      ; "script"
      ; "lifecycle"
      ; "replace"
      ; "failure"
      ; "executable"
      ; "sha256"
      ; "runtime"
      ; "protocol"
      ; "timeout"
      ; "max_input"
      ; "max_output"
      ]
  ; shell [ "secrets" ] [ "replacement"; "merge" ]
  ; shell [ "secrets"; "from_env" ] [ "name"; "optional" ]
  ; shell [ "secrets"; "from_file" ] [ "path"; "optional"; "strip" ]
  ; shell [ "secrets"; "literal" ] [ "value" ]
  ; shell [ "audit" ] [ "format"; "path"; "content"; "failure" ]
  ; shell
      [ "audit"; "filter" ]
      [ "script"
      ; "lifecycle"
      ; "executable"
      ; "sha256"
      ; "runtime"
      ; "protocol"
      ; "timeout"
      ; "max_input"
      ; "max_output"
      ]
  ; tool
      [ "tool" ]
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
  ; tool [ "tool"; "command" ] [ "program"; "executable_ref" ]
  ; tool [ "tool"; "command"; "arg" ] [ "value" ]
  ; tool [ "tool"; "command"; "secret_arg" ] [ "env"; "prefix" ]
  ; tool [ "tool"; "command"; "path_arg" ] [ "base"; "path" ]
  ; tool [ "tool"; "arguments" ] [ "mode"; "min_count"; "max_count"; "max_item_bytes" ]
  ; read_file [ "tool" ] [ "name"; "description" ]
  ; read_file [ "tool"; "read" ] [ "id"; "path"; "description" ]
  ]
;;

let same_path a b = List.equal String.equal a b

let find ~root ~path =
  List.find_map entries ~f:(fun entry ->
    Option.some_if (Root.equal root entry.root && same_path path entry.path) entry.names)
;;

let allowed_for_parser_path_exn ~root ~path =
  match find ~root ~path with
  | Some names -> names
  | None ->
    failwith
      (sprintf "missing ChatMD shell attribute schema: %s" (String.concat ~sep:"/" path))
;;

let matcher = function
  | E.Any_command
  | Program
  | Basename
  | Resolved_path
  | Trusted_executable
  | Program_regex
  | Argv_prefix
  | Argument
  | Argument_contains
  | Effect
  | No_unknown_effects
  | Raw_shell
  | Chatml_match
  | All
  | Any
  | Not -> true
  | _ -> false
;;

let matcher_parent_chain ancestors =
  let tail =
    match ancestors with
    | E.Policy :: Rule :: tail -> Some tail
    | Interceptors :: Interceptor :: Match :: tail -> Some tail
    | _ -> None
  in
  Option.value_map tail ~default:false ~f:(fun tail ->
    List.for_all tail ~f:(function
      | E.All | Any | Not -> true
      | _ -> false))
;;

let path_of_context (context : Context.t) =
  let element = E.to_string context.element in
  let path parent = Some [ parent; element ] in
  match context.root, context.ancestors, context.element with
  | ( Root.Shell_access
    , []
    , ( E.Capabilities
      | Resolver
      | Environment
      | Limits
      | Backends
      | Policy
      | Approvals
      | Reviewers
      | Interceptors
      | Effect_analysis
      | Secrets
      | Audit ) ) -> Some [ element ]
  | Shell_access, [ E.Capabilities ], (Read | Write) -> path "capabilities"
  | Shell_access, [ E.Resolver ], (Search_path | Trusted_root | Executable) ->
    path "resolver"
  | Shell_access, [ E.Environment ], (Set | Pass | Unset | Unset_prefix | Path | Path_env)
    -> path "environment"
  | Shell_access, [ E.Backends ], (Seatbelt | Bubblewrap | Direct | External_backend) ->
    path "backends"
  | ( Shell_access
    , [ E.Backends; External_backend ]
    , ( Arg
      | Cwd_value
      | Target_executable
      | Command_argv
      | Read_roots
      | Write_roots
      | Network_flag
      | Resource_limit_args ) ) -> path "backends"
  | Shell_access, [ E.Policy ], Rule -> path "policy"
  | Shell_access, [ E.Reviewers ], Reviewer -> path "reviewers"
  | Shell_access, [ E.Interceptors ], Interceptor -> path "interceptors"
  | Shell_access, [ E.Interceptors; Interceptor ], Match -> path "interceptors"
  | Shell_access, [ E.Effect_analysis ], Analyzer -> path "effect_analysis"
  | Shell_access, [ E.Secrets ], (From_env | From_file | Literal) -> path "secrets"
  | Shell_access, [ E.Audit ], Filter -> path "audit"
  | Shell_access, ancestors, element
    when matcher element && matcher_parent_chain ancestors ->
    Some [ "policy"; E.to_string element ]
  | Tool_shell, [], (E.Command | Arguments) -> Some [ "tool"; element ]
  | Tool_shell, [ E.Command ], (Arg | Secret_arg | Path_arg) ->
    Some [ "tool"; "command"; element ]
  | Tool_read_file, [], E.Read -> Some [ "tool"; element ]
  | _ -> None
;;

let allowed (context : Context.t) =
  Option.bind (path_of_context context) ~f:(fun path -> find ~root:context.root ~path)
;;

let names choices = List.map choices ~f:fst

let bool_attribute path attribute =
  match path, attribute with
  | ( [ "capabilities" ]
    , ("network" | "child_processes" | "arbitrary_code" | "privilege_change") )
  | [ "capabilities"; ("read" | "write") ], "optional"
  | [ "resolver" ], "allow_relative_search_path"
  | [ "resolver"; "executable" ], ("trusted" | "override")
  | [ "environment"; "pass" ], ("required" | "secret")
  | [ "environment"; "path_env" ], "required"
  | [ "backends" ], "accept_declared_confinement"
  | [ "backends"; "seatbelt" ], "allow_system_reads"
  | [ "backends"; "bubblewrap" ], ("private_tmp" | "proc")
  | [ "policy"; "rule" ], "override"
  | [ "approvals" ], "durable"
  | [ "effect_analysis"; "analyzer" ], "replace"
  | [ "secrets"; ("from_env" | "from_file") ], "optional"
  | [ "secrets"; "from_file" ], "strip" -> true
  | _ -> false
;;

let choices context ~attribute =
  let path = path_of_context context in
  let open Value_choices in
  let selected =
    match context.root, path, attribute with
    | Root.Shell_access, Some [ "capabilities" ], "sandbox" ->
      Some (Closed (names Enum.sandbox))
    | Shell_access, Some [ "environment" ], "inherit" ->
      Some (Closed (names Enum.environment_inherit))
    | Shell_access, Some [ "environment"; "path_env" ], "position" ->
      Some (Closed (names Enum.path_position))
    | Shell_access, Some [ "backends"; "external_backend" ], "confinement" ->
      Some (Closed (names Enum.confinement))
    | Shell_access, Some [ "policy"; "effect" ], "name" ->
      Some (Closed (names Enum.process_effect))
    | Shell_access, Some [ "policy" ], "default"
    | Shell_access, Some [ "policy"; "rule" ], "action" ->
      Some (Closed (names Enum.policy_action))
    | Shell_access, Some [ "approvals" ], "scopes" ->
      Some (Comma_separated_tokens (names Enum.approval_scope))
    | Shell_access, Some [ "approvals" ], "provider" ->
      Some (Closed (names Enum.approval_provider))
    | Shell_access, Some [ "approvals" ], "unavailable" ->
      Some (Closed (names Enum.approval_unavailable))
    | Shell_access, Some [ "interceptors"; "interceptor" ], "phase" ->
      Some (Closed (names Enum.interceptor_phase))
    | Shell_access, Some [ "reviewers" ], "strategy" ->
      Some (Closed [ reviewer_strategy ])
    | Shell_access, Some [ "reviewers"; "reviewer" ], "kind" ->
      Some (Closed Reviewer_kind.names)
    | Shell_access, Some [ "effect_analysis"; "analyzer" ], "kind" ->
      Some (Closed Analyzer_kind.names)
    | Shell_access, Some [ "audit" ], "format" -> Some (Closed (names Enum.audit_format))
    | Shell_access, Some [ "audit" ], "content" ->
      Some (Closed (names Enum.audit_content))
    | Shell_access, Some [ "audit" ], "failure" ->
      Some (Closed (names Enum.audit_failure))
    | Tool_shell, Some [ "tool"; "arguments" ], "mode" ->
      Some (Closed (names Enum.tool_arguments_mode))
    | _, Some _, "merge" -> Some (Closed (names Enum.merge))
    | _, Some _, "when" -> Some (Closed (names Enum.platform))
    | Shell_access, Some [ "policy"; "chatml_match" ], "failure"
    | Shell_access, Some [ "interceptors"; "interceptor" ], "failure"
    | Shell_access, Some [ "effect_analysis"; "analyzer" ], "failure" ->
      Some (Closed (names Enum.hook_failure))
    | Shell_access, Some path, attribute when bool_attribute path attribute ->
      Some (Closed (names Enum.bool))
    | _ -> None
  in
  Option.bind selected ~f:(fun values ->
    Option.bind (allowed context) ~f:(fun allowed ->
      if List.mem allowed attribute ~equal:String.equal then Some values else None))
;;
