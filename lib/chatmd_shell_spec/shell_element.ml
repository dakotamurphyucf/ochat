open! Core
open Jsonaf.Export

type t =
  | Capabilities
  | Read
  | Write
  | Resolver
  | Search_path
  | Trusted_root
  | Executable
  | Environment
  | Set
  | Pass
  | Unset
  | Unset_prefix
  | Path
  | Path_env
  | Limits
  | Backends
  | Seatbelt
  | Bubblewrap
  | Direct
  | External_backend
  | Cwd_value
  | Target_executable
  | Command_argv
  | Read_roots
  | Write_roots
  | Network_flag
  | Resource_limit_args
  | Policy
  | Rule
  | All
  | Any
  | Not
  | Any_command
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
  | Approvals
  | Reviewers
  | Reviewer
  | Interceptors
  | Interceptor
  | Match
  | Effect_analysis
  | Analyzer
  | Secrets
  | From_env
  | From_file
  | Literal
  | Audit
  | Filter
  | Command
  | Arg
  | Secret_arg
  | Path_arg
  | Arguments
[@@deriving sexp, compare, equal, hash, bin_io, jsonaf]

let names =
  [ "capabilities", Capabilities
  ; "read", Read
  ; "write", Write
  ; "resolver", Resolver
  ; "search_path", Search_path
  ; "trusted_root", Trusted_root
  ; "executable", Executable
  ; "environment", Environment
  ; "set", Set
  ; "pass", Pass
  ; "unset", Unset
  ; "unset_prefix", Unset_prefix
  ; "path", Path
  ; "path_env", Path_env
  ; "limits", Limits
  ; "backends", Backends
  ; "seatbelt", Seatbelt
  ; "bubblewrap", Bubblewrap
  ; "direct", Direct
  ; "external_backend", External_backend
  ; "cwd_value", Cwd_value
  ; "target_executable", Target_executable
  ; "command_argv", Command_argv
  ; "read_roots", Read_roots
  ; "write_roots", Write_roots
  ; "network_flag", Network_flag
  ; "resource_limit_args", Resource_limit_args
  ; "policy", Policy
  ; "rule", Rule
  ; "all", All
  ; "any", Any
  ; "not", Not
  ; "any_command", Any_command
  ; "program", Program
  ; "basename", Basename
  ; "resolved_path", Resolved_path
  ; "trusted_executable", Trusted_executable
  ; "program_regex", Program_regex
  ; "argv_prefix", Argv_prefix
  ; "argument", Argument
  ; "argument_contains", Argument_contains
  ; "effect", Effect
  ; "no_unknown_effects", No_unknown_effects
  ; "raw_shell", Raw_shell
  ; "chatml_match", Chatml_match
  ; "approvals", Approvals
  ; "reviewers", Reviewers
  ; "reviewer", Reviewer
  ; "interceptors", Interceptors
  ; "interceptor", Interceptor
  ; "match", Match
  ; "effect_analysis", Effect_analysis
  ; "analyzer", Analyzer
  ; "secrets", Secrets
  ; "from_env", From_env
  ; "from_file", From_file
  ; "literal", Literal
  ; "audit", Audit
  ; "filter", Filter
  ; "command", Command
  ; "arg", Arg
  ; "secret_arg", Secret_arg
  ; "path_arg", Path_arg
  ; "arguments", Arguments
  ]
;;

let of_string name = List.Assoc.find names name ~equal:String.equal

let to_string t =
  List.find_map_exn names ~f:(fun (name, candidate) ->
    Option.some_if (equal t candidate) name)
;;
