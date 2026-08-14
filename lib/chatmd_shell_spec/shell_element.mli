open! Core

(** Closed set of nested shell declaration element names recognized by the
    ChatMD lexer and parser. *)

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

(** [of_string name] returns the shell element represented by [name]. *)
val of_string : string -> t option

(** [to_string t] returns the canonical lower-case element name. *)
val to_string : t -> string
