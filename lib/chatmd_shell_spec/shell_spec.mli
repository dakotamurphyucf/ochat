open! Core

(** Serializable shell runtime declarations. These values contain no live host
    resources and are safe to construct during ChatMD parsing. *)

module Runtime_id : sig
  type t = private string [@@deriving sexp, compare, equal, hash, bin_io, jsonaf]

  (** [of_string ?source value] validates and returns a runtime identifier. *)
  val of_string : ?source:Source_ref.t -> string -> (t, Diagnostic.t) result

  (** [to_string t] returns the identifier text. *)
  val to_string : t -> string

  (** [qualify ~namespace t] applies [namespace] to an unqualified identifier. *)
  val qualify : namespace:string option -> t -> t
end

type 'a setting =
  | Inherit
  | Set of 'a
  | Clear
[@@deriving sexp, compare, equal, hash, bin_io, jsonaf]

type merge =
  | Append
  | Replace
[@@deriving sexp, compare, equal, hash, bin_io, jsonaf]

type sandbox =
  | Required
  | Preferred
  | Direct_unsafe
[@@deriving sexp, compare, equal, hash, bin_io, jsonaf]

type capability_root =
  | Path of Path_expr.t
  | Path_env of
      { name : string
      ; optional : bool
      }
[@@deriving sexp, compare, equal, hash, bin_io, jsonaf]

type capabilities =
  { sandbox : sandbox setting
  ; network : bool setting
  ; child_processes : bool setting
  ; arbitrary_code : bool setting
  ; privilege_change : bool setting
  ; read : capability_root list
  ; write : capability_root list
  ; merge : merge
  }
[@@deriving sexp, compare, equal, hash, bin_io, jsonaf]

type executable =
  { id : string
  ; path : Path_expr.t
  ; sha256 : string option
  ; trusted : bool
  ; override : bool
  }
[@@deriving sexp, compare, equal, hash, bin_io, jsonaf]

type resolver =
  { search_path : Path_expr.t list
  ; trusted_root : Path_expr.t list
  ; executables : executable list
  ; allow_relative_search_path : bool setting
  ; merge : merge
  }
[@@deriving sexp, compare, equal, hash, bin_io, jsonaf]

type environment_inherit =
  | None_
  | Safe
  | Selected
  | All_sanitized
  | Raw
[@@deriving sexp, compare, equal, hash, bin_io, jsonaf]

type path_position =
  | Prepend
  | Append_path
[@@deriving sexp, compare, equal, hash, bin_io, jsonaf]

type environment_operation =
  | Set_env of
      { name : string
      ; value : string
      }
  | Pass_env of
      { name : string
      ; required : bool
      ; secret : bool
      }
  | Unset_env of string
  | Unset_prefix of string
  | Path of
      { position : path_position
      ; path : Path_expr.t
      }
  | Path_env of
      { name : string
      ; suffix : string option
      ; position : path_position
      ; required : bool
      }
[@@deriving sexp, compare, equal, hash, bin_io, jsonaf]

type environment =
  { inherit_ : environment_inherit setting
  ; operations : environment_operation list
  ; merge : merge
  }
[@@deriving sexp, compare, equal, hash, bin_io, jsonaf]

type limits =
  { wall_time : Duration.t option setting
  ; idle_time : Duration.t option setting
  ; max_stdin : Duration.bytes setting
  ; stdout : Duration.bytes setting
  ; stderr : Duration.bytes setting
  ; total_output : Duration.bytes setting
  ; cpu_time : Duration.t option setting
  ; memory : Duration.bytes setting
  ; file_size : Duration.bytes setting
  ; open_files : int setting
  }
[@@deriving sexp, compare, equal, hash, bin_io, jsonaf]

type platform =
  | Macos
  | Linux
  | Windows
  | Any
[@@deriving sexp, compare, equal, hash, bin_io, jsonaf]

type confinement =
  | Verified_confinement
  | Declared_confinement
  | No_confinement
[@@deriving sexp, compare, equal, hash, bin_io, jsonaf]

type repeated_flag = { flag : string }
[@@deriving sexp, compare, equal, hash, bin_io, jsonaf]

type backend_atom =
  | Literal_atom of string
  | Cwd_atom
  | Target_executable_atom
  | Command_argv_atom
  | Read_roots_atom of repeated_flag
  | Write_roots_atom of repeated_flag
  | Network_flag_atom of string
  | Resource_limit_args_atom
[@@deriving sexp, compare, equal, hash, bin_io, jsonaf]

type backend =
  | Seatbelt of
      { id : string option
      ; when_ : platform
      ; executable : Path_expr.t option
      ; allow_system_reads : bool
      }
  | Bubblewrap of
      { id : string option
      ; when_ : platform
      ; executable : Path_expr.t option
      ; private_tmp : bool
      ; proc : bool
      ; dev : string
      }
  | Direct of
      { id : string option
      ; when_ : platform
      }
  | External of
      { id : string option
      ; when_ : platform
      ; executable : Path_expr.t
      ; sha256 : string option
      ; confinement : confinement
      ; atoms : backend_atom list
      }
[@@deriving sexp, compare, equal, hash, bin_io, jsonaf]

type backends =
  { values : backend list
  ; accept_declared_confinement : bool
  ; merge : merge
  }
[@@deriving sexp, compare, equal, hash, bin_io, jsonaf]

type policy_action =
  | Allow
  | Ask
  | Deny
[@@deriving sexp, compare, equal, hash, bin_io, jsonaf]

type process_effect =
  | Read_path
  | Write_path
  | Network
  | Child_processes
  | Arbitrary_code
  | Privilege_change
  | Unknown
[@@deriving sexp, compare, equal, hash, bin_io, jsonaf]

type lifecycle =
  | Invocation
  | Session
  | Runtime
[@@deriving sexp, compare, equal, hash, bin_io, jsonaf]

type hook_protocol = Shell_hook_json_v1
[@@deriving sexp, compare, equal, hash, bin_io, jsonaf]

type executable_hook =
  { executable : Path_expr.t
  ; sha256 : string option
  ; runtime : string
  ; protocol : hook_protocol
  ; timeout : Duration.t option
  ; max_input : Duration.bytes option
  ; max_output : Duration.bytes option
  }
[@@deriving sexp, compare, equal, hash, bin_io, jsonaf]

type extension =
  | Chatml_extension of
      { script : string
      ; lifecycle : lifecycle
      }
  | Executable_extension of executable_hook
[@@deriving sexp, compare, equal, hash, bin_io, jsonaf]

type hook_failure =
  | Conservative_failure
  | Deny_failure
  | Error_failure
  | No_match_failure
  | Unknown_failure
  | Keep_failure
[@@deriving sexp, compare, equal, hash, bin_io, jsonaf]

type chatml_matcher =
  { script : string
  ; function_ : string
  ; failure : hook_failure
  }
[@@deriving sexp, compare, equal, hash, bin_io, jsonaf]

type matcher =
  | Any_command
  | Program of string
  | Basename of string
  | Resolved_path of Path_expr.t
  | Trusted_executable
  | Program_regex of string
  | Argv_prefix of string list
  | Argument of string
  | Argument_contains of string
  | Effect of
      { kind : process_effect
      ; under : Path_expr.t option
      }
  | No_unknown_effects
  | Raw_shell_request
  | Chatml_match of chatml_matcher
  | All of matcher list
  | Any of matcher list
  | Not of matcher
[@@deriving sexp, compare, equal, hash, bin_io, jsonaf]

type policy_rule =
  { id : string
  ; action : policy_action
  ; matcher : matcher
  ; override : bool
  }
[@@deriving sexp, compare, equal, hash, bin_io, jsonaf]

type policy =
  { default : policy_action setting
  ; rules : policy_rule list
  ; merge : merge
  }
[@@deriving sexp, compare, equal, hash, bin_io, jsonaf]

type approval_provider =
  | Ui
  | No_provider
[@@deriving sexp, compare, equal, hash, bin_io, jsonaf]

type approval_unavailable =
  | Deny_unavailable
  | Error_unavailable
[@@deriving sexp, compare, equal, hash, bin_io, jsonaf]

type approval_scope =
  | Once
  | Exact_session
  | Prefix_session
  | Durable_exact
[@@deriving sexp, compare, equal, hash, bin_io, jsonaf]

type approvals =
  { provider : approval_provider setting
  ; unavailable : approval_unavailable setting
  ; scopes : approval_scope list
  ; durable : bool setting
  }
[@@deriving sexp, compare, equal, hash, bin_io, jsonaf]

type reviewer =
  | Ui_reviewer of { id : string }
  | Chatml_reviewer of
      { id : string
      ; script : string
      ; lifecycle : lifecycle
      ; failure : hook_failure
      }
  | Executable_reviewer of
      { id : string
      ; hook : executable_hook
      ; failure : hook_failure
      }
  | Model_reviewer of
      { id : string
      ; agent : string
      ; model : string option
      ; failure : hook_failure
      }
[@@deriving sexp, compare, equal, hash, bin_io, jsonaf]

type reviewers =
  { values : reviewer list
  ; merge : merge
  }
[@@deriving sexp, compare, equal, hash, bin_io, jsonaf]

type interceptor_phase =
  | Before
  | After
[@@deriving sexp, compare, equal, hash, bin_io, jsonaf]

type interceptor =
  { id : string
  ; phase : interceptor_phase
  ; extension : extension
  ; matcher : matcher option
  ; failure : hook_failure
  }
[@@deriving sexp, compare, equal, hash, bin_io, jsonaf]

type interceptors =
  { values : interceptor list
  ; merge : merge
  }
[@@deriving sexp, compare, equal, hash, bin_io, jsonaf]

type effect_analyzer =
  { id : string
  ; extension : extension
  ; replace : bool
  ; failure : hook_failure
  }
[@@deriving sexp, compare, equal, hash, bin_io, jsonaf]

type effect_analysis =
  { analyzers : effect_analyzer list
  ; merge : merge
  }
[@@deriving sexp, compare, equal, hash, bin_io, jsonaf]

type secret_source =
  | From_env of
      { name : string
      ; optional : bool
      }
  | From_file of
      { path : Path_expr.t
      ; optional : bool
      ; strip : bool
      }
  | Literal of string
[@@deriving sexp, compare, equal, hash, bin_io, jsonaf]

type secrets =
  { replacement : string setting
  ; sources : secret_source list
  ; merge : merge
  }
[@@deriving sexp, compare, equal, hash, bin_io, jsonaf]

type audit_format =
  | No_audit
  | Stderr
  | Jsonl
  | Session
[@@deriving sexp, compare, equal, hash, bin_io, jsonaf]

type audit_content =
  | Metadata
  | Redacted
  | Full
[@@deriving sexp, compare, equal, hash, bin_io, jsonaf]

type audit_failure =
  | Continue
  | Deny_start
  | Terminate
[@@deriving sexp, compare, equal, hash, bin_io, jsonaf]

type audit =
  { format : audit_format
  ; path : Path_expr.t option
  ; content : audit_content
  ; failure : audit_failure
  ; filter : extension option
  }
[@@deriving sexp, compare, equal, hash, bin_io, jsonaf]

type t =
  { id : Runtime_id.t
  ; extends : string option
  ; requested_profile : string option
  ; resolved_profile : string option
  ; cwd : Path_expr.t setting
  ; pipefail : bool setting
  ; capabilities : capabilities option
  ; resolver : resolver option
  ; environment : environment option
  ; limits : limits option
  ; backends : backends option
  ; policy : policy option
  ; approvals : approvals option
  ; reviewers : reviewers option
  ; interceptors : interceptors option
  ; effect_analysis : effect_analysis option
  ; secrets : secrets option
  ; audit : audit option
  ; source : Source_ref.t
  }
[@@deriving sexp, compare, equal, hash, bin_io, jsonaf]
