open! Core
module P = Path_expr
module S = Shell_spec

let path value = Result.ok_or_failwith (P.parse value)
let duration value = Result.ok_or_failwith (Duration.parse value)
let bytes value = Result.ok_or_failwith (Duration.parse_bytes value)

let capabilities =
  { S.sandbox = Set Required
  ; network = Set false
  ; child_processes = Set false
  ; arbitrary_code = Set false
  ; privilege_change = Set false
  ; read = []
  ; write = []
  ; merge = Append
  }
;;

let resolver =
  { S.search_path = []
  ; trusted_root = []
  ; executables = []
  ; allow_relative_search_path = Set false
  ; merge = Append
  }
;;

let environment = { S.inherit_ = Set Safe; operations = []; merge = Append }

let limits =
  { S.wall_time = Set (Some (duration "180s"))
  ; idle_time = Set (Some (duration "60s"))
  ; max_stdin = Set (bytes "1MiB")
  ; stdout = Set (bytes "1000000B")
  ; stderr = Set (bytes "1000000B")
  ; total_output = Set (bytes "1500000B")
  ; cpu_time = Set None
  ; memory = Clear
  ; file_size = Clear
  ; open_files = Clear
  }
;;

let backends =
  { S.values =
      [ Seatbelt
          { id = Some "builtin-seatbelt"
          ; when_ = Macos
          ; executable = None
          ; allow_system_reads = true
          }
      ; Bubblewrap
          { id = Some "builtin-bubblewrap"
          ; when_ = Linux
          ; executable = None
          ; private_tmp = true
          ; proc = true
          ; dev = "minimal"
          }
      ]
  ; accept_declared_confinement = false
  ; merge = Append
  }
;;

let policy = { S.default = Set Ask; rules = []; merge = Append }

let approvals =
  { S.provider = Set Ui
  ; unavailable = Set Deny_unavailable
  ; scopes = [ Once; Exact_session ]
  ; durable = Set false
  }
;;

let secrets = { S.replacement = Set "[REDACTED]"; sources = []; merge = Append }

let audit =
  { S.format = No_audit
  ; path = None
  ; content = Redacted
  ; failure = Continue
  ; filter = None
  }
;;

let runtime ~id ~source =
  { S.id
  ; extends = None
  ; requested_profile = None
  ; resolved_profile = None
  ; cwd = Set (path "${tool_dir}")
  ; pipefail = Set false
  ; capabilities = Some capabilities
  ; resolver = Some resolver
  ; environment = Some environment
  ; limits = Some limits
  ; backends = Some backends
  ; policy = Some policy
  ; approvals = Some approvals
  ; reviewers = None
  ; interceptors = None
  ; effect_analysis = None
  ; secrets = Some secrets
  ; audit = Some audit
  ; source
  }
;;

let legacy_capabilities =
  { capabilities with
    sandbox = Set Direct_unsafe
  ; network = Set true
  ; child_processes = Set true
  ; arbitrary_code = Set true
  ; privilege_change = Set true
  ; read = [ Path (path "/") ]
  ; write = [ Path (path "/") ]
  }
;;

let legacy_backends =
  { S.values = [ Direct { id = Some "legacy-direct"; when_ = Any } ]
  ; accept_declared_confinement = false
  ; merge = Append
  }
;;

let legacy_runtime ~source =
  let id =
    match S.Runtime_id.of_string ~source "legacy:custom" with
    | Ok id -> id
    | Error diagnostic -> failwith (Diagnostic.to_string diagnostic)
  in
  { (runtime ~id ~source) with
    capabilities = Some legacy_capabilities
  ; environment = Some { environment with inherit_ = Set Raw }
  ; backends = Some legacy_backends
  }
;;
