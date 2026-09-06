open! Core
module S = Shell_spec

type profile_ref =
  { requested : string
  ; runtime_id : S.Runtime_id.t
  ; source : Source_ref.t
  }

let aliases =
  [ "builtin:workspace-readonly", "builtin:workspace-readonly@1"
  ; "builtin:workspace-development", "builtin:workspace-development@1"
  ; "builtin:hook-worker", "builtin:hook-worker@1"
  ; "builtin:yolo", "builtin:yolo@1"
  ]
;;

let versions = List.map aliases ~f:snd |> String.Set.of_list

let resolve_alias name =
  match List.Assoc.find aliases name ~equal:String.equal with
  | Some version -> Some version
  | None -> Option.some_if (Set.mem versions name) name
;;

let path value = Result.ok_or_failwith (Path_expr.parse value)

let backend platform sandbox =
  match platform, sandbox with
  | S.Macos, `Sandbox ->
    [ S.Seatbelt
        { id = Some "builtin-seatbelt"
        ; when_ = Macos
        ; executable = None
        ; allow_system_reads = true
        }
    ]
  | Linux, `Sandbox ->
    [ Bubblewrap
        { id = Some "builtin-bubblewrap"
        ; when_ = Linux
        ; executable = None
        ; private_tmp = true
        ; proc = true
        ; dev = "minimal"
        }
    ]
  | (Windows | Any), `Sandbox -> []
  | _, `Direct -> [ Direct { id = Some "builtin-direct"; when_ = Any } ]
;;

let capabilities
      ~sandbox
      ~read
      ~write
      ~network
      ~child_processes
      ~arbitrary_code
      ~privilege_change
  =
  let root value : S.capability_root = S.Path (path value) in
  { S.sandbox = Set sandbox
  ; network = Set network
  ; child_processes = Set child_processes
  ; arbitrary_code = Set arbitrary_code
  ; privilege_change = Set privilege_change
  ; read = List.map read ~f:root
  ; write = List.map write ~f:root
  ; merge = Replace
  }
;;

let resolver ~unrestricted =
  { S.search_path =
      (if unrestricted
       then []
       else
         List.map [ "/usr/local/bin"; "/usr/bin"; "/bin"; "/usr/sbin"; "/sbin" ] ~f:path)
  ; trusted_root = (if unrestricted then [ path "/" ] else [ path "/usr"; path "/bin" ])
  ; executables = []
  ; allow_relative_search_path = Set false
  ; merge = Replace
  }
;;

let policy default rules = { S.default = Set default; rules; merge = Replace }
let rule id action matcher = { S.id; action; matcher; override = false }

let readonly_policy =
  policy
    Ask
    [ rule
        "builtin-deny-destructive"
        Deny
        (Any
           (List.map [ "rm"; "mv"; "cp"; "chmod"; "chown"; "sudo" ] ~f:(fun name ->
              S.Basename name)))
    ; rule
        "builtin-allow-read-tools"
        Allow
        (Any
           (List.map
              [ "pwd"; "ls"; "cat"; "head"; "tail"; "wc"; "grep"; "rg" ]
              ~f:(fun name -> S.Basename name)))
    ]
;;

let approvals provider =
  { S.provider = Set provider
  ; unavailable = Set Deny_unavailable
  ; scopes = [ Once; Exact_session ]
  ; durable = Set false
  }
;;

let base profile concrete =
  let runtime = Manifest_defaults.runtime ~id:profile.runtime_id ~source:profile.source in
  { runtime with
    requested_profile = Some profile.requested
  ; resolved_profile = Some concrete
  }
;;

let readonly platform profile concrete =
  let runtime = base profile concrete in
  { runtime with
    cwd = Set (path "${workspace}")
  ; capabilities =
      Some
        (capabilities
           ~sandbox:Required
           ~read:[ "${workspace}" ]
           ~write:[]
           ~network:false
           ~child_processes:false
           ~arbitrary_code:false
           ~privilege_change:false)
  ; resolver = Some (resolver ~unrestricted:false)
  ; backends =
      Some
        { S.values = backend platform `Sandbox
        ; accept_declared_confinement = false
        ; merge = Replace
        }
  ; policy = Some readonly_policy
  }
;;

let development platform profile concrete =
  let runtime = base profile concrete in
  { runtime with
    cwd = Set (path "${workspace}")
  ; capabilities =
      Some
        (capabilities
           ~sandbox:Preferred
           ~read:[ "${workspace}" ]
           ~write:[ "${workspace}" ]
           ~network:false
           ~child_processes:true
           ~arbitrary_code:true
           ~privilege_change:false)
  ; resolver = Some (resolver ~unrestricted:false)
  ; backends =
      Some
        { S.values = backend platform `Sandbox @ backend platform `Direct
        ; accept_declared_confinement = false
        ; merge = Replace
        }
  ; policy = Some (policy Ask [])
  }
;;

let yolo platform profile concrete =
  let runtime = base profile concrete in
  { runtime with
    cwd = Set (path "${workspace}")
  ; capabilities =
      Some
        (capabilities
           ~sandbox:Direct_unsafe
           ~read:[ "/" ]
           ~write:[ "/" ]
           ~network:true
           ~child_processes:true
           ~arbitrary_code:true
           ~privilege_change:true)
  ; resolver = Some (resolver ~unrestricted:true)
  ; environment = Some { S.inherit_ = Set Raw; operations = []; merge = Replace }
  ; backends =
      Some
        { S.values = backend platform `Direct
        ; accept_declared_confinement = false
        ; merge = Replace
        }
  ; policy = Some (policy Allow [])
  ; approvals = Some (approvals No_provider)
  ; reviewers = None
  ; interceptors = None
  ; effect_analysis = None
  }
;;

let hook_worker platform profile concrete =
  let runtime = base profile concrete in
  let strict_limits =
    { (Option.value_exn runtime.limits) with
      wall_time = Set (Some (Result.ok_or_failwith (Duration.parse "5s")))
    ; idle_time = Set (Some (Result.ok_or_failwith (Duration.parse "2s")))
    ; max_stdin = Set (Result.ok_or_failwith (Duration.parse_bytes "1MiB"))
    ; stdout = Set (Result.ok_or_failwith (Duration.parse_bytes "256KiB"))
    ; stderr = Set (Result.ok_or_failwith (Duration.parse_bytes "64KiB"))
    ; total_output = Set (Result.ok_or_failwith (Duration.parse_bytes "320KiB"))
    }
  in
  { runtime with
    cwd = Set (path "${source_dir}")
  ; capabilities =
      Some
        (capabilities
           ~sandbox:Required
           ~read:[ "${source_dir}" ]
           ~write:[]
           ~network:false
           ~child_processes:false
           ~arbitrary_code:true
           ~privilege_change:false)
  ; resolver = Some (resolver ~unrestricted:false)
  ; limits = Some strict_limits
  ; backends =
      Some
        { S.values = backend platform `Sandbox
        ; accept_declared_confinement = false
        ; merge = Replace
        }
  ; policy = Some (policy Deny [])
  ; approvals = Some (approvals No_provider)
  ; reviewers = None
  ; interceptors = None
  ; effect_analysis = None
  }
;;

let unknown profile =
  Diagnostic.error
    ~source:profile.source
    ~path:[ "runtime"; S.Runtime_id.to_string profile.runtime_id; "extends" ]
    ~code:"shell.unknown_builtin_profile"
    ("unknown built-in shell profile: " ^ profile.requested)
;;

let expand ~platform profile =
  match resolve_alias profile.requested with
  | None -> Error (unknown profile)
  | Some ("builtin:workspace-readonly@1" as concrete) ->
    Ok (readonly platform profile concrete)
  | Some ("builtin:workspace-development@1" as concrete) ->
    Ok (development platform profile concrete)
  | Some ("builtin:hook-worker@1" as concrete) -> Ok (hook_worker platform profile concrete)
  | Some ("builtin:yolo@1" as concrete) -> Ok (yolo platform profile concrete)
  | Some _ -> Error (unknown profile)
;;
