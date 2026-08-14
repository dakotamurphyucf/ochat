open! Core
module D = Diagnostic
module S = Shell_spec

exception Merge_error of D.t

let fail source path code message =
  raise_notrace (Merge_error (D.error ~source ~path ~code message))
;;

let setting base = function
  | S.Inherit -> base
  | (Set _ | Clear) as value -> value
;;

let values merge base local =
  match merge with
  | S.Append -> base @ local
  | Replace -> local
;;

let replace_by_id values ~id ~id_of replacement =
  List.map values ~f:(fun value ->
    if String.equal (id_of value) id then replacement else value)
;;

let merge_named_exn ~source ~path ~merge ~id_of ~is_override ~normalize base local =
  let initial = if S.equal_merge merge Replace then [] else base in
  List.fold local ~init:initial ~f:(fun values value ->
    let id = id_of value in
    if List.exists values ~f:(fun current -> String.equal (id_of current) id)
    then
      if is_override value
      then replace_by_id values ~id ~id_of (normalize value)
      else fail source path "shell.duplicate_named_entry" ("duplicate entry: " ^ id)
    else values @ [ normalize value ])
;;

let capabilities (base : S.capabilities) (child : S.capabilities) =
  { S.sandbox = setting base.sandbox child.sandbox
  ; network = setting base.network child.network
  ; child_processes = setting base.child_processes child.child_processes
  ; arbitrary_code = setting base.arbitrary_code child.arbitrary_code
  ; privilege_change = setting base.privilege_change child.privilege_change
  ; read = values child.merge base.read child.read
  ; write = values child.merge base.write child.write
  ; merge = Append
  }
;;

let resolver source (base : S.resolver) (child : S.resolver) =
  let executables =
    merge_named_exn
      ~source
      ~path:[ "resolver"; "executable" ]
      ~merge:child.merge
      ~id_of:(fun (value : S.executable) -> value.id)
      ~is_override:(fun (value : S.executable) -> value.override)
      ~normalize:(fun (value : S.executable) -> { value with override = false })
      base.executables
      child.executables
  in
  { S.search_path = values child.merge base.search_path child.search_path
  ; trusted_root = values child.merge base.trusted_root child.trusted_root
  ; executables
  ; allow_relative_search_path =
      setting base.allow_relative_search_path child.allow_relative_search_path
  ; merge = Append
  }
;;

let environment (base : S.environment) (child : S.environment) =
  { S.inherit_ = setting base.inherit_ child.inherit_
  ; operations = values child.merge base.operations child.operations
  ; merge = Append
  }
;;

let limits (base : S.limits) (child : S.limits) =
  { S.wall_time = setting base.wall_time child.wall_time
  ; idle_time = setting base.idle_time child.idle_time
  ; max_stdin = setting base.max_stdin child.max_stdin
  ; stdout = setting base.stdout child.stdout
  ; stderr = setting base.stderr child.stderr
  ; total_output = setting base.total_output child.total_output
  ; cpu_time = setting base.cpu_time child.cpu_time
  ; memory = setting base.memory child.memory
  ; file_size = setting base.file_size child.file_size
  ; open_files = setting base.open_files child.open_files
  }
;;

let backend_id = function
  | S.Seatbelt { id; _ }
  | Bubblewrap { id; _ }
  | Direct { id; _ }
  | External { id; _ } -> id
;;

let add_backend_exn source values value =
  match backend_id value with
  | None -> values @ [ value ]
  | Some id ->
    if
      List.exists values ~f:(fun current ->
        Option.equal String.equal (backend_id current) (Some id))
    then
      fail source [ "backends" ] "shell.duplicate_named_entry" ("duplicate entry: " ^ id)
    else values @ [ value ]
;;

let backends source (base : S.backends) (child : S.backends) =
  let initial = if S.equal_merge child.merge Replace then [] else base.values in
  { S.values = List.fold child.values ~init:initial ~f:(add_backend_exn source)
  ; accept_declared_confinement = child.accept_declared_confinement
  ; merge = Append
  }
;;

let policy source (base : S.policy) (child : S.policy) =
  let rules =
    merge_named_exn
      ~source
      ~path:[ "policy"; "rule" ]
      ~merge:child.merge
      ~id_of:(fun (value : S.policy_rule) -> value.id)
      ~is_override:(fun (value : S.policy_rule) -> value.override)
      ~normalize:(fun (value : S.policy_rule) -> { value with override = false })
      base.rules
      child.rules
  in
  { S.default = setting base.default child.default; rules; merge = Append }
;;

let approvals (base : S.approvals) (child : S.approvals) =
  { S.provider = setting base.provider child.provider
  ; unavailable = setting base.unavailable child.unavailable
  ; scopes = (if List.is_empty child.scopes then base.scopes else child.scopes)
  ; durable = setting base.durable child.durable
  }
;;

let secrets (base : S.secrets) (child : S.secrets) =
  { S.replacement = setting base.replacement child.replacement
  ; sources = values child.merge base.sources child.sources
  ; merge = Append
  }
;;

let section base child ~merge =
  match child with
  | None -> base
  | Some child -> Some (merge (Option.value_exn base) child)
;;

let runtime_exn ~(base : S.t) (child : S.t) =
  { S.id = child.id
  ; extends = None
  ; requested_profile = Option.first_some child.requested_profile base.requested_profile
  ; resolved_profile = Option.first_some child.resolved_profile base.resolved_profile
  ; cwd = setting base.cwd child.cwd
  ; pipefail = setting base.pipefail child.pipefail
  ; capabilities = section base.capabilities child.capabilities ~merge:capabilities
  ; resolver = section base.resolver child.resolver ~merge:(resolver child.source)
  ; environment = section base.environment child.environment ~merge:environment
  ; limits = section base.limits child.limits ~merge:limits
  ; backends = section base.backends child.backends ~merge:(backends child.source)
  ; policy = section base.policy child.policy ~merge:(policy child.source)
  ; approvals = section base.approvals child.approvals ~merge:approvals
  ; reviewers = Option.first_some child.reviewers base.reviewers
  ; interceptors = Option.first_some child.interceptors base.interceptors
  ; effect_analysis = Option.first_some child.effect_analysis base.effect_analysis
  ; secrets = section base.secrets child.secrets ~merge:secrets
  ; audit = Option.first_some child.audit base.audit
  ; source = child.source
  }
;;

let runtime ~base child =
  try Ok (runtime_exn ~base child) with
  | Merge_error diagnostic -> Error [ diagnostic ]
;;
