open! Core
module D = Chatmd_shell_spec.Diagnostic
module M = Chatmd_shell_spec.Manifest
module MC = Chatmd_shell_spec.Manifest_compiler
module T = Chatmd_shell_spec.Shell_tool_spec

type script =
  { tool_name : string
  ; path : string
  ; verification_path : string
  ; sha256 : string
  ; size : int
  ; executable_path : string
  ; executable_sha256 : string
  ; arguments_before_model : string list
  ; max_source_bytes : int
  }
[@@deriving sexp, compare, equal]

type inspection =
  { manifest_sha256 : string
  ; runtime_ids : string list
  ; tool_names : string list
  ; scripts : script list
  }
[@@deriving sexp, compare, equal]

type extension_binding =
  { runtime_id : string
  ; instance : Chatml_extension.instance
  }

type t =
  { manifest : M.t
  ; runtimes : Runtime.t String.Map.t
  ; tools : T.t String.Map.t
  ; scripts : script String.Map.t
  ; extension_bindings : extension_binding list
  ; inspection : inspection
  }

type prepared =
  { manifest : M.t
  ; runtimes : Chatmd_shell_spec.Shell_spec.t list
  ; extensions : Chatml_extension.compiled String.Map.t
  ; admin_policy : Admin_policy.t
  }

type error =
  { code : string
  ; message : string
  ; runtime_id : string option
  }
[@@deriving sexp, compare, equal]

let authorizer_error (error : Manifest_authorizer.error) =
  { code = error.code; message = error.message; runtime_id = None }
;;

let diagnostic_error diagnostic =
  { code = diagnostic.D.code; message = diagnostic.message; runtime_id = None }
;;

let runtime_error (error : Runtime.error) =
  { code = error.code; message = error.message; runtime_id = Some error.runtime_id }
;;

let instantiate_runtime
      ~sw
      ~host
      ~manifest
      ~approval_provider
      ~admin_policy
      ~approval_store
      ~audit_sequence
      ~extensions
      ~runtime_instances
      ~register_extension_instance
      ~worker_runtimes
      ~model_completion
      runtime
  =
  Runtime.create
    ~sw
    ~host
    ~manifest
    ~platform:manifest.M.payload.platform
    ~approval_provider
    ~admin_policy
    ~approval_store
    ~audit_sequence
    ~extensions
    ~runtime_instances
    ~register_extension_instance
    ~worker_runtimes
    ?model_completion
    runtime
;;

let runtime_order manifest runtimes =
  let specifications =
    List.map runtimes ~f:(fun runtime ->
      ( Chatmd_shell_spec.Shell_spec.Runtime_id.to_string
          runtime.Chatmd_shell_spec.Shell_spec.id
      , runtime ))
    |> String.Map.of_alist_exn
  in
  let dependencies =
    List.fold
      manifest.M.payload.dependencies
      ~init:String.Map.empty
      ~f:(fun map dependency ->
        if M.equal_edge_kind dependency.kind M.Worker_runtime
        then
          Map.update map dependency.from_id ~f:(fun values ->
            dependency.to_id :: Option.value values ~default:[])
        else map)
  in
  let rec visit temporary permanent ordered id =
    if Set.mem permanent id
    then temporary, permanent, ordered
    else if Set.mem temporary id
    then temporary, permanent, ordered
    else (
      let temporary = Set.add temporary id in
      let temporary, permanent, ordered =
        List.fold
          (Map.find dependencies id |> Option.value ~default:[])
          ~init:(temporary, permanent, ordered)
          ~f:(fun (temporary, permanent, ordered) dependency ->
            visit temporary permanent ordered dependency)
      in
      Set.remove temporary id, Set.add permanent id, id :: ordered)
  in
  let _, _, reversed =
    Map.keys specifications
    |> List.fold
         ~init:(String.Set.empty, String.Set.empty, [])
         ~f:(fun (temporary, permanent, ordered) id ->
           visit temporary permanent ordered id)
  in
  List.rev reversed |> List.filter_map ~f:(Map.find specifications)
;;

let runtime_map
      ~sw
      ~host
      ~manifest
      ~approval_provider
      ~admin_policy
      ~approval_store
      ~extensions
      ~model_completion
      runtimes
  =
  Result.bind
    (Audit_sink.session_sequence_counter ~session_dir:host.Host.session_dir
     |> Result.map_error ~f:(fun error ->
       [ { code = error.code; message = error.message; runtime_id = None } ]))
    ~f:(fun audit_sequence ->
      let runtime_instances = String.Table.create () in
      let extension_bindings = ref [] in
      let register_extension_instance ~runtime_id ~lifecycle instance =
        match lifecycle with
        | Chatmd_shell_spec.Shell_spec.Invocation -> ()
        | Runtime | Session ->
          let runtime_id =
            match lifecycle with
            | Runtime -> "*"
            | Session -> runtime_id
            | Invocation -> assert false
          in
          if
            not
              (List.exists !extension_bindings ~f:(fun binding ->
                 phys_equal binding.instance instance))
          then extension_bindings := { runtime_id; instance } :: !extension_bindings
      in
      runtime_order manifest runtimes
      |> List.fold_result ~init:String.Map.empty ~f:(fun worker_runtimes specification ->
        instantiate_runtime
          ~sw
          ~host
          ~manifest
          ~approval_provider
          ~admin_policy
          ~approval_store
          ~audit_sequence
          ~extensions
          ~runtime_instances
          ~register_extension_instance
          ~worker_runtimes
          ~model_completion
          specification
        |> Result.map_error ~f:(List.map ~f:runtime_error)
        |> Result.map ~f:(fun runtime ->
          Map.set worker_runtimes ~key:(Runtime.id runtime) ~data:runtime))
      |> Result.map ~f:(fun runtimes -> runtimes, List.rev !extension_bindings))
;;

let extension_map scripts =
  List.map scripts ~f:(fun script ->
    Chatml_extension.compile ~script
    |> Result.map ~f:(fun compiled -> Chatml_extension.id compiled, compiled))
  |> Result.all
  |> Result.map_error ~f:(List.map ~f:diagnostic_error)
  |> Result.map ~f:String.Map.of_alist_exn
;;

let admin_error (violation : Admin_policy.violation) =
  { code = violation.code
  ; message =
      sprintf
        "requested=%s; ceiling=%s; policy=%s; remediation=%s"
        violation.requested
        violation.ceiling
        violation.policy_source
        violation.remediation
  ; runtime_id = violation.runtime_id
  }
;;

let prepare_with_policy ~admin_policy ~manifest ~material =
  match MC.effective_runtimes material ~manifest with
  | Error diagnostics -> Error (List.map diagnostics ~f:diagnostic_error)
  | Ok runtimes ->
    Result.bind
      (Admin_policy.evaluate admin_policy ~manifest ~runtimes
       |> Result.map_error ~f:(List.map ~f:admin_error))
      ~f:(fun () ->
        match MC.effective_scripts material ~manifest with
        | Error diagnostics -> Error (List.map diagnostics ~f:diagnostic_error)
        | Ok scripts ->
          Result.map (extension_map scripts) ~f:(fun extensions ->
            { manifest; runtimes; extensions; admin_policy }))
;;

let prepare ~manifest ~material =
  prepare_with_policy ~admin_policy:Admin_policy.permissive ~manifest ~material
;;

let tool_map manifest =
  List.map manifest.M.payload.tools ~f:(fun tool -> tool.T.name, tool)
  |> String.Map.of_alist_exn
;;

let script_error ?runtime_id code message = Error { code; message; runtime_id }

let runtime_for_tool runtimes (tool : T.t) =
  Map.find runtimes tool.runtime
  |> Result.of_option
       ~error:
         { code = "shell.tool_runtime_missing"
         ; message = "shell tool runtime is not instantiated: " ^ tool.runtime
         ; runtime_id = Some tool.runtime
         }
;;

let prepare_executable runtime path =
  Runtime.resolve_executable runtime path
  |> Result.map_error ~f:(fun message ->
    { code = "shell.script_executable_unavailable"
    ; message
    ; runtime_id = Some (Runtime.id runtime)
    })
;;

let host_result runtime result =
  Result.map_error result ~f:(fun error ->
    { code = error.Host.code
    ; message = error.message
    ; runtime_id = Some (Runtime.id runtime)
    })
;;

let load_script runtime (tool : T.t) specification =
  Runtime.load_file
    runtime
    ~source:tool.source
    ~max_bytes:specification.T.max_source_bytes
    specification.script
  |> host_result runtime
;;

let script_command runtime (tool : T.t) specification identity =
  match specification.T.interpreter with
  | Some interpreter ->
    Runtime.resolve_path runtime ~source:tool.source interpreter
    |> host_result runtime
    |> Result.map ~f:(fun path ->
      path, specification.fixed_arguments @ [ identity.Runtime.path ])
  | None when specification.executable -> Ok (identity.path, specification.fixed_arguments)
  | None ->
    script_error
      ~runtime_id:(Runtime.id runtime)
      "shell.script_not_executable"
      "script mode requires an interpreter or executable=true"
;;

let script tool specification identity executable arguments_before_model =
  { tool_name = tool.T.name
  ; path = identity.Runtime.path
  ; verification_path = identity.verification_path
  ; sha256 = identity.sha256
  ; size = identity.size
  ; executable_path = executable.Shell_access.Executable.canonical_path
  ; executable_sha256 = executable.fingerprint.sha256
  ; arguments_before_model
  ; max_source_bytes = specification.T.max_source_bytes
  }
;;

let prepare_script runtimes (tool : T.t) specification =
  let open Result.Let_syntax in
  let%bind runtime = runtime_for_tool runtimes tool in
  let%bind identity = load_script runtime tool specification in
  let%bind executable_path, arguments =
    script_command runtime tool specification identity
  in
  let%map executable = prepare_executable runtime executable_path in
  script tool specification identity executable arguments
;;

let script_map runtimes tools =
  List.filter_map tools ~f:(fun (tool : T.t) ->
    match tool.mode with
    | Script_file specification -> Some (prepare_script runtimes tool specification)
    | Fixed _ | Structured | Chain | Raw _ -> None)
  |> Result.all
  |> Result.map_error ~f:List.return
  |> Result.map ~f:(fun scripts ->
    List.map scripts ~f:(fun script -> script.tool_name, script)
    |> String.Map.of_alist_exn)
;;

let create manifest runtimes scripts extension_bindings =
  let tools = tool_map manifest in
  let inspection =
    { manifest_sha256 = manifest.M.sha256
    ; runtime_ids = Map.keys runtimes
    ; tool_names = Map.keys tools
    ; scripts = Map.data scripts
    }
  in
  { manifest; runtimes; tools; scripts; extension_bindings; inspection }
;;

let extension_kind instance =
  Chatml_extension.instance_kind instance
  |> Chatmd_shell_spec.Chatmd_script_spec.kind_to_string
;;

let snapshot_matches manifest binding snapshot =
  let instance = binding.instance in
  String.equal
    snapshot.Session.Shell_state.Extension_snapshot.runtime_id
    binding.runtime_id
  && String.equal snapshot.extension_id (Chatml_extension.instance_id instance)
  && String.equal snapshot.extension_kind (extension_kind instance)
  && String.equal snapshot.manifest_sha256 manifest.M.sha256
  && String.equal
       snapshot.source_sha256
       (Chatml_extension.instance_source_sha256 instance)
;;

let captured_at_ns host =
  Eio.Time.now (Eio.Stdenv.clock host.Host.env)
  |> Time_ns.Span.of_sec
  |> Time_ns.of_span_since_epoch
  |> Time_ns.to_int63_ns_since_epoch
  |> Int63.to_int64
;;

let persisted_snapshot host manifest binding state =
  let instance = binding.instance in
  Session.Shell_state.Extension_snapshot.
    { extension_id = Chatml_extension.instance_id instance
    ; extension_kind = extension_kind instance
    ; runtime_id = binding.runtime_id
    ; manifest_sha256 = manifest.M.sha256
    ; source_sha256 = Chatml_extension.instance_source_sha256 instance
    ; state
    ; captured_at_ns = captured_at_ns host
    }
;;

let restore_binding manifest snapshots binding =
  match List.filter snapshots ~f:(snapshot_matches manifest binding) with
  | [] -> Ok ()
  | [ snapshot ] ->
    Chatml_extension.restore binding.instance snapshot.state
    |> Result.map_error ~f:(fun diagnostic ->
      { code = diagnostic.D.code
      ; message = diagnostic.message
      ; runtime_id = Some binding.runtime_id
      })
  | _ :: _ :: _ ->
    Error
      { code = "shell.extension_snapshot_duplicate"
      ; message = "multiple persisted snapshots match one stateful shell extension"
      ; runtime_id = Some binding.runtime_id
      }
;;

let replace_snapshot manifest binding snapshot snapshots =
  snapshot
  :: List.filter snapshots ~f:(fun candidate ->
    not (snapshot_matches manifest binding candidate))
;;

let configure_snapshot_handler host manifest snapshots persist binding =
  Chatml_extension.set_snapshot_handler binding.instance (fun state ->
    let snapshot = persisted_snapshot host manifest binding state in
    let updated = replace_snapshot manifest binding snapshot !snapshots in
    Result.map (persist updated) ~f:(fun () -> snapshots := updated))
;;

let configure_extension_snapshots host manifest extension_bindings initial persist =
  let snapshots = ref initial in
  Result.bind
    (List.map extension_bindings ~f:(restore_binding manifest initial) |> Result.all)
    ~f:(fun _ ->
      Option.iter persist ~f:(fun persist ->
        List.iter extension_bindings ~f:(fun binding ->
          configure_snapshot_handler host manifest snapshots persist binding));
      Ok ())
;;

let instantiate_prepared
      ~sw
      ~host
      ~grant
      ~approval_provider
      ?(approval_store = Shell_access.Approval.create_store ())
      ?model_completion
      ?(extension_snapshots = [])
      ?persist_extension_snapshots
      prepared
  =
  let manifest = prepared.manifest in
  match Manifest_authorizer.verify grant manifest with
  | Error error -> Error [ authorizer_error error ]
  | Ok () ->
    Result.bind
      (runtime_map
         ~sw
         ~host
         ~manifest
         ~approval_provider
         ~admin_policy:prepared.admin_policy
         ~approval_store
         ~extensions:prepared.extensions
         ~model_completion
         prepared.runtimes)
      ~f:(fun (runtimes, extension_bindings) ->
        Result.bind
          (configure_extension_snapshots
             host
             manifest
             extension_bindings
             extension_snapshots
             persist_extension_snapshots
           |> Result.map_error ~f:List.return)
          ~f:(fun () ->
            Result.map (script_map runtimes manifest.payload.tools) ~f:(fun scripts ->
              create manifest runtimes scripts extension_bindings)))
;;

let instantiate_internal
      ~sw
      ~host
      ~manifest
      ~grant
      ~material
      ~approval_provider
      ~admin_policy
      ~approval_store
      ~model_completion
  =
  Result.bind (prepare_with_policy ~manifest ~material ~admin_policy) ~f:(fun prepared ->
    instantiate_prepared
      ~sw
      ~host
      ~grant
      ~approval_provider
      ~approval_store
      ?model_completion
      prepared)
;;

let instantiate ~sw ~host ~manifest ~grant ~material ~approval_provider =
  instantiate_internal
    ~sw
    ~host
    ~manifest
    ~grant
    ~material
    ~approval_provider
    ~admin_policy:Admin_policy.permissive
    ~approval_store:(Shell_access.Approval.create_store ())
    ~model_completion:None
;;

let instantiate_with_model_completion
      ~sw
      ~host
      ~manifest
      ~grant
      ~material
      ~approval_provider
      ~model_completion
  =
  instantiate_internal
    ~sw
    ~host
    ~manifest
    ~grant
    ~material
    ~approval_provider
    ~admin_policy:Admin_policy.permissive
    ~approval_store:(Shell_access.Approval.create_store ())
    ~model_completion:(Some model_completion)
;;

let manifest (t : t) = t.manifest
let inspection (t : t) = t.inspection
let runtime (t : t) id = Map.find t.runtimes id
let tool (t : t) name = Map.find t.tools name
let script (t : t) name = Map.find t.scripts name
let runtimes (t : t) = Map.data t.runtimes
let tools (t : t) = Map.data t.tools
