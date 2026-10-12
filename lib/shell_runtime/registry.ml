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
  ; runtime_order : Runtime.t list
  ; admin_policy : Admin_policy.t
  ; host : Host.t
  ; resource_check : unit -> (unit, string) Result.t
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
      ~prepare_extension
      ~check_execution
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
    ~prepare_extension
    ~check_execution
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
      ~prepare_extension
      ~check_execution
      runtimes
  =
  Result.bind
    (Audit_sink.session_sequence_counter ~session_dir:host.Host.session_dir
     |> Result.map_error ~f:(fun error ->
       [ { code = error.code; message = error.message; runtime_id = None } ]))
    ~f:(fun audit_sequence ->
      let runtime_instances = String.Table.create () in
      let extension_bindings = ref [] in
      let created_runtimes = ref [] in
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
          ~prepare_extension
          ~check_execution
          specification
        |> Result.map_error ~f:(List.map ~f:runtime_error)
        |> Result.map ~f:(fun runtime ->
          created_runtimes := runtime :: !created_runtimes;
          Map.set worker_runtimes ~key:(Runtime.id runtime) ~data:runtime))
      |> Result.map ~f:(fun runtimes ->
        runtimes, List.rev !extension_bindings, List.rev !created_runtimes))
;;

let extension_map ~compile_extension scripts =
  List.map scripts ~f:(fun script ->
    compile_extension script
    |> Result.bind ~f:(fun compiled ->
      if Chatml_extension.matches_script compiled script
      then Ok (Chatml_extension.id compiled, compiled)
      else
        Error
          [ D.error
              ~source:script.Chatmd_shell_spec.Chatmd_script_spec.source_ref
              ~code:"shell.chatml_compiled_mismatch"
              "Compiled shell hook does not match its authorized declaration"
          ]))
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

let prepare_with_compiler_bounded
      material
      ~compile_extension
      ~admin_policy
      ~manifest
      ~max_manifest_bytes
      ~checkpoint
  =
  let open Result.Let_syntax in
  checkpoint ();
  let%bind runtimes =
    MC.effective_runtimes material ~manifest
    |> Result.map_error ~f:(List.map ~f:diagnostic_error)
  in
  let%bind () =
    Admin_policy.evaluate admin_policy ~manifest ~runtimes
    |> Result.map_error ~f:(List.map ~f:admin_error)
  in
  let%bind scripts =
    MC.effective_scripts material ~manifest
    |> Result.map_error ~f:(List.map ~f:diagnostic_error)
  in
  let%bind extensions = extension_map ~compile_extension scripts in
  let hooks =
    Map.fold extensions ~init:[] ~f:(fun ~key:_ ~data hooks ->
      checkpoint ();
      List.rev_append (Chatml_extension.prepared_hooks data ~checkpoint) hooks)
  in
  let%map manifest =
    M.with_prepared_hooks manifest hooks ~max_bytes:max_manifest_bytes ~checkpoint
    |> Result.map_error ~f:(fun error ->
      [ { code = "shell.execution_manifest_invalid"
        ; message = Error.to_string_hum error
        ; runtime_id = None
        }
      ])
  in
  { manifest; runtimes; extensions; admin_policy }
;;

let prepare_with_compiler ~compile_extension ~admin_policy ~manifest ~material =
  prepare_with_compiler_bounded
    material
    ~compile_extension
    ~admin_policy
    ~manifest
    ~max_manifest_bytes:(8 * 1024 * 1024)
    ~checkpoint:(fun () -> ())
;;

let prepared_manifest (prepared : prepared) = prepared.manifest

let prepare_with_policy ~admin_policy ~manifest ~material =
  prepare_with_compiler
    ~compile_extension:(fun script -> Chatml_extension.compile ~script)
    ~admin_policy
    ~manifest
    ~material
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

let create
      ~host
      ~admin_policy
      ~resource_check
      ~runtime_order
      manifest
      runtimes
      scripts
      extension_bindings
  =
  let tools = tool_map manifest in
  let inspection =
    { manifest_sha256 = manifest.M.sha256
    ; runtime_ids = Map.keys runtimes
    ; tool_names = Map.keys tools
    ; scripts = Map.data scripts
    }
  in
  { manifest
  ; runtimes
  ; tools
  ; scripts
  ; extension_bindings
  ; inspection
  ; host
  ; admin_policy
  ; resource_check
  ; runtime_order
  }
;;

let extension_kind instance =
  Chatml_extension.instance_kind instance
  |> Chatmd_shell_spec.Chatmd_script_spec.kind_to_string
;;

let snapshot_matches ?(equal = String.equal) manifest binding snapshot =
  let instance = binding.instance in
  equal snapshot.Session.Shell_state.Extension_snapshot.runtime_id binding.runtime_id
  && equal snapshot.extension_id (Chatml_extension.instance_id instance)
  && equal snapshot.extension_kind (extension_kind instance)
  && equal snapshot.manifest_sha256 manifest.M.sha256
  && equal snapshot.source_sha256 (Chatml_extension.instance_source_revision instance)
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
    ; source_sha256 = Chatml_extension.instance_source_revision instance
    ; state
    ; captured_at_ns = captured_at_ns host
    }
;;

let restore_binding ~checkpoint ~equal manifest snapshots binding =
  match
    List.filter snapshots ~f:(fun snapshot ->
      checkpoint ();
      snapshot_matches ~equal manifest binding snapshot)
  with
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

let configure_snapshot_handler host manifest mutex snapshots persist binding =
  Chatml_extension.set_snapshot_handler binding.instance (fun state ->
    let snapshot = persisted_snapshot host manifest binding state in
    let outcome =
      Eio.Mutex.use_rw ~protect:true mutex (fun () ->
        (* Different instances have different call locks. Serialize their shared
           durable list through adoption, while keeping a failed callback from
           poisoning this publication lock. The instance owner handles rollback. *)
        try
          let updated = replace_snapshot manifest binding snapshot !snapshots in
          Ok (Result.map (persist updated) ~f:(fun () -> snapshots := updated))
        with
        | exn -> Error (exn, Backtrace.Exn.most_recent ()))
    in
    match outcome with
    | Ok result -> result
    | Error (exn, backtrace) -> Exn.raise_with_original_backtrace exn backtrace)
;;

let configure_extension_snapshots
      ?(checkpoint = ignore)
      ?(equal = String.equal)
      host
      manifest
      extension_bindings
      initial
      persist
  =
  let snapshots = ref initial in
  let mutex = Eio.Mutex.create () in
  Result.bind
    (List.map extension_bindings ~f:(fun binding ->
       checkpoint ();
       restore_binding ~checkpoint ~equal manifest initial binding)
     |> Result.all)
    ~f:(fun _ ->
      Option.iter persist ~f:(fun persist ->
        List.iter extension_bindings ~f:(fun binding ->
          configure_snapshot_handler host manifest mutex snapshots persist binding));
      Ok ())
;;

module Staged = struct
  type registry = t

  type t =
    { registry : registry
    ; gate : Activation_gate.t
    }

  let manifest t = t.registry.manifest
  let runtime t id = Map.find t.registry.runtimes id
  let script t name = Map.find t.registry.scripts name
  let tools t = Map.data t.registry.tools
  let check t = Activation_gate.check t.gate
end

let instantiate_prepared_after
      ~sw
      ~host
      ~grant
      ~approval_provider
      ?(approval_store = Shell_access.Approval.create_store ())
      ?model_completion
      ?(extension_snapshots = [])
      ?persist_extension_snapshots
      ~preflight
      prepared
  =
  let manifest = prepared.manifest in
  let gate = Activation_gate.create () in
  let instances = ref [] in
  let close () =
    Activation_gate.close gate;
    List.iter !instances ~f:Chatml_extension.close
  in
  Eio.Switch.on_release sw close;
  let prepare_extension ~lifecycle compiled =
    Chatml_extension.prepare_instance ~env:host.Host.env ~lifecycle compiled
    |> Result.map ~f:(fun instance ->
      instances := instance :: !instances;
      instance)
  in
  let result =
    Exn.protect
      ~f:(fun () ->
        let open Result.Let_syntax in
        let%bind () =
          Manifest_authorizer.verify grant manifest
          |> Result.map_error ~f:(fun error -> [ authorizer_error error ])
        in
        let%bind runtimes, extension_bindings, runtime_order =
          runtime_map
            ~sw
            ~host
            ~manifest
            ~approval_provider
            ~admin_policy:prepared.admin_policy
            ~approval_store
            ~extensions:prepared.extensions
            ~model_completion
            ~prepare_extension
            ~check_execution:(fun () -> Activation_gate.check gate)
            prepared.runtimes
        in
        Eio.Fiber.check ();
        let%bind scripts = script_map runtimes manifest.payload.tools in
        let registry =
          create
            ~host
            ~admin_policy:prepared.admin_policy
            ~resource_check:(fun () -> Activation_gate.check gate)
            ~runtime_order
            manifest
            runtimes
            scripts
            extension_bindings
        in
        let staged = Staged.{ registry; gate } in
        let%bind admitted = preflight staged in
        Eio.Fiber.check ();
        let%bind () =
          List.fold_result (List.rev !instances) ~init:() ~f:(fun () instance ->
            Eio.Fiber.check ();
            Chatml_extension.activate instance
            |> Result.map_error ~f:(fun diagnostic -> [ diagnostic_error diagnostic ]))
        in
        let%bind () =
          configure_extension_snapshots
            host
            manifest
            extension_bindings
            extension_snapshots
            persist_extension_snapshots
          |> Result.map_error ~f:List.return
        in
        Eio.Fiber.check ();
        (* Install last so ready access is revoked before native release hooks. *)
        Eio.Switch.on_release sw close;
        let%map () =
          Activation_gate.ready gate
          |> Result.map_error ~f:(fun message ->
            [ { code = "shell.registry_closed"; message; runtime_id = None } ])
        in
        registry, admitted)
      ~finally:(fun () ->
        Activation_gate.fail gate;
        match Activation_gate.check gate with
        | Ok () -> ()
        | Error _ -> List.iter !instances ~f:Chatml_extension.close)
  in
  result
;;

let instantiate_prepared
      ~sw
      ~host
      ~grant
      ~approval_provider
      ?approval_store
      ?model_completion
      ?extension_snapshots
      ?persist_extension_snapshots
      prepared
  =
  instantiate_prepared_after
    ~sw
    ~host
    ~grant
    ~approval_provider
    ?approval_store
    ?model_completion
    ?extension_snapshots
    ?persist_extension_snapshots
    ~preflight:(fun _ -> Ok ())
    prepared
  |> Result.map ~f:fst
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

let redact_tool_input t ~tool_name value =
  match tool t tool_name with
  | None -> value
  | Some tool ->
    (match runtime t tool.T.runtime with
     | None -> value
     | Some runtime -> Runtime.redact runtime value)
;;

let script (t : t) name = Map.find t.scripts name
let runtimes (t : t) = Map.data t.runtimes
let tools (t : t) = Map.data t.tools

module Code_view = struct
  type registry = t

  type t =
    { registry : registry
    ; gate : Activation_gate.t
    ; work_steps : int
    }

  let work_steps t = t.work_steps

  let check t =
    Result.bind (t.registry.resource_check ()) ~f:(fun () -> Activation_gate.check t.gate)
  ;;

  module Execution = struct
    type view = t

    type t =
      { view : view
      ; registry : registry
      ; active : bool Atomic.t
      ; checkpoint : unit -> unit
      }

    let check t =
      if Atomic.get t.active
      then check t.view
      else Error "shell code admission has expired"
    ;;

    let registry t = Result.map (check t) ~f:(fun () -> t.registry)

    let prepare_executor t config =
      let open Result.Let_syntax in
      let%bind () = check t in
      let selected =
        List.find_map t.view.registry.runtime_order ~f:(fun runtime ->
          t.checkpoint ();
          Eio.Fiber.check ();
          let candidate = Runtime.executor_config runtime in
          if Shell_access.Executor.same_resource_owner config candidate
          then Some (Shell_access.Executor.code candidate)
          else None)
      in
      let config =
        match selected with
        | None -> config
        | Some code -> Shell_access.Executor.with_code config code
      in
      let%map () = check t in
      Shell_access.Executor.with_execution_check config ~check:(fun () -> check t)
    ;;
  end

  let with_execution t ~checkpoint f =
    let open Result.Let_syntax in
    Eio.Fiber.check ();
    checkpoint ();
    Eio.Fiber.check ();
    let%bind () = check t in
    let active = Atomic.make true in
    let admission () =
      if Atomic.get active then check t else Error "shell code admission has expired"
    in
    Exn.protect
      ~finally:(fun () -> Atomic.set active false)
      ~f:(fun () ->
        let wrap runtime =
          checkpoint ();
          Eio.Fiber.check ();
          Runtime.with_execution_check runtime ~check:admission
        in
        let runtimes = Map.map t.registry.runtimes ~f:wrap in
        let runtime_order = List.map t.registry.runtime_order ~f:wrap in
        let registry =
          { t.registry with runtimes; runtime_order; resource_check = admission }
        in
        let execution : Execution.t = { view = t; registry; active; checkpoint } in
        let%map () = admission () in
        f execution)
  ;;
end

let rebind_code
      original
      ~prepared
      ~grant
      ~sw
      ?(extension_snapshots = [])
      ?persist_extension_snapshots
      ~max_work_steps
      ~checkpoint
      ()
  =
  let module B = Registry_code_budget in
  let manifest = prepared.manifest in
  let gate = Activation_gate.create () in
  let instances = ref [] in
  let close () =
    Activation_gate.close gate;
    List.iter !instances ~f:Chatml_extension.close
  in
  Eio.Switch.check sw;
  Eio.Switch.on_release sw close;
  let result =
    Exn.protect
      ~finally:(fun () ->
        Activation_gate.fail gate;
        match Activation_gate.check gate with
        | Ok () -> ()
        | Error _ -> close ())
      ~f:(fun () ->
        B.run ~max_work_steps ~checkpoint (fun budget ->
          let poll () = B.work budget in
          let error code message = [ { code; message; runtime_id = None } ] in
          let open Result.Let_syntax in
          poll ();
          let%bind () =
            original.resource_check ()
            |> Result.map_error ~f:(error "shell.registry_closed")
          in
          let%bind () =
            Manifest_authorizer.verify grant manifest
            |> Result.map_error ~f:(fun error -> [ authorizer_error error ])
          in
          let%bind validated =
            Registry_code_validation.create
              ~original_manifest:original.manifest
              ~original_runtimes:original.runtime_order
              ~original_tools:original.tools
              ~original_policy:original.admin_policy
              ~candidate_manifest:manifest
              ~candidate_runtimes:prepared.runtimes
              ~candidate_policy:prepared.admin_policy
              ~budget
            |> Result.map_error ~f:(fun error ->
              [ { code = error.Registry_resource_footprint.Error.code
                ; message = "native resource footprint changed: " ^ error.field
                ; runtime_id = error.runtime_id
                }
              ])
          in
          let runtime_instances = String.Table.create () in
          let extension_bindings = ref [] in
          let register_extension_instance ~runtime_id ~lifecycle instance =
            poll ();
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
                     poll ();
                     phys_equal binding.instance instance))
              then (
                poll ();
                extension_bindings := { runtime_id; instance } :: !extension_bindings)
          in
          let prepare_extension ~lifecycle compiled =
            poll ();
            poll ();
            Chatml_extension.prepare_instance ~env:original.host.env ~lifecycle compiled
            |> Result.map ~f:(fun instance ->
              instances := instance :: !instances;
              instance)
          in
          let check_execution () =
            Result.bind (original.resource_check ()) ~f:(fun () ->
              Activation_gate.check gate)
          in
          (* Reuse the original map skeleton and native creation order. Only the
             temporary comparator closes over this invocation's work ledger. *)
          let module Key = struct
            module T = struct
              type t = string [@@deriving sexp_of]

              let compare = B.compare_string budget
            end

            include T
            include Comparator.Make (T)
          end
          in
          let empty = Map.empty (module Key) in
          let skeleton values =
            Map.filter_mapi original.runtimes ~f:(fun ~key ~data:_ ->
              poll ();
              Map.find values key)
          in
          let%bind created, ordered =
            List.fold_result
              (Registry_code_validation.runtimes validated)
              ~init:(empty, [])
              ~f:(fun (created, ordered) (runtime, specification) ->
                poll ();
                let worker_runtimes = skeleton created in
                let%map runtime =
                  Runtime.rebind_code
                    runtime
                    ~manifest
                    ~specification
                    ~extensions:prepared.extensions
                    ~runtime_instances
                    ~register_extension_instance
                    ~prepare_extension
                    ~worker_runtimes
                    ~check_execution
                    ~checkpoint:poll
                  |> Result.map_error ~f:(List.map ~f:runtime_error)
                in
                poll ();
                ( Map.set created ~key:(Runtime.id runtime) ~data:runtime
                , runtime :: ordered ))
          in
          let runtimes = skeleton created in
          let runtime_order =
            List.fold ordered ~init:[] ~f:(fun result value ->
              poll ();
              value :: result)
          in
          let extension_bindings =
            List.fold !extension_bindings ~init:[] ~f:(fun result value ->
              poll ();
              value :: result)
          in
          let tools = Registry_code_validation.tools validated in
          let inspection =
            { original.inspection with manifest_sha256 = manifest.M.sha256 }
          in
          let registry =
            { original with
              manifest
            ; runtimes
            ; runtime_order
            ; tools
            ; extension_bindings
            ; inspection
            }
          in
          (* Snapshot descriptors and matching scans share this same ledger;
             restored state is additionally bounded by each compiled instance. *)
          List.iter extension_snapshots ~f:(fun snapshot ->
            poll ();
            let open Session.Shell_state.Extension_snapshot in
            List.iter
              [ snapshot.extension_id
              ; snapshot.extension_kind
              ; snapshot.runtime_id
              ; snapshot.manifest_sha256
              ; snapshot.source_sha256
              ]
              ~f:(fun value -> ignore (B.equal_string budget value value : bool)));
          let ordered_instances =
            List.fold !instances ~init:[] ~f:(fun ordered instance ->
              poll ();
              instance :: ordered)
          in
          let%bind () =
            List.fold_result ordered_instances ~init:() ~f:(fun () instance ->
              poll ();
              Chatml_extension.activate instance
              |> Result.map_error ~f:(fun diagnostic -> [ diagnostic_error diagnostic ]))
          in
          let%bind () =
            configure_extension_snapshots
              ~checkpoint:poll
              ~equal:(B.equal_string budget)
              original.host
              manifest
              extension_bindings
              extension_snapshots
              persist_extension_snapshots
            |> Result.map_error ~f:List.return
          in
          poll ();
          let%map () =
            Activation_gate.ready gate
            |> Result.map_error ~f:(error "shell.registry_closed")
          in
          registry)
        |> fun result ->
        (match result with
         | Ok (Ok _) -> ()
         | Error _ | Ok (Error _) -> close ());
        result)
  in
  match result with
  | Error Invalid_limit ->
    Error
      [ { code = "shell.code_invalid_limit"
        ; message = "invalid code work allowance"
        ; runtime_id = None
        }
      ]
  | Error Work_limit ->
    Error
      [ { code = "shell.code_work_limit"
        ; message = "code work allowance exhausted"
        ; runtime_id = None
        }
      ]
  | Ok (Error errors) -> Error errors
  | Ok (Ok (registry, work_steps)) -> Ok ({ registry; gate; work_steps } : Code_view.t)
;;
