open Core
module MC = Chatmd_shell_spec.Manifest_compiler
module Reference = Chatmd_shell_spec.Script_reference
module Registry = Shell_runtime.Registry

type t =
  { registry : Registry.prepared
  ; programs : Shell_program.t list
  }

let registry t = t.registry

let prepare ?startup ?programs ~env ~limits ~checkpoint ~admin_policy ~manifest material =
  Startup_compilation.with_context
    ?startup
    ~limits
    ~env
    ~checkpoint
    ~map_error:(fun e ->
      [ Registry.
          { code = e.Chatml_compilation.code; message = e.message; runtime_id = None }
      ])
    ~f:(fun startup ->
      Startup_compilation.within
        startup
        ~map_error:(fun e ->
          [ Registry.
              { code = e.Chatml_compilation.code; message = e.message; runtime_id = None }
          ])
        ~f:(fun () ->
          let checkpoint () = Startup_compilation.checkpoint startup in
          let open Result.Let_syntax in
          match programs with
          | None ->
            let compile_extension script =
              let%bind () =
                Startup_compilation.admit_text
                  startup
                  (Chatmd_shell_spec.Chatmd_script_spec.source_text script)
                |> Result.map_error ~f:(fun e ->
                  [ Chatmd_shell_spec.Diagnostic.error
                      ~source:script.source_ref
                      ~code:e.Chatml_compilation.code
                      e.message
                  ])
              in
              let%bind limits =
                Startup_compilation.remaining_limits startup
                |> Result.map_error ~f:(fun e ->
                  [ Chatmd_shell_spec.Diagnostic.error
                      ~source:script.source_ref
                      ~code:e.Chatml_compilation.code
                      e.message
                  ])
              in
              Shell_runtime.Chatml_extension.compile_with_limits ~env ~limits ~script
            in
            let%map registry =
              Registry.prepare_with_compiler
                ~compile_extension
                ~admin_policy
                ~manifest
                ~material
            in
            { registry; programs = [] }
          | Some programs ->
            let%bind runtimes =
              MC.effective_runtimes material ~manifest
              |> Result.map_error
                   ~f:
                     (List.map ~f:(fun (error : Chatmd_shell_spec.Diagnostic.t) ->
                        Registry.
                          { code = error.code
                          ; message = error.message
                          ; runtime_id = None
                          }))
            in
            let entrypoints =
              List.concat_map runtimes ~f:Reference.of_runtime
              |> List.fold ~init:String.Map.empty ~f:(fun entries reference ->
                match reference.Reference.entrypoint with
                | None -> entries
                | Some entrypoint ->
                  Map.add_multi entries ~key:reference.script ~data:entrypoint)
            in
            let selected = ref [] in
            let compile_extension script =
              Shell_program.compile
                ~startup
                programs
                ~script
                ~entrypoints:
                  (Map.find_multi
                     entrypoints
                     script.Chatmd_shell_spec.Chatmd_script_spec.id)
                ~env
                ~checkpoint
                ~limits
                ~max_dependency_depth:128
              |> Result.map ~f:(fun program ->
                selected := program :: !selected;
                Shell_program.compiled program)
            in
            let%map registry =
              Registry.prepare_with_compiler
                ~compile_extension
                ~admin_policy
                ~manifest
                ~material
            in
            { registry; programs = List.rev !selected }))
    ()
;;

let admit_tools t ~capabilities ~checkpoint =
  List.fold_result t.programs ~init:() ~f:(fun () program ->
    Shell_program.admit_tools program ~capabilities ~checkpoint)
;;
