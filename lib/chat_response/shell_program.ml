open Core
module H = Shell_runtime.Chatml_extension
module Spec = Chatmd_shell_spec.Chatmd_script_spec
module D = Chatmd_shell_spec.Diagnostic

type t =
  { compiled : H.compiled
  ; selected : Captured_script.t list
  }

let compile
      ?startup
      inventory
      ~script
      ~entrypoints
      ~env
      ~checkpoint
      ~limits
      ~max_dependency_depth
  =
  Startup_compilation.with_context
    ?startup
    ~limits
    ~env
    ~checkpoint
    ~map_error:(fun e ->
      [ D.error ~source:script.Spec.source_ref ~code:e.Chatml_compilation.code e.message ])
    ~f:(fun startup ->
      Startup_compilation.within
        startup
        ~map_error:(fun e ->
          [ D.error
              ~source:script.Spec.source_ref
              ~code:e.Chatml_compilation.code
              e.message
          ])
        ~f:(fun () ->
          let checkpoint () = Startup_compilation.checkpoint startup in
          let open Result.Let_syntax in
          let%bind () =
            Startup_compilation.admit_text startup (Spec.source_text script)
            |> Result.map_error ~f:(fun e ->
              [ D.error
                  ~source:script.source_ref
                  ~code:e.Chatml_compilation.code
                  e.message
              ])
          in
          let default = H.default_entrypoint script.Spec.kind in
          let alternatives =
            entrypoints
            |> List.dedup_and_sort ~compare:String.compare
            |> List.filter ~f:(fun name -> not (String.equal name default))
          in
          let compile entrypoint =
            checkpoint ();
            let%bind contract =
              H.compiler_contract script.kind ~entrypoint ~checkpoint
              |> Result.map_error ~f:List.return
            in
            let%bind selected =
              Captured_script.select
                inventory
                ~declaration:(Shell script)
                ~contract
                ~checkpoint
              |> Result.map_error ~f:(fun error ->
                [ D.error
                    ~source:script.source_ref
                    ~code:error.Chatml_compilation.code
                    error.message
                ])
            in
            let%bind () =
              Startup_compilation.admit_prepared
                startup
                (Captured_script.prepared selected)
              |> Result.map_error ~f:(fun e ->
                [ D.error
                    ~source:script.source_ref
                    ~code:e.Chatml_compilation.code
                    e.message
                ])
            in
            let%bind limits =
              Startup_compilation.remaining_limits startup
              |> Result.map_error ~f:(fun e ->
                [ D.error
                    ~source:script.source_ref
                    ~code:e.Chatml_compilation.code
                    e.message
                ])
            in
            let%map compiled =
              H.compile_prepared
                (Captured_script.prepared selected)
                ~script
                ~entrypoint
                ~limits
                ~max_dependency_depth
                ~env
                ()
            in
            compiled, selected
          in
          let%bind compiled, selected = compile default in
          let%bind alternatives =
            List.fold_result alternatives ~init:[] ~f:(fun prepared name ->
              let%map alternative = compile name in
              alternative :: prepared)
            |> Result.map ~f:List.rev
          in
          let%map compiled =
            match alternatives with
            | [] -> Ok compiled
            | _ ->
              H.with_prepared_entrypoints
                compiled
                ~alternatives:(List.map alternatives ~f:fst)
                ~checkpoint
              |> Result.map_error ~f:List.return
          in
          { compiled; selected = selected :: List.map alternatives ~f:snd }))
    ()
;;

let compiled t = t.compiled

let admit_tools t ~capabilities ~checkpoint =
  List.fold_result t.selected ~init:() ~f:(fun () selected ->
    Captured_script.admit_tools selected ~capabilities ~checkpoint)
;;
