open Core
module X = Chatml_execution
module V = Chatml.Chatml_value_codec
module L = Chatml.Chatml_lang
module R = Chatml_host_runtime

let result = function
  | Ok _ -> "ok"
  | Error error -> error.X.code
;;

let%expect_test
    "JSON import rejects oversized and cyclic projections before initialization"
  =
  Eio_main.run (fun env ->
    let program =
      Chatml_compilation.compile
        ~env
        ~target:One_off_v1
        ~source:"let main input = Task.pure(input)"
        ()
      |> Result.map_error ~f:(fun error -> error.Chatml_compilation.message)
      |> Result.ok_or_failwith
    in
    let rec cyclic = `Array [ cyclic ] in
    List.iter
      [ ( "allocation"
        , `Array (List.init 1000 ~f:(fun _ -> `Null))
        , { X.default_limits with allocation_bytes = 1024 } )
      ; "cycle", cyclic, X.default_limits
      ; ( "projected depth"
        , `Object [ "value", `Null ]
        , { X.default_limits with max_depth = 2 } )
      ]
      ~f:(fun (name, input, limits) ->
        let entered = ref false in
        let outcome =
          X.with_control ~policy:(Bounded limits) ~env (fun control ->
            let argument = V.import_json ?control input in
            entered := true;
            X.run_in_scope
              ~control
              ~config:
                (R.default_runtime_config
                   ~surface:Chatml.Chatml_extension_surface.one_off_v1
                   ())
              ~program
              ~entrypoint:"main"
              ~arguments:[ argument ]
              ())
        in
        print_s [%sexp (name : string), (result outcome : string), (!entered : bool)]));
  [%expect
    {|
    (allocation chatml.allocation_limit false)
    (cycle chatml.value_limit false)
    ("projected depth" chatml.value_limit false)
    |}]
;;

let%expect_test
    "context conversion shares the budget across individually small capabilities"
  =
  Eio_main.run (fun env ->
    let context : Chat_response.Moderation.Context.t =
      { session_id = "fixture"
      ; now_ms = 0
      ; phase = Tool_invoked
      ; items = []
      ; session_meta = `Null
      ; available_tools =
          List.init 20 ~f:(fun index ->
            Chat_response.Moderation.Tool_desc.
              { name = Int.to_string index
              ; description = ""
              ; input_schema = `String (String.make 128 'x')
              })
      }
    in
    let imports = ref 0 in
    let outcome =
      X.with_control
        ~policy:(Bounded { X.default_limits with allocation_bytes = 2048 })
        ~env
        (fun control ->
           let control =
             Option.map control ~f:(fun c ->
               { c with
                 L.before_json_import =
                   (fun json ->
                     Int.incr imports;
                     c.before_json_import json)
               })
           in
           Chat_response.Moderation.Context.to_value ?control context)
    in
    print_s [%sexp (result outcome : string), (!imports > 0 && !imports < 20 : bool)]);
  [%expect {| (chatml.allocation_limit true) |}]
;;

let%expect_test
    "export rejects cyclic values and nested domain imports cannot erase parent budgets"
  =
  Eio_main.run (fun env ->
    let array = [| L.VVariant ("Null", []) |] in
    let cycle = L.VVariant ("Array", [ L.VArray array ]) in
    array.(0) <- cycle;
    print_endline
      (result (X.with_control ~env (fun control -> V.export_json ?control cycle)));
    let child_result = ref "" in
    let outcome =
      X.with_control
        ~policy:(Bounded { X.default_limits with allocation_bytes = 1024 })
        ~env
        (fun control ->
           ignore (V.import_json ?control (`String (String.make 600 'a')) : L.value);
           let context = X.capture_context () in
           child_result
           := Eio.Domain_manager.run (Eio.Stdenv.domain_mgr env) (fun () ->
                X.with_control ~policy:Unrestricted ~context ~env (fun child ->
                  V.import_json ?control:child (`String (String.make 600 'b')))
                |> result);
           Option.iter control ~f:(fun c -> c.checkpoint ()))
    in
    print_s [%sexp (!child_result : string), (result outcome : string)]);
  [%expect
    {|
    chatml.value_limit
    (chatml.allocation_limit chatml.allocation_limit)
    |}]
;;
