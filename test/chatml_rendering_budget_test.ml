open Core
module X = Chatml_execution
module R = Chatml_host_runtime
module L = Chatml.Chatml_lang
module B = Chatml.Chatml_builtin_spec

let run env ?(policy = X.Bounded X.default_limits) source input =
  let surface = Chatml.Chatml_builtin_surface.moderator_surface in
  let program = R.compile_script ~surface ~source () |> Result.ok_or_failwith in
  X.run
    ~env
    ~policy
    ~config:(R.default_runtime_config ~surface ())
    ~program
    ~entrypoint:"main"
    ~arguments:[ input ]
    ()
;;

let outcome = function
  | Ok _ -> "ok"
  | Error error -> error.X.code
;;

let%expect_test "rendering deferred tasks respects host limits before output" =
  Eio_main.run (fun env ->
    let prefix =
      {|let rec chain n task =
  if n == 0 then task else chain(n - 1, Task.map(task, fun value -> value))
let main input =
  let pending = chain(200, Task.pure(input)) in
|}
    in
    let printed = ref 0 in
    B.set_print_sink (fun _ -> Int.incr printed);
    Exn.protect ~finally:B.clear_print_sink ~f:(fun () ->
      List.iter
        [ "to_string", "Task.pure(to_string(pending))"
        ; "print", "let ignored = print(pending) in Task.pure(input)"
        ]
        ~f:(fun (name, suffix) ->
          List.iter
            [ "small", X.Bounded { X.default_limits with max_depth = 32 }
            ; "large", X.Bounded { X.default_limits with max_depth = 512 }
            ; "trusted", X.Unrestricted
            ]
            ~f:(fun (policy_name, policy) ->
              let result = run env ~policy (prefix ^ suffix) L.VUnit in
              print_s
                [%sexp (name : string), (policy_name : string), (outcome result : string)]));
      print_s [%sexp "print sink calls", (!printed : int)]));
  [%expect
    {|
    (to_string small chatml.value_limit)
    (to_string large ok)
    (to_string trusted ok)
    (print small chatml.value_limit)
    (print large ok)
    (print trusted ok)
    ("print sink calls" 2)
    |}]
;;

let%expect_test "a mutated cycle hidden in a deferred task is rejected before rendering" =
  Eio_main.run (fun env ->
    let prefix =
      {|let main input =
  let values = [input] in
  let pending = Task.pure(`Array(values)) in
  let ignored = values[0] <- `Array(values) in
|}
    in
    let printed = ref 0 in
    B.set_print_sink (fun _ -> Int.incr printed);
    Exn.protect ~finally:B.clear_print_sink ~f:(fun () ->
      List.iter
        [ "Task.pure(to_string(pending))"
        ; "let ignored = print(pending) in Task.pure(())"
        ]
        ~f:(fun suffix ->
          print_endline (outcome (run env (prefix ^ suffix) (L.VVariant ("Null", [])))));
      print_s [%sexp "print sink calls", (!printed : int)]));
  [%expect
    {|
    chatml.value_limit
    chatml.value_limit
    ("print sink calls" 0)
    |}]
;;

let%expect_test "validation pays the same parser allocation budget as parsing" =
  Eio_main.run (fun env ->
    let input = L.VString ("\"" ^ String.make 4096 'x' ^ "\"") in
    List.iter [ "parse"; "parse_opt"; "validate" ] ~f:(fun operation ->
      let source =
        "let main input = let ignored = Json." ^ operation ^ "(input) in Task.pure(())"
      in
      List.iter
        [ "small", X.Bounded { X.default_limits with allocation_bytes = 32768 }
        ; "default", X.Bounded X.default_limits
        ]
        ~f:(fun (name, policy) ->
          print_s
            [%sexp
              (operation : string)
            , (name : string)
            , (outcome (run env ~policy source input) : string)])));
  [%expect
    {|
    (parse small chatml.allocation_limit)
    (parse default ok)
    (parse_opt small chatml.allocation_limit)
    (parse_opt default ok)
    (validate small chatml.allocation_limit)
    (validate default ok)
    |}]
;;

let%expect_test "diagnostic previews terminate on cycles and cap aggregate output" =
  let values = [| L.VUnit |] in
  let task = L.VTask (L.TPure (L.VArray values)) in
  values.(0) <- task;
  print_endline (B.value_to_debug_string ~max_depth:4 task);
  print_endline (B.value_to_debug_string ~max_nodes:4 ~max_depth:100 task);
  print_endline
    (B.values_to_debug_string ~max_bytes:32 [ task; L.VString (String.make 100_000 'x') ]);
  print_s
    [%sexp
      (String.length
         (B.values_to_debug_string (List.init 10_000 ~f:(fun _ -> L.VString "hello")))
       <= 4096
       : bool)];
  [%expect
    {|
    pure([|pure([|...|])|])
    pure([|pure([|...
    pure([|pure([|pure([|pure([|p...
    true
    |}]
;;

let%expect_test
    "JSON preflight bounds nesting without treating string contents as structure"
  =
  Eio_main.run (fun env ->
    let nested = String.make 200 '[' ^ "null" ^ String.make 200 ']' in
    let quoted = Jsonaf.to_string (`String ("\\\"" ^ nested ^ "\"\\")) in
    List.iter [ "parse"; "parse_opt"; "validate" ] ~f:(fun operation ->
      let source =
        "let main input = let ignored = Json." ^ operation ^ "(input) in Task.pure(())"
      in
      List.iter
        [ "nested", nested; "quoted", quoted; "invalid", "[broken" ]
        ~f:(fun (name, input) ->
          print_s
            [%sexp
              (operation : string)
            , (name : string)
            , (outcome (run env source (L.VString input)) : string)])));
  [%expect
    {|
    (parse nested chatml.value_limit)
    (parse quoted ok)
    (parse invalid chatml.execution_failed)
    (parse_opt nested chatml.value_limit)
    (parse_opt quoted ok)
    (parse_opt invalid ok)
    (validate nested chatml.value_limit)
    (validate quoted ok)
    (validate invalid ok)
    |}]
;;

let%expect_test "runtime diagnostics are lazy, bounded and do not invalidate commit" =
  let module D = Chatml.Chatml_debug_log in
  D.clear_sink ();
  D.emit (fun () -> failwith "disabled diagnostics evaluated their payload");
  let program =
    R.compile_script
      ~source:
        "let initial_state = 0\nlet on_event context state event = Task.pure(state + 1)"
      ()
    |> Result.ok_or_failwith
  in
  let session =
    R.instantiate_session
      (R.default_runtime_config ())
      program
      ~entrypoints:{ initial_state_name = "initial_state"; on_event_name = "on_event" }
    |> Result.ok_or_failwith
  in
  let cyclic = [| L.VUnit |] in
  cyclic.(0) <- L.VArray cyclic;
  let context = L.VRecord (String.Map.singleton "phase" (L.VString "turn_start")) in
  let lines = ref [] in
  let installs = ref 0 in
  let commit_logs = ref 0 in
  D.set_sink (fun line ->
    lines := line :: !lines;
    match String.is_substring line ~substring:"recent_effects=" with
    | false -> ()
    | true ->
      Int.incr commit_logs;
      failwith "diagnostic sink unavailable after commit");
  Exn.protect ~finally:D.clear_sink ~f:(fun () ->
    R.restore session ~state:(L.VInt 0) ~queued_events:[ L.VArray cyclic ] ~halted:false
    |> Result.ok_or_failwith;
    R.handle_event
      ~prepare_commit:(fun ~local_effects:_ -> Ok (fun () -> Int.incr installs))
      session
      ~context
      ~event:(L.VArray cyclic)
    |> Result.ok_or_failwith;
    print_s
      [%sexp
        (B.value_to_debug_string (R.current_state session) : string)
      , (!installs : int)
      , (!commit_logs : int)
      , (List.for_all !lines ~f:(fun line -> String.length line < 20_000) : bool)]);
  [%expect {| (1 1 1 true) |}]
;;

let%expect_test "allocation preflight stops filtering before invoking the predicate" =
  Eio_main.run (fun env ->
    let calls = ref 0 in
    B.set_print_sink (fun _ -> Int.incr calls);
    Exn.protect ~finally:B.clear_print_sink ~f:(fun () ->
      let result =
        run
          env
          ~policy:(X.Bounded { X.default_limits with allocation_bytes = 1024 })
          {|let main input =
  let ignored = Array.filter(input, fun value -> let ignored = print(value) in true) in
  Task.pure(())|}
          (L.VArray (Array.create ~len:100 L.VUnit))
      in
      print_s [%sexp (outcome result : string), (!calls : int)]));
  [%expect {| (chatml.allocation_limit 0) |}]
;;

let%expect_test "pretty JSON reserves indentation as well as compact escaping" =
  Eio_main.run (fun env ->
    let rec nested depth =
      match depth with
      | 0 -> L.VVariant ("Null", [])
      | _ -> L.VVariant ("Array", [ L.VArray [| nested (depth - 1) |] ])
    in
    List.iter [ "stringify"; "pretty" ] ~f:(fun operation ->
      let result =
        run
          env
          ~policy:(X.Bounded { X.default_limits with allocation_bytes = 32768 })
          ("let main input = Task.pure(Json." ^ operation ^ "(input))")
          (nested 30)
      in
      print_s [%sexp (operation : string), (outcome result : string)]));
  [%expect
    {|
    (stringify ok)
    (pretty chatml.allocation_limit)
    |}]
;;
