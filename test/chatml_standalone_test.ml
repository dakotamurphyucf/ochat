open Core
module R = Chatml_host_runtime
module X = Chatml_execution
module L = Chatml.Chatml_lang

let compile env source =
  match Chatml_compilation.compile ~env ~target:One_off_v1 ~source () with
  | Ok program -> program
  | Error error -> failwith error.message
;;

let config ?(handlers = R.default_handlers) () : R.runtime_config =
  { surface = Chatml.Chatml_extension_surface.one_off_v1
  ; operations = R.default_operations ~handlers ()
  }
;;

let summarize = function
  | Ok value ->
    Chatml.Chatml_value_codec.value_to_jsonaf_result value
    |> Result.ok_or_failwith
    |> Jsonaf.to_string
  | Error error -> error.X.code
;;

let%expect_test "unannotated recursive JSON compiles against the entrypoint and executes" =
  Eio_main.run (fun env ->
    let program =
      compile
        env
        {|let rec tree n =
  if n == 0 then `Null
  else let child = tree(n - 1) in `Array([child, child])
let main input = Task.pure(tree(2))|}
    in
    X.run
      ~env
      ~config:(config ())
      ~program
      ~entrypoint:"main"
      ~arguments:[ L.VVariant ("Null", []) ]
      ()
    |> summarize
    |> print_endline);
  [%expect {| [[null,null],[null,null]] |}]
;;

let%expect_test
    "standalone entrypoints use fresh globals and task continuations without lifecycle \
     hooks"
  =
  Eio_main.run (fun env ->
    let program =
      compile
        env
        {|let count = [0]
module Work = struct
  let finish = fun value ->
    let ignored = count[0] <- count[0] + 1 in
    Task.pure(`String(to_string(count[0])))
end
let main = fun input -> Task.bind(Tool.call("fixture", input), fun result ->
  match result with
  | `Ok(value) -> Work.finish(value)
  | `Error(code) -> Task.fail(code))|}
    in
    let phases = ref [] in
    let configuration =
      config
        ~handlers:
          { R.default_handlers with
            on_tool_call =
              (fun session ~name ~args ->
                assert (String.equal name "fixture");
                phases := R.current_phase session :: !phases;
                Eio.Fiber.yield ();
                Ok (L.VVariant ("Ok", [ args ])))
          }
        ()
    in
    let run () =
      X.run
        ~env
        ~config:configuration
        ~program
        ~entrypoint:"main"
        ~arguments:[ L.VVariant ("Null", []) ]
        ()
      |> summarize
    in
    let first, second = Eio.Fiber.pair run run in
    print_s
      [%sexp { first : string; second : string; phases = (!phases : string option list) }]);
  [%expect {| ((first "\"1\"") (second "\"1\"") (phases ((standalone) (standalone)))) |}]
;;

let%expect_test
    "pure execution limits cover initializers loops modules and caught task continuations"
  =
  Eio_main.run (fun env ->
    let cases =
      [ ( "initializer"
        , {|let rec loop x = loop(x)
let never = loop(0)
let main input = Task.pure(input)|}
        )
      ; ( "tail recursion"
        , {|let rec loop x = loop(x)
let main input = loop(input)|}
        )
      ; ( "while"
        , {|let main input = let ignored = while true do () done in Task.pure(input)|} )
      ; ( "builtin callback"
        , {|let rec loop x = loop(x)
let main input = let ignored = Array.map([0], fun value -> loop(value)) in Task.pure(input)|}
        )
      ; ( "module"
        , {|module Work = struct
let rec loop x = loop(x)
end
let main input = Work.loop(input)|}
        )
      ; ( "caught continuation"
        , {|let rec loop x = loop(x)
let main input = Task.catch(Task.bind(Task.pure(input), fun value -> loop(value)),
  fun message -> Task.pure(`String("caught")))|}
        )
      ]
    in
    List.iter cases ~f:(fun (name, source) ->
      let program = compile env source in
      let result =
        X.run
          ~policy:(Bounded { X.default_limits with fuel = 1000 })
          ~env
          ~config:(config ())
          ~program
          ~entrypoint:"main"
          ~arguments:[ L.VVariant ("Null", []) ]
          ()
        |> summarize
      in
      print_s [%sexp (name : string), (result : string)]));
  [%expect
    {|
    (initializer chatml.execution_limit)
    ("tail recursion" chatml.execution_limit)
    (while chatml.execution_limit)
    ("builtin callback" chatml.execution_limit)
    (module chatml.execution_limit)
    ("caught continuation" chatml.execution_limit)
    |}]
;;

let%expect_test
    "parallel executions keep independent fuel across yields and closure callbacks"
  =
  Eio_main.run (fun env ->
    let program =
      compile
        env
        {|let rec sum n acc = if n == 0 then acc else sum(n - 1, acc + 1)
let main input = Task.pure(`String(to_string(sum(1000, 0))))|}
    in
    let run fuel =
      X.run
        ~policy:(Bounded { X.default_limits with fuel })
        ~env
        ~config:(config ())
        ~program
        ~entrypoint:"main"
        ~arguments:[ L.VVariant ("Null", []) ]
        ()
      |> summarize
    in
    let limited, completed =
      Eio.Fiber.pair (fun () -> run 1000) (fun () -> run 100_000)
    in
    print_s [%sexp { limited : string; completed : string }]);
  [%expect {| ((limited chatml.execution_limit) (completed "\"1000\"")) |}]
;;

let%expect_test
    "cancelling pure or blocked external execution releases its runtime context"
  =
  Eio_main.run (fun env ->
    List.iter [ `Pure; `External ] ~f:(fun mode ->
      let started, started_u = Eio.Promise.create () in
      let never, _ = Eio.Promise.create () in
      let captured = ref None in
      let source =
        match mode with
        | `Pure ->
          {|let rec loop x = loop(x)
let main input = Task.bind(Log.info("ready"), fun ignored -> loop(input))|}
        | `External ->
          {|let main input = Task.bind(Tool.call("wait", input), fun result -> Task.pure(input))|}
      in
      let program = compile env source in
      let handlers =
        { R.default_handlers with
          on_log =
            (fun session ~level:_ ~message:_ ->
              captured := Some session;
              Eio.Promise.resolve started_u ();
              Ok ())
        ; on_tool_call =
            (fun session ~name:_ ~args:_ ->
              captured := Some session;
              Eio.Promise.resolve started_u ();
              Eio.Promise.await never)
        }
      in
      let cancelled =
        Eio.Time.with_timeout_exn (Eio.Stdenv.clock env) 5. (fun () ->
          Eio.Fiber.first
            (fun () ->
               X.run
                 ~policy:(Bounded { X.default_limits with fuel = 10_000_000 })
                 ~env
                 ~config:(config ~handlers ())
                 ~program
                 ~entrypoint:"main"
                 ~arguments:[ L.VVariant ("Null", []) ]
                 ()
               |> ignore;
               false)
            (fun () ->
               Eio.Promise.await started;
               Eio.Fiber.yield ();
               true))
      in
      let inactive = Option.is_none (R.current_phase (Option.value_exn !captured)) in
      print_s [%sexp { mode : [ `Pure | `External ]; cancelled : bool; inactive : bool }]));
  [%expect
    {|
    ((mode Pure) (cancelled true) (inactive true))
    ((mode External) (cancelled true) (inactive true))
    |}]
;;

let%expect_test
    "oversized allocations and cyclic or expanding JSON fail before tool effects"
  =
  Eio_main.run (fun env ->
    let cases =
      [ ( "array allocation"
        , "let ignored = Array.init(1000000, fun index -> fail(\"allocation callback \
           ran\")) in Task.pure(input)" )
      ; ( "array append"
        , "let values = Array.make(20, 0) in let ignored = Array.append(values, values) \
           in Task.pure(input)" )
      ; "string doubling", "let rec grow s = grow(s ++ s) in grow(\"a\")"
      ; ( "replacement"
        , "let ignored = String.replace_all(\"aaaaaaaa\", \"a\", \"replacement \
           replacement replacement replacement\") in Task.pure(input)" )
      ; ( "shared JSON"
        , "let rec tree n = if n == 0 then `Null else let child = tree(n - 1) in \
           `Array([child, child]) in Task.pure(tree(20))" )
      ; ( "cyclic JSON"
        , "let values : json array = [`Null] in let cycle = `Array(values) in let \
           ignored = values[0] <- cycle in Task.pure(cycle)" )
      ]
    in
    List.iter cases ~f:(fun (name, body) ->
      let program =
        compile
          env
          ("let main input = Task.bind(("
           ^ body
           ^ "), fun value -> Task.bind(Tool.call(\"effect\", value), fun ignored -> \
              Task.pure(value)))")
      in
      let result =
        X.run
          ~policy:
            (Bounded
               { X.default_limits with
                 max_value_bytes = 256
               ; max_array_items = 32
               ; max_depth = 16
               })
          ~env
          ~config:
            (config
               ~handlers:
                 { R.default_handlers with
                   on_tool_call =
                     (fun _ ~name:_ ~args:_ -> failwith "unexpected tool effect")
                 }
               ())
          ~program
          ~entrypoint:"main"
          ~arguments:[ L.VVariant ("Null", []) ]
          ()
        |> summarize
      in
      print_s [%sexp (name : string), (result : string)]));
  [%expect
    {|
    ("array allocation" chatml.value_limit)
    ("array append" chatml.value_limit)
    ("string doubling" chatml.value_limit)
    (replacement chatml.value_limit)
    ("shared JSON" chatml.value_limit)
    ("cyclic JSON" chatml.value_limit)
    |}]
;;

let%expect_test
    "allocation budget is cumulative and a blocked external deadline cleans up"
  =
  Eio_main.run (fun env ->
    let program =
      compile
        env
        {|let rec repeat n = if n == 0 then () else let ignored = Array.make(32, 0) in repeat(n - 1)
let main input =
  let count = match input with | `String("single") -> 1 | _ -> 64 in
  let ignored = repeat(count) in Task.pure(`Null)|}
    in
    let run input =
      X.run
        ~policy:(Bounded { X.default_limits with allocation_bytes = 8192 })
        ~env
        ~config:(config ())
        ~program
        ~entrypoint:"main"
        ~arguments:[ L.VVariant ("String", [ VString input ]) ]
        ()
      |> summarize
    in
    let single = run "single"
    and repeated = run "repeated" in
    let blocked =
      compile
        env
        "let main input = Task.bind(Tool.call(\"wait\", input), fun result -> \
         Task.pure(input))"
    in
    let captured = ref None in
    let never, _ = Eio.Promise.create () in
    let deadline =
      X.run
        ~policy:(Bounded { X.default_limits with wall_seconds = 0.01 })
        ~env
        ~config:
          (config
             ~handlers:
               { R.default_handlers with
                 on_tool_call =
                   (fun session ~name:_ ~args:_ ->
                     captured := Some session;
                     Eio.Promise.await never)
               }
             ())
        ~program:blocked
        ~entrypoint:"main"
        ~arguments:[ L.VVariant ("Null", []) ]
        ()
      |> summarize
    in
    let inactive = Option.is_none (R.current_phase (Option.value_exn !captured)) in
    print_s
      [%sexp { single : string; repeated : string; deadline : string; inactive : bool }]);
  [%expect
    {|
    ((single null) (repeated chatml.allocation_limit)
     (deadline chatml.execution_timeout) (inactive true))
    |}]
;;

let%expect_test "host resource policy can be unrestricted or exceed suggested defaults" =
  Eio_main.run (fun env ->
    let program =
      compile
        env
        {|let main input =
  let values = Array.make(20000, 0) in
  Task.bind(Tool.call("fixture", `String(to_string(Array.length(values)))), fun result ->
    match result with
    | `Ok(value) -> Task.pure(value)
    | `Error(message) -> Task.fail(message))|}
    in
    let called = ref 0 in
    let configuration =
      config
        ~handlers:
          { R.default_handlers with
            on_tool_call =
              (fun _ ~name:_ ~args ->
                incr called;
                Ok (L.VVariant ("Ok", [ args ])))
          }
        ()
    in
    let run policy =
      X.run
        ~policy
        ~env
        ~config:configuration
        ~program
        ~entrypoint:"main"
        ~arguments:[ L.VVariant ("Null", []) ]
        ()
      |> summarize
    in
    let bounded = run (Bounded X.default_limits) in
    let unrestricted = run Unrestricted in
    let custom =
      run
        (Bounded
           { fuel = 20_000_000
           ; max_tasks = 200_000
           ; wall_seconds = 600.
           ; max_value_bytes = 32 * 1024 * 1024
           ; max_array_items = 2_000_000
           ; max_depth = 512
           ; allocation_bytes = 512 * 1024 * 1024
           })
    in
    print_s
      [%sexp
        { bounded : string
        ; unrestricted : string
        ; custom : string
        ; tool_calls = (!called : int)
        }]);
  [%expect
    {|
    ((bounded chatml.value_limit) (unrestricted "\"20000\"") (custom "\"20000\"")
     (tool_calls 2))
    |}]
;;
