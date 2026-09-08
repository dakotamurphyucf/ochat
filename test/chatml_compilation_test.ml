open Core
module C = Chatml_compilation
module R = Chatml_host_runtime
module L = Chatml.Chatml_lang

let get = function
  | Ok value -> value
  | Error (e : C.error) -> failwith (e.code ^ ": " ^ e.message)
;;

let expect_error code = function
  | Error (e : C.error) ->
    if not (String.equal code e.code)
    then failwith ("expected " ^ code ^ "; got " ^ e.code ^ ": " ^ e.message)
  | Ok _ -> failwith ("expected " ^ code)
;;

let () =
  Eio_main.run (fun env ->
    let worker = Filename.concat (Core_unix.getcwd ()) (Sys.get_argv ()).(1) in
    let compile ?limits target source =
      C.compile ?limits ~env ~worker ~target ~source ()
    in
    List.iter
      [ C.One_off_v1, "let main = fun input -> Task.pure(input)"
      ; Tool_v1, "let run = fun ctx input -> Task.pure(`Complete(input))"
      ; ( Moderator_v1
        , "let initial_state = 0\n\
           let on_event = fun ctx state event -> Task.pure(state + 1)" )
      ]
      ~f:(fun (target, source) ->
        ignore (compile target source |> get : R.compiled_script);
        ignore
          (compile target ("let poison = fail(\"must not execute\")\n" ^ source) |> get
           : R.compiled_script));
    let program =
      compile
        C.Moderator_v1
        "let initial_state = 0\n\
         let on_event = fun ctx state event -> let x = 2 in Task.pure(state + x)"
      |> get
    in
    let config =
      R.default_runtime_config ~surface:Chatml.Chatml_extension_surface.moderator_v1 ()
    in
    let session =
      R.instantiate_session
        config
        program
        ~entrypoints:{ initial_state_name = "initial_state"; on_event_name = "on_event" }
      |> Result.ok_or_failwith
    in
    R.handle_event
      session
      ~context:(L.VRecord (String.Map.singleton "phase" (L.VString "session_start")))
      ~event:(L.VVariant ("Session_start", []))
    |> Result.ok_or_failwith;
    (match R.current_state session with
     | L.VInt 2 -> ()
     | _ -> assert false);
    List.iter
      [ "let main = 1"
      ; "let main = fun a b -> Task.pure(a)"
      ; "let main = fun input -> Model.call(\"agent\", input)"
      ]
      ~f:(fun source ->
        expect_error "chatml.invalid_handler" (compile C.One_off_v1 source));
    ignore
      (compile
         C.One_off_v1
         "let label = \"foo#bar\"\nlet main = fun input -> Task.pure(input)"
       |> get
       : R.compiled_script);
    expect_error
      "chatml.source_limit"
      (compile ~limits:{ C.default_limits with max_source_bytes = 1 } C.One_off_v1 "long");
    expect_error
      "chatml.invalid_limits"
      (compile ~limits:{ C.default_limits with wall_seconds = 31. } C.One_off_v1 "");
    expect_error
      "chatml.compiler_unavailable"
      (C.compile ~env ~worker:"relative" ~target:C.One_off_v1 ~source:"" ());
    let dir = Eio.Stdenv.cwd env in
    let loader =
      Source_loader.captured_filesystem ~root:dir ~sources:[ "schema.json", "true" ]
    in
    let declarations =
      Prompt.Chat_markdown.parse_chat_inputs
        ~source_loader:loader
        ~dir
        {|<script id="handler" language="chatml" kind="tool">let poison = fail("must not run")
let run = fun ctx input -> Task.pure(`Complete(input))</script><tool name="custom" type="chatml" script="handler" entrypoint="run" input_schema="schema.json" output_schema="schema.json"/>|}
    in
    let script =
      List.find_map_exn declarations ~f:(function
        | Prompt.Chat_markdown.Extension_script value -> Some value
        | _ -> None)
    in
    let tool =
      List.find_map_exn declarations ~f:(function
        | Prompt.Chat_markdown.Tool (Extension value) -> Some value
        | _ -> None)
    in
    let capabilities =
      Chat_response.Tool_capability.create
        ~owner:"test"
        ~resource_fingerprint:(Chatmd_shell_spec.Source_ref.digest "resources")
        []
      |> Result.map_error ~f:(fun e -> e.Chat_response.Tool_capability.message)
      |> Result.ok_or_failwith
    in
    let prepared =
      Chat_response.Extension_compiler.prepare_isolated
        ~env
        ~worker
        ~scripts:[ script ]
        ~capabilities
        tool
      |> Result.map_error ~f:(fun errors ->
        [%sexp (errors : Chatmd_shell_spec.Diagnostic.t list)] |> Sexp.to_string)
      |> Result.ok_or_failwith
    in
    assert (
      List.is_empty
        (Chat_response.Tool_capability.references
           (Chat_response.Extension_compiler.capabilities prepared)));
    let temp = Core_unix.mkdtemp "/tmp/ochat-compiler-test.XXXXXX" in
    let dir = Eio.Path.(Eio.Stdenv.fs env / temp) in
    Exn.protect
      ~f:(fun () ->
        let pidfile = Eio.Path.(dir / "pid")
        and fake = Eio.Path.(dir / "worker") in
        let fake_worker script =
          Eio.Path.save ~create:(`Or_truncate 0o700) fake script;
          Eio.Path.native_exn fake
        in
        let worker =
          fake_worker
            ("#!/bin/sh\necho $$ > '"
             ^ Eio.Path.native_exn pidfile
             ^ "'\nexec /bin/sleep 30\n")
        in
        let ensure_dead () =
          let pid =
            Eio.Path.load pidfile |> String.strip |> Int.of_string |> Pid.of_int
          in
          match Signal_unix.send Signal.zero (`Pid pid) with
          | `Ok -> failwith "compiler process survived cleanup"
          | `No_such_process -> ()
        in
        expect_error
          "chatml.compile_timeout"
          (C.compile
             ~limits:{ C.default_limits with wall_seconds = 1. }
             ~env
             ~worker
             ~target:C.One_off_v1
             ~source:""
             ());
        ensure_dead ();
        (match
           Eio.Time.with_timeout_exn (Eio.Stdenv.clock env) 1. (fun () ->
             C.compile ~env ~worker ~target:C.One_off_v1 ~source:"" ())
         with
         | _ -> failwith "outer cancellation was swallowed"
         | exception Eio.Time.Timeout -> ());
        ensure_dead ();
        let emit output =
          let quoted =
            "'" ^ String.substr_replace_all output ~pattern:"'" ~with_:"'\"'\"'" ^ "'"
          in
          fake_worker ("#!/bin/sh\n/bin/cat >/dev/null\nprintf '%s' " ^ quoted ^ "\n")
        in
        List.iter
          [ "(Compiled(version 0)(target One_off_v1)(contract())(artifact()))"
          ; "(Compiled(version 1)(target One_off_v1)(contract())(artifact()))"
          ]
          ~f:(fun output ->
            let worker = emit output in
            expect_error
              "chatml.compiler_protocol"
              (C.compile ~env ~worker ~target:C.One_off_v1 ~source:"" ()));
        let worker = emit (String.make 513 '(' ^ String.make 513 ')') in
        expect_error
          "chatml.compiler_failed"
          (C.compile ~env ~worker ~target:C.One_off_v1 ~source:"" ());
        let wrong_source = "let main = fun input -> Task.pure(input)" in
        let compiled =
          R.compile_script
            ~surface:Chatml.Chatml_extension_surface.one_off_v1
            ~required_bindings:Chatml.Chatml_extension_surface.one_off_entrypoints
            ~source:wrong_source
            ()
          |> Result.ok_or_failwith
        in
        let artifact = R.Private_compiler_transport.export compiled in
        let pair name value = Sexp.List [ Atom name; value ] in
        let output =
          Sexp.List
            [ Atom "Compiled"
            ; pair "version" (Atom "1")
            ; pair "target" (C.sexp_of_target C.One_off_v1)
            ; pair "contract" (C.contract C.One_off_v1)
            ; pair "artifact" artifact
            ]
          |> Sexp.to_string
        in
        let worker = emit output in
        expect_error
          "chatml.compiler_failed"
          (C.compile ~env ~worker ~target:C.One_off_v1 ~source:"different source" ());
        let worker = fake_worker "#!/bin/sh\nprintf 'invalid response'\n" in
        expect_error
          "chatml.compiler_failed"
          (C.compile ~env ~worker ~target:C.One_off_v1 ~source:"" ()))
      ~finally:(fun () -> Eio.Path.rmtree ~missing_ok:true dir);
    print_endline
      "Isolated ChatML compilation: contracts, artifact execution, limits and process \
       cleanup PASS")
;;
