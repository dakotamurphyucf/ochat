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
    let compile ?limits target source = C.compile ?limits ~env ~target ~source () in
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
      Chat_response.Extension_compiler.prepare_in_domain
        ~env
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
    let module EC = Chat_response.Extension_compiler in
    let module CM = Prompt.Chat_markdown in
    let module Spec = Chatmd_shell_spec.Extension_spec in
    let definition ?limits elements =
      EC.prepare_definition_in_domain ?limits ~env ~capabilities elements
    in
    let admitted result =
      result
      |> Result.map_error ~f:(fun errors ->
        [%sexp (errors : Chatmd_shell_spec.Diagnostic.t list)] |> Sexp.to_string)
      |> Result.ok_or_failwith
    in
    let reject code = function
      | Ok _ -> failwith ("expected " ^ code)
      | Error errors ->
        assert (
          List.exists errors ~f:(fun error ->
            String.equal error.Chatmd_shell_spec.Diagnostic.code code))
    in
    let shared =
      definition (declarations @ [ CM.Tool (Extension { tool with name = "second" }) ])
      |> admitted
    in
    assert (List.length (EC.compiled_scripts shared) = 1);
    let prepared = EC.prepared_tools shared in
    assert (List.length prepared = 2);
    assert (
      phys_equal
        (EC.program (List.nth_exn prepared 0))
        (EC.program (List.nth_exn prepared 1)));
    let unused = { script with id = "unused" } in
    let shared_source =
      definition (declarations @ [ CM.Extension_script unused ]) |> admitted
    in
    let programs = EC.compiled_scripts shared_source |> List.map ~f:snd in
    assert (phys_equal (List.nth_exn programs 0) (List.nth_exn programs 1));
    let changed_script script text =
      { script with
        Spec.source = Chatmd_shell_spec.Chatmd_script_spec.Inline text
      ; source_sha256 = Chatmd_shell_spec.Source_ref.digest text
      }
    in
    reject
      "chatml.invalid_handler"
      (definition
         (declarations @ [ CM.Extension_script (changed_script unused "let run = 0") ]));
    let lifecycle =
      changed_script
        { script with id = "lifecycle"; kind = Spec.Moderator_script }
        "let initial_state = fail(\"must not initialize\")\n\
         let on_event = fun ctx state event -> Task.pure(state)"
    in
    let lifecycle_only = definition [ CM.Extension_script lifecycle ] |> admitted in
    assert (List.is_empty (EC.prepared_tools lifecycle_only));
    assert (List.length (EC.compiled_scripts lifecycle_only) = 1);
    reject
      "chatml.invalid_definition"
      (definition (declarations @ [ CM.Extension_script script ]));
    reject "chatml.invalid_definition" (definition [ CM.Tool (Extension tool) ]);
    reject
      "chatml.invalid_definition"
      (definition
         [ CM.Extension_script script
         ; CM.Tool (Extension { tool with uses = [ tool.name ] })
         ]);
    reject
      "chatml.invalid_limits"
      (definition ~limits:{ C.default_limits with wall_seconds = 0. } []);
    reject
      "chatml.definition_limit"
      (definition
         (List.init 129 ~f:(fun i ->
            CM.Extension_script { script with id = Int.to_string i })));
    reject
      "chatml.source_mismatch"
      (definition
         [ CM.Extension_script { unused with source_sha256 = String.make 64 '0' } ]);
    let corrupt =
      { tool with
        output_schema = { tool.output_schema with source_sha256 = String.make 64 '0' }
      }
    in
    reject
      "chatmd.schema_digest_mismatch"
      (definition [ CM.Extension_script script; CM.Tool (Extension corrupt) ]);
    reject
      "capability.not_selected"
      (definition
         [ CM.Extension_script script
         ; CM.Tool (Extension { tool with uses = [ "missing" ] })
         ]);
    reject
      "chatml.invalid_binding"
      (definition
         [ CM.Extension_script script
         ; CM.Tool
             (Extension
                { tool with
                  implementation = Standalone { script = script.id; entrypoint = "wrong" }
                })
         ]);
    reject
      "chatml.compile_timeout"
      (definition
         ~limits:{ C.default_limits with wall_seconds = 1e-12 }
         [ CM.Extension_script script ]);
    (* Concurrent compilers must own independent inference/slot state. Both
       positive and negative results agree with serial compilation. *)
    Eio.Fiber.all
      (List.init 16 ~f:(fun i () ->
         let source =
           if i mod 2 = 0
           then "let main = fun input -> Task.pure(input)"
           else "let main = 1"
         in
         if i mod 2 = 0
         then ignore (compile C.One_off_v1 source |> get : R.compiled_script)
         else expect_error "chatml.invalid_handler" (compile C.One_off_v1 source)));
    (* A caller's cancellation must propagate, never become an invalid-handler
       result. The timeout owns and joins the domain before returning. *)
    (match
       Eio.Time.with_timeout_exn (Eio.Stdenv.clock env) 0. (fun () ->
         compile C.One_off_v1 "let main = fun input -> Task.pure(input)")
     with
     | _ -> failwith "outer cancellation was swallowed"
     | exception Eio.Time.Timeout -> ());
    let checkpoints = ref 0 in
    (match
       R.compile_script
         ~checkpoint:(fun () ->
           incr checkpoints;
           if !checkpoints = 3 then raise Exit)
         ~source:"let value = 1"
         ()
     with
     | _ -> failwith "compiler swallowed a stage checkpoint exception"
     | exception Exit -> ());
    assert (!checkpoints = 3);
    print_endline
      "Domain ChatML compilation: contracts, execution, concurrent state, source limits \
       and cooperative cancellation PASS")
;;
