open Core
module V = Chat_response.Authoring_validation
module C = Chat_response.Tool_capability

let host
      ?(runtime_identity = "fixture-runtime-1")
      ?(surface = V.Ordinary)
      ?(targets = [ V.One_off_script; Standalone_tool; Moderator ])
      ?(max_source_bytes = Chatml_compilation.default_limits.max_source_bytes)
      ()
  =
  V.create_host
    ~runtime_identity
    ~targets
    ~moderator_surface:surface
    ~compilation:{ Chatml_compilation.default_limits with max_source_bytes }
  |> Result.ok_or_failwith
;;

let registry calls =
  let module Definition = struct
    type input = string

    let name = "read_file"
    let description = Some "validation fixture"
    let type_ = "function"
    let parameters = `Object [ "type", `String "object" ]
    let input_of_string input = input
  end
  in
  let implementation =
    Ochat_function.create_function
      (module Definition)
      (fun _ ->
         incr calls;
         failwith "readonly validation invoked a tool")
  in
  C.create
    ~owner:"validation-fixture"
    ~resource_fingerprint:(Chatmd_shell_spec.Source_ref.digest "read roots")
    [ Chatmd_shell_spec.Source_ref.digest "reader-v1", implementation ]
  |> Result.map_error ~f:(fun error -> error.C.message)
  |> Result.ok_or_failwith
;;

let request ?(tools = [ "read_file" ]) ?schemas target source =
  `Object
    ([ "version", `Number "1"
     ; "target", `String target
     ; "source", `String source
     ; "tools", `Array (List.map tools ~f:(fun name -> `String name))
     ]
     @
     match schemas with
     | None -> []
     | Some (input, output) -> [ "input_schema", input; "output_schema", output ])
;;

let with_registry f =
  Eio_main.run (fun env ->
    Mirage_crypto_rng_unix.use_default ();
    let calls = ref 0 in
    f env (registry calls);
    [%test_eq: int] 0 !calls)
;;

let bundle_request ?(tools = [ "read_file" ]) ?(root_file = "child.chatmd") sources =
  `Object
    [ "version", `Number "1"
    ; "target", `String "generated_chatmd"
    ; "root_file", `String root_file
    ; ( "sources"
      , `Array
          (List.map sources ~f:(fun (path, text) ->
             `Object [ "path", `String path; "text", `String text ])) )
    ; "tools", `Array (List.map tools ~f:(fun name -> `String name))
    ]
;;

let%expect_test
    "generated validation checks captured imports without evaluating initializers"
  =
  with_registry (fun env capabilities ->
    let host = host ~targets:[ V.Generated_chatmd ] () in
    let sources =
      [ ( "child.chatmd"
        , {|<developer>Child.</developer><import src="tools.chatmd"/>
<script id="owner" language="chatml" kind="moderator" api="extensibility-v1">
let initial_state = fail("INITIALIZER-MUST-NOT-RUN")
let on_event ctx state event = Task.pure(state)
</script>|}
        )
      ; "tools.chatmd", {|<tool type="inherited" name="read_file"/>|}
      ]
    in
    let validate ?(host = host) request = V.validate ~env ~host ~capabilities request in
    let good = validate (bundle_request sources) in
    assert (V.valid good);
    assert (Option.is_some good.validation_id);
    assert (List.mem good.checked "captured_source_closure" ~equal:String.equal);
    assert (List.mem good.deferred "session_creation" ~equal:String.equal);
    assert (List.mem good.deferred "initializer_evaluation" ~equal:String.equal);
    assert (
      not
        (String.is_substring
           (V.to_json good |> Jsonaf.to_string)
           ~substring:"INITIALIZER-MUST-NOT-RUN"));
    let identity report =
      assert (V.valid report);
      Option.value_exn report.V.validation_id
    in
    [%test_eq: string]
      (identity good)
      (identity (validate (bundle_request (List.rev sources))));
    let changed = List.map sources ~f:(fun (path, text) -> path, text ^ "\n") in
    assert (
      not (String.equal (identity good) (identity (validate (bundle_request changed)))));
    let limits = { Chatmd_source_bundle.default_limits with max_files = 12 } in
    let configured =
      V.configure_generated host ~limits ~catalog:None |> Result.ok_or_failwith
    in
    assert (
      not
        (String.equal
           (identity good)
           (identity (validate ~host:configured (bundle_request sources)))));
    let failures =
      [ "missing captured import", bundle_request [ List.hd_exn sources ]
      ; ( "ambient file import"
        , bundle_request [ "child.chatmd", {|<import src="Readme.md"/>|} ] )
      ; ( "outside path"
        , bundle_request [ "child.chatmd", {|<import src="../Readme.md"/>|} ] )
      ; ( "native reconfiguration"
        , bundle_request [ "child.chatmd", {|<tool name="read_file"/>|} ] )
      ; ( "unknown inherited helper"
        , bundle_request
            [ "child.chatmd", {|<tool type="inherited" name="ochat_validate"/>|} ] )
      ; ( "delegated model surface"
        , bundle_request
            [ ( "child.chatmd"
              , {|<script id="owner" language="chatml" kind="moderator" api="extensibility-v1">
let initial_state = 0
let on_event ctx state event = let* result = Model.call("worker", `Null) in Task.pure(state)
</script>|}
              )
            ] )
      ; "duplicate source", bundle_request (List.hd_exn sources :: sources)
      ; "removed authority", bundle_request ~tools:[] sources
      ; ( "unsupported reasoning"
        , bundle_request
            [ "child.chatmd", {|<config reasoning_effort="not-an-effort"/>|} ] )
      ]
    in
    List.iter failures ~f:(fun (label, request) ->
      let report = validate request in
      assert (not (V.valid report));
      assert (not (List.is_empty report.diagnostics));
      List.iter report.diagnostics ~f:(fun d ->
        List.iter d.topic_ids ~f:(fun topic ->
          assert (List.Assoc.mem V.topics topic ~equal:String.equal)));
      print_endline (label ^ ": rejected"));
    print_endline
      "captured bundle checked; no initializers or native tools; identities bind closure \
       and host limits");
  [%expect
    {|
    missing captured import: rejected
    ambient file import: rejected
    outside path: rejected
    native reconfiguration: rejected
    unknown inherited helper: rejected
    delegated model surface: rejected
    duplicate source: rejected
    removed authority: rejected
    unsupported reasoning: rejected
    captured bundle checked; no initializers or native tools; identities bind closure and host limits |}]
;;

let%expect_test
    "generated authoring packages and helper selection stay inside delegated authority"
  =
  Eio_main.run (fun env ->
    Mirage_crypto_rng_unix.use_default ();
    let module M = Chatmd_shell_spec.Authoring_metadata in
    let help = V.help Generated_chatmd in
    let make name =
      let module Definition = struct
        type input = string

        let name = name
        let description = None
        let type_ = "function"
        let parameters = `True
        let input_of_string text = text
      end
      in
      Ochat_function.create_function
        (module Definition)
        (fun _ -> failwith "validation ran helper")
    in
    let names = [ "author"; M.helper_name Reference; M.helper_name Validation ] in
    let capabilities =
      C.create
        ~owner:"generated-help"
        ~resource_fingerprint:(Chatmd_shell_spec.Source_ref.digest "roots")
        ~metadata:
          [ ("author", M.{ authoring = Some help; helper = None })
          ; (M.helper_name Reference, M.{ authoring = None; helper = Some Reference })
          ; M.helper_name Validation, V.helper_metadata
          ]
        (List.map names ~f:(fun name ->
           Chatmd_shell_spec.Source_ref.digest name, make name))
      |> Result.map_error ~f:(fun e -> e.C.message)
      |> Result.ok_or_failwith
    in
    let catalog marker =
      Chat_response.Authoring_policy.catalog
        ~identity:(Chatmd_shell_spec.Source_ref.digest marker)
        ~packages:[ help ]
        ~topics:(List.map help.topics ~f:(fun id -> id, [ M.Child_agent ]))
      |> Result.map_error ~f:(fun e -> e.Chat_response.Authoring_policy.message)
      |> Result.ok_or_failwith
    in
    let make_host marker =
      V.configure_generated
        (host ~targets:[ V.Generated_chatmd ] ())
        ~limits:Chatmd_source_bundle.default_limits
        ~catalog:(Some (catalog marker))
      |> Result.ok_or_failwith
    in
    let request tools =
      bundle_request ~tools [ "child.chatmd", {|<tool type="inherited" name="author"/>|} ]
    in
    let validate host tools = V.validate ~env ~host ~capabilities (request tools) in
    let first = validate (make_host "corpus-1") names in
    assert (V.valid first);
    let second = validate (make_host "corpus-2") names in
    assert (V.valid second);
    assert (not (Option.equal String.equal first.validation_id second.validation_id));
    List.iter
      [ [ "author" ]; [ "author"; M.helper_name Validation ] ]
      ~f:(fun tools -> assert (not (V.valid (validate (make_host "corpus-1") tools))));
    assert (not (V.valid (validate (host ~targets:[ V.Generated_chatmd ] ()) names)));
    print_endline
      "installed child package selects existing helpers only; absent helpers/catalog \
       reject; corpus changes invalidate report identity");
  [%expect
    {| installed child package selects existing helpers only; absent helpers/catalog reject; corpus changes invalidate report identity |}]
;;

let%expect_test "all inline targets validate without initializer or tool effects" =
  with_registry (fun env capabilities ->
    let poison = "let poison = fail(\"PRIVATE-CANDIDATE-SENTINEL\")\n" in
    List.iter
      [ ( "one_off_script"
        , "let main input = Task.bind(Tool.call(\"read_file\", input), fun result -> \
           Task.pure(input))"
        , None )
      ; ( "standalone_tool"
        , "let run ctx input = Task.pure(`Complete(input))"
        , Some (`True, `True) )
      ; ( "moderator"
        , "let initial_state = fun x -> x\n\
           let on_event ctx state event = Task.pure(state)"
        , None )
      ]
      ~f:(fun (target, source, schemas) ->
        let report =
          V.validate
            ~env
            ~host:(host ())
            ~capabilities
            (request ?schemas target (poison ^ source))
        in
        assert (V.valid report);
        assert (Option.is_some report.validation_id);
        assert (List.mem report.checked "entrypoints" ~equal:String.equal);
        assert (
          not
            (String.is_substring
               (V.to_json report |> Jsonaf.to_string)
               ~substring:"PRIVATE-CANDIDATE-SENTINEL"));
        print_s
          [%sexp
            (target : string)
          , (report.checked : string list)
          , (report.deferred : string list)]));
  [%expect
    {|
    (one_off_script (request selected_capabilities syntax types entrypoints)
     (initializer_evaluation runtime_tool_calls current_permissions
      external_effects runtime_input_output chatmd_declarations))
    (standalone_tool
     (request selected_capabilities input_schema output_schema syntax types
      entrypoints)
     (initializer_evaluation runtime_tool_calls current_permissions
      external_effects runtime_input_output chatmd_declarations))
    (moderator (request selected_capabilities syntax types entrypoints)
     (initializer_evaluation runtime_tool_calls current_permissions
      external_effects runtime_input_output chatmd_declarations
      state_serialization))
    |}]
;;

let%expect_test
    "source-bound diagnostics point to relevant topics and reject forged context"
  =
  with_registry (fun env capabilities ->
    let good = "let main input = Task.pure(input)" in
    let forged =
      match request "one_off_script" good with
      | `Object fields -> `Object (("runtime_identity", `String "forged") :: fields)
      | _ -> assert false
    in
    List.iter
      [ ( "call syntax"
        , host ()
        , request "one_off_script" "let main input = Task.pure input" )
      ; ( "entrypoint"
        , host ()
        , request "one_off_script" "let main input = Task.pure(input + 1)" )
      ; ( "schema"
        , host ()
        , request
            ~schemas:(`Object [ "$ref", `String "file:///private" ], `True)
            "standalone_tool"
            "let run ctx input = Task.pure(`Complete(input))" )
      ; "tools", host (), request ~tools:[ "private_tool" ] "one_off_script" good
      ; ( "host target"
        , host ~targets:[ V.One_off_script ] ()
        , request
            "moderator"
            "let initial_state = 0\nlet on_event ctx state event = Task.pure(state)" )
      ; "source limit", host ~max_source_bytes:8 (), request "one_off_script" good
      ; "forged context", host (), forged
      ]
      ~f:(fun (label, host, json) ->
        let report = V.validate ~env ~host ~capabilities json in
        assert (not (V.valid report));
        let diagnostics =
          List.map report.diagnostics ~f:(fun d ->
            ( d.V.diagnostic.code
            , d.topic_ids
            , Option.map d.diagnostic.source ~f:(fun s ->
                s.Chatmd_shell_spec.Source_ref.start_pos.line) ))
        in
        List.iter report.diagnostics ~f:(fun d ->
          List.iter d.topic_ids ~f:(fun id ->
            assert (List.Assoc.mem V.topics id ~equal:String.equal)));
        print_s
          [%sexp
            (label : string), (diagnostics : (string * string list * int option) list)]));
  [%expect
    {|
    ("call syntax"
     ((chatml.type_error
       (chatml.types chatml.syntax.calls runtime.invocations.one-off) (1))))
    (entrypoint
     ((chatml.type_error
       (chatml.types chatml.syntax.calls runtime.invocations.one-off) (1))))
    (schema ((schema.invalid (chatmd.declarations.schemas) ())))
    (tools ((capability.not_selected (runtime.authority.tool-selection) ())))
    ("host target"
     ((authoring.unavailable_target (runtime.invocations.validation) ())))
    ("source limit" ((chatml.source_limit (runtime.invocations.validation) ())))
    ("forged context"
     ((authoring.invalid_request (runtime.invocations.validation) ())))
    |}]
;;

let%expect_test
    "validation receipts bind source, schemas, selected authority and the host contract"
  =
  with_registry (fun env capabilities ->
    let source = "let main input = Task.pure(input)" in
    let validate ?(host = host ()) json = V.validate ~env ~host ~capabilities json in
    let identity report =
      assert (V.valid report);
      Option.value_exn report.V.validation_id
    in
    let baseline = request "one_off_script" source |> validate |> identity in
    assert (String.equal baseline (request "one_off_script" source |> validate |> identity));
    List.iter
      [ request "one_off_script" (source ^ "\n(* edited *)") |> validate
      ; request ~tools:[] "one_off_script" source |> validate
      ; request "one_off_script" source
        |> validate ~host:(host ~runtime_identity:"fixture-runtime-2" ())
      ; request "one_off_script" source |> validate ~host:(host ~max_source_bytes:1024 ())
      ]
      ~f:(fun report -> assert (not (String.equal baseline (identity report))));
    let standalone output =
      request
        ~schemas:(`True, output)
        "standalone_tool"
        "let run ctx input = Task.pure(`Complete(input))"
      |> validate
      |> identity
    in
    assert (
      not
        (String.equal
           (standalone `True)
           (standalone (`Object [ "type", `String "string" ]))));
    let moderator =
      request
        "moderator"
        "let initial_state = 0\nlet on_event ctx state event = Task.pure(state)"
    in
    let ordinary = validate moderator |> identity in
    let delegated = validate ~host:(host ~surface:V.Delegated ()) moderator |> identity in
    assert (not (String.equal ordinary delegated));
    let unavailable_model =
      request
        "moderator"
        "let initial_state = 0\n\
         let on_event ctx state event = Task.bind(Model.call(\"worker\", `Null), fun \
         result -> Task.pure(state))"
    in
    assert (V.valid (validate unavailable_model));
    assert (
      not (V.valid (validate ~host:(host ~surface:V.Delegated ()) unavailable_model)));
    print_endline
      "stable for identical inputs; changes with source, selection, schema, runtime, \
       policy and target surface";
    print_endline
      "delegated moderator guidance/validation cannot enable ambient Model operations");
  [%expect
    {|
    stable for identical inputs; changes with source, selection, schema, runtime, policy and target surface
    delegated moderator guidance/validation cannot enable ambient Model operations
    |}]
;;

let%expect_test "diagnostic byte caps preserve valid Unicode and JSON" =
  with_registry (fun env capabilities ->
    List.iter
      [ String.make 255 'x' ^ "🦊"; String.concat (List.init 3000 ~f:(fun _ -> "é")) ]
      ~f:(fun name ->
        let report =
          V.validate
            ~env
            ~host:(host ())
            ~capabilities
            (request
               ~schemas:(`Object [ name, `True ], `True)
               "standalone_tool"
               "let run ctx input = Task.pure(`Complete(input))")
        in
        assert (not (V.valid report));
        List.iter report.diagnostics ~f:(fun d ->
          assert (String.length d.diagnostic.message <= 4096);
          List.iter d.diagnostic.path ~f:(fun part -> assert (String.length part <= 256)));
        let encoded = V.to_json report |> Jsonaf.to_string in
        assert (Stdlib.String.is_valid_utf_8 encoded);
        ignore (Jsonaf.of_string encoded : Jsonaf.t)));
  [%expect {||}]
;;

let%expect_test
    "validation propagates caller cancellation and metadata is internally consistent"
  =
  with_registry (fun env capabilities ->
    let cancelled =
      match
        Eio.Time.with_timeout_exn (Eio.Stdenv.clock env) 0. (fun () ->
          V.validate
            ~env
            ~host:(host ())
            ~capabilities
            (request "one_off_script" "let main input = Task.pure(input)"))
      with
      | _ -> false
      | exception Eio.Time.Timeout -> true
    in
    assert cancelled;
    List.iter [ V.One_off_script; Standalone_tool; Moderator ] ~f:(fun target ->
      let help = V.help target in
      Chatmd_shell_spec.Authoring_metadata.validate_help help |> Result.ok_or_failwith;
      List.iter help.topics ~f:(fun id ->
        assert (List.Assoc.mem V.topics id ~equal:String.equal)));
    Chatmd_shell_spec.Authoring_metadata.validate
      ~tool_name:"ochat_validate"
      V.helper_metadata
    |> Result.ok_or_failwith;
    ignore
      (Chat_response.Authoring_policy.catalog
         ~identity:(Chatmd_shell_spec.Source_ref.digest "validation-hook-corpus")
         ~packages:(List.map [ V.One_off_script; Standalone_tool; Moderator ] ~f:V.help)
         ~topics:
           (List.map V.topics ~f:(fun (id, _) ->
              ( id
              , [ Chatmd_shell_spec.Authoring_metadata.One_off_script
                ; Standalone_tool
                ; Moderator_tool
                ] )))
       |> Result.map_error ~f:(fun error -> error.Chat_response.Authoring_policy.message)
       |> Result.ok_or_failwith
       : Chat_response.Authoring_policy.catalog);
    print_endline
      "cancelled validation returns no receipt; metadata topics and helper role are valid");
  [%expect
    {| cancelled validation returns no receipt; metadata topics and helper role are valid |}]
;;
