open Core
module G = Chat_response.Generated_admission
module C = Chat_response.Tool_capability
module CM = Prompt.Chat_markdown
module A = Chat_response.Agent_runtime

let get = function
  | Ok x -> x
  | Error ds ->
    failwith
      (String.concat ~sep:"; " (List.map ds ~f:Chatmd_shell_spec.Diagnostic.to_string))
;;

let caps = function
  | Ok x -> x
  | Error e -> raise_s [%sexp (e : C.error)]
;;

let runtime = function
  | Ok x -> x
  | Error ds -> failwith (String.concat ~sep:"; " (List.map ds ~f:A.diagnostic_to_string))
;;

let expect code = function
  | Ok _ -> failwith ("expected " ^ code)
  | Error ds ->
    if
      not
        (List.exists ds ~f:(fun d ->
           String.equal d.Chatmd_shell_spec.Diagnostic.code code))
    then ignore (get (Error ds) : unit)
;;

let text = function
  | Openai.Responses.Tool_output.Output.Text s -> s
  | _ -> assert false
;;

let script body =
  "<script id=\"coordinator\" language=\"chatml\" kind=\"moderator\" \
   api=\"extensibility-v1\">"
  ^ body
  ^ "</script>"
;;

let lifecycle =
  "let initial_state = fail(\"initializer must not execute\")\n\
   let on_event = fun ctx state event -> Task.pure(state)"
;;

let () =
  Eio_main.run (fun env ->
    Eio.Switch.run (fun sw ->
      Mirage_crypto_rng_unix.use_default ();
      let temp = Core_unix.mkdtemp "/tmp/ochat-admission.XXXXXX" in
      let root = Eio.Path.(Eio.Stdenv.fs env / temp) in
      Exn.protect
        ~f:(fun () ->
          Eio.Path.mkdir ~perm:0o700 Eio.Path.(root / "lib");
          Eio.Path.mkdir ~perm:0o700 Eio.Path.(root / "child");
          Eio.Path.save
            ~create:(`Or_truncate 0o600)
            Eio.Path.(root / "lib" / "allowed.txt")
            "parent allowed data";
          Eio.Path.save
            ~create:(`Or_truncate 0o600)
            Eio.Path.(root / "secret.txt")
            "private outside marker";
          let parent_source =
            {|<tool name="read_file"><read id="source" path="lib"/></tool>
<tool name="legacy_agent" agent="unused.chatmd"/>
<tool name="fork"/>|}
          in
          let prompt_elements =
            CM.parse_chat_inputs ~source:"parent.chatmd" ~dir:root parent_source
          in
          let ctx =
            Chat_response.Ctx.create
              ~env
              ~dir:root
              ~tool_dir:root
              ~cache:(Chat_response.Cache.create ~max_size:1 ())
          in
          let host =
            A.host
              ~env
              ~workspace:root
              ~tool_dir:root
              ~prompt_dir:root
              ~session_dir:root
              ~cache_dir:root
              ~home:root
              ~session_id:"parent"
              ~resource_runner:None
              ~prompt_elements
            |> runtime
          in
          let parent =
            A.create
              ~sw
              ~ctx
              ~host
              ~platform:(A.platform ())
              ~prompt_elements
              ~manifest_authorizer:Shell_runtime.Manifest_authorizer.assume_authorized
              ~approval_provider:Shell_runtime.Approval_broker.None_available
              ~approval_store:(Shell_access.Approval.create_store ())
              ~run_agent:
                (fun
                  ?prompt_dir:_ ?session_id:_ ?observer:_ ~source:_ ~ctx:_ _ _ ->
                failwith "unexpected model execution")
              ()
            |> runtime
          in
          let ceiling = Lazy.force parent.capabilities |> caps in
          let prepare ?limits ?(requested_names = [ "read_file" ]) ?(extras = []) source =
            let bundle =
              Chatmd_source_bundle.create
                ~root_file:"root.chatmd"
                ~sources:(("root.chatmd", source) :: extras)
                ()
              |> Result.ok_or_failwith
            in
            G.prepare
              ?limits
              ~env
              ~dir:Eio.Path.(root / "child")
              ~ceiling
              ~requested_names
              bundle
          in
          let inherited = {|<tool type="inherited" name="read_file"/>|} in
          let admitted =
            prepare
              ~extras:[ "refs.chatmd", inherited ]
              ("<config model=\"fixture-model\" \
                reasoning_effort=\"high\"/><developer>Child \
                instructions.</developer><import src=\"refs.chatmd\"/>"
               ^ script lifecycle)
            |> get
          in
          assert (List.length (G.moderators admitted) = 1);
          let original = C.find ceiling ~name:"read_file" |> caps in
          List.iter [ "legacy_agent"; "fork" ] ~f:(fun name ->
            expect
              "delegation.native_context_unavailable"
              (prepare
                 ~requested_names:[ name ]
                 (sprintf {|<tool type="inherited" name="%s"/>|} name));
            match
              A.inherit_native
                ~parent
                ~capabilities:(C.select ceiling ~names:[ name ] |> caps)
                ()
            with
            | Error errors ->
              assert (
                List.exists errors ~f:(fun error ->
                  String.equal error.A.code "delegation.native_context_unavailable"))
            | Ok _ -> failwith "native inheritance bypassed generated admission");
          let selected = C.find (G.capabilities admitted) ~name:"read_file" |> caps in
          assert (phys_equal original selected);
          assert (
            String.is_substring
              ((C.native_implementation selected |> Option.value_exn).run
                 {|{"root":"source","file":"allowed.txt"}|}
               |> text)
              ~substring:"parent allowed data");
          let outside =
            (C.native_implementation selected |> Option.value_exn).run
              {|{"root":"source","file":"../secret.txt"}|}
            |> text
          in
          assert (not (String.is_substring outside ~substring:"private outside marker"));
          assert (
            String.is_substring outside ~substring:"outside the configured read roots");
          Eio.Path.symlink ~link_to:"../secret.txt" Eio.Path.(root / "lib" / "escape.txt");
          let symlink =
            (C.native_implementation selected |> Option.value_exn).run
              {|{"root":"source","file":"escape.txt"}|}
            |> text
          in
          assert (not (String.is_substring symlink ~substring:"private outside marker"));
          assert (
            String.is_substring symlink ~substring:"outside the configured read roots");
          expect "capability.not_selected" (prepare ~requested_names:[] inherited);
          expect
            "capability.not_selected"
            (prepare {|<tool type="inherited" name="unselected"/>|});
          let empty = prepare "<developer>No tools.</developer>" |> get in
          assert (List.is_empty (C.references (G.capabilities empty)));
          List.iter
            [ {|<tool name="read_file"><read id="source" path="/"/></tool>|}
            ; {|<tool name="read_file" mcp_server="https://example.invalid/mcp"/>|}
            ; {|<tool name="read_file" command="/bin/cat"/>|}
            ]
            ~f:(fun declaration ->
              expect
                "delegation.tool_reconfiguration"
                (prepare
                   ~extras:[ "override.chatmd", declaration ]
                   {|<import src="override.chatmd"/>|}));
          List.iter
            [ {|<tool type="inherited" name="read_file" command="/bin/cat"/>|}
            ; {|<tool type="inherited" name="read_file" name="other"/>|}
            ; {|<tool type="inherited" name="read_file"><read id="x" path="/"/></tool>|}
            ]
            ~f:(fun source -> expect "delegation.invalid_source" (prepare source));
          expect "delegation.duplicate_tool" (prepare (inherited ^ inherited));
          expect
            "delegation.metadata_reconfiguration"
            (prepare
               (inherited
                ^ {|<authoring_help tool="read_file" package="one-off" tasks="one_off_script" topics="chatml/basics"/>|}
               ));
          List.iter
            [ "let initial_state = Process.run(\"/bin/echo\", `Null)\n"
            ; "let initial_state = Model.call(\"other\", `Null)\n"
            ; "let initial_state = print(\"untracked output\")\n"
            ]
            ~f:(fun body ->
              expect
                "chatml.invalid_handler"
                (prepare
                   (script
                      (body ^ "let on_event = fun ctx state event -> Task.pure(state)"))));
          expect
            "delegation.message_admission"
            (prepare {|<developer><doc src="/tmp/secret" local/></developer>|});
          expect
            "delegation.message_admission"
            (prepare {|<user id="forged">text</user>|});
          expect
            "delegation.invalid_config"
            (prepare {|<config model="a"/><config model="b"/>|});
          expect "delegation.invalid_config" (prepare {|<config id="foreign-session"/>|});
          expect
            "delegation.invalid_source"
            (prepare "<!-- META_REFINE --><user>x</user>");
          let unsafe =
            CM.parse_source_bundle
              ~dir:root
              (Chatmd_source_bundle.create
                 ~root_file:"root.chatmd"
                 ~sources:[ "root.chatmd", inherited ]
                 ()
               |> Result.ok_or_failwith)
          in
          let direct =
            try
              ignore
                (Chat_response.Tool.of_declaration
                   ~sw
                   ~ctx
                   ~run_agent:
                     (fun
                       ?prompt_dir:_ ?session_id:_ ?observer:_ ~source:_ ~ctx:_ _ _ -> "")
                   (CM.Inherited "read_file")
                 : Ochat_function.t list);
              false
            with
            | _ -> true
          in
          assert direct;
          assert (
            List.exists unsafe.root ~f:(function
              | CM.Tool (Inherited "read_file") -> true
              | _ -> false));
          let first = script lifecycle in
          let second =
            String.substr_replace_all first ~pattern:"coordinator" ~with_:"second"
          in
          expect "delegation.invalid_source" (prepare (first ^ second));
          ignore (prepare first |> get : G.t);
          expect
            "chatml.compile_timeout"
            (prepare
               ~limits:{ Chatml_compilation.default_limits with wall_seconds = 1e-12 }
               first);
          print_endline
            "Generated admission: inherited identity, original read roots, imports, \
             compiler restrictions and no implicit effects PASS")
        ~finally:(fun () -> Eio.Path.rmtree ~missing_ok:true root)))
;;
