open Core
module CM = Prompt.Chat_markdown
module Spec = Chatmd_shell_spec.Extension_spec
module Compiler = Chat_response.Extension_compiler
module Caps = Chat_response.Tool_capability

let digest = Chatmd_shell_spec.Source_ref.digest

let get = function
  | Ok value -> value
  | Error errors -> raise_s [%sexp (errors : Chatmd_shell_spec.Diagnostic.t list)]
;;

let cap_get = function
  | Ok value -> value
  | Error error -> raise_s [%sexp (error : Caps.error)]
;;

let native name calls =
  let module Definition = struct
    type input = string

    let name = name
    let type_ = "function"
    let description = None
    let parameters = `True
    let input_of_string input = input
  end
  in
  Ochat_function.create_function
    (module Definition)
    (fun value ->
       incr calls;
       Openai.Responses.Tool_output.Output.Text value)
;;

let with_definition ?(moderator = false) body f =
  Eio_main.run (fun env ->
    Mirage_crypto_rng_unix.use_default ();
    let dir = Eio.Stdenv.cwd env in
    let source_loader =
      Source_loader.captured_filesystem
        ~root:dir
        ~sources:[ "handler.chatml", body; "schema.json", "true" ]
    in
    let root =
      if moderator
      then
        {|<script id="handler" language="chatml" kind="moderator" api="extensibility-v1" src="handler.chatml"/><tool name="extension" type="moderator" moderator="handler" input_schema="schema.json" output_schema="schema.json"/>|}
      else
        {|<script id="handler" language="chatml" kind="tool" src="handler.chatml"/><tool name="extension" type="chatml" script="handler" entrypoint="run" input_schema="schema.json" output_schema="schema.json"><uses tool="selected"/></tool>|}
    in
    let parsed = CM.parse_chat_inputs ~source:"root.chatmd" ~source_loader ~dir root in
    let script =
      List.find_map_exn parsed ~f:(function
        | CM.Extension_script script -> Some script
        | _ -> None)
    in
    let tool =
      List.find_map_exn parsed ~f:(function
        | CM.Tool (Extension tool) -> Some tool
        | _ -> None)
    in
    let calls = ref 0 in
    let selected = native "selected" calls in
    let capabilities =
      Caps.create
        ~owner:"test"
        ~resource_fingerprint:(digest "resources")
        [ digest "selected", selected; digest "other", native "other" calls ]
      |> cap_get
    in
    f script tool capabilities selected;
    assert (!calls = 0))
;;

let source =
  "let poison = fail(\"initializer must not run\")\n\
   let run = fun ctx input -> Task.pure(`Complete(input))"
;;

let%test_unit
    "prepared tool binds exact selected implementations without executing source"
  =
  with_definition source (fun script tool capabilities selected ->
    let prepared = Compiler.prepare ~scripts:[ script ] ~capabilities tool |> get in
    let bound = Caps.find (Compiler.capabilities prepared) ~name:"selected" |> cap_get in
    assert (phys_equal (Caps.native_implementation bound |> Option.value_exn) selected);
    assert (Result.is_error (Caps.find (Compiler.capabilities prepared) ~name:"other"));
    assert (
      Result.is_ok
        (Chatmd_shell_spec.Tool_schema.validate (Compiler.input_schema prepared) `Null));
    let again = Compiler.prepare ~scripts:[ script ] ~capabilities tool |> get in
    assert (String.equal (Compiler.fingerprint prepared) (Compiler.fingerprint again));
    let narrower =
      Compiler.prepare ~scripts:[ script ] ~capabilities { tool with uses = [] } |> get
    in
    assert (List.is_empty (Caps.references (Compiler.capabilities narrower)));
    assert (
      not (String.equal (Compiler.fingerprint prepared) (Compiler.fingerprint narrower))))
;;

let%test_unit
    "prepared tool rejects forged versions dependencies schemas digests and limits"
  =
  with_definition source (fun script tool capabilities _ ->
    let prepare ?(scripts = [ script ]) tool =
      Compiler.prepare ~scripts ~capabilities tool
    in
    List.iter
      [ { tool with version = 2 }
      ; { tool with uses = [ "missing" ] }
      ; { tool with uses = [ "selected"; "selected" ] }
      ; { tool with
          implementation = Standalone { script = script.id; entrypoint = "other" }
        }
      ; { tool with input_schema = { tool.input_schema with source_text = "false" } }
      ]
      ~f:(fun tool -> assert (Result.is_error (prepare tool)));
    assert (Result.is_error (prepare ~scripts:[] tool));
    assert (Result.is_error (prepare ~scripts:[ script; script ] tool));
    List.iter
      [ { script with version = 2 }
      ; { script with kind = Moderator_script }
      ; { script with source_sha256 = digest "different" }
      ; { script with limits = { script.limits with fuel = 0 } }
      ]
      ~f:(fun script -> assert (Result.is_error (prepare ~scripts:[ script ] tool)));
    assert (
      Result.is_error
        (Compiler.prepare ~max_source_bytes:1 ~scripts:[ script ] ~capabilities tool));
    assert (
      Result.is_error
        (Compiler.prepare
           ~max_source_bytes:((1024 * 1024) + 1)
           ~scripts:[ script ]
           ~capabilities
           tool));
    let body = "let unrelated = 1" in
    let missing = { script with source = Inline body; source_sha256 = digest body } in
    assert (Result.is_error (prepare ~scripts:[ missing ] tool)))
;;

let%test_unit
    "prepared moderator uses owning capabilities and validates the event handler"
  =
  with_definition
    ~moderator:true
    {|let initial_state = 0
let on_event = fun ctx state event ->
  match event with
  | `Tool_invoked(invocation) -> Task.bind(Invocation.resolve(invocation.context.invocation_id, `Complete(invocation.input)), fun ignored -> Task.pure(state + 1))
  | _ -> Task.pure(state)|}
    (fun script tool capabilities _ ->
       let prepared = Compiler.prepare ~scripts:[ script ] ~capabilities tool |> get in
       assert (
         String.equal
           (Caps.fingerprint capabilities)
           (Caps.fingerprint (Compiler.capabilities prepared)));
       assert (
         Result.is_error
           (Compiler.prepare
              ~scripts:[ script ]
              ~capabilities
              { tool with uses = [ "selected" ] }));
       let body = "let initial_state = 0" in
       let missing = { script with source = Inline body; source_sha256 = digest body } in
       assert (Result.is_error (Compiler.prepare ~scripts:[ missing ] ~capabilities tool)))
;;
