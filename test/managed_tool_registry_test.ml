open Core
module C = Chat_response.Tool_capability
module R = Chat_response.Managed_tool_registry
module E = Chat_response.Extension_compiler
module Spec = Chatmd_shell_spec.Extension_spec
module CM = Prompt.Chat_markdown

let capability = function
  | Ok value -> value
  | Error error -> failwith error.C.message
;;

let prepared = function
  | Ok value -> value
  | Error errors ->
    failwith
      (String.concat
         ~sep:"; "
         (List.map errors ~f:Chatmd_shell_spec.Diagnostic.to_string))
;;

let digest = Chatmd_shell_spec.Source_ref.digest

let%expect_test
    "managed bindings retain compiled dependencies and invalidate changed authority"
  =
  Eio_main.run (fun env ->
    Mirage_crypto_rng_unix.use_default ();
    let calls = ref 0 in
    let module Native = struct
      type input = string

      let name = "native"
      let description = Some "echo"
      let type_ = "function"
      let parameters = `True
      let input_of_string value = value
    end
    in
    let native =
      Ochat_function.create_function
        (module Native)
        (fun input ->
           incr calls;
           Openai.Responses.Tool_output.Output.Text input)
    in
    let base resource =
      C.create
        ~owner:"owner"
        ~resource_fingerprint:(digest resource)
        [ digest "native-v1", native ]
      |> capability
    in
    let original = base "restricted" in
    let loader =
      Source_loader.captured_filesystem
        ~root:(Eio.Stdenv.cwd env)
        ~sources:[ "schema.json", "true" ]
    in
    let elements =
      CM.parse_chat_inputs
        ~dir:(Eio.Stdenv.cwd env)
        ~source_loader:loader
        {|<tool name="native"/>
<script id="leaf-code" language="chatml" kind="tool">
let never = fail("initializer must not run")
let run ctx input = Task.bind(Tool.call("native", input), fun result -> match result with
| `Ok(value) -> Task.pure(`Complete(value)) | `Error(code) -> Task.fail(code))
</script>
<script id="root-code" language="chatml" kind="tool">
let run ctx input = Task.bind(Tool.call("leaf", input), fun result -> match result with
| `Ok(value) -> Task.pure(`Complete(value)) | `Error(code) -> Task.fail(code))
</script>
<script id="owner-code" language="chatml" kind="moderator" api="extensibility-v1">
let initial_state = fail("moderator must not initialize")
let on_event ctx state event = Task.pure(state)
</script>
<tool name="leaf" type="chatml" script="leaf-code" entrypoint="run" input_schema="schema.json" output_schema="schema.json"><uses tool="native"/></tool>
<tool name="root" type="chatml" script="root-code" entrypoint="run" input_schema="schema.json" output_schema="schema.json"><uses tool="leaf"/></tool>
<tool name="review" type="moderator" moderator="owner-code" input_schema="schema.json" output_schema="schema.json"/>|}
    in
    let build base elements = R.prepare ~env ~owner:"owner" ~capabilities:base elements in
    let first = build original elements |> prepared in
    let restricted =
      C.create
        ~owner:"owner"
        ~resource_fingerprint:(digest "restricted")
        ~delegation_restrictions:[ "native", "requires its original actor services" ]
        [ digest "native-v1", native ]
      |> capability
    in
    let restricted_managed = build restricted elements |> prepared in
    let selected_restricted =
      C.select (R.capabilities restricted_managed) ~names:[ "root" ] |> capability
    in
    (match R.delegate_standalone restricted_managed ~selected:selected_restricted with
     | Error error ->
       [%test_eq: string] "delegation.native_context_unavailable" error.code
     | Ok _ -> failwith "private dependency bypassed native delegation restriction");
    let pins =
      Chat_response.Background_request.capability_pins original
      |> Result.map_error ~f:(fun error -> error.Agent_protocol.Error.message)
      |> Result.ok_or_failwith
    in
    assert (
      Result.is_error
        (Chat_response.Background_request.rebind_capabilities
           ~pins
           ~capabilities:restricted));
    let registry = R.capabilities first in
    let find name = C.find registry ~name |> capability in
    assert (phys_equal (find "native") (C.find original ~name:"native" |> capability));
    let root = find "root"
    and leaf = find "leaf"
    and review = find "review" in
    assert (Option.is_none (C.native_implementation root));
    assert (Option.is_none (C.native_implementation review));
    let root_program = R.resolve first root |> capability in
    let public = C.select registry ~names:[ "root" ] |> capability in
    let delegated = R.delegate_standalone first ~selected:public |> capability in
    let execution_registry = R.delegation_registry delegated in
    let delegated_caps = R.capabilities execution_registry in
    let names caps =
      C.references caps
      |> List.map ~f:(fun reference -> reference.C.name)
      |> List.sort ~compare:String.compare
    in
    [%test_eq: string list] [ "root" ] (names (R.delegation_selection delegated));
    [%test_eq: string list] [ "leaf"; "native"; "root" ] (names delegated_caps);
    assert (phys_equal (R.resolve execution_registry root |> capability) root_program);
    assert (
      phys_equal (C.find delegated_caps ~name:"native" |> capability) (find "native"));
    let exposed = R.delegation_definition delegated in
    [%test_eq: string list]
      [ "root" ]
      (List.map (E.prepared_tools exposed) ~f:(fun p -> (E.declaration p).name));
    assert (List.is_empty (E.compiled_scripts exposed));
    assert (List.is_empty (E.compiled_scripts (R.definition execution_registry)));
    assert (Result.is_error (C.find delegated_caps ~name:"review"));
    R.revalidate execution_registry ~current:delegated_caps |> capability;
    assert (Result.is_error (R.revalidate execution_registry ~current:public));
    let stateful = C.select registry ~names:[ "review" ] |> capability in
    (match R.delegate_standalone first ~selected:stateful with
     | Error e -> [%test_eq: string] "delegation.owner_dispatch_unavailable" e.code
     | Ok _ -> failwith "stateful tool lost its original owner");
    let selected = E.capabilities root_program in
    assert (phys_equal (C.find selected ~name:"leaf" |> capability) leaf);
    assert (Result.is_error (C.find selected ~name:"native"));
    assert (
      phys_equal
        (C.find (E.capabilities (R.resolve first leaf |> capability)) ~name:"native"
         |> capability)
        (find "native"));
    let same = build (base "restricted") elements |> prepared in
    let same_root = C.find (R.capabilities same) ~name:"root" |> capability in
    assert (
      String.equal (C.permission_fingerprint root) (C.permission_fingerprint same_root));
    assert (Result.is_error (R.resolve first same_root));
    assert (Result.is_error (R.delegate_standalone same ~selected:public));
    assert (Result.is_error (R.revalidate first ~current:(R.capabilities same)));
    R.revalidate first ~current:registry |> capability;
    let changed = build (base "broader") elements |> prepared in
    let changed_root = C.find (R.capabilities changed) ~name:"root" |> capability in
    assert (
      not
        (String.equal
           (C.permission_fingerprint root)
           (C.permission_fingerprint changed_root)));
    let without_native =
      C.select registry ~names:[ "root"; "leaf"; "review" ] |> capability
    in
    assert (Result.is_error (R.revalidate first ~current:without_native));
    let mutate f =
      List.map elements ~f:(function
        | CM.Tool (Extension tool) -> CM.Tool (Extension (f tool))
        | other -> other)
    in
    let code result =
      match result with
      | Ok _ -> "accepted"
      | Error errors -> (List.hd_exn errors).Chatmd_shell_spec.Diagnostic.code
    in
    let missing =
      mutate (fun tool ->
        if String.equal tool.name "root" then { tool with uses = [ "missing" ] } else tool)
    in
    let with_stateful_dependency =
      mutate (fun tool ->
        match String.equal tool.name "leaf" with
        | true -> { tool with uses = [ "review" ] }
        | false -> tool)
      |> build original
      |> prepared
    in
    let stateful_root =
      C.select (R.capabilities with_stateful_dependency) ~names:[ "root" ] |> capability
    in
    (match R.delegate_standalone with_stateful_dependency ~selected:stateful_root with
     | Error e -> [%test_eq: string] "delegation.owner_dispatch_unavailable" e.code
     | Ok _ -> failwith "transitive stateful dependency lost its owner");
    let cyclic =
      mutate (fun tool ->
        if String.equal tool.name "leaf" then { tool with uses = [ "root" ] } else tool)
    in
    let corrupt =
      mutate (fun tool ->
        { tool with
          output_schema = { tool.output_schema with source_sha256 = digest "forged" }
        })
    in
    let schema_changed =
      mutate (fun tool ->
        let source_text = {|{"type":"string"}|} in
        { tool with
          output_schema =
            { tool.output_schema with source_text; source_sha256 = digest source_text }
        })
    in
    let changed_schema = build original schema_changed |> prepared in
    let code_changed =
      List.map elements ~f:(function
        | CM.Extension_script script when String.equal script.id "leaf-code" ->
          let text = "let added = 1\n" ^ Spec.script_text script in
          CM.Extension_script
            { script with source = Inline text; source_sha256 = digest text }
        | other -> other)
    in
    let changed_code = build original code_changed |> prepared in
    assert (
      not
        (String.equal
           (C.permission_fingerprint root)
           (C.permission_fingerprint
              (C.find (R.capabilities changed_code) ~name:"root" |> capability))));
    let retag_native =
      CM.Authoring_help
        { tool = "native"
        ; help =
            { version = 1
            ; package = "custom"
            ; tasks = [ One_off_script ]
            ; topics = [ "custom.one-off" ]
            ; required_helpers = []
            }
        ; source_ref = (E.declaration root_program).source_ref
        }
      :: elements
    in
    assert (
      String.equal (code (build original retag_native)) "authoring.metadata_override");
    assert (
      not
        (String.equal
           (C.permission_fingerprint root)
           (C.permission_fingerprint
              (C.find (R.capabilities changed_schema) ~name:"root" |> capability))));
    assert (Int.equal !calls 0);
    print_s
      [%sexp
        (List.map (C.references registry) ~f:(fun reference -> reference.C.name)
         : string list)
      , (List.map (C.references selected) ~f:(fun reference -> reference.C.name)
         : string list)
      , (code (build original missing) : string)
      , (code (build original cyclic) : string)
      , (code (build original corrupt) : string)
      , (!calls : int)]);
  [%expect
    {|
    ((leaf native review root) (leaf) capability.not_selected
     chatml.invalid_definition chatmd.schema_digest_mismatch 0)
    |}]
;;
