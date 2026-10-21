open! Core
module A = Chatmd_authoring_assistance
module P = Agent_protocol
module W = P.Authoring_assistance.Assistance
module S = Chatml_source

let ok sexp_of_error = function
  | Ok value -> value
  | Error error -> raise_s (sexp_of_error error)
;;

let source ?(name = "root.chatmd") contents =
  let id =
    S.Id.create ~package_namespace:"assistance-test" ~relative_path:name
    |> Or_error.ok_exn
  in
  S.create id ~contents ~expected_digest:None ~max_bytes:None ~checkpoint:ignore
  |> Or_error.ok_exn
;;

let input contents =
  Chatmd_parser_input.create
    ~role:Root
    ~path:"root.chatmd"
    ~authored:contents
    ~effective:contents
    ~max_input_bytes:65536
    ~checkpoint:ignore
  |> ok Chatmd_parser_input.Error.sexp_of_t
;;

let generator = P.Id.Generator.create ~bytes:(fun n -> String.make n '\000')
let host = P.Id.Server.create_with generator
let context_id = P.Id.Authoring_context.create_with generator

let make_request captured ~cursor_byte =
  let source =
    P.Authoring_common.Source.create
      ~id:(S.id captured)
      ~digest:(S.digest captured)
      ~byte_length:(S.byte_length captured)
      ~editor_revision:None
    |> ok P.Error.sexp_of_t
  in
  let context =
    P.Authoring_context_core.Context.create
      ~id:context_id
      ~host
      ~origin:(Supplied_document { source; default_environment = None })
      ~generation:0
      ~basis:(Document { source; selection = None })
      ~basis_fingerprint:(S.Digest.of_contents "basis" ~checkpoint:ignore)
      ~editable_sources:[]
    |> ok P.Error.sexp_of_t
  in
  W.Request.create
    ~context:(P.Authoring_context_core.Context.Expected.of_view context)
    ~source
    ~cursor_byte
    ~max_items:32
  |> ok P.Error.sexp_of_t
;;

let limits () =
  A.Limits.create
    ~max_work_steps:250_000
    ~max_authored_bytes:65536
    ~max_total_captured_bytes:262144
    ~max_parsed_inputs:32
    ~max_paths:32
    ~max_selected_tools:32
    ~max_candidates:32
    ~max_label_bytes:1024
  |> Or_error.ok_exn
;;

let facts ?project ?(paths = []) ?(selected_tools = []) text ~cursor_byte =
  let captured = source text in
  let input = input text in
  let request = make_request captured ~cursor_byte in
  A.Facts.create
    ~request
    ~authored:captured
    ~lexical_input:input
    ~parsed:None
    ~captured_inputs:[]
    ~project
    ~selected:None
    ~paths
    ~selected_tools
    ~limits:(limits ())
    ~checkpoint:ignore
  |> ok A.Error.sexp_of_t
;;

let suggestions ?project ?paths ?selected_tools text ~cursor_byte =
  let facts = facts ?project ?paths ?selected_tools text ~cursor_byte in
  let result = A.produce facts ~checkpoint:ignore |> ok A.Error.sexp_of_t in
  W.Result.items result
  |> List.map ~f:(fun item -> W.Suggestion.kind item, W.Suggestion.insert_text item)
;;

let has items kind text =
  List.exists items ~f:(fun (actual_kind, actual_text) ->
    W.Suggestion.compare_kind kind actual_kind = 0 && String.equal text actual_text)
;;

let project () =
  let captured =
    source
      ~name:"project.json"
      {|{"schema_version":1,"namespace":"p","source_dirs":["src"],"modules":{"A":{"implementation":"src/a.chatml"}},"environments":{"dev":{"modules":{}},"test":{"modules":{}}},"entries":{"main":{"source":"src/a.chatml","target":"one_off_v1"}}}|}
  in
  Chatml_project.decode captured ~limits:Document_schema.Limits.default ~checkpoint:ignore
  |> ok (fun _ -> Sexp.Atom "project decode failed")
;;

let route captured spelling =
  A.Path_candidate.
    { authored_base_id = S.id captured
    ; authored_base_digest = S.digest captured
    ; base_directory = None
    ; edit_spelling = spelling
    }
;;

let edit text = A.Path_candidate.Edit_spelling.create text |> ok A.Error.sexp_of_t

let parse sources =
  Eio_main.run (fun env ->
    let dir = Eio.Stdenv.cwd env in
    let root = List.Assoc.find_exn sources "root.chatmd" ~equal:String.equal in
    let loader = Source_loader.captured_filesystem ~root:dir ~sources in
    let limits =
      Prompt.Chat_markdown.Limits.create
        ~max_input_bytes:65536
        ~max_parse_calls:128
        ~max_tokens:65536
        ~max_depth:64
      |> Or_error.ok_exn
    in
    Prompt.Chat_markdown.prepare_chat_inputs
      ~source:"root.chatmd"
      ~source_loader:loader
      ~dir
      ~preprocess:Fn.id
      ~limits
      ~checkpoint:ignore
      root)
;;

let%expect_test "seven families use actual schema and source-bound cursor" =
  let tag = suggestions "<us" ~cursor_byte:3 in
  let attribute = suggestions "<script lan" ~cursor_byte:11 in
  let value = suggestions "<script kind=\"mo" ~cursor_byte:16 in
  let template = suggestions "" ~cursor_byte:0 in
  assert (has tag Tag "user");
  assert (has attribute Attribute "language");
  assert (has value Value "moderator");
  assert (has template Template "<user></user>");
  ignore
    (Prompt.Chat_markdown.Parsed.elements (parse [ "root.chatmd", "<user></user>" ])
     : Prompt.Chat_markdown.top_level_elements list);
  print_endline "tag, attribute, value, template: parser-schema/cursor PASS";
  [%expect {| tag, attribute, value, template: parser-schema/cursor PASS |}]
;;

let%expect_test "decoded project and admitted path have distinct evidence" =
  let project = project () in
  let environment = {|<script environment="de|} in
  let items = suggestions ~project environment ~cursor_byte:(String.length environment) in
  assert (has items Environment "dev");
  let path_text = {|<script project="./pr|} in
  let captured = source path_text in
  let edit_spelling = edit "./project.json" in
  let path =
    A.Path_candidate.create
      ~origin:(Project_source { project; route = route captured edit_spelling })
      ~edit_spelling
  in
  let items =
    suggestions ~project ~paths:[ path ] path_text ~cursor_byte:(String.length path_text)
  in
  assert (has items Path "./project.json");
  let doc = {|<user><doc src="../gu|} in
  let root = P.Id.Authoring_root.create_with generator in
  let path =
    A.Path_candidate.create
      ~origin:(Admitted_root root)
      ~edit_spelling:(edit "../guide.md")
  in
  let items = suggestions ~paths:[ path ] doc ~cursor_byte:(String.length doc) in
  assert (has items Path "../guide.md");
  print_endline "environment, declared project and host path: source evidence PASS";
  [%expect {| environment, declared project and host path: source evidence PASS |}]
;;

let%expect_test "selected tool names keep a distinct registry stamp" =
  let id = P.Id.Capability.create_with generator in
  let tool =
    A.Selected_tool.create
      ~id
      ~name:"read_file"
      ~binding_fingerprint:(S.Digest.of_contents "binding" ~checkpoint:ignore)
      ~selection_fingerprint:(S.Digest.of_contents "registry" ~checkpoint:ignore)
    |> ok A.Error.sexp_of_t
  in
  let text = {|<tool name="t" type="chatml"><uses tool="re|} in
  let items =
    suggestions ~selected_tools:[ tool ] text ~cursor_byte:(String.length text)
  in
  assert (has items Reference "read_file");
  print_endline "reference: selected tool stamp PASS";
  [%expect {| reference: selected tool stamp PASS |}]
;;

let%expect_test "only relevant unrepresentable tool text truncates" =
  let make name =
    A.Selected_tool.create
      ~id:(P.Id.Capability.create_with generator)
      ~name
      ~binding_fingerprint:(S.Digest.of_contents name ~checkpoint:ignore)
      ~selection_fingerprint:(S.Digest.of_contents "registry" ~checkpoint:ignore)
    |> ok A.Error.sexp_of_t
  in
  let tools = [ make "read_file"; make "bad\255" ] in
  let text = {|<tool name="t" type="chatml"><uses tool="|} in
  let result =
    A.produce
      (facts ~selected_tools:tools text ~cursor_byte:(String.length text))
      ~checkpoint:ignore
    |> ok A.Error.sexp_of_t
  in
  assert (
    match W.Result.status result with
    | Truncated Unrepresentable_text -> true
    | _ -> false);
  assert (
    List.exists (W.Result.items result) ~f:(fun item ->
      String.equal (W.Suggestion.insert_text item) "read_file"));
  let unrelated =
    A.produce (facts ~selected_tools:tools "<us" ~cursor_byte:3) ~checkpoint:ignore
    |> ok A.Error.sexp_of_t
  in
  assert (
    match W.Result.status unrelated with
    | Complete -> true
    | _ -> false);
  print_endline "relevant malformed display text only: PASS";
  [%expect {| relevant malformed display text only: PASS |}]
;;

let%expect_test "declared member provenance rejects invented and stale routes" =
  let project = project () in
  let text = {|<script src="src/a|} in
  let captured = source text in
  let spelling = edit "src/a.chatml" in
  let member = Source.Portable_path.create "src/a.chatml" |> Or_error.ok_exn in
  let make ?(member = member) ?(route = route captured spelling) () =
    A.Path_candidate.create
      ~origin:
        (Decoded_member
           { project
           ; role = Chatml_project.Declared_paths.Module_implementation
           ; member
           ; route
           })
      ~edit_spelling:spelling
  in
  assert (
    has
      (suggestions ~project ~paths:[ make () ] text ~cursor_byte:(String.length text))
      Path
      "src/a.chatml");
  let invalid_member =
    Source.Portable_path.create "src/forged.chatml" |> Or_error.ok_exn
  in
  let rejected path =
    let request = make_request captured ~cursor_byte:(String.length text) in
    let input = input text in
    match
      A.Facts.create
        ~request
        ~authored:captured
        ~lexical_input:input
        ~parsed:None
        ~captured_inputs:[]
        ~project:(Some project)
        ~selected:None
        ~paths:[ path ]
        ~selected_tools:[]
        ~limits:(limits ())
        ~checkpoint:ignore
    with
    | Error A.Error.Invalid_path_provenance -> true
    | _ -> false
  in
  assert (rejected (make ~member:invalid_member ()));
  let stale_route =
    { (route captured spelling) with
      authored_base_digest = S.Digest.of_contents "stale" ~checkpoint:ignore
    }
  in
  assert (rejected (make ~route:stale_route ()));
  print_endline "decoded member, invented path and stale base: PASS";
  [%expect {| decoded member, invented path and stale base: PASS |}]
;;

let%expect_test "attribute edits preserve strict decoded bytes and tag boundaries" =
  let root = P.Id.Authoring_root.create_with generator in
  let path value =
    A.Path_candidate.create ~origin:(Admitted_root root) ~edit_spelling:(edit value)
  in
  let cases =
    [ {|<user><doc src="a|}, "a&b", "a&amp;b", {| src="a&amp;b"|}
    ; {|<user><doc src='a|}, "a&b", "a&amp;b", {| src='a&amp;b'|}
    ; {|<user><doc src=a|}, "a&b", "a&b", " src=a&b"
    ; {|<user><doc src=a|}, "a<b", "a<b", " src=a<b"
    ; {|<user><doc src='a|}, "a'b", "a&apos;b", {| src='a&apos;b'|}
    ; {|<user><doc src="a|}, "a>b", "a&gt;b", {| src="a&gt;b"|}
    ]
  in
  List.iter cases ~f:(fun (text, value, inserted, completed) ->
    let items =
      suggestions ~paths:[ path value ] text ~cursor_byte:(String.length text)
    in
    assert (has items Path inserted);
    let decoded = Chatmd_attribute_lex.complete_exn completed ~checkpoint:ignore in
    assert (
      List.mem
        decoded
        ("src", Some value)
        ~equal:(fun (name_a, value_a) (name_b, value_b) ->
          String.equal name_a name_b && Option.equal String.equal value_a value_b)));
  let unquoted = {|<user><doc src=a|} in
  let unquoted_items =
    suggestions
      ~paths:[ path "a>b"; path "a'b" ]
      unquoted
      ~cursor_byte:(String.length unquoted)
  in
  assert (not (has unquoted_items Path "a>b"));
  assert (not (has unquoted_items Path "a'b"));
  ignore
    (parse [ "root.chatmd", "<user><doc src=a&b/></user>" ]
     : Prompt.Chat_markdown.Parsed.t);
  ignore
    (parse [ "root.chatmd", "<user><doc src=a<b/></user>" ]
     : Prompt.Chat_markdown.Parsed.t);
  ignore
    (parse [ "root.chatmd", {|<user><doc src="a&gt;b"/></user>|} ]
     : Prompt.Chat_markdown.Parsed.t);
  let middle = {|<user><doc src="aXb"|} in
  let items = suggestions ~paths:[ path "a&b" ] middle ~cursor_byte:18 in
  assert (not (has items Path "a&amp;b"));
  print_endline "quoted/unquoted edits, parser boundary and middle-token: PASS";
  [%expect {| quoted/unquoted edits, parser boundary and middle-token: PASS |}]
;;

let%expect_test "installed builtin suggestion resolves the embedded asset table" =
  let text = {|<user><doc src="builtin:ch|} in
  let result =
    A.produce (facts text ~cursor_byte:(String.length text)) ~checkpoint:ignore
    |> ok A.Error.sexp_of_t
  in
  let asset =
    Builtin_instructions.Id.of_string "chatml"
    |> Or_error.ok_exn
    |> Builtin_instructions.find
    |> ok Builtin_instructions.Error.sexp_of_t
  in
  assert (
    List.exists (W.Result.items result) ~f:(fun item ->
      String.equal (W.Suggestion.insert_text item) "builtin:chatml"
      &&
      match W.Suggestion.evidence item with
      | Installed_builtin { id; sha256 } ->
        String.equal id "chatml"
        && String.equal (S.Digest.to_hex sha256) (Builtin_instructions.Asset.sha256 asset)
      | _ -> false));
  print_endline "embedded builtin ID and digest: PASS";
  [%expect {| embedded builtin ID and digest: PASS |}]
;;

let%expect_test "actual parsed script reference carries captured declaration owner" =
  let text =
    {|<script id="worker" language="chatml" kind="tool">let run = fun ctx input -> Task.pure(`Complete(input))</script><tool name="do_work" type="chatml" script="worker" entrypoint="run" input_schema="schema.json" output_schema="schema.json"/>|}
  in
  let parsed = parse [ "root.chatmd", text; "schema.json", {|{"type":"object"}|} ] in
  let captured = source text in
  let lexical_input = Prompt.Chat_markdown.Parsed.root_input parsed in
  let capture =
    A.Captured_input.create
      ~input:lexical_input
      ~source:captured
      ~max_bytes:65536
      ~checkpoint:ignore
    |> ok A.Error.sexp_of_t
  in
  let cursor_byte =
    let marker = {|script="worker"|} in
    let start = String.substr_index_exn text ~pattern:marker in
    start + String.length marker - 1
  in
  let request = make_request captured ~cursor_byte in
  let facts =
    A.Facts.create
      ~request
      ~authored:captured
      ~lexical_input
      ~parsed:(Some parsed)
      ~captured_inputs:[ capture ]
      ~project:None
      ~selected:None
      ~paths:[]
      ~selected_tools:[]
      ~limits:(limits ())
      ~checkpoint:ignore
    |> ok A.Error.sexp_of_t
  in
  let result = A.produce facts ~checkpoint:ignore |> ok A.Error.sexp_of_t in
  assert (
    List.exists (W.Result.items result) ~f:(fun item ->
      W.Suggestion.compare_kind (W.Suggestion.kind item) Reference = 0
      && String.equal (W.Suggestion.insert_text item) "worker"
      &&
      match W.Suggestion.evidence item with
      | Parsed_document source ->
        P.Authoring_common.Source.equal source (W.Request.source request)
      | _ -> false));
  let imported_script =
    {|<script id="worker" language="chatml" kind="tool">let run = fun ctx input -> Task.pure(`Complete(input))</script><tool name="inside" type="chatml" script="worker" entrypoint="run" input_schema="../schema.json" output_schema="../schema.json"/>|}
  in
  let imported_root =
    {|<import src="nested/part.chatmd"/><tool name="do_work" type="chatml" script="worker" entrypoint="run" input_schema="schema.json" output_schema="schema.json"/>|}
  in
  let parsed =
    parse
      [ "root.chatmd", imported_root
      ; "nested/part.chatmd", imported_script
      ; "schema.json", {|{"type":"object"}|}
      ]
  in
  let captured = source imported_root in
  let lexical_input = Prompt.Chat_markdown.Parsed.root_input parsed in
  let captures =
    Prompt.Chat_markdown.Parsed.inputs parsed
    |> List.map ~f:(fun input ->
      let text, name =
        if Chatmd_parser_input.Role.equal (Chatmd_parser_input.role input) Root
        then imported_root, "root.chatmd"
        else imported_script, "nested/part.chatmd"
      in
      A.Captured_input.create
        ~input
        ~source:(source ~name text)
        ~max_bytes:65536
        ~checkpoint:ignore
      |> ok A.Error.sexp_of_t)
  in
  let marker = {|script="worker"|} in
  let cursor_byte =
    String.substr_index_exn imported_root ~pattern:marker + String.length marker - 1
  in
  let request = make_request captured ~cursor_byte in
  let facts =
    A.Facts.create
      ~request
      ~authored:captured
      ~lexical_input
      ~parsed:(Some parsed)
      ~captured_inputs:captures
      ~project:None
      ~selected:None
      ~paths:[]
      ~selected_tools:[]
      ~limits:(limits ())
      ~checkpoint:ignore
    |> ok A.Error.sexp_of_t
  in
  let result = A.produce facts ~checkpoint:ignore |> ok A.Error.sexp_of_t in
  assert (
    List.exists (W.Result.items result) ~f:(fun item ->
      match W.Suggestion.evidence item with
      | Parsed_document definition ->
        String.equal (W.Suggestion.insert_text item) "worker"
        && String.equal
             (S.Id.relative_path (P.Authoring_common.Source.id definition))
             "nested/part.chatmd"
      | _ -> false));
  let imported_input =
    Prompt.Chat_markdown.Parsed.inputs parsed
    |> List.find_exn ~f:(fun input ->
      not (Chatmd_parser_input.Role.equal (Chatmd_parser_input.role input) Root))
  in
  let imported_source = source ~name:"nested/part.chatmd" imported_script in
  let cursor_byte =
    let marker = {|script="worker"|} in
    String.substr_index_exn imported_script ~pattern:marker + String.length marker - 1
  in
  let request = make_request imported_source ~cursor_byte in
  let imported_facts =
    A.Facts.create
      ~request
      ~authored:imported_source
      ~lexical_input:imported_input
      ~parsed:(Some parsed)
      ~captured_inputs:captures
      ~project:None
      ~selected:None
      ~paths:[]
      ~selected_tools:[]
      ~limits:(limits ())
      ~checkpoint:ignore
    |> ok A.Error.sexp_of_t
  in
  let imported_result =
    A.produce imported_facts ~checkpoint:ignore |> ok A.Error.sexp_of_t
  in
  assert (
    List.exists (W.Result.items imported_result) ~f:(fun item ->
      String.equal (W.Suggestion.insert_text item) "worker"
      &&
      match W.Suggestion.evidence item with
      | Parsed_document definition ->
        P.Authoring_common.Source.equal definition (W.Request.source request)
      | _ -> false));
  let foreign_source = source ~name:"foreign/part.chatmd" imported_script in
  let foreign_request = make_request foreign_source ~cursor_byte in
  assert (
    match
      A.Facts.create
        ~request:foreign_request
        ~authored:foreign_source
        ~lexical_input:imported_input
        ~parsed:(Some parsed)
        ~captured_inputs:captures
        ~project:None
        ~selected:None
        ~paths:[]
        ~selected_tools:[]
        ~limits:(limits ())
        ~checkpoint:ignore
    with
    | Error Foreign_import_capture -> true
    | Ok _ | Error _ -> false);
  print_endline "root/imported script reference owners: PASS";
  [%expect {| root/imported script reference owners: PASS |}]
;;

let%expect_test "selected closure member is qualified and tied to current authored base" =
  let fixture = Fixtures.make () in
  let selected = Fixtures.compose fixture |> Support.ok in
  let prepared = Chatml_project_build.prepared selected in
  let project =
    Chatml_project.decode
      fixture.declaration
      ~limits:Document_schema.Limits.default
      ~checkpoint:ignore
    |> ok (fun _ -> Sexp.Atom "fixture project decode failed")
  in
  let text = {|<script src="../lib/src/u|} in
  let captured = source ~name:"nested/root.chatmd" text in
  let spelling = edit "../lib/src/util.chatml" in
  let route =
    { (route captured spelling) with
      base_directory = Some (Source.Portable_path.create "nested" |> Or_error.ok_exn)
    }
  in
  let make source_id =
    A.Path_candidate.create
      ~origin:(Selected_closure { prepared; source_id; route })
      ~edit_spelling:spelling
  in
  let request = make_request captured ~cursor_byte:(String.length text) in
  let input = input text in
  let admit path =
    A.Facts.create
      ~request
      ~authored:captured
      ~lexical_input:input
      ~parsed:None
      ~captured_inputs:[]
      ~project:(Some project)
      ~selected:(Some selected)
      ~paths:[ path ]
      ~selected_tools:[]
      ~limits:(limits ())
      ~checkpoint:ignore
  in
  let admitted = admit (make (S.id fixture.util)) |> ok A.Error.sexp_of_t in
  let result = A.produce admitted ~checkpoint:ignore |> ok A.Error.sexp_of_t in
  assert (
    List.exists (W.Result.items result) ~f:(fun item ->
      String.equal (W.Suggestion.insert_text item) "../lib/src/util.chatml"
      &&
      match W.Suggestion.evidence item with
      | Selected_catalog stamp ->
        S.Digest.equal
          (W.Selected_catalog.input_fingerprint stamp)
          (Chatml_prepared_program.fingerprint prepared)
      | _ -> false));
  let forged =
    S.Id.create ~package_namespace:"foreign" ~relative_path:"src/util.chatml"
    |> Or_error.ok_exn
  in
  (match admit (make forged) with
   | Error A.Error.Invalid_path_provenance -> ()
   | _ -> failwith "foreign package member not in selected closure admitted");
  let changed_project_source =
    S.create
      (S.id fixture.declaration)
      ~contents:
        {|{"schema_version":1,"namespace":"root","source_dirs":["src"],"environments":{"changed":{}}}|}
      ~expected_digest:None
      ~max_bytes:None
      ~checkpoint:ignore
    |> Or_error.ok_exn
  in
  let changed_project =
    Chatml_project.decode
      changed_project_source
      ~limits:Document_schema.Limits.default
      ~checkpoint:ignore
    |> ok (fun _ -> Sexp.Atom "changed project decode failed")
  in
  (match
     A.Facts.create
       ~request
       ~authored:captured
       ~lexical_input:input
       ~parsed:None
       ~captured_inputs:[]
       ~project:(Some changed_project)
       ~selected:(Some selected)
       ~paths:[]
       ~selected_tools:[]
       ~limits:(limits ())
       ~checkpoint:ignore
   with
   | Error A.Error.Selected_project_mismatch -> ()
   | _ -> failwith "stale project bytes paired with selected Build");
  print_endline "nested sibling edit, selected member, foreign ID: PASS";
  [%expect {| nested sibling edit, selected member, foreign ID: PASS |}]
;;

let%expect_test "work, item and cancellation boundaries are typed and atomic" =
  let text = "<script " in
  let captured = source text in
  let lexical_input = input text in
  let request = make_request captured ~cursor_byte:(String.length text) in
  let small =
    A.Limits.create
      ~max_work_steps:1
      ~max_authored_bytes:65536
      ~max_total_captured_bytes:262144
      ~max_parsed_inputs:32
      ~max_paths:32
      ~max_selected_tools:32
      ~max_candidates:32
      ~max_label_bytes:1024
    |> Or_error.ok_exn
  in
  let admit ~limits ~checkpoint =
    A.Facts.create
      ~request
      ~authored:captured
      ~lexical_input
      ~parsed:None
      ~captured_inputs:[]
      ~project:None
      ~selected:None
      ~paths:[]
      ~selected_tools:[]
      ~limits
      ~checkpoint
  in
  (match admit ~limits:small ~checkpoint:ignore with
   | Error A.Error.Work_limit -> ()
   | _ -> failwith "tight admission work was not rejected");
  let rec exact_admission_limit n =
    if n > 256 then failwith "exact admission work boundary not found";
    let limits =
      A.Limits.create
        ~max_work_steps:n
        ~max_authored_bytes:65536
        ~max_total_captured_bytes:262144
        ~max_parsed_inputs:32
        ~max_paths:32
        ~max_selected_tools:32
        ~max_candidates:32
        ~max_label_bytes:1024
      |> Or_error.ok_exn
    in
    match admit ~limits ~checkpoint:ignore with
    | Error A.Error.Work_limit -> exact_admission_limit (n + 1)
    | Error error -> raise_s (A.Error.sexp_of_t error)
    | Ok admitted -> admitted
  in
  let exact = exact_admission_limit 1 in
  let exact_result = A.produce exact ~checkpoint:ignore |> ok A.Error.sexp_of_t in
  assert (
    match W.Result.status exact_result with
    | Truncated Work_limit -> true
    | _ -> false);
  let exception Cancel of unit ref in
  let token = ref () in
  (match admit ~limits:(limits ()) ~checkpoint:(fun () -> raise (Cancel token)) with
   | exception Cancel actual when phys_equal actual token -> ()
   | _ -> failwith "admission swallowed callback identity");
  let admitted = admit ~limits:(limits ()) ~checkpoint:ignore |> ok A.Error.sexp_of_t in
  (match A.produce admitted ~checkpoint:(fun () -> raise (Cancel token)) with
   | exception Cancel actual when phys_equal actual token -> ()
   | _ -> failwith "producer swallowed callback identity");
  let one =
    W.Request.create
      ~context:(W.Request.context request)
      ~source:(W.Request.source request)
      ~cursor_byte:(String.length text)
      ~max_items:1
    |> ok P.Error.sexp_of_t
  in
  let one_fact =
    A.Facts.create
      ~request:one
      ~authored:captured
      ~lexical_input
      ~parsed:None
      ~captured_inputs:[]
      ~project:None
      ~selected:None
      ~paths:[]
      ~selected_tools:[]
      ~limits:(limits ())
      ~checkpoint:ignore
    |> ok A.Error.sexp_of_t
  in
  let result = A.produce one_fact ~checkpoint:ignore |> ok A.Error.sexp_of_t in
  assert (List.length (W.Result.items result) = 1);
  assert (
    match W.Result.status result with
    | Truncated Item_limit -> true
    | _ -> false);
  let many =
    W.Request.create
      ~context:(W.Request.context request)
      ~source:(W.Request.source request)
      ~cursor_byte:(W.Request.cursor_byte request)
      ~max_items:512
    |> ok P.Error.sexp_of_t
  in
  let builder = W.Result.Builder.create ~request:many |> ok P.Error.sexp_of_t in
  let range =
    W.Range.create
      ~source:(W.Request.source many)
      ~start_byte:(W.Request.cursor_byte many)
      ~end_byte:(W.Request.cursor_byte many)
    |> ok P.Error.sexp_of_t
  in
  let suggestion =
    W.Suggestion.create
      ~kind:Tag
      ~label:(String.make 4096 'x')
      ~insert_text:"x"
      ~replace:range
      ~evidence:Lexical
    |> ok P.Error.sexp_of_t
  in
  let rec fill accepted =
    if accepted > 512 then failwith "builder exceeded item bound";
    match W.Result.Builder.add builder suggestion with
    | Error error -> raise_s (P.Error.sexp_of_t error)
    | Ok Added -> fill (accepted + 1)
    | Ok (Full Result_bytes) -> accepted
    | Ok (Full _) -> failwith "builder reported item limit before byte boundary"
  in
  let accepted = fill 0 in
  assert (accepted > 0 && accepted < 512);
  let foreign = source ~name:"other.chatmd" text in
  let foreign_source =
    P.Authoring_common.Source.create
      ~id:(S.id foreign)
      ~digest:(S.digest foreign)
      ~byte_length:(S.byte_length foreign)
      ~editor_revision:None
    |> ok P.Error.sexp_of_t
  in
  let foreign_range =
    W.Range.create ~source:foreign_source ~start_byte:0 ~end_byte:0
    |> ok P.Error.sexp_of_t
  in
  let foreign_item =
    W.Suggestion.create
      ~kind:Tag
      ~label:"foreign"
      ~insert_text:"foreign"
      ~replace:foreign_range
      ~evidence:Lexical
    |> ok P.Error.sexp_of_t
  in
  (match W.Result.Builder.add builder foreign_item with
   | Error _ -> ()
   | Ok _ -> failwith "full builder accepted a foreign replacement");
  let bounded =
    W.Result.Builder.finish builder ~status:Complete |> ok P.Error.sexp_of_t
  in
  assert (
    match W.Result.status bounded with
    | Truncated Result_bytes -> true
    | _ -> false);
  ignore
    (W.Result.of_json (W.Result.to_json bounded) |> ok P.Error.sexp_of_t : W.Result.t);
  let item_builder = W.Result.Builder.create ~request:one |> ok P.Error.sexp_of_t in
  (match W.Result.Builder.add item_builder suggestion with
   | Ok Added -> ()
   | _ -> failwith "first item rejected by builder");
  (match W.Result.Builder.add item_builder suggestion with
   | Ok (Full Item_limit) -> ()
   | _ -> failwith "builder did not report item limit");
  let item_bounded =
    W.Result.Builder.finish item_builder ~status:Complete |> ok P.Error.sexp_of_t
  in
  assert (
    match W.Result.status item_bounded with
    | Truncated Item_limit -> true
    | _ -> false);
  let finite_statuses =
    let open W.Status in
    [ Complete
    ; Truncated Item_limit
    ; Truncated Work_limit
    ; Truncated Result_bytes
    ; Truncated Unrepresentable_text
    ; Unavailable No_local_context
    ; Unavailable Catalog_unavailable
    ; Unavailable Source_map_unavailable
    ]
  in
  let envelope_size status =
    W.Result.create ~request:many ~items:[] ~status
    |> ok P.Error.sexp_of_t
    |> W.Result.to_json
    |> Jsonaf.to_string
    |> String.length
  in
  let reserved_size =
    envelope_size (W.Status.Unavailable W.Status.Source_map_unavailable)
  in
  assert (
    List.for_all finite_statuses ~f:(fun status -> envelope_size status <= reserved_size));
  print_endline "work cap, callback identity, item and output budget: PASS";
  [%expect {| work cap, callback identity, item and output budget: PASS |}]
;;
