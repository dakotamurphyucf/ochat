open Core
module S = Chatml_source
module A = Chatml_semantics
module P = Agent_protocol
module W = P.Authoring.Analysis
module Q = Chatml_authoring_query
module H = Support

let ok = function
  | Ok value -> value
  | Error error -> Error.raise_s (Q.Error.sexp_of_t error)
;;

let wire_ok = function
  | Ok value -> value
  | Error error -> Error.raise_s (P.Error.sexp_of_t error)
;;

let wire_source source =
  P.Authoring.Source.create
    ~id:(S.id source)
    ~digest:(S.digest source)
    ~byte_length:(S.byte_length source)
    ~editor_revision:None
  |> wire_ok
;;

let generator = P.Id.Generator.create ~bytes:(fun length -> String.make length '\000')
let host = P.Id.Server.create_with generator
let context_id = P.Id.Authoring_context.create_with generator
let bundle_id = P.Id.Authoring_bundle.create_with generator

let expected contract =
  let contract =
    Chatml.Chatml_compiler_contract.fingerprint contract ~checkpoint:ignore
  in
  P.Authoring.Context.create
    ~id:context_id
    ~host
    ~origin:(P.Authoring.Context.Uploaded_project bundle_id)
    ~generation:0
    ~basis:
      (P.Authoring.Context.Project
         { base_graph = S.Digest.of_contents "base" ~checkpoint:ignore
         ; selection = { environment = "fixture"; compiler_contract = contract }
         })
    ~basis_fingerprint:(S.Digest.of_contents "basis" ~checkpoint:ignore)
    ~editable_sources:[]
  |> wire_ok
  |> P.Authoring.Context.Expected.of_view
;;

let capture_identity identity ~input_fingerprint contract =
  W.Capture.create
    ~context:(expected contract)
    ~target:W.Target.Project_input
    ~input_fingerprint
    ~environment:
      (Chatml_prepared_program.Environment.name (A.Identity.environment identity))
    ~required_contract:(A.Identity.required_contract identity)
    ~selected_contract:(A.Identity.selected_contract identity)
  |> wire_ok
;;

let capture snapshot contract =
  let identity = A.Snapshot.identity snapshot in
  capture_identity
    identity
    ~input_fingerprint:(A.Identity.input_fingerprint identity)
    contract
;;

let site_projection sources location ~checkpoint =
  checkpoint ();
  let source =
    List.find_exn sources ~f:(fun source ->
      S.Id.equal (S.id source) (S.Location.source location))
  in
  let span = S.Location.span location in
  let native =
    W.Byte_range.create
      ~source:(wire_source source)
      ~start_byte:span.left.offset
      ~end_byte:span.right.offset
    |> wire_ok
  in
  W.Site.create ~native ~projection:None |> wire_ok |> Result.return
;;

let limits ?(items = 32) ?(labels = 32_768) ?(work = 100_000) () =
  Q.Limits.create
    ~max_items:items
    ~max_member_depth:6
    ~max_member_edges:2048
    ~max_label_bytes:labels
    ~max_work_steps:work
    ~type_display:
      (Chatml_type_display.Limits.create
         ~max_nodes:4096
         ~max_edges:8192
         ~max_depth:128
         ~max_work_steps:100_000
         ~max_text_bytes:4096
         ~max_output_bytes:32_768
       |> Or_error.ok_exn)
  |> Or_error.ok_exn
;;

let query snapshot contract source ~byte_offset ~kind ~max_items ~limits =
  let capture = capture snapshot contract in
  let request =
    W.Query.Request.create
      ~evidence:
        (W.Query.Certified
           { capture
           ; snapshot = W.Snapshot_id.create "test-snapshot" |> wire_ok
           ; validated_graph = S.Digest.of_contents "validated" ~checkpoint:ignore
           })
      ~source:(wire_source source)
      ~byte_offset
      ~kind
      ~max_items
    |> wire_ok
  in
  let payload =
    Q.project
      (Q.Evidence.Complete snapshot)
      ~request
      ~sites:(site_projection (A.Snapshot.sources snapshot))
      ~limits
      ~checkpoint:ignore
    |> ok
  in
  let result = W.Query.create ~request ~payload |> wire_ok in
  let decoded = W.Query.of_json (W.Query.to_json result) |> wire_ok in
  assert (W.Query.Request.equal (W.Query.request decoded) request);
  W.Query.payload decoded
;;

let certified_request snapshot contract source ~byte_offset ~kind ~max_items =
  W.Query.Request.create
    ~evidence:
      (W.Query.Certified
         { capture = capture snapshot contract
         ; snapshot = W.Snapshot_id.create "test-snapshot" |> wire_ok
         ; validated_graph = S.Digest.of_contents "validated" ~checkpoint:ignore
         })
    ~source:(wire_source source)
    ~byte_offset
    ~kind
    ~max_items
  |> wire_ok
;;

let%expect_test "source-owned semantic payloads retain selected public view" =
  let contract = H.contract () in
  let entry = "open Catalog;; let result = identity(3)" in
  let implementation = "let identity x = x;; let private_value = 7" in
  let input =
    H.input
      contract
      ~interfaces:[ "catalog.chatml", "val identity : fun(int) -> int", [] ]
      [ "entry.chatml", entry, [ "Catalog", "catalog.chatml" ]
      ; "catalog.chatml", implementation, []
      ]
  in
  let snapshot = H.complete contract input in
  let source = H.source input "entry.chatml" in
  let position = String.substr_index_exn entry ~pattern:"identity(3)" in
  let candidate =
    query
      snapshot
      contract
      source
      ~byte_offset:position
      ~kind:(W.Query.Candidates W.Query.Value)
      ~max_items:16
      ~limits:(limits ())
  in
  (match candidate with
   | W.Query.Candidates_result (Selected page) ->
     let identity =
       List.find_exn page.items ~f:(fun binding ->
         String.equal (W.Query.Item.binding_name binding) "identity")
     in
     let selected =
       W.Query.Item.binding_selected_signature identity |> Option.value_exn
     in
     let first_members_truncated =
       match W.Query.Item.binding_value_members (List.hd_exn page.items) with
       | W.Query.Available members -> members.truncated
       | Unavailable _ -> false
     in
     print_s
       [%sexp
         { selected = (W.Query.Display.text selected : string)
         ; complete =
             (W.Query.Display.completeness selected : W.Query.Display.completeness)
         ; public_identity =
             (List.exists page.items ~f:(fun binding ->
                String.equal (W.Query.Item.binding_name binding) "identity")
              : bool)
         ; private_absent =
             (not
                (List.exists page.items ~f:(fun binding ->
                   String.equal (W.Query.Item.binding_name binding) "private_value"))
              : bool)
         ; first_members_truncated : bool
         }]
   | _ -> failwith "expected selected candidates");
  let definition =
    query
      snapshot
      contract
      source
      ~byte_offset:position
      ~kind:W.Query.References
      ~max_items:1
      ~limits:(limits ~items:1 ())
  in
  let reference_symbol_id =
    match definition with
    | W.Query.References_result (Available { items = [ reference ]; _ }) ->
      (match W.Query.Item.reference_target reference with
       | W.Query.Item.Symbol id -> id
       | _ -> failwith "cross-file reference lost symbol")
    | _ -> failwith "expected actual reference"
  in
  let definitions =
    query
      snapshot
      contract
      (H.source input "catalog.chatml")
      ~byte_offset:(String.substr_index_exn implementation ~pattern:"identity")
      ~kind:W.Query.Definitions
      ~max_items:1
      ~limits:(limits ~items:1 ())
  in
  (match definitions with
   | W.Query.Definitions_result (Available { items = [ symbol ]; truncated = false }) ->
     [%test_eq: string] (W.Query.Item.symbol_name symbol) "identity";
     [%test_eq: int] (W.Query.Item.symbol_id symbol) reference_symbol_id;
     let principal = W.Query.Item.principal_signature symbol |> Option.value_exn in
     assert (
       W.Query.Display.equal_completeness
         (W.Query.Display.completeness principal)
         Complete);
     [%test_eq: string] (W.Query.Display.text principal) "fun('a) -> 'a";
     (match W.Query.Item.symbol_origin symbol with
      | W.Query.Item.Authored site ->
        let owner = W.Site.native site |> W.Byte_range.source |> P.Authoring.Source.id in
        [%test_eq: string] (S.Id.relative_path owner) "catalog.chatml"
      | _ -> failwith "definition lost authored owner")
   | _ -> failwith "expected one actual definition");
  let type_at =
    query
      snapshot
      contract
      source
      ~byte_offset:(String.length entry - 1)
      ~kind:W.Query.Type_at
      ~max_items:1
      ~limits:(limits ~items:1 ())
  in
  (match type_at with
   | W.Query.Type_result (Selected display) ->
     [%test_eq: string] (W.Query.Display.text display) "int"
   | _ -> failwith "expected selected actual type");
  let configured =
    query
      snapshot
      contract
      source
      ~byte_offset:position
      ~kind:W.Query.Configured_modules
      ~max_items:16
      ~limits:(limits ~items:512 ())
  in
  (match configured with
   | W.Query.Configured_modules_result (Selected page) ->
     let catalog =
       List.find_exn page.items ~f:(fun item ->
         String.equal (W.Query.Item.configured_module_name item) "Catalog")
     in
     (match W.Query.Item.configured_module_exports catalog with
      | W.Query.Available exports ->
        let values = W.Query.Item.export_values exports in
        let names = List.map values.items ~f:W.Query.Item.binding_name in
        [%test_eq: string list] names [ "identity" ]
      | Unavailable _ -> failwith "checked companion exports unavailable")
   | _ -> failwith "configured module availability was lost");
  [%expect
    {|
    ((selected "fun(int) -> int") (complete Complete) (public_identity true)
     (private_absent true) (first_members_truncated true))
    |}]
;;

let%test_unit
    "query rejects stale source and forged sites without swallowing interruption"
  =
  let contract = H.contract () in
  let body = "let x = 1;; let y = x" in
  let input = H.input contract [ "entry.chatml", body, [] ] in
  let snapshot = H.complete contract input in
  let source = H.source input "entry.chatml" in
  let changed =
    S.create
      (S.id source)
      ~contents:(body ^ " ")
      ~expected_digest:None
      ~max_bytes:None
      ~checkpoint:ignore
    |> Or_error.ok_exn
  in
  let position = String.substr_index_exn body ~pattern:"= x" + 2 in
  let request source =
    certified_request
      snapshot
      contract
      source
      ~byte_offset:position
      ~kind:W.Query.References
      ~max_items:2
  in
  let project ?(budget = limits ()) request sites ~checkpoint =
    Q.project (Q.Evidence.Complete snapshot) ~request ~sites ~limits:budget ~checkpoint
  in
  (match project (request changed) (site_projection [ source ]) ~checkpoint:ignore with
   | Error Q.Error.Stale_source -> ()
   | _ -> failwith "stale source admitted");
  (match project (request source) (site_projection [ changed ]) ~checkpoint:ignore with
   | Error Q.Error.Invalid_site_projection -> ()
   | _ -> failwith "forged native site admitted");
  let identity = A.Snapshot.identity snapshot in
  let bad_capture =
    W.Capture.create
      ~context:(expected contract)
      ~target:W.Target.Project_input
      ~input_fingerprint:(A.Identity.input_fingerprint identity)
      ~environment:"foreign-environment"
      ~required_contract:(A.Identity.required_contract identity)
      ~selected_contract:(A.Identity.selected_contract identity)
    |> wire_ok
  in
  let bad_request =
    W.Query.Request.create
      ~evidence:
        (W.Query.Certified
           { capture = bad_capture
           ; snapshot = W.Snapshot_id.create "test-snapshot" |> wire_ok
           ; validated_graph = S.Digest.of_contents "validated" ~checkpoint:ignore
           })
      ~source:(wire_source source)
      ~byte_offset:position
      ~kind:W.Query.References
      ~max_items:2
    |> wire_ok
  in
  (match project bad_request (site_projection [ source ]) ~checkpoint:ignore with
   | Error Q.Error.Evidence_mismatch -> ()
   | _ -> failwith "foreign selected environment admitted");
  let candidate_request =
    certified_request
      snapshot
      contract
      source
      ~byte_offset:position
      ~kind:(W.Query.Candidates W.Query.Value)
      ~max_items:1
  in
  (match
     project
       ~budget:(limits ~labels:1 ())
       candidate_request
       (site_projection [ source ])
       ~checkpoint:ignore
   with
   | Error Q.Error.Label_byte_limit -> ()
   | _ -> failwith "aggregate label budget reset");
  let gap = String.substr_index_exn body ~pattern:";;" in
  (match
     query
       snapshot
       contract
       source
       ~byte_offset:gap
       ~kind:W.Query.References
       ~max_items:1
       ~limits:(limits ~items:1 ())
   with
   | W.Query.References_result (Available { items = []; _ }) -> ()
   | _ -> failwith "checked empty lookup became unavailable");
  (match
     query
       snapshot
       contract
       (H.source input "project.json")
       ~byte_offset:0
       ~kind:W.Query.References
       ~max_items:1
       ~limits:(limits ~items:1 ())
   with
   | W.Query.References_result (Unavailable W.Query.Not_checked) -> ()
   | _ -> failwith "metadata source acquired checked facts");
  let polls = ref 0 in
  assert (
    Result.is_ok
      (project (request source) (site_projection [ source ]) ~checkpoint:(fun () ->
         Int.incr polls)));
  assert (!polls > 2);
  (match
     project
       ~budget:(limits ~work:(!polls - 1) ())
       (request source)
       (site_projection [ source ])
       ~checkpoint:ignore
   with
   | Error Q.Error.Work_limit -> ()
   | _ -> failwith "late aggregate work budget reset");
  let exception Interrupted of unit ref in
  let token = ref () in
  let seen = ref 0 in
  match
    project (request source) (site_projection [ source ]) ~checkpoint:(fun () ->
      Int.incr seen;
      if Int.equal !seen (!polls - 1) then raise (Interrupted token))
  with
  | exception Interrupted actual -> assert (phys_equal actual token)
  | Ok _ | Error _ -> failwith "interruption was converted"
;;

let%test_unit "provisional prefix and failed syntax keep distinct exact source owners" =
  let contract = H.contract () in
  let body = "let x = 1;; let broken =" in
  let input = H.input contract [ "entry.chatml", body, [] ] in
  let full_source = H.source input "entry.chatml" in
  let provisional =
    match
      A.analyze_prefix
        input
        ~source:(S.id full_source)
        ~cursor_byte:(String.length body)
        ~contract
        ~max_dependency_depth:16
        ~limits:(H.limits ())
        ~checkpoint:ignore
    with
    | Ok (A.Prefix.Provisional value) -> value
    | _ -> failwith "actual completed prefix unavailable"
  in
  let prefix_source =
    List.find_exn
      (A.Snapshot.sources (A.Provisional.snapshot provisional))
      ~f:(fun source -> S.Id.equal (S.id source) (S.id full_source))
  in
  let proof = Q.Prefix_origin.create provisional ~full_source ~checkpoint:ignore |> ok in
  let prefix_location =
    S.locate prefix_source ~start_byte:4 ~end_byte:5 ~checkpoint:ignore |> Or_error.ok_exn
  in
  let full_location =
    Q.Prefix_origin.full_location proof prefix_location ~checkpoint:ignore
    |> ok
    |> Option.value_exn
  in
  assert (S.Digest.equal (S.Location.source_digest full_location) (S.digest full_source));
  assert (not (S.Digest.equal (S.digest full_source) (S.digest prefix_source)));
  let identity = A.Snapshot.identity (A.Provisional.snapshot provisional) in
  let capture =
    capture_identity
      identity
      ~input_fingerprint:(A.Provisional.full_input_fingerprint provisional)
      contract
  in
  let prefix =
    W.Prefix.create
      ~capture
      ~full_source:(wire_source full_source)
      ~prefix_source:(wire_source prefix_source)
      ~cursor_byte:(String.length body)
      ~end_byte:(A.Provisional.end_byte provisional)
      ~prefix_graph:(S.Digest.of_contents "test-prefix-graph" ~checkpoint:ignore)
      ~snapshot:(W.Snapshot_id.create "prefix-snapshot" |> wire_ok)
    |> wire_ok
  in
  let request =
    W.Query.Request.create
      ~evidence:(W.Query.Provisional prefix)
      ~source:(wire_source prefix_source)
      ~byte_offset:8
      ~kind:W.Query.Type_at
      ~max_items:1
    |> wire_ok
  in
  let payload =
    Q.project
      (Q.Evidence.Provisional { provisional; full_source })
      ~request
      ~sites:(site_projection [ prefix_source ])
      ~limits:(limits ~items:1 ())
      ~checkpoint:ignore
    |> ok
  in
  let wire = W.Query.create ~request ~payload |> wire_ok in
  assert (Result.is_ok (W.Query.of_json (W.Query.to_json wire)));
  (match payload with
   | W.Query.Type_result (Selected _) -> ()
   | _ -> failwith "prefix type unavailable");
  let syntax_input =
    H.input contract [ "entry.chatml", "let x = 1;; let broken = true + 1", [] ]
  in
  let syntax_source = H.source syntax_input "entry.chatml" in
  let partial =
    match H.analyze contract syntax_input with
    | Ok (A.Failed value) -> value
    | _ -> failwith "invalid full input unexpectedly checked"
  in
  let syntax_capture =
    let identity = A.Partial.identity partial in
    capture_identity
      identity
      ~input_fingerprint:(A.Identity.input_fingerprint identity)
      contract
  in
  let syntax_request =
    W.Query.Request.create
      ~evidence:(W.Query.Syntax_only syntax_capture)
      ~source:(wire_source syntax_source)
      ~byte_offset:4
      ~kind:W.Query.Syntax_at
      ~max_items:2
    |> wire_ok
  in
  let syntax_payload =
    Q.syntax_at
      partial
      ~request:syntax_request
      ~sites:(site_projection [ syntax_source ])
      ~limits:(limits ())
      ~checkpoint:ignore
    |> ok
  in
  (match syntax_payload with
   | W.Query.Syntax_result (Available page) -> assert (not (List.is_empty page.items))
   | _ -> failwith "failed input lost actual parsed syntax");
  let wire = W.Query.create ~request:syntax_request ~payload:syntax_payload |> wire_ok in
  assert (Result.is_ok (W.Query.of_json (W.Query.to_json wire)))
;;

let%test_unit "actual ChatMD RAW ownership composes with a proved provisional prefix" =
  let module Markdown = Prompt.Chat_markdown in
  let module Extraction = Markdown.Declaration.Extraction in
  let authored =
    {|<script id="s" language="chatml" kind="tool">RAW|let x = 1;; let y = x;;|RAW let broken =</script>|}
  in
  let parsed =
    Eio_main.run (fun env ->
      let dir = Eio.Stdenv.cwd env in
      let loader =
        Source_loader.captured_filesystem ~root:dir ~sources:[ "root.chatmd", authored ]
      in
      let limits =
        Markdown.Limits.create
          ~max_input_bytes:100_000
          ~max_parse_calls:128
          ~max_tokens:10_000
          ~max_depth:64
        |> Or_error.ok_exn
      in
      Markdown.prepare_chat_inputs
        ~source:"root.chatmd"
        ~source_loader:loader
        ~dir
        ~preprocess:Fn.id
        ~limits
        ~checkpoint:ignore
        authored)
  in
  let declaration =
    Markdown.Parsed.declarations parsed
    |> List.find_exn ~f:(fun value ->
      String.equal (Markdown.Declaration.script_id value) "s")
  in
  let body = Markdown.Declaration.script_body declaration in
  let contract = H.contract () in
  let input = H.input contract [ "entry.chatml", body, [] ] in
  let full_source = H.source input "entry.chatml" in
  let bound =
    match Extraction.bind declaration ~compiled_source:full_source ~checkpoint:ignore with
    | Ok value -> value
    | Error error -> Error.raise_s (Extraction.sexp_of_error error)
  in
  let provisional =
    match
      A.analyze_prefix
        input
        ~source:(S.id full_source)
        ~cursor_byte:(String.length body)
        ~contract
        ~max_dependency_depth:16
        ~limits:(H.limits ())
        ~checkpoint:ignore
    with
    | Ok (A.Prefix.Provisional value) -> value
    | _ -> failwith "actual RAW script prefix unavailable"
  in
  let prefix =
    List.find_exn
      (A.Snapshot.sources (A.Provisional.snapshot provisional))
      ~f:(fun source -> S.Id.equal (S.id source) (S.id full_source))
  in
  let proof = Q.Prefix_origin.create provisional ~full_source ~checkpoint:ignore |> ok in
  let use = String.substr_index_exn (S.contents prefix) ~pattern:"= x" + 2 in
  let native =
    S.locate prefix ~start_byte:use ~end_byte:(use + 1) ~checkpoint:ignore
    |> Or_error.ok_exn
  in
  let full_location =
    Q.Prefix_origin.full_location proof native ~checkpoint:ignore
    |> ok
    |> Option.value_exn
  in
  (match Extraction.project_raw bound full_location ~checkpoint:ignore with
   | Ok (Exact_inline span) ->
     assert (phys_equal (Extraction.input span) (Markdown.Parsed.root_input parsed));
     let authored_use = String.substr_index_exn authored ~pattern:"= x" + 2 in
     [%test_eq: int] (Extraction.start_byte span) authored_use;
     [%test_eq: int] (Extraction.end_byte span) (authored_use + 1)
   | _ -> failwith "RAW-origin prefix site was not exact");
  let wrong_full =
    S.create
      (S.id full_source)
      ~contents:(body ^ " ")
      ~expected_digest:None
      ~max_bytes:None
      ~checkpoint:ignore
    |> Or_error.ok_exn
  in
  (match
     Q.Prefix_origin.create provisional ~full_source:wrong_full ~checkpoint:ignore
   with
   | Error Q.Error.Evidence_mismatch -> ()
   | _ -> failwith "foreign full revision established a prefix proof");
  let wrong_prefix =
    S.create
      (S.id prefix)
      ~contents:(S.contents prefix ^ " ")
      ~expected_digest:None
      ~max_bytes:None
      ~checkpoint:ignore
    |> Or_error.ok_exn
  in
  let stale_location =
    S.locate wrong_prefix ~start_byte:use ~end_byte:(use + 1) ~checkpoint:ignore
    |> Or_error.ok_exn
  in
  match Q.Prefix_origin.full_location proof stale_location ~checkpoint:ignore with
  | Error Q.Error.Stale_source -> ()
  | _ -> failwith "foreign prefix revision acquired authored projection"
;;
