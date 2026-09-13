open Core
open Authoring_context_tests
module G = Agent_protocol.Authoring_guidance
module Digest = Chatmd_shell_spec.Source_ref

let receipt response = Option.value_exn response.Q.receipt

let%expect_test
    "paged reference receipts identify exact emitted fragments without claiming complete \
     context"
  =
  let service =
    Q.create ~secret:"receipt-pagination-test-key" ~max_tokens:1_000_000 ()
    |> Result.ok_or_failwith
  in
  let host = host () in
  let capabilities = capabilities () in
  let scope = "receipt-session:0" in
  let query request = Q.query_with_receipt service ~host ~capabilities ~scope request in
  let topic budget =
    request
      ~task:"moderator_tool"
      ~topic_id:"reference.signatures"
      ~max_tokens:budget
      "topic"
  in
  let first = query (topic 1500) in
  require_json (Q.query service ~host ~capabilities ~scope (topic 1500)) first.json;
  let rec collect remaining reversed response =
    assert (remaining > 0);
    assert (not (has_error response.Q.json));
    let reversed = response :: reversed in
    match field response.json "next_cursor" with
    | `Null -> List.rev reversed
    | `String cursor ->
      let minimum = field (field response.json "budget") "minimum_next_tokens" in
      let budget =
        match minimum with
        | `Number value -> Int.max 1500 (Int.of_string value)
        | `Null -> 1500
        | _ -> assert false
      in
      collect
        (remaining - 1)
        reversed
        (query (request ~cursor ~max_tokens:budget "continue"))
    | _ -> assert false
  in
  let pages = collect 100 [] first in
  assert (List.length pages > 1);
  let whole = query (topic 1_000_000) in
  let complete = List.hd_exn (receipt whole).topics in
  assert complete.topic.complete;
  let seen =
    List.concat_map pages ~f:(fun response ->
      match response.receipt with
      | None ->
        assert (List.is_empty (items response.json));
        []
      | Some receipt ->
        assert (Q.matches_response receipt response.json);
        assert (not (Q.matches_response receipt (`Object [ "items", `Array [] ])));
        assert (String.equal receipt.scope scope);
        assert (String.equal receipt.query_identity (Q.fingerprint service));
        assert (String.equal receipt.host_identity (V.host_fingerprint host));
        assert (String.equal receipt.capability_fingerprint (C.fingerprint capabilities));
        assert (String.equal receipt.surface_id "moderator_v1");
        let topic = List.hd_exn receipt.topics in
        assert (List.length receipt.topics = 1);
        assert (not topic.topic.complete);
        assert (String.equal topic.topic.document_sha256 complete.topic.document_sha256);
        assert (topic.total_parts = complete.total_parts);
        let emitted = items response.json in
        assert (List.length emitted = List.length topic.parts);
        List.iter2_exn emitted topic.parts ~f:(fun item part ->
          assert (String.equal (Digest.digest (Jsonaf.to_string item)) part.Q.item_sha256));
        topic.parts)
  in
  assert (
    List.equal
      Int.equal
      (List.init complete.total_parts ~f:Fn.id)
      (List.map seen ~f:(fun part -> part.Q.index)));
  assert (
    List.equal
      String.equal
      (List.map complete.parts ~f:(fun part -> part.Q.item_sha256))
      (List.map seen ~f:(fun part -> part.Q.item_sha256)));
  let last = List.last_exn pages in
  require_json `True (field last.json "complete");
  assert (not (List.hd_exn (receipt last).topics).topic.complete);
  print_endline
    "unchanged JSON; exact part indexes/hashes; final page is not the whole topic; \
     altered output fails binding";
  [%expect
    {| unchanged JSON; exact part indexes/hashes; final page is not the whole topic; altered output fails binding |}]
;;

let native name =
  let module Definition = struct
    type input = string

    let name = name
    let description = Some "Selected fixture tool."
    let type_ = "function"
    let parameters = `True
    let input_of_string input = input
  end
  in
  ( Digest.digest name
  , Ochat_function.create_function
      (module Definition)
      (fun _ -> failwith "reference query executed a tool") )
;;

let cap_ok result =
  Result.map_error result ~f:(fun error -> error.C.message) |> Result.ok_or_failwith
;;

let%expect_test
    "prepared multi-topic pages preserve whole-reference hashes and exact coverage"
  =
  let service =
    Q.create ~secret:"receipt-prepared-workflow-key" ~max_tokens:1_000_000 ()
    |> Result.ok_or_failwith
  in
  let host = host () in
  let capabilities = capabilities () in
  let query request =
    Q.query_with_receipt service ~host ~capabilities ~scope:"workflow:0" request
  in
  let prepare budget =
    request
      ~task:"background_workflow"
      ~features:[ "timers"; "notifications" ]
      ~max_tokens:budget
      "prepare"
  in
  let whole = query (prepare 1_000_000) in
  let rec collect remaining response =
    assert (remaining > 0);
    assert (not (has_error response.Q.json));
    let topics =
      Option.value_map response.receipt ~default:[] ~f:(fun receipt -> receipt.Q.topics)
    in
    let source_items =
      List.filter (items response.json) ~f:(fun item ->
        Option.is_some (Jsonaf.member "topic_id" item))
    in
    let page_hashes =
      List.concat_map topics ~f:(fun topic ->
        List.map topic.Q.parts ~f:(fun part -> part.Q.item_sha256))
    in
    assert (
      List.equal
        String.equal
        (List.map source_items ~f:(fun item -> Digest.digest (Jsonaf.to_string item))
         |> List.sort ~compare:String.compare)
        (List.sort page_hashes ~compare:String.compare));
    match field response.json "next_cursor" with
    | `Null -> topics
    | `String cursor ->
      let minimum = field (field response.json "budget") "minimum_next_tokens" in
      let budget =
        match minimum with
        | `Number value -> Int.max 6000 (Int.of_string value)
        | _ -> 6000
      in
      topics
      @ collect (remaining - 1) (query (request ~cursor ~max_tokens:budget "continue"))
    | _ -> assert false
  in
  let paged = collect 100 (query (prepare 6000)) in
  let complete = (receipt whole).topics in
  assert (List.length complete > 3);
  List.iter complete ~f:(fun expected ->
    let fragments =
      List.filter paged ~f:(fun topic ->
        String.equal topic.Q.topic.id expected.Q.topic.id)
    in
    List.iter fragments ~f:(fun topic ->
      assert (String.equal topic.Q.topic.document_sha256 expected.topic.document_sha256);
      assert (G.equal_source topic.topic.source expected.topic.source));
    let all_parts = List.concat_map fragments ~f:(fun topic -> topic.Q.parts) in
    assert (
      List.equal
        Int.equal
        (List.map expected.parts ~f:(fun part -> part.Q.index))
        (List.map all_parts ~f:(fun part -> part.Q.index)));
    assert (
      List.equal
        String.equal
        (List.map expected.parts ~f:(fun part -> part.Q.item_sha256))
        (List.map all_parts ~f:(fun part -> part.Q.item_sha256))));
  print_endline
    "orientation is not a topic read; every emitted source item is covered exactly once \
     across prepared pages";
  [%expect
    {| orientation is not a topic read; every emitted source item is covered exactly once across prepared pages |}]
;;

let%expect_test
    "virtual tool references bind selected schemas and preserve caller scope separately \
     from JSON"
  =
  Mirage_crypto_rng_unix.use_default ();
  let service = Q.create ~secret:"receipt-tools-test-key" () |> Result.ok_or_failwith in
  let host = host () in
  let ceiling =
    C.create
      ~owner:"reference-test"
      ~resource_fingerprint:(Digest.digest "fixture")
      [ native "read"; native "write" ]
    |> cap_ok
  in
  let narrow = C.select ceiling ~names:[ "read" ] |> cap_ok in
  let query capabilities scope =
    Q.query_with_receipt
      service
      ~host
      ~capabilities
      ~scope
      (request ~task:"one_off_script" ~topic_id:"reference.tools" "topic")
  in
  let wide = query ceiling "parent:0" in
  let reduced = query narrow "parent:0" in
  let other = query narrow "child:0" in
  let wide_topic = List.hd_exn (receipt wide).topics in
  let narrow_topic = List.hd_exn (receipt reduced).topics in
  assert (wide_topic.total_parts = 2 && narrow_topic.total_parts = 1);
  assert (wide_topic.topic.complete && narrow_topic.topic.complete);
  assert (
    not (String.equal wide_topic.topic.document_sha256 narrow_topic.topic.document_sha256));
  require_json reduced.json other.json;
  assert (String.equal (receipt reduced).scope "parent:0");
  assert (String.equal (receipt other).scope "child:0");
  assert (
    not
      (String.equal
         (receipt wide).capability_fingerprint
         (receipt reduced).capability_fingerprint));
  let empty = C.select ceiling ~names:[] |> cap_ok in
  assert (Option.is_none (query empty "parent:0").receipt);
  print_endline
    "selected tools change the topic hash; identical JSON does not erase caller scope; \
     empty tool list has no read receipt";
  [%expect
    {| selected tools change the topic hash; identical JSON does not erase caller scope; empty tool list has no read receipt |}]
;;

let%expect_test
    "search, rejected requests and unavailable authored topics cannot create read \
     receipts"
  =
  Mirage_crypto_rng_unix.use_default ();
  let package =
    Authored_reference_tests.package "reports" "Report conventions, owned by the author."
  in
  let service =
    Q.create ~secret:"receipt-authored-test-key" ~authored_packages:[ package ] ()
    |> Result.ok_or_failwith
  in
  let host = host () in
  let metadata =
    Chatmd_shell_spec.Authoring_metadata.{ authoring = Some package.help; helper = None }
  in
  let selected =
    C.create
      ~owner:"author"
      ~resource_fingerprint:(Digest.digest "fixture")
      ~metadata:[ "author", metadata ]
      [ native "author" ]
    |> cap_ok
  in
  let query ?(capabilities = selected) request =
    Q.query_with_receipt service ~host ~capabilities ~scope:"author:0" request
  in
  let topic = request ~task:"one_off_script" ~topic_id:"custom.reports.rules" "topic" in
  let response = query topic in
  let references = (receipt response).topics in
  let custom =
    List.find_exn references ~f:(fun topic ->
      String.equal topic.Q.topic.id "custom.reports.rules")
  in
  (match custom.topic.source with
   | G.Authored digest -> assert (String.length digest = 64)
   | Installed _ -> assert false);
  assert (
    List.exists references ~f:(fun topic ->
      match topic.Q.topic.source with
      | G.Installed _ -> true
      | Authored _ -> false));
  assert (List.for_all references ~f:(fun topic -> topic.Q.topic.complete));
  let empty = C.select selected ~names:[] |> cap_ok in
  List.iter
    [ query (request ~task:"one_off_script" ~query:"reports" "search")
    ; query ~capabilities:empty topic
    ; query
        (request
           ~task:"one_off_script"
           ~topic_id:"reference.signatures"
           ~max_tokens:1
           "topic")
    ; query
        (match topic with
         | `Object fields -> `Object (("receipt", `True) :: fields)
         | _ -> assert false)
    ]
    ~f:(fun response -> assert (Option.is_none response.Q.receipt));
  print_endline
    "authored and installed origins remain distinct; search/errors/private misses/forged \
     fields carry no read receipt";
  [%expect
    {| authored and installed origins remain distinct; search/errors/private misses/forged fields carry no read receipt |}]
;;
