open Core
open Authoring_materialization_test
module R = Chat_response.Authoring_rediscovery

let request task topic =
  `Object
    [ "version", `Number "1"
    ; "operation", `String "topic"
    ; "task", `String (M.task_id task)
    ; "topic_id", `String topic
    ; "query", `Null
    ; "features", `Null
    ; "cursor", `Null
    ; "max_tokens", `Number "32000"
    ]
;;

let reference context host policy materialization ~sequence task topic =
  let response =
    Q.query_with_receipt
      context
      ~host
      ~capabilities:(P.capabilities policy)
      ~scope:(A.scope materialization)
      (request task topic)
  in
  let receipt = Option.value_exn response.receipt in
  let id = History_entry.Id.create ~namespace:"surface-read" ~sequence |> ok in
  let entry = Codec.user_text ~id (Jsonaf.to_string response.json) |> Codec.to_protocol in
  let guidance =
    G.create_surface_reference
      ~surface_id:receipt.surface_id
      ~context_identity:(A.context_identity materialization)
      ~policy_fingerprint:(P.fingerprint policy)
      ~topics:(List.map receipt.topics ~f:(fun topic -> topic.Q.topic))
      ~fragments:
        (List.map receipt.topics ~f:(fun topic ->
           G.
             { topic_id = topic.topic.id
             ; total_parts = topic.total_parts
             ; parts =
                 List.map topic.parts ~f:(fun part ->
                   { index = part.Q.index; item_sha256 = part.item_sha256 })
             }))
      ~payload:entry.payload
    |> protocol_ok
  in
  { entry with provenance = Runtime_authoring guidance }
;;

let guidance entry =
  match entry.H.provenance with
  | Runtime_authoring guidance -> guidance
  | _ -> assert false
;;

let%expect_test
    "compacted virtual references retain their actual surfaces and selected schema \
     versions"
  =
  fixture (fun context host _ plan ->
    let policy = plan Manual in
    let current = Authoring_rediscovery_test.make context host policy in
    let history =
      [ reference
          context
          host
          policy
          current
          ~sequence:0
          One_off_script
          "reference.signatures"
      ; reference
          context
          host
          policy
          current
          ~sequence:1
          Child_agent
          "reference.signatures"
      ; reference context host policy current ~sequence:2 One_off_script "reference.tools"
      ]
      |> restored
    in
    let known = Presence.remember ~previous:[] ~history |> protocol_ok in
    let messages = A.refresh current ~known ~effective:[] |> protocol_ok in
    assert (List.length messages = 2);
    List.iter messages ~f:(fun message ->
      let value = message.A.guidance in
      assert (value.version = 3);
      assert (G.equal value (G.to_json value |> G.of_json |> protocol_ok));
      let surface = Option.value_exn value.surface_id in
      let task =
        match surface with
        | "one_off_v1" -> M.One_off_script
        | "delegated_moderator_v1" -> Child_agent
        | _ -> assert false
      in
      assert (
        String.is_substring (Jsonaf.to_string message.payload) ~substring:(M.task_id task));
      List.iter value.topics ~f:(fun topic ->
        assert (not topic.G.complete);
        let actual =
          Q.query_with_receipt
            context
            ~host
            ~capabilities:(P.capabilities policy)
            ~scope:(A.scope current)
            (request task topic.id)
          |> fun response -> Option.value_exn response.Q.receipt
        in
        let actual =
          List.find_exn actual.topics ~f:(fun actual ->
            String.equal actual.Q.topic.id topic.id)
        in
        assert (G.equal_topic topic { actual.topic with complete = false })));
    let hashes =
      List.concat_map messages ~f:(fun message -> message.A.guidance.topics)
      |> List.filter ~f:(fun topic -> String.equal topic.G.id "reference.signatures")
      |> List.map ~f:(fun topic -> topic.G.document_sha256)
    in
    assert (List.length (List.dedup_and_sort hashes ~compare:String.compare) = 2);
    let effective = entries ~start:20 messages |> restored in
    assert (List.is_empty (A.refresh current ~known ~effective |> protocol_ok));
    let narrowed_policy = plan ~selected_names:[ "script" ] Manual in
    let narrowed = Authoring_rediscovery_test.make context host narrowed_policy in
    let refreshed = A.refresh narrowed ~known ~effective:[] |> protocol_ok in
    assert (List.length refreshed = 1);
    let value = (List.hd_exn refreshed).guidance in
    assert (Option.equal String.equal value.surface_id (Some "one_off_v1"));
    let schema topics =
      List.find_exn topics ~f:(fun topic -> String.equal topic.G.id "reference.tools")
    in
    let old_schema = schema (guidance (List.nth_exn history 2)).topics in
    let new_schema = schema value.topics in
    assert (not (String.equal old_schema.document_sha256 new_schema.document_sha256));
    let actual =
      Q.virtual_topics
        context
        ~host
        ~capabilities:(P.capabilities narrowed_policy)
        ~task:One_off_script
      |> ok
      |> schema
    in
    assert (G.equal_topic actual new_schema);
    let unavailable = make_host ~targets:[ V.Standalone_tool ] () in
    let no_target = Authoring_rediscovery_test.make context unavailable narrowed_policy in
    assert (List.is_empty (A.refresh no_target ~known ~effective:[] |> protocol_ok));
    let helper_policy = plan ~selected_names:[ "ochat_authoring_context" ] Manual in
    let helper = Authoring_rediscovery_test.make context host helper_policy in
    assert (List.is_empty (A.initial helper));
    assert (List.length (A.refresh helper ~known ~effective:[] |> protocol_ok) = 2);
    print_endline
      "two compiler surfaces survive compaction; pointers match real queries; narrowed \
       schemas and withdrawn targets rechecked; helper-only works");
  [%expect
    {| two compiler surfaces survive compaction; pointers match real queries; narrowed schemas and withdrawn targets rechecked; helper-only works |}]
;;

let%expect_test
    "surface metadata is closed and legacy references never acquire a guessed target"
  =
  fixture (fun context host _ plan ->
    let policy = plan Manual in
    let current = Authoring_rediscovery_test.make context host policy in
    let entry =
      reference
        context
        host
        policy
        current
        ~sequence:0
        One_off_script
        "reference.signatures"
    in
    let value = guidance entry in
    let legacy =
      G.create_reference
        ~context_identity:value.context_identity
        ~policy_fingerprint:value.policy_fingerprint
        ~topics:value.topics
        ~fragments:value.fragments
        ~payload:entry.payload
      |> protocol_ok
    in
    assert (G.equal legacy (G.to_json legacy |> G.of_json |> protocol_ok));
    let changed fields = G.of_json (`Object fields) in
    let fields =
      match G.to_json value with
      | `Object fields -> fields
      | _ -> assert false
    in
    assert (
      Result.is_error
        (changed (List.Assoc.remove fields "surface_id" ~equal:String.equal)));
    assert (
      Result.is_error
        (changed
           (List.Assoc.add fields ~equal:String.equal "surface_id" (`String "unknown"))));
    assert (
      Result.is_error
        (changed (List.Assoc.add fields ~equal:String.equal "version" (`Number "2"))));
    let old_history =
      [ { entry with provenance = Runtime_authoring legacy } ] |> restored
    in
    let known = Presence.remember ~previous:[] ~history:old_history |> protocol_ok in
    assert (List.is_empty (A.refresh current ~known ~effective:[] |> protocol_ok));
    let second =
      reference context host policy current ~sequence:1 Child_agent "reference.signatures"
    in
    let known =
      Presence.remember ~previous:[] ~history:[ entry; second ] |> protocol_ok
    in
    let renderer = R.create ~context ~host ~policy |> ok in
    let render ?max_topics ?max_bytes () =
      R.render_all
        ?max_topics
        ?max_bytes
        renderer
        ~context_identity:(A.context_identity current)
        ~known
        ~effective:[]
        ~inserting:[]
        ()
      |> protocol_ok
    in
    let one = render ~max_topics:1 () in
    assert (List.length one = 1);
    assert (String.is_substring (List.hd_exn one).text ~substring:"\"truncated\":true");
    let bytes = String.length (List.hd_exn one).text + 1 in
    let bounded = render ~max_bytes:bytes () in
    assert (List.length bounded = 1);
    assert (
      List.sum (module Int) bounded ~f:(fun pointer -> String.length pointer.R.text)
      <= bytes);
    print_endline
      "v2 retained without guessed surface; malformed v3 rejected; multiple surfaces \
       share one topic/byte budget");
  [%expect
    {| v2 retained without guessed surface; malformed v3 rejected; multiple surfaces share one topic/byte budget |}]
;;
