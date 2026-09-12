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

let entry_of_response policy materialization ~sequence response =
  let receipt = Option.value_exn response.Q.receipt in
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

let reference context host policy materialization ~sequence task topic =
  Q.query_with_receipt
    context
    ~host
    ~capabilities:(P.capabilities policy)
    ~scope:(A.scope materialization)
    (request task topic)
  |> entry_of_response policy materialization ~sequence
;;

let guidance entry =
  match entry.H.provenance with
  | Runtime_authoring guidance -> guidance
  | _ -> assert false
;;

let%expect_test
    "X10 runtime, capability and compiler changes invalidate old pages and rediscover \
     current contracts"
  =
  Eio_main.run (fun env ->
    fixture (fun context host _ plan ->
      let policy =
        plan ~selected_names:[ "script"; "child"; "ochat_authoring_context" ] Manual
      in
      let original = Authoring_rediscovery_test.make context host policy in
      let prepare =
        `Object
          [ "version", `Number "1"
          ; "operation", `String "prepare"
          ; "task", `String "background_workflow"
          ; "query", `Null
          ; "topic_id", `Null
          ; "features", `Null
          ; "cursor", `Null
          ; "max_tokens", `Number "6000"
          ]
      in
      let continue ?(budget = 6000) cursor =
        `Object
          [ "version", `Number "1"
          ; "operation", `String "continue"
          ; "task", `Null
          ; "query", `Null
          ; "topic_id", `Null
          ; "features", `Null
          ; "cursor", `String cursor
          ; "max_tokens", `Number (Int.to_string budget)
          ]
      in
      let query host policy request =
        Q.query_with_receipt
          context
          ~host
          ~capabilities:(P.capabilities policy)
          ~scope:(A.scope original)
          request
      in
      let rec collect remaining host policy response =
        assert (remaining > 0);
        match Jsonaf.member_exn "next_cursor" response.Q.json with
        | `Null -> [ response ]
        | `String cursor ->
          let budget =
            match
              Jsonaf.member_exn "budget" response.json
              |> Jsonaf.member_exn "minimum_next_tokens"
            with
            | `Number value -> Int.max 6000 (Int.of_string value)
            | _ -> 6000
          in
          response
          :: collect
               (remaining - 1)
               host
               policy
               (query host policy (continue ~budget cursor))
        | _ -> assert false
      in
      let first = query host policy prepare in
      let old_cursor = Jsonaf.member_exn "next_cursor" first.json |> Jsonaf.string_exn in
      let pages = collect 100 host policy first in
      let history =
        List.filter pages ~f:(fun response -> Option.is_some response.Q.receipt)
        |> List.mapi ~f:(fun sequence response ->
          entry_of_response policy original ~sequence response)
        |> restored
      in
      let known = Presence.remember ~previous:[] ~history |> protocol_ok in
      let narrow = plan ~selected_names:[ "script"; "ochat_authoring_context" ] Manual in
      let runtime = make_host ~runtime:"changed-runtime-v2" () in
      List.iter
        [ "runtime", runtime, policy, "moderator_v1"
        ; "capabilities", host, narrow, "moderator_v1"
        ; ( "surface-and-capabilities"
          , V.for_delegated runtime
          , narrow
          , "delegated_moderator_v1" )
        ; ( "execution-availability"
          , V.without_persisted_children host
          , policy
          , "moderator_v1" )
        ]
        ~f:(fun (label, current_host, current_policy, expected_surface) ->
          let current =
            Authoring_rediscovery_test.make context current_host current_policy
          in
          let rejected = query current_host current_policy (continue old_cursor) in
          assert (Option.is_none rejected.receipt);
          assert (Option.is_some (Jsonaf.member "error" rejected.json));
          let stale =
            Presence.inspect
              ~policy:current_policy
              ~context_identity:(A.context_identity current)
              ~known
              ~effective:history
            |> protocol_ok
          in
          assert (
            List.for_all stale.observations ~f:(fun observation ->
              Presence.equal_presence observation.presence Stale_context));
          let compacted = A.refresh current ~known ~effective:[] |> protocol_ok in
          let retained_stale =
            A.refresh current ~known ~effective:history |> protocol_ok
          in
          List.iter [ compacted; retained_stale ] ~f:(fun messages ->
            assert (not (List.is_empty messages));
            assert (
              List.for_all messages ~f:(fun message ->
                G.equal_purpose message.A.guidance.purpose Rediscovery));
            assert (
              List.exists messages ~f:(fun message ->
                List.exists message.A.guidance.topics ~f:(fun topic ->
                  String.equal topic.G.id "runtime.jobs.acknowledgement"))));
          let fresh =
            collect
              100
              current_host
              current_policy
              (query current_host current_policy prepare)
          in
          List.iter fresh ~f:(fun response ->
            assert (
              String.equal
                (Jsonaf.member_exn "surface" response.Q.json |> Jsonaf.string_exn)
                expected_surface);
            assert (
              String.equal
                (Jsonaf.member_exn "runtime_identity" response.json |> Jsonaf.string_exn)
                (V.runtime_identity current_host)));
          let text =
            List.concat_map fresh ~f:(fun response ->
              Jsonaf.member_exn "items" response.Q.json |> Jsonaf.list_exn)
            |> List.filter_map ~f:(fun item ->
              Jsonaf.member "text" item |> Option.bind ~f:Jsonaf.string)
            |> String.concat ~sep:"\n"
          in
          assert (String.is_substring text ~substring:"background_job_completed");
          assert (String.is_substring text ~substring:"Internal_event");
          let candidate =
            {|let initial_state = 0
let on_event ctx state event = match event with
| `Internal_event(payload) ->
  (match Json.get_field(payload, "kind") with
  | `Some(`String("background_job_completed")) -> Task.pure(state + 1)
  | _ -> Task.pure(state))
| _ -> Task.pure(state)|}
          in
          let report =
            V.validate
              ~env
              ~host:current_host
              ~capabilities:(P.capabilities current_policy)
              (`Object
                  [ "version", `Number "1"
                  ; "target", `String "moderator"
                  ; "source", `String candidate
                  ; "tools", `Array []
                  ])
          in
          assert (Jsonaf.member_exn "valid" (V.to_json report) |> Jsonaf.bool_exn);
          assert (String.equal report.runtime_identity (V.runtime_identity current_host));
          let fresh_history =
            List.filter fresh ~f:(fun response -> Option.is_some response.Q.receipt)
            |> List.mapi ~f:(fun sequence response ->
              entry_of_response
                current_policy
                current
                ~sequence:(sequence + 1000)
                response)
            |> restored
          in
          assert (
            List.is_empty
              (A.refresh current ~known ~effective:fresh_history |> protocol_ok));
          let selected =
            List.hd_exn fresh
            |> fun response ->
            Jsonaf.member_exn "items" response.Q.json
            |> Jsonaf.list_exn
            |> List.hd_exn
            |> Jsonaf.member_exn "content"
            |> Jsonaf.member_exn "selected_tools"
            |> Jsonaf.list_exn
            |> List.map ~f:(fun tool ->
              Jsonaf.member_exn "name" tool |> Jsonaf.string_exn)
          in
          assert (
            List.equal
              String.equal
              (List.sort selected ~compare:String.compare)
              (P.capabilities current_policy
               |> C.references
               |> List.map ~f:(fun reference -> reference.C.name)
               |> List.sort ~compare:String.compare));
          print_endline
            (label
             ^ ": stale cursor rejected; old content not current; fresh full pages \
                satisfy contracts"));
      let unsupported = make_host ~targets:[ One_off_script ] () in
      let rejected = query unsupported narrow prepare in
      assert (Option.is_none rejected.receipt);
      assert (Option.is_some (Jsonaf.member "error" rejected.json));
      let current = Authoring_rediscovery_test.make context unsupported narrow in
      let pointers = A.refresh current ~known ~effective:[] |> protocol_ok in
      assert (
        not
          (List.exists pointers ~f:(fun message ->
             List.exists message.A.guidance.topics ~f:(fun topic ->
               String.equal topic.G.id "runtime.jobs.acknowledgement"))));
      print_endline
        "withdrawn background target rejects preparation and omits incompatible pointer"));
  [%expect
    {|
    runtime: stale cursor rejected; old content not current; fresh full pages satisfy contracts
    capabilities: stale cursor rejected; old content not current; fresh full pages satisfy contracts
    surface-and-capabilities: stale cursor rejected; old content not current; fresh full pages satisfy contracts
    execution-availability: stale cursor rejected; old content not current; fresh full pages satisfy contracts
    withdrawn background target rejects preparation and omits incompatible pointer
    |}]
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
