open Core
module F = Authoring_materialization_test
module A = Chat_response.Authoring_materialization
module R = Chat_response.Authoring_rediscovery
module P = Chat_response.Authoring_policy
module Q = Chat_response.Authoring_context
module C = Authoring_corpus
module G = Agent_protocol.Authoring_guidance
module H = Agent_protocol.History
module Presence = Chat_response.Authoring_presence
module Caps = Chat_response.Tool_capability
module Metadata = Chatmd_shell_spec.Authoring_metadata

let ok = F.ok
let get = F.protocol_ok

let receipt ~context ~host ~policy ~identity ~number id =
  let corpus = Q.corpus_for_host context ~host in
  let topic = C.topic corpus ~id |> ok in
  let source =
    match topic.origin with
    | Installed -> G.Installed (C.identity corpus)
    | Authored owner -> G.Authored owner.package_sha256
  in
  let payload =
    `Object [ "role", `String "user"; "content", `String "previous exact topic prose" ]
  in
  let guidance =
    G.create
      ~context_identity:identity
      ~policy_fingerprint:(P.fingerprint policy)
      ~purpose:Reference
      ~payload
      ~topics:[ { id; document_sha256 = topic.sha256; source; complete = true } ]
    |> get
  in
  let entry_id = History_entry.Id.create ~namespace:"read" ~sequence:number |> ok in
  ( Presence.{ entry_id; guidance }
  , H.
      { id = entry_id
      ; role = User
      ; kind = Message
      ; payload
      ; provenance = Runtime_authoring guidance
      ; redacted = false
      } )
;;

let make context host policy =
  A.create
    ~context
    ~host
    ~policy
    ~capabilities:(P.capabilities policy)
    ~scope:"pointer-test:0"
    ()
  |> ok
;;

let pointer messages =
  List.find_exn messages ~f:(fun message ->
    G.equal_purpose message.A.guidance.purpose Rediscovery)
;;

let text message = Jsonaf.to_string message.A.payload

let package name contents =
  let id = "custom." ^ name ^ ".conventions" in
  C.
    { help =
        Metadata.
          { version = 1
          ; package = name
          ; tasks = [ One_off_script ]
          ; topics = [ id ]
          ; required_helpers = []
          }
    ; topics =
        [ { id
          ; title = "Author conventions"
          ; prerequisites = [ "chatml.syntax.calls" ]
          ; surfaces = [ "one_off_v1" ]
          ; source_name = name ^ ".md"
          ; text = contents
          }
        ]
    }
;;

let%expect_test
    "rediscovery rechecks authored package visibility and reports changed versions"
  =
  F.fixture (fun _ host ceiling _ ->
    let reports = package "reports" "REPORT-BODY-SENTINEL" in
    let private_package = package "private" "PRIVATE-BODY-SENTINEL" in
    let context packages =
      Q.create ~secret:"pointer-packages" ~authored_packages:packages () |> ok
    in
    let old_context = context [ reports; private_package ] in
    let metadata =
      List.map (Caps.references ceiling) ~f:(fun reference ->
        let binding = Caps.find ceiling ~name:reference.name |> F.cap_ok in
        let original = Caps.metadata binding in
        ( reference.name
        , match reference.name with
          | "script" -> { original with authoring = Some reports.help }
          | "child" -> { original with authoring = Some private_package.help }
          | _ -> original ))
    in
    let ceiling =
      Caps.create
        ~metadata
        ~owner:"pointer-packages"
        ~resource_fingerprint:(F.digest "resources")
        (List.map (Caps.references ceiling) ~f:(fun reference ->
           let binding = Caps.find ceiling ~name:reference.name |> F.cap_ok in
           ( reference.implementation_revision
           , Caps.native_implementation binding |> Option.value_exn )))
      |> F.cap_ok
    in
    let policy names =
      P.resolve ~policy:Manual ~ceiling ~selected_names:names () |> F.policy_ok
    in
    let old_policy = policy [ "script"; "child" ] in
    let report, _ =
      receipt
        ~context:old_context
        ~host
        ~policy:old_policy
        ~identity:(F.digest "old-context")
        ~number:1
        "custom.reports.conventions"
    in
    let private_read, _ =
      receipt
        ~context:old_context
        ~host
        ~policy:old_policy
        ~identity:(F.digest "old-context")
        ~number:2
        "custom.private.conventions"
    in
    let current_context =
      context [ package "reports" "NEW-REPORT-BODY-SENTINEL"; private_package ]
    in
    let current = make current_context host (policy [ "script" ]) in
    let message =
      A.refresh current ~known:[ report; private_read ] ~effective:[] |> get |> pointer
    in
    let visible = List.hd_exn message.guidance.topics in
    assert (String.equal visible.id "custom.reports.conventions");
    assert (List.length message.guidance.topics = 1);
    (match visible.source with
     | Authored _ -> ()
     | Installed _ -> assert false);
    let old_hash = (List.hd_exn report.guidance.topics).document_sha256 in
    assert (not (String.equal old_hash visible.document_sha256));
    assert (String.is_substring (text message) ~substring:old_hash);
    assert (String.is_substring (text message) ~substring:visible.document_sha256);
    List.iter
      [ "custom.private"; "BODY-SENTINEL"; "ochat_authoring_context" ]
      ~f:(fun excluded ->
        assert (not (String.is_substring (text message) ~substring:excluded)));
    let needs_private =
      { reports with
        topics =
          List.map reports.topics ~f:(fun topic ->
            { topic with prerequisites = [ "custom.private.conventions" ] })
      }
    in
    let manual_unavailable =
      make (context [ needs_private; private_package ]) host (policy [ "script" ])
    in
    assert (
      List.is_empty
        (A.refresh manual_unavailable ~known:[ report; private_read ] ~effective:[] |> get));
    print_endline
      "withdrawn private package omitted; current and remembered hashes differ; authored \
       metadata carries no prose");
  [%expect
    {| withdrawn private package omitted; current and remembered hashes differ; authored metadata carries no prose |}]
;;

let%expect_test
    "compacted references produce deduplicated current pointers, never topic prose"
  =
  F.fixture (fun context host _ plan ->
    let policy = plan Auto in
    let materialization = make context host policy in
    let known, original =
      receipt
        ~context
        ~host
        ~policy
        ~identity:(A.context_identity materialization)
        ~number:1
        "chatml.tasks"
    in
    let initial = A.initial materialization |> F.entries in
    assert (
      List.is_empty
        (A.refresh materialization ~known:[ known ] ~effective:(initial @ [ original ])
         |> get));
    let messages = A.refresh materialization ~known:[ known ] ~effective:initial |> get in
    let message = pointer messages in
    assert (List.length messages = 1);
    assert (
      not (String.is_substring (text message) ~substring:"previous exact topic prose"));
    assert (List.for_all message.guidance.topics ~f:(fun topic -> not topic.G.complete));
    assert (String.is_substring (text message) ~substring:"ochat_authoring_context");
    let with_pointer = initial @ F.entries ~start:10 messages in
    assert (
      List.is_empty
        (A.refresh materialization ~known:[ known ] ~effective:with_pointer |> get));
    let stale_pointer = List.last_exn with_pointer in
    let pointer_only = Presence.remember ~previous:[] ~history:[ stale_pointer ] |> get in
    assert (
      List.is_empty
        (A.refresh materialization ~known:pointer_only ~effective:initial |> get));
    let modified = { stale_pointer with payload = `String "changed pointer" } in
    assert (
      List.length
        (A.refresh materialization ~known:[ known ] ~effective:(initial @ [ modified ])
         |> get)
      = 1);
    let absent =
      Presence.inspect
        ~policy
        ~context_identity:(A.context_identity materialization)
        ~known:[ known ]
        ~effective:with_pointer
      |> get
    in
    assert (
      List.for_all absent.observations ~f:(fun observation ->
        Presence.equal_presence observation.presence Absent));
    let redacted = { original with redacted = true } in
    assert (
      List.is_empty
        (A.refresh materialization ~known:[ known ] ~effective:(initial @ [ redacted ])
         |> get));
    print_endline
      "one pointer; repeated input deduplicates; modified pointer refreshed; missing \
       prose remains absent");
  [%expect
    {| one pointer; repeated input deduplicates; modified pointer refreshed; missing prose remains absent |}]
;;

let%expect_test
    "manual rediscovery keeps exact helper selection and ordinary agents stay unchanged"
  =
  F.fixture (fun context host _ plan ->
    let old_policy = plan Auto in
    let remembered, _ =
      receipt
        ~context
        ~host
        ~policy:old_policy
        ~identity:(F.digest "old-context")
        ~number:1
        "chatml.tasks"
    in
    List.iter [ []; [ "ochat_authoring_context" ] ] ~f:(fun helpers ->
      let policy = plan ~selected_names:("script" :: helpers) Manual in
      let materialization = make context host policy in
      assert (List.is_empty (A.initial materialization));
      let messages =
        A.refresh materialization ~known:[ remembered ] ~effective:[] |> get
      in
      assert (List.length messages = 1);
      let message = pointer messages in
      assert (
        Bool.equal
          (String.is_substring (text message) ~substring:"ochat_authoring_context")
          (not (List.is_empty helpers)));
      assert (not (String.is_substring (text message) ~substring:"ochat_validate"));
      assert (String.is_substring (text message) ~substring:"script");
      assert (
        Option.is_some
          (Agent_session.Authoring_runtime.prepare
             ~admitted:policy
             ~host
             ~elements:[]
             ~capabilities:(P.capabilities policy)
             ()
           |> ok)));
    let ordinary = make context host (plan ~selected_names:[ "ordinary" ] Auto) in
    assert (List.is_empty (A.refresh ordinary ~known:[ remembered ] ~effective:[] |> get));
    let preload = make context host (plan (Preload [ "chatml.tasks" ])) in
    let refreshed = A.refresh preload ~known:[ remembered ] ~effective:[] |> get in
    assert (
      List.for_all refreshed ~f:(fun message ->
        not (G.equal_purpose message.A.guidance.purpose Rediscovery)));
    print_endline
      "manual has no primer or extra helpers; ordinary has no pointer; preloaded prose \
       is not repeated as metadata");
  [%expect
    {| manual has no primer or extra helpers; ordinary has no pointer; preloaded prose is not repeated as metadata |}]
;;

let%expect_test
    "pointer bounds retain whole recent topics and unavailable targets do not leak"
  =
  F.fixture (fun context _host _ plan ->
    let host = F.make_host ~targets:[ One_off_script ] () in
    let policy = plan ~selected_names:[ "script" ] Auto in
    let materialization = make context host policy in
    let make number id =
      receipt ~context ~host ~policy ~identity:(F.digest "old-version") ~number id |> fst
    in
    let first = make 1 "chatml.tasks" in
    let second = make 2 "chatml.json" in
    let unavailable = make 3 "runtime.invocation-context" in
    let renderer = R.create ~context ~host ~policy |> ok in
    let render ?max_topics ?max_bytes known =
      R.render
        ?max_topics
        ?max_bytes
        renderer
        ~context_identity:(A.context_identity materialization)
        ~known
        ~effective:[]
        ~inserting:[]
        ()
    in
    let one =
      render ~max_topics:1 [ first; second; unavailable ] |> get |> Option.value_exn
    in
    assert (
      List.equal
        String.equal
        [ "chatml.json" ]
        (List.map one.topics ~f:(fun topic -> topic.G.id)));
    assert (String.is_substring one.text ~substring:"\"truncated\":true");
    assert (not (String.is_substring one.text ~substring:"runtime.invocation-context"));
    let by_bytes =
      render ~max_bytes:(String.length one.text + 1) [ first; second ]
      |> get
      |> Option.value_exn
    in
    assert (String.length by_bytes.text <= String.length one.text + 1);
    assert (List.length by_bytes.topics = 1);
    let payload = `Object [ "role", `String "user"; "content", `String one.text ] in
    let guidance =
      G.create
        ~context_identity:(A.context_identity materialization)
        ~policy_fingerprint:(P.fingerprint policy)
        ~purpose:Rediscovery
        ~payload
        ~topics:one.topics
      |> get
    in
    let current_pointer =
      H.
        { id = History_entry.Id.create ~namespace:"pointer" ~sequence:1 |> ok
        ; role = User
        ; kind = Message
        ; payload
        ; provenance = Runtime_authoring guidance
        ; redacted = false
        }
    in
    assert (
      Option.is_none
        (R.render
           ~max_topics:1
           renderer
           ~context_identity:(A.context_identity materialization)
           ~known:[ first; second ]
           ~effective:[ current_pointer ]
           ~inserting:[]
           ()
         |> get));
    assert (Result.is_error (render ~max_bytes:1 [ first ]));
    print_endline
      "count and UTF-8 budgets keep whole recent entries; unsupported target omitted; \
       undersized budget rejects");
  [%expect
    {| count and UTF-8 budgets keep whole recent entries; unsupported target omitted; undersized budget rejects |}]
;;
