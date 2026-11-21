open! Core
open Agent_server_test_support
module P = Agent_protocol
module Actor = Agent_session.Session_actor
module State = Agent_session.Session_state
module D = Agent_server.Daemon
module Handle = Agent_client.Session_handle
module Provider = Support.Tui_stream_provider
module Fixture = Recovery_note_provider_scenario
module Citation = P.Recovery_window.Citation

let field = Fixture.field
let str = Fixture.str
let num value = `Number (Int.to_string value)
let equal_json = Document_schema.Json.equal
let gif = "R0lGODlhAQABAIAAAAAAAP///ywAAAAAAQABAAACAUwAOw=="
let document = "<p>A rendered paragraph</p><!-- retained-original-marker -->"
let private_excerpt = "α-private-excerpt"

let names =
  [ "context_list_windows"
  ; "context_list_items"
  ; "context_search"
  ; "context_read_text"
  ; "context_read_media"
  ; "run_chatml"
  ; "agent_create"
  ; "agent_send"
  ]
;;

let prompt =
  {|<config model="gpt-4.1" max_tokens="100"/>
<developer>Select only the listed local history tools.</developer>|}
  ^ (List.map names ~f:(sprintf "<tool name=%S/>") |> String.concat)
;;

let complete request id =
  match Fixture.output request ~id with
  | Complete value -> value
  | outcome ->
    raise_s [%sexp "history handoff was not delivered", (outcome : P.Invocation.outcome)]
;;

let expect_failure request id code =
  match Fixture.output request ~id with
  | Fail failure -> [%test_eq: string] failure.code code
  | outcome ->
    raise_s [%sexp "expected body-free failure", (outcome : P.Invocation.outcome)]
;;

let media ~kind citation part =
  `Object
    [ "citation", Citation.to_json citation
    ; "part", num part
    ; "kind", str kind
    ; "offset", num 0
    ; "expected_sha256", `Null
    ]
;;

let text ?(preparation_revision = `Null) citation ~start ~stop ~max_bytes =
  `Object
    [ "citation", Citation.to_json citation
    ; "preparation_revision", preparation_revision
    ; "part", num 0
    ; "start", num start
    ; "stop", num stop
    ; "max_bytes", num max_bytes
    ]
;;

let page cursor = [ "limit", num 1; "cursor", cursor ]
let rows data = field "rows" data |> Jsonaf.list_exn

let recover_citation scope (row : P.History.entry) =
  Citation.create
    scope
    ~occurrence:row.id
    ~version:(P.Recovery_item_version.canonical row.content_revision)
  |> protocol_ok
;;

let image_data request ~id =
  field "input" (Provider.body request)
  |> Jsonaf.list_exn
  |> List.find_exn ~f:(fun row ->
    Option.exists (Jsonaf.member "call_id" row) ~f:(equal_json (str id))
    && Option.exists
         (Jsonaf.member "type" row)
         ~f:(equal_json (str "function_call_output")))
  |> field "output"
  |> Jsonaf.list_exn
  |> List.find_exn ~f:(fun part -> equal_json (field "type" part) (str "input_image"))
  |> field "image_url"
  |> Jsonaf.string_exn
;;

let script ~source ~tools ~input =
  `Object
    [ "source", str source
    ; "input", input
    ; "tools", `Array (List.map tools ~f:str)
    ; "timeout_ms", num 3_000
    ]
;;

let await_idle env (entry : Agent_server.Session_registry.entry) =
  Support.Tui_fixture.await env (fun () ->
    let current = Actor.state entry.actor |> protocol_ok in
    if Option.is_none current.active_operation then Some current else None)
;;

let eligible_scope daemon ~proof ~session (entry : Agent_server.Session_registry.entry) =
  let owner =
    P.Session_ref.create
      ~server_id:(Agent_store.Session_store.server_id (D.store daemon))
      ~session_id:(Handle.session_id session)
  in
  Actor.enable_recovery_windows
    entry.actor
    ~actor:proof
    ~attachment_id:(Handle.attachment session).id
    ~authorize:(fun _ ->
      Agent_server.Session_factory.check_registered_owner
        (D.factory daemon)
        entry
        ~session:owner)
  |> protocol_ok
;;

let selected_tools request =
  field "tools" (Provider.body request)
  |> Jsonaf.list_exn
  |> List.map ~f:(fun tool -> Jsonaf.string_exn (field "name" tool))
;;

let check_chatml_child env ~sw ~daemon ~proof ~provider ~client ~create ~parent_citation =
  let windows = "context_list_windows" in
  let read_text = "context_read_text" in
  let source =
    {|let main input =
  let* response = Tool.call("agent_create", input) in
  match response with
  | `Ok(receipt) -> Task.pure(receipt)
  | `Error(code) -> Task.fail(code)|}
  in
  let prompt =
    {|<config model="gpt-4.1" max_tokens="100"/>
<developer>Inspect only this managed session's local history.</developer>
<tool type="inherited" name="context_list_windows"/>
<tool type="inherited" name="context_read_text"/>|}
  in
  let inherited = [ windows; read_text ] in
  let input =
    `Object
      [ "version", num 1
      ; "root_file", str "child.chatmd"
      ; "sources", `Array [ `Object [ "path", str "child.chatmd"; "text", str prompt ] ]
      ; "tools", `Array (List.map inherited ~f:str)
      ; "start_immediately", `False
      ; "idempotency_key", str "authored-history-child"
      ]
  in
  let opened = create (script ~source ~input ~tools:("agent_create" :: inherited)) in
  let session_id = P.Id.Session.of_json (field "session_id" opened) |> protocol_ok in
  let session =
    Handle.attach
      ~sw
      ~clock:(Eio.Stdenv.clock env)
      ~connection:client
      ~session_id
      ~mode:Owner_read_write
      ~subscribe:false
      ()
    |> protocol_ok
  in
  let entry =
    Agent_server.Session_registry.find (D.registry daemon) session_id |> Option.value_exn
  in
  let expected_scope = eligible_scope daemon ~proof ~session entry in
  ignore (Handle.start session ~queue_if_limited:false |> protocol_ok : P.Session.t);
  let first = Provider.request_count provider in
  let submitted =
    Handle.send_message
      session
      { kind = Plain_text
      ; text = "Inspect this session's recovery windows."
      ; attachments = []
      }
    |> protocol_ok
  in
  assert (Option.is_some submitted.operation_id);
  let request = Provider.await_request provider env first in
  [%test_eq: string list]
    (selected_tools request
     |> List.filter ~f:(String.is_prefix ~prefix:"context_")
     |> List.sort ~compare:String.compare)
    inherited;
  Fixture.function_call request ~id:"child-windows" ~name:windows (`Object (page `Null));
  let followup = Provider.await_request provider env (first + 1) in
  let window =
    complete followup "child-windows" |> field "items" |> Jsonaf.list_exn |> List.hd_exn
  in
  assert (
    equal_json (field "scope" window) (P.Recovery_window.Scope.to_json expected_scope));
  Fixture.function_call
    followup
    ~id:"foreign-text"
    ~name:read_text
    (text parent_citation ~start:0 ~stop:2 ~max_bytes:2);
  let rejected = Provider.await_request provider env (first + 2) in
  expect_failure rejected "foreign-text" "recovery.invalid_request";
  Provider.reply_summary rejected "This child has its own open recovery window.";
  let state = await_idle env entry in
  (match
     List.find_exn state.invocations ~f:(fun i ->
       String.equal i.context.tool_name windows)
   with
   | { context = { input = `Null; _ }
     ; status = Resolved (Complete `Null) | Published (Complete `Null)
     ; _
     } -> ()
   | _ -> failwith "child retained its protected tool data");
  Handle.close session;
  session_id, entry
;;

let check_unscoped_send
      env
      ~provider
      ~send
      child
      (entry : Agent_server.Session_registry.entry)
  =
  let first = Provider.request_count provider in
  send ();
  let request = Provider.await_request provider env first in
  Fixture.function_call
    request
    ~id:"parent-send"
    ~name:"agent_send"
    (`Object
        [ "session_id", P.Id.Session.to_json child
        ; "message", str "Try to recover the child's windows without transcript scope."
        ; "idempotency_key", str "authored-history-unscoped-send"
        ]);
  let first_result = Provider.await_request provider env (first + 1) in
  let second_result = Provider.await_request provider env (first + 2) in
  let parent, child =
    if List.mem (selected_tools first_result) "agent_send" ~equal:String.equal
    then first_result, second_result
    else second_result, first_result
  in
  ignore (complete parent "parent-send" : Jsonaf.t);
  Provider.reply_summary parent "The managed message was submitted.";
  Fixture.function_call
    child
    ~id:"unscoped-windows"
    ~name:"context_list_windows"
    (`Object (page `Null));
  let outcome = Provider.await_request provider env (first + 3) in
  expect_failure outcome "unscoped-windows" "recovery.permission_denied";
  Provider.reply_summary outcome "Recovery requires a current transcript owner.";
  ignore (await_idle env entry : State.t)
;;

let run env environment =
  let live = ref true in
  let pause_auth = ref None in
  let auth ~sw:_ _ =
    Option.iter !pause_auth ~f:(fun pause -> pause ());
    Openai.Responses_driver.Auth.bearer "recovery-history-loopback"
  in
  Fixture.with_fixture
    env
    environment
    ~prompt_xml:prompt
    ~extra_features:[ Image_input ]
    ~restrict:(fun proof ->
      Operator_authorization.restrict proof ~is_current:(fun () -> !live))
    ~auth
    ~max_sessions:2
    (fun ~sw ~provider ~daemon ~proof ~client ->
       let session = Fixture.open_session sw env client ~mode:Owner_read_write in
       let id = Handle.session_id session in
       let entry =
         Agent_server.Session_registry.find (D.registry daemon) id |> Option.value_exn
       in
       let state () = Actor.state entry.actor |> protocol_ok in
       let scope = eligible_scope daemon ~proof ~session entry in
       ignore (Handle.start session ~queue_if_limited:false |> protocol_ok : P.Session.t);
       let await_idle () = await_idle env entry in
       let await = Provider.await_request provider env in
       let send kind text =
         let result =
           Support.Tui_fixture.await env (fun () ->
             match Handle.send_message session { kind; text; attachments = [] } with
             | Error
                 { code = Conflict
                 ; message = "session lifecycle is already reserved"
                 ; retryable = true
                 ; _
                 } -> None
             | result -> Some (protocol_ok result))
         in
         assert (Option.is_some result.operation_id);
         result.history_id
       in
       let find occurrence =
         List.find_exn (state ()).conversation.canonical_history ~f:(fun row ->
           P.History.Id.equal row.id occurrence)
       in
       let seed kind value =
         let index = Provider.request_count provider in
         let occurrence = send kind value in
         Provider.reply_summary (await index) "Ready.";
         ignore (await_idle () : State.t);
         recover_citation scope (find occurrence)
       in
       let first = seed Plain_text (private_excerpt ^ " one") in
       let second = seed Plain_text (private_excerpt ^ " two") in
       let root = (Support.Temporary_environment.roots environment).temporary in
       let save name bytes =
         let path = Filename.concat root name in
         Eio.Path.save
           Eio.Path.(Eio.Stdenv.fs env / path)
           ~create:(`Or_truncate 0o600)
           bytes;
         path
       in
       let image_path = save "exact.gif" (Base64.decode_exn gif) in
       let document_path = save "exact.html" document in
       let multipart =
         seed
           Chatmd
           (sprintf
              "<user><doc local markdown src=%S/><img local src=%S/></user>"
              document_path
              image_path)
       in
       let start = Provider.request_count provider in
       ignore
         (send Plain_text "Recover exact ordinary local history now." : P.History.Id.t);
       let request_index = ref start in
       let call ~id ~name args =
         Fixture.function_call (await !request_index) ~id ~name args;
         Int.incr request_index;
         await !request_index
       in
       let tools =
         field "tools" (Provider.body (await start))
         |> Jsonaf.list_exn
         |> List.map ~f:(fun tool -> field "name" tool |> Jsonaf.string_exn)
         |> String.Set.of_list
       in
       let expected = String.Set.of_list names in
       assert (Set.is_subset expected ~of_:tools);
       [%test_eq: String.Set.t]
         (Set.filter tools ~f:(String.is_prefix ~prefix:"context_"))
         (Set.filter expected ~f:(String.is_prefix ~prefix:"context_"));
       let windows =
         call ~id:"windows" ~name:"context_list_windows" (`Object (page `Null))
       in
       let item =
         complete windows "windows" |> field "items" |> Jsonaf.list_exn |> List.hd_exn
       in
       assert (equal_json (field "scope" item) (P.Recovery_window.Scope.to_json scope));
       let basis ?(family = "canonical") cursor =
         [ "window", P.Recovery_window.Scope.to_json scope
         ; "family", str family
         ; "role", str "user"
         ]
         @ page cursor
       in
       let items = call ~id:"items" ~name:"context_list_items" (`Object (basis `Null)) in
       let first_listed = complete items "items" |> rows |> List.hd_exn in
       assert (equal_json (field "citation" first_listed) (Citation.to_json first));
       let search ?family id cursor =
         let reply =
           call
             ~id
             ~name:"context_search"
             (`Object (("term", str private_excerpt) :: basis ?family cursor))
         in
         complete reply id
       in
       let hits = search "first-hit" `Null in
       let hit = rows hits |> List.hd_exn in
       assert (equal_json (field "citation" hit) (Citation.to_json first));
       assert (equal_json (field "stop" hit) (num (String.length private_excerpt)));
       let more = search "next-hit" (field "next" hits) in
       assert (
         equal_json
           (field "citation" (rows more |> List.hd_exn))
           (Citation.to_json second));
       let read_text ?preparation_revision id citation ~start ~stop ~max_bytes =
         call
           ~id
           ~name:"context_read_text"
           (text ?preparation_revision citation ~start ~stop ~max_bytes)
       in
       let read =
         read_text
           "text"
           first
           ~start:0
           ~stop:(String.length private_excerpt)
           ~max_bytes:2
       in
       let reply = complete read "text" in
       assert (equal_json (field "text" reply) (str "α"));
       assert (equal_json (field "next_offset" reply) (num 2));
       let effective_hit =
         search ~family:"effective" "effective-hit" `Null |> rows |> List.hd_exn
       in
       let effective = Citation.of_json (field "citation" effective_hit) |> protocol_ok in
       let effective_read =
         read_text
           "effective-text"
           effective
           ~preparation_revision:(field "preparation_revision" effective_hit)
           ~start:0
           ~stop:2
           ~max_bytes:2
       in
       assert (
         equal_json (field "text" (complete effective_read "effective-text")) (str "α"));
       let invalid = read_text "split" first ~start:1 ~stop:2 ~max_bytes:2 in
       expect_failure invalid "split" "recovery.invalid_request";
       let stale =
         let revision =
           match Citation.version first with
           | Canonical rev -> P.History.Content_revision.succ rev |> protocol_ok
           | Effective _ -> assert false
         in
         Citation.create
           scope
           ~occurrence:(Citation.occurrence first)
           ~version:(P.Recovery_item_version.canonical revision)
         |> protocol_ok
       in
       let absent = read_text "stale" stale ~start:0 ~stop:2 ~max_bytes:2 in
       expect_failure absent "stale" "recovery.unavailable";
       let forbidden =
         List.find_exn (state ()).conversation.canonical_history ~f:(fun row ->
           P.History.equal_role row.role System)
         |> recover_citation scope
       in
       let excluded = read_text "developer" forbidden ~start:0 ~stop:2 ~max_bytes:2 in
       expect_failure excluded "developer" "recovery.unavailable";
       let wrong =
         call
           ~id:"wrong-part"
           ~name:"context_read_media"
           (media ~kind:"image" multipart 0)
       in
       expect_failure wrong "wrong-part" "recovery.unavailable";
       let image =
         call ~id:"image" ~name:"context_read_media" (media ~kind:"image" multipart 1)
       in
       [%test_eq: string] (image_data image ~id:"image") ("data:image/gif;base64," ^ gif);
       let doc =
         call ~id:"doc" ~name:"context_read_media" (media ~kind:"document" multipart 0)
       in
       assert (equal_json (complete doc "image") `Null);
       let body = complete doc "doc" in
       [%test_eq: string]
         (field "data_base64" body |> Jsonaf.string_exn |> Base64.decode_exn)
         document;
       let direct =
         let source =
           {|let main input =
  let* response = Tool.call("context_list_windows", input) in
  match response with
  | `Error(code) -> Task.pure(`String(code))
  | _ -> Task.fail("protected body reached authored code")|}
         in
         call
           ~id:"script-denial"
           ~name:"run_chatml"
           (script
              ~source
              ~input:(`Object (page `Null))
              ~tools:[ "context_list_windows" ])
       in
       [%test_eq: string]
         (complete direct "script-denial" |> Jsonaf.string_exn)
         "invocation.protected_owner_required";
       Provider.reply_summary direct "History recovered.";
       let ended = await_idle () in
       let check_result (invocation : P.Invocation.t) =
         if
           Set.mem tools invocation.context.tool_name
           && String.is_prefix invocation.context.tool_name ~prefix:"context_"
         then (
           assert (equal_json invocation.context.input `Null);
           match invocation.status with
           | Resolved (Complete `Null | Fail _) | Published (Complete `Null | Fail _) ->
             ()
           | _ -> failwith "recovery invocation retained its projection")
       in
       List.iter ended.invocations ~f:check_result;
       let check_history (row : P.History.entry) =
         match row.kind with
         | Tool_output ->
           let bytes = Jsonaf.to_string row.payload in
           List.iter
             [ private_excerpt; gif; Base64.encode_string document ]
             ~f:(fun substring -> assert (not (String.is_substring bytes ~substring)))
         | _ -> ()
       in
       List.iter ended.conversation.canonical_history ~f:check_history;
       let child, child_entry =
         check_chatml_child
           env
           ~sw
           ~daemon
           ~proof
           ~provider
           ~client
           ~parent_citation:first
           ~create:(fun script ->
             let index = Provider.request_count provider in
             ignore (send Plain_text "Create a managed history session." : P.History.Id.t);
             Fixture.function_call
               (await index)
               ~id:"script-child"
               ~name:"run_chatml"
               script;
             let response = await (index + 1) in
             let receipt = complete response "script-child" in
             Provider.reply_summary response "The managed session was created.";
             ignore (await_idle () : State.t);
             receipt)
       in
       check_unscoped_send env ~provider child child_entry ~send:(fun () ->
         ignore (send Plain_text "Submit without transcript scope." : P.History.Id.t));
       ignore (await_idle () : State.t);
       let cutpoint = Provider.request_count provider in
       ignore
         (send Plain_text "Attempt a second protected history read." : P.History.Id.t);
       let initial = await cutpoint in
       let entered, at_auth = Eio.Promise.create () in
       let resume, continue = Eio.Promise.create () in
       pause_auth
       := Some
            (fun () ->
              pause_auth := None;
              Eio.Promise.resolve at_auth ();
              Eio.Promise.await resume);
       Fixture.function_call
         initial
         ~id:"revoked"
         ~name:"context_read_media"
         (media ~kind:"image" multipart 1);
       Eio.Time.with_timeout_exn (Eio.Stdenv.clock env) 5. (fun () ->
         Eio.Promise.await entered);
       live := false;
       Eio.Promise.resolve continue ();
       let final = await_idle () in
       [%test_eq: int] (Provider.request_count provider) (cutpoint + 1);
       List.iter final.invocations ~f:check_result;
       List.iter final.conversation.canonical_history ~f:check_history;
       Handle.close session)
;;
