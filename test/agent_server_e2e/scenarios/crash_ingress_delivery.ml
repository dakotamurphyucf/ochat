open Core
module F = Crash_recovery_fixture
module P = Agent_protocol
module B = Support.Background_fixture
module C = Support.Config_fixture
module Process = Support.Process_manager
module Http = Support.Http_driver

let sources =
  [ ( "agent.chatmd"
    , [%blob "../../chatml_extensibility_fixtures/x08-external-completion/agent.chatmd"] )
  ; ( "any.json"
    , [%blob "../../chatml_extensibility_fixtures/x08-external-completion/any.json"] )
  ; ( "string.json"
    , [%blob "../../chatml_extensibility_fixtures/x08-external-completion/string.json"] )
  ]
;;

let with_host env environment fixture ~recover f =
  Eio.Switch.run (fun sw ->
    let child =
      F.child
        ~sw
        env
        environment
        ~case:"ingress"
        ~arguments:[ "ingress"; C.config_path fixture; Bool.to_string recover ]
    in
    Exn.protect
      ~finally:(fun () -> F.terminate env child)
      ~f:(fun () ->
        F.await_marker env child "ingress-host-ready";
        let client =
          Http.create
            ~sw
            ~env
            ~port:(C.http_port fixture)
            ~token:(Some (C.admin_token fixture))
          |> Result.ok_or_failwith
        in
        Exn.protect
          ~finally:(fun () -> Http.shutdown client)
          ~f:(fun () ->
            let implementation =
              P.Initialize.Implementation.create ~name:"ingress-crash" ~version:"test"
              |> F.protocol_ok
            in
            let initialize =
              P.Initialize.Request.create
                ~implementation
                ~protocol_min:P.Version.ingress_minimum
                ~protocol_max:P.Version.current
                ~features:[]
                ~event_encodings:[ Json ]
                ~max_inbound_event_bytes:(2 * 1024 * 1024)
                ()
              |> F.protocol_ok
            in
            ignore (F.request client (Protocol_initialize initialize) : P.Method_result.t);
            Eio.Time.with_timeout_exn (Eio.Stdenv.clock env) 20. (fun () ->
              f child client))))
;;

let registration snapshot =
  List.find_map snapshot.P.Snapshot.canonical_history.entries ~f:(fun entry ->
    match
      Agent_session.History_codec.of_protocol entry |> F.protocol_ok |> History_entry.item
    with
    | Openai.Responses.Item.Function_call_output { output = Text text; _ } ->
      (match Jsonaf.of_string text |> P.Invocation.outcome_of_json |> F.protocol_ok with
       | Pending (Subscription _, acknowledgement) ->
         let fields = P.Json_codec.fields acknowledgement |> F.protocol_ok in
         Some
           (P.Json_codec.required_as fields "registration_id" P.Id.Capability.of_json
            |> F.protocol_ok)
       | _ -> None)
    | _ -> None)
  |> Option.value_exn
;;

let state env fixture session = B.checkpoint env fixture session |> Option.value_exn

let handlers state =
  List.count state.Agent_session.Session_state.moderator_executions ~f:(fun execution ->
    P.Moderator_execution.equal_phase execution.context.phase Internal_event)
;;

let frames state =
  List.filter
    state.Agent_session.Session_state.conversation.canonical_history
    ~f:(fun entry ->
      match entry.P.History.provenance with
      | Runtime_notification _ -> true
      | _ -> false)
;;

let test env environment =
  let fixture = F.fixture env environment "ingress-lost-ack" in
  List.iter sources ~f:(fun (name, contents) ->
    let path =
      if String.equal name "agent.chatmd"
      then C.prompt_path fixture
      else Filename.concat (Filename.dirname (C.prompt_path fixture)) name
    in
    F.write env path contents);
  F.write
    env
    (C.config_path fixture)
    (C.configuration fixture ()
     |> String.substr_replace_all
          ~pattern:"(tool_default deny)"
          ~with_:"(tool_default allow)");
  let session, request, receipt =
    with_host env environment fixture ~recover:false (fun child client ->
      let session = B.create client "ingress:create" in
      ignore
        (F.request
           client
           (Session_send_message
              { session_id = session.summary.id
              ; attachment_id = session.attachment_id
              ; content = { kind = Plain_text; text = "Call watch."; attachments = [] }
              ; idempotency_key = F.key "ingress:send"
              })
         : P.Method_result.t);
      ignore
        (B.await env "ingress registration acknowledged" (fun () ->
           let current = state env fixture session in
           Option.some_if
             (Option.is_none current.active_operation
              && not (List.is_empty current.ingress_registrations))
             current)
         : Agent_session.Session_state.t);
      let request : P.Ingress.Submit_request.t =
        { session_id = session.summary.id
        ; registration_id = registration (F.get client session.summary.id)
        ; namespace = "external.report"
        ; idempotency_key = F.key "ingress:result"
        ; payload = `Object [ "value", `String "finished" ]
        }
      in
      Eio.Fiber.both
        (fun () ->
           match Http.request client (Ingress_submit request) with
           | Error _ -> ()
           | Ok _ -> F.fail "receipt was acknowledged before the crash boundary")
        (fun () ->
           F.await_marker env child "ingress-receipt-saved";
           F.kill env child);
      let saved = state env fixture session in
      F.require
        (handlers saved = 0 && List.is_empty (frames saved))
        "handler ran before ingress acceptance crash";
      let registration = List.hd_exn saved.ingress_registrations in
      F.require
        (List.length registration.receipts = 1)
        "accepted receipt did not survive journal sync";
      session, request, List.hd_exn registration.receipts)
  in
  let expected : P.Ingress.Acknowledgement.t =
    { session_id = request.session_id
    ; registration_id = request.registration_id
    ; event_id = receipt.id
    ; idempotency_key = receipt.key
    ; payload_sha256 = receipt.payload_sha256
    ; accepted_at = receipt.accepted_at
    }
  in
  let previous_frames = ref None in
  for reopen = 1 to 2 do
    with_host env environment fixture ~recover:true (fun child client ->
      (match F.request client (Ingress_submit request) with
       | Ingress_submit acknowledgement ->
         F.require
           (P.Ingress.Acknowledgement.equal expected acknowledgement)
           "retry changed the durable acknowledgement"
       | _ -> F.fail "retry returned the wrong result");
      let recovered =
        B.await env "ingress recovery notification" (fun () ->
          let current = state env fixture session in
          Option.some_if
            (handlers current = 1
             && List.length (frames current) = 1
             && Option.is_none current.active_operation
             && List.for_all current.deliveries ~f:(fun delivery ->
               match delivery.wake_disposition with
               | Some (Accepted_wake _) -> true
               | _ -> false))
            current)
      in
      F.require
        (List.length (List.hd_exn recovered.ingress_registrations).receipts = 1)
        "retry duplicated ingress receipt";
      F.require
        (List.is_empty recovered.permissions && Option.is_none recovered.failure)
        "ingress recovery failed or changed permissions";
      (match !previous_frames with
       | None -> previous_frames := Some (frames recovered)
       | Some previous ->
         F.require_equal
           "stable ingress notification"
           [%sexp_of: P.History.entry list]
           previous
           (frames recovered));
      for _ = 1 to 10 do
        Eio.Time.sleep (Eio.Stdenv.clock env) 0.03;
        let calls =
          String.split_lines (Process.stdout child).contents
          |> List.count ~f:(String.is_prefix ~prefix:"ingress-provider ")
        in
        F.require
          (calls = if reopen = 1 then 1 else 0)
          "ingress recovery lost or repeated its model continuation"
      done;
      F.kill env child)
  done
;;
