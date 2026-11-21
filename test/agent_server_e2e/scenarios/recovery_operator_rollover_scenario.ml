open! Core
module P = Agent_protocol
module Wire = P.Recovery_rollover_control
module R = Agent_session.Recovery_rollover
module Client = Agent_client.Recovery_windows
module Scope = P.Recovery_window.Scope
module Stock = Recovery_capacity_scenario
module Checkpoint = Recovery_checkpoint_scenario
module Fixture = Crash_recovery_fixture
module Provider = Support.Tui_stream_provider
module Config = Support.Config_fixture
module Http = Support.Http_driver
module Data = Support.Data_integrity_http

let ok = Fixture.protocol_ok
let require = Fixture.require
let key = Fixture.key
let body = "The actual transcript contains a private rollover fixture."
let response = "The provider's response must stay outside status."

let expect_error result code =
  match result with
  | Error (failure : P.Error.t) when P.Error.equal_code failure.code code -> ()
  | Error failure -> raise_s [%sexp "unexpected rollover failure", (failure : P.Error.t)]
  | Ok _ -> failwith "rollover unexpectedly succeeded"
;;

let with_client ~sw env fixture ~token ?(lose_rollover_reply = false) f =
  let client =
    Http.create ~sw ~env ~port:(Config.http_port fixture) ~token:(Some token)
    |> Result.ok_or_failwith
  in
  let lost = ref false in
  let transport =
    Agent_client.Transport.create
      ~request:(fun command ->
        let open Result.Let_syntax in
        let%bind rpc = Http.request client command in
        match command with
        | P.Command.Recovery_rollover _ when lose_rollover_reply && not !lost ->
          lost := true;
          Error
            (P.Error.create
               Interrupted
               ~message:"fixture lost the reply"
               ~retryable:true
               ())
        | _ -> Ok rpc.result)
      ~next_notification:(fun () -> None)
      ~close:(fun () ->
        ignore (Http.close_connection client : (Http.response, string) result);
        Http.shutdown client)
  in
  let connection = Agent_client.Connection.create transport in
  Exn.protect
    ~finally:(fun () -> Agent_client.Connection.close connection)
    ~f:(fun () ->
      let initialized =
        Agent_client.Session_handle.initialize_with_features
          connection
          ~implementation_name:"recovery-operator-rollover-e2e"
          ~implementation_version:"dev"
          ~features:[ P.Recovery_query.Features.windows; Wire.feature ]
        |> ok
      in
      require
        (List.mem initialized.enabled_features Wire.feature ~equal:String.equal)
        "stock recovery control failed negotiation";
      f client connection initialized)
;;

let inspect connection session ?attachment_id ?key () =
  Client.inspect_rollover connection Wire.Inspect.{ session; attachment_id; key } |> ok
;;

let available status f =
  match (status : Wire.Status.t).view with
  | Unavailable -> failwith "expected a current local recovery window"
  | Available { current; previous; capacity; receipt; actions } ->
    f current previous capacity receipt actions
;;

let basis connection session attachment_id =
  available (inspect connection session ~attachment_id ()) (fun current _ _ _ _ ->
    current)
;;

let assert_no_contents receipt sent_key =
  let json = Wire.Receipt.to_json receipt |> Jsonaf.to_string in
  List.iter
    [ body
    ; response
    ; P.Idempotency_key.to_string sent_key
    ; "attachment_id"
    ; "principal"
    ; "operation"
    ; "archive_sha256"
    ; "checkpoint_notes"
    ; "provider_account"
    ; "endpoint"
    ]
    ~f:(fun substring ->
      require
        (not (String.is_substring json ~substring))
        ("public receipt unexpectedly contained " ^ substring))
;;

let compare_durable env fixture session attachment_id key observed =
  let state : Agent_session.Session_state.t =
    Support.Background_fixture.checkpoint env fixture { summary = session; attachment_id }
    |> Option.value_exn
  in
  let principal = Option.value_exn state.identity.creating_principal in
  let stored =
    R.find_operator (Option.value_exn state.recovery_rollover) ~principal ~key
    |> Option.value_exn
  in
  match observed.Wire.Receipt.outcome, stored.status with
  | Committed shown, Terminal (Committed actual) ->
    require
      (Scope.equal actual.successor shown.successor
       && Int64.equal actual.cut_revision shown.cut_revision
       && Scope.equal stored.intent.expected.scope observed.expected.scope
       && Int64.equal stored.intent.expected.revision observed.expected.revision)
      "stock response disagreed with the authoritative journal"
  | _ -> failwith "expected the original committed durable rollover receipt"
;;

let check_windows connection session old next =
  let page = P.Page.Request.create ~limit:10 () |> ok in
  let page =
    Client.list connection { session; generation = None; state = None; page } |> ok
  in
  let matches scope expected =
    List.exists page.items ~f:(fun (row : P.Recovery_query.Summary.t) ->
      Scope.equal scope row.scope && P.Recovery_query.State.equal expected row.state)
  in
  require
    (Int.equal (List.length page.items) 2
     && Option.is_none page.next_cursor
     && matches old Sealed
     && matches next Open)
    "lost reply/retry did not preserve exactly one successor"
;;

let without_force ~sw env fixture =
  with_client
    ~sw
    env
    fixture
    ~token:(Config.public_token fixture)
    (fun client connection initialized ->
       require
         (not (P.Principal.has_scope initialized.principal Administer_configuration))
         "limited source owner unexpectedly has force authority";
       let session, attachment =
         Data.create ~mode:Owner_read_write client "rollover-limited-owner"
       in
       let reference =
         P.Session_ref.create ~server_id:initialized.server_id ~session_id:session.id
       in
       let scope =
         Stock.enable connection ~server_id:initialized.server_id session attachment.id
       in
       Stock.start client session attachment.id "rollover-start-limited";
       let original =
         available
           (inspect connection reference ~attachment_id:attachment.id ())
           (fun current _ _ _ actions ->
              require
                (actions.rollover && not actions.force)
                "current limited owner should only see ordinary rollover";
              current)
       in
       let command =
         Wire.Request.
           { expected = original
           ; attachment_id = attachment.id
           ; key = key "rollover-denied"
           ; force = true
           }
       in
       expect_error (Client.request_rollover connection command) Permission_denied;
       available
         (inspect connection reference ~key:command.key ())
         (fun current _ _ receipt _ ->
            require
              (Wire.Basis.equal current original && Option.is_none receipt)
              "denied force wrote a durable intent or changed the cut");
       let ordinary =
         { command with force = false; key = key "rollover-no-checkpoint" }
       in
       let receipt = Client.request_rollover connection ordinary |> ok in
       (match receipt.outcome with
        | Failed Checkpoint_required -> ()
        | _ -> failwith "ordinary rollover without checkpoint did not fail durably");
       available
         (inspect connection reference ~key:ordinary.key ())
         (fun current _ _ replay actions ->
            require
              (Scope.equal current.scope scope
               && Option.exists replay ~f:(Wire.Receipt.equal receipt)
               && not (actions.rollover || actions.force))
              "failed receipt did not preserve the old window and observer-only actions");
       reference, ordinary.key)
;;

let run env environment =
  let fixture = Data.fixture env environment "recovery-operator-rollover" in
  let public_scopes =
    Set.remove Config.all_scopes P.Scope.Administer_configuration |> Set.to_list
  in
  Config.grant_public_scopes fixture public_scopes;
  Fixture.write
    env
    (Config.prompt_path fixture)
    {|<config model="gpt-4.1" max_tokens="100"/><developer>Preserve private history.</developer>|};
  let config_path =
    Stock.configuration fixture Stock.policy
    |> Config.write_configuration fixture ~name:"rollover-control.sexp"
  in
  Eio.Switch.run (fun sw ->
    let reservation = Support.Port_reservation.create ~sw ~env in
    let port = Support.Port_reservation.port reservation in
    Support.Port_reservation.release reservation;
    let provider = Provider.start ~sw ~env ~port in
    let admin = Config.admin_token fixture in
    let session, command, committed, successor =
      Stock.with_stock env fixture ~port ~config_path (fun _ _ _ ->
        let limited_session, limited_key = without_force ~sw env fixture in
        let session, reference, command =
          with_client
            ~sw
            env
            fixture
            ~token:admin
            ~lose_rollover_reply:true
            (fun client connection initialized ->
               let session, attachment =
                 Data.create ~mode:Owner_read_write client "rollover-admin-owner"
               in
               let reference =
                 P.Session_ref.create
                   ~server_id:initialized.server_id
                   ~session_id:session.id
               in
               require
                 (Wire.Status.equal
                    (inspect connection reference ())
                    (Wire.Status.create reference Unavailable |> ok))
                 "unselected recovery did not return explicit unavailable status";
               let window =
                 Stock.enable
                   connection
                   ~server_id:initialized.server_id
                   session
                   attachment.id
               in
               Stock.start client session attachment.id "rollover-start-admin";
               let before =
                 available
                   (inspect connection reference ~attachment_id:attachment.id ())
                   (fun current previous capacity receipt actions ->
                      require
                        (Scope.equal current.scope window
                         && Option.is_none previous
                         && Option.is_none receipt
                         && actions.rollover
                         && actions.force)
                        "initial status did not expose the actual available actions";
                      (match capacity with
                       | Unknown { reason = Awaiting_preparation; target = Some _ } -> ()
                       | _ -> failwith "unprepared target fabricated capacity evidence");
                      current)
               in
               available
                 (inspect connection limited_session ~key:limited_key ())
                 (fun _ _ _ receipt actions ->
                    require
                      (Option.is_none receipt && not actions.force)
                      "another principal's exact key disclosed a receipt");
               Checkpoint.send
                 client
                 session
                 attachment.id
                 ~key:"rollover-transcript"
                 body;
               let inference = Provider.await_request provider env 0 in
               available
                 (inspect connection reference ~attachment_id:attachment.id ())
                 (fun _ _ _ _ actions ->
                    require
                      (not (actions.rollover || actions.force))
                      "active foreground was advertised as an idle cut");
               Provider.reply_summary inference response;
               Checkpoint.await_idle env client session;
               available (inspect connection reference ()) (fun _ _ capacity _ _ ->
                 match capacity with
                 | Observed
                     { target
                     ; freshness = Retained
                     ; remaining = Some (Estimated, Available remaining)
                     } ->
                   require
                     (String.equal target.profile "first-party-openai-responses"
                      && String.equal target.model "gpt-4.1"
                      && Int64.(remaining > 0L))
                     "capacity evidence used the wrong target or remaining count"
                 | _ ->
                   failwith "idle status lost the actual retained capacity measurement");
               let stale =
                 Wire.Request.
                   { expected = before
                   ; attachment_id = attachment.id
                   ; key = key "rollover-stale"
                   ; force = true
                   }
               in
               expect_error (Client.request_rollover connection stale) Conflict;
               let expected = basis connection reference attachment.id in
               let command =
                 { stale with expected; key = key "rollover-lost-response" }
               in
               expect_error (Client.request_rollover connection command) Interrupted;
               session, reference, command)
        in
        with_client ~sw env fixture ~token:admin (fun client connection _ ->
          let observed = inspect connection reference ~key:command.key () in
          let committed, successor =
            available observed (fun current previous capacity receipt actions ->
              let committed = Option.value_exn receipt in
              require
                (Option.exists previous ~f:(Scope.equal command.expected.scope)
                 && not (actions.rollover || actions.force))
                "reconnected observer did not get bound current and previous windows";
              (match capacity with
               | Unknown { reason = Stale_preparation; _ } -> ()
               | _ -> failwith "successor misattributed predecessor capacity");
              match committed.outcome with
              | Committed { successor; quality = Archive_only_missing_checkpoint; _ }
                when Scope.equal successor current.scope -> committed, successor
              | _ -> failwith "lost reply did not reconcile to the actual committed cut")
          in
          assert_no_contents committed command.key;
          let writer = Stock.reattach client session "rollover-reconnect-owner" in
          let duplicate = { command with attachment_id = writer } in
          require
            (Wire.Receipt.equal
               committed
               (Client.request_rollover connection duplicate |> ok))
            "exact reattached retry changed the receipt";
          expect_error
            (Client.request_rollover connection { duplicate with force = false })
            Idempotency_conflict;
          expect_error
            (Client.request_rollover
               connection
               { duplicate with expected = basis connection reference writer })
            Idempotency_conflict;
          compare_durable env fixture session writer command.key committed;
          check_windows connection reference command.expected.scope successor;
          session, command, committed, successor))
    in
    Stock.with_stock env fixture ~port ~config_path (fun _ _ _ ->
      with_client ~sw env fixture ~token:admin (fun client connection _ ->
        let reference = Wire.Request.session command in
        available
          (inspect connection reference ~key:command.key ())
          (fun current _ capacity receipt _ ->
             require
               (Scope.equal current.scope successor
                && Option.exists receipt ~f:(Wire.Receipt.equal committed))
               "stock restart did not recover the same receipt";
             match capacity with
             | Unknown { reason = Awaiting_preparation; _ } -> ()
             | _ -> failwith "restart fabricated current provider evidence");
        let writer = Stock.reattach client session "rollover-restart-owner" in
        let duplicate = { command with attachment_id = writer } in
        require
          (Wire.Receipt.equal
             committed
             (Client.request_rollover connection duplicate |> ok))
          "restarted duplicate did not read the same terminal receipt";
        check_windows connection reference command.expected.scope successor;
        require
          (Int.equal (Provider.request_count provider) 1)
          "operator rollover or replay submitted an unexpected provider request")))
;;
