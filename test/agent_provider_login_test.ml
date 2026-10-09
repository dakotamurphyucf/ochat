open! Core
module P = Agent_protocol
module C = Agent_client
module DTO = P.Provider_operator

let checked = function
  | Ok value -> value
  | Error error -> raise_s [%sexp (error : P.Error.t)]
;;

let%expect_test
    "shared provider login retains uncertainty and privately consumes challenges"
  =
  Eio_main.run (fun _ ->
    let profile = DTO.Profile_id.of_string "host-codex" |> checked in
    let flow =
      DTO.Flow_ref.
        { server_id = P.Id.Server.of_string "srv_login_client" |> checked
        ; profile
        ; flow_id = DTO.Flow_id.of_string "flow-test" |> checked
        ; expires_at = P.Timestamp.of_string "2026-10-09T12:00:00Z" |> checked
        }
    in
    let challenge =
      DTO.Private_challenge.device
        ~verification_uri:(Uri.of_string "https://example.invalid/device")
        ~user_code:"TEST-CODE"
      |> checked
    in
    let admissions = ref 0 in
    let connection =
      C.Transport.create
        ~request:(function
          | P.Command.Provider_login_begin _ ->
            incr admissions;
            Error (P.Error.create Interrupted ~message:"reply lost" ~retryable:false ())
          | Provider_login_challenge _ ->
            Ok (P.Public.Result.Private_provider_challenge challenge)
          | _ -> assert false)
        ~next_notification:(fun () -> failwith "login must not consume session events")
        ~close:ignore
      |> C.Connection.create
    in
    let owner = C.Connection.claim_notifications connection |> checked in
    let request key =
      DTO.Login_request.
        { profile
        ; mode = Device
        ; idempotency_key = P.Idempotency_key.of_string key |> checked
        }
    in
    assert (Result.is_error (C.Provider_login.begin_login connection (request "original")));
    assert (
      Result.is_error (C.Provider_login.begin_login connection (request "replacement")));
    assert (Int.equal !admissions 1);
    assert (Int.equal (List.length (C.Connection.pending_commands connection)) 1);
    assert (
      Result.is_error
        (C.Connection.request_without_history
           connection
           (Provider_login_challenge { flow })));
    let received = C.Provider_login.challenge connection { flow } |> checked in
    let explicit =
      DTO.Private_challenge.with_device_prompt
        received
        ~f:(fun ~verification_uri ~user_code ->
          String.equal (Uri.to_string verification_uri) "https://example.invalid/device"
          && String.equal user_code "TEST-CODE")
      |> Option.value ~default:false
    in
    assert explicit;
    let debug = DTO.Private_challenge.sexp_of_t received |> Sexp.to_string in
    assert (not (String.is_substring debug ~substring:"TEST-CODE"));
    assert (Int.equal (List.length (C.Connection.pending_commands connection)) 1);
    C.Connection.release_notifications owner;
    C.Connection.close connection;
    print_endline
      "one login admission; original uncertainty retained; explicit private challenge; \
       notification owner untouched");
  [%expect
    {| one login admission; original uncertainty retained; explicit private challenge; notification owner untouched |}]
;;

let public result =
  P.Public.Result.Non_history.of_internal result
  |> Result.map ~f:(fun value -> P.Public.Result.Non_history value)
;;

let%expect_test
    "profile selection preserves host CAS, original key and lost-reply receipt"
  =
  Eio_main.run (fun _ ->
    let profile = DTO.Profile_id.of_string "host-codex" |> checked in
    let selected : DTO.Selection_result.t =
      { profile; revision = DTO.Revision.of_string "selection-8" |> checked }
    in
    let original : DTO.Select_request.t =
      { profile
      ; expected_revision = DTO.Revision.of_string "selection-7" |> checked
      ; idempotency_key = P.Idempotency_key.of_string "original-selection" |> checked
      }
    in
    let initialization =
      P.Initialize.Response.create
        ~protocol_name:"ochat.agent"
        ~selected_version:P.Version.current
        ~implementation:
          (P.Initialize.Implementation.create ~name:"selection-fixture" ~version:"1"
           |> checked)
        ~server_id:(P.Id.Server.of_string "srv_selection_fixture" |> checked)
        ~enabled_features:[]
        ~extensions:None
        ~principal:
          (P.Principal.create
             ~id:(P.Id.Principal.of_string "pri_selection_fixture" |> checked)
             ~authentication_kind:"test"
             ~scopes:(P.Scope.Set.of_list [ Provider_view; Provider_select ])
             ~attributes:[]
           |> checked)
        ~limits:
          { max_request_bytes = 1_048_576
          ; max_event_bytes = 1_048_576
          ; max_page_size = 1000
          ; max_attachments_per_connection = 8
          }
        ~event_retention:
          { minimum_age_ms = 1000
          ; maximum_events = 1000
          ; oldest_replayable_sequence = None
          }
        ~timing:
          { heartbeat_interval_ms = 1000
          ; owner_lease_duration_ms = 60_000
          ; owner_renew_after_ms = 30_000
          ; disconnect_grace_default_ms = 1000
          }
        ~server_time:(P.Timestamp.of_string "2026-10-09T12:00:00Z" |> checked)
      |> checked
    in
    let admissions = ref 0 in
    let lookups = ref 0 in
    let reject = ref None in
    let lost_reply = ref true in
    let connection =
      C.Transport.create
        ~request:(function
          | P.Command.Protocol_initialize _ -> public (Protocol_initialize initialization)
          | Provider_select request ->
            incr admissions;
            assert (DTO.Profile_id.equal request.profile original.profile);
            assert (
              DTO.Revision.equal request.expected_revision original.expected_revision);
            (match !reject with
             | Some code ->
               Error
                 (P.Error.create
                    code
                    ~message:"host selection refused"
                    ~retryable:false
                    ())
             | None ->
               assert (
                 P.Idempotency_key.equal request.idempotency_key original.idempotency_key);
               if !lost_reply
               then
                 Error
                   (P.Error.create
                      Interrupted
                      ~message:"selection reply lost"
                      ~retryable:false
                      ())
               else public (Provider_select selected))
          | Command_receipt request ->
            incr lookups;
            assert (String.equal request.method_name "provider.select");
            assert (
              Jsonaf.exactly_equal
                request.original_params
                (DTO.Select_request.to_json original));
            public (Command_receipt (Committed (Provider_selection selected)))
          | _ -> failwith "selection must use its original method or receipt")
        ~next_notification:(fun () -> failwith "selection must not consume notifications")
        ~close:ignore
      |> C.Connection.create
    in
    let owner = C.Connection.claim_notifications connection |> checked in
    Exn.protect
      ~finally:(fun () ->
        C.Connection.release_notifications owner;
        C.Connection.close connection)
      ~f:(fun () ->
        C.Session_handle.initialize
          connection
          ~implementation_name:"selection-client"
          ~implementation_version:"1"
        |> checked
        |> ignore;
        let code = function
          | Ok _ -> failwith "host refusal must remain an error"
          | Error (error : P.Error.t) -> error.code
        in
        reject := Some Permission_denied;
        assert (
          P.Error.equal_code
            (code (C.Provider_login.select_profile connection original))
            Permission_denied);
        reject := Some Conflict;
        assert (
          P.Error.equal_code
            (code (C.Provider_login.select_profile connection original))
            Conflict);
        assert (List.is_empty (C.Connection.pending_commands connection));
        reject := None;
        lost_reply := true;
        assert (
          P.Error.equal_code
            (code (C.Provider_login.select_profile connection original))
            Interrupted);
        let replacement =
          { original with
            idempotency_key =
              P.Idempotency_key.of_string "replacement-selection" |> checked
          }
        in
        assert (
          P.Error.equal_code
            (code (C.Provider_login.select_profile connection replacement))
            Interrupted);
        assert (Int.equal !admissions 3);
        let pending = C.Connection.pending_commands connection |> List.hd_exn in
        let receipt = C.Connection.reconcile connection pending |> checked in
        (match receipt with
         | Committed (Provider_selection outcome) ->
           assert (DTO.Profile_id.equal outcome.profile selected.profile);
           assert (DTO.Revision.equal outcome.revision selected.revision)
         | Missing | Unavailable | Pending _ | Failed _ | Committed _ ->
           failwith "selection must reconcile its actual retained outcome");
        assert (List.is_empty (C.Connection.pending_commands connection));
        (* An explicit same-key retry after a known committed receipt may return
           the host's retained ordinary result; no fresh-key replay occurred. *)
        lost_reply := false;
        let completed = C.Provider_login.select_profile connection original |> checked in
        assert (DTO.Profile_id.equal completed.profile selected.profile);
        assert (DTO.Revision.equal completed.revision selected.revision);
        assert (List.is_empty (C.Connection.pending_commands connection));
        print_s [%sexp ((!admissions, !lookups) : int * int)]));
  [%expect {| (4 1) |}]
;;
