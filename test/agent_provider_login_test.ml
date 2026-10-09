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
