open! Core
module P = Agent_protocol
module DTO = P.Provider_operator
module M = Credential_registry_model
module S = Private_storage
module Records = Provider_operator.Owner_records
module Intents = Provider_operator.Command_intents

let ok value =
  Result.map_error value ~f:(fun _ -> "synthetic fixture failed") |> Result.ok_or_failwith
;;

let id value = M.Id.create value |> ok

let with_directory f =
  Mirage_crypto_rng_unix.use_default ();
  Eio_main.run (fun env ->
    let path =
      "/tmp/ochat-operator-" ^ P.Id.Transaction.to_string (P.Id.Transaction.create ())
    in
    let anchor = Eio.Path.(Eio.Stdenv.fs env / path) in
    Eio.Path.mkdir ~perm:0o700 anchor;
    Exn.protect
      ~finally:(fun () -> Eio.Path.rmtree anchor)
      ~f:(fun () ->
        Eio.Switch.run (fun sw ->
          let directory =
            S.Directory.open_or_create
              ~sw
              ~anchor
              ~components:[ S.Name.create "private" |> ok ]
            |> ok
          in
          f env sw directory)))
;;

let%expect_test
    "flow claim is separate from metadata and persists original owner across reopen"
  =
  with_directory (fun _ sw directory ->
    let records =
      Records.create directory ~incarnation:(id "incarnation") ~maximum_records:4 |> ok
    in
    let owner = P.Id.Principal.create () in
    let flow =
      { DTO.Flow_ref.server_id = P.Id.Server.create ()
      ; profile = DTO.Profile_id.of_string "codex" |> ok
      ; flow_id = DTO.Flow_id.of_string "operation_one" |> ok
      ; expires_at = P.Timestamp.of_string "2030-01-01T00:00:00Z" |> ok
      }
    in
    let claim = Records.claim records flow ~sw |> ok in
    let record =
      Records.Record.create
        ~incarnation:(id "incarnation")
        ~owner
        ~operation:(id "operation_one")
        ~binding:(id "codex")
        ~key:(P.Idempotency_key.of_string "login-one" |> ok)
        ~mode:Device
        ~flow
    in
    ignore (Records.begin_ records record |> ok : Records.Record.t);
    assert (Records.is_live records flow |> ok);
    (match Records.claim records flow ~sw with
     | Error Busy -> ()
     | _ -> failwith "foreign owner took flow claim");
    let reopened =
      Records.create directory ~incarnation:(id "incarnation") ~maximum_records:4 |> ok
    in
    let restored = Records.find reopened flow |> ok in
    assert (P.Id.Principal.equal (Records.Record.owner restored) owner);
    assert (M.Id.equal (Records.Record.operation restored) (id "operation_one"));
    ignore (Records.begin_ reopened record |> ok : Records.Record.t);
    assert (List.length (Records.list reopened |> ok) = 1);
    ignore (Records.set_phase reopened flow ~phase:Completed |> ok : Records.Record.t);
    (match Records.set_phase reopened flow ~phase:Pending with
     | Error Conflict -> ()
     | _ -> failwith "terminal flow regressed");
    S.Lock.release claim;
    assert (not (Records.is_live reopened flow |> ok));
    print_endline
      "one original operation; independent claim blocks takeover; release proves unowned");
  [%expect
    {| one original operation; independent claim blocks takeover; release proves unowned |}]
;;

let%expect_test
    "intent preserves original operation and rejects changed request across restart"
  =
  with_directory (fun _ _ directory ->
    let intents = Intents.create directory ~host:(id "host") ~maximum_records:4 |> ok in
    let principal = P.Id.Principal.create () in
    let key = P.Idempotency_key.of_string "logout-one" |> ok in
    let params = `Object [ "profile", `String "codex" ] in
    let admission =
      Intents.begin_
        intents
        ~principal
        ~key
        ~method_name:"provider.logout"
        ~params
        ~operation:(id "first_operation")
      |> ok
    in
    let original =
      match admission with
      | Fresh original -> original
      | _ -> failwith "expected fresh"
    in
    let reopened = Intents.create directory ~host:(id "host") ~maximum_records:4 |> ok in
    (match
       Intents.begin_
         reopened
         ~principal
         ~key
         ~method_name:"provider.logout"
         ~params
         ~operation:(id "wrong_retry_operation")
       |> ok
     with
     | Existing restored ->
       assert (M.Id.equal (Intents.Intent.operation restored) (id "first_operation"))
     | Fresh _ -> failwith "retry acquired new operation");
    (match
       Intents.begin_
         reopened
         ~principal
         ~key
         ~method_name:"provider.logout"
         ~params:(`Object [ "profile", `String "other" ])
         ~operation:(id "another")
     with
     | Error Conflict -> ()
     | _ -> failwith "changed request accepted");
    (match
       Intents.begin_
         reopened
         ~principal
         ~key:(P.Idempotency_key.of_string "different-intent" |> ok)
         ~method_name:"provider.logout"
         ~params
         ~operation:(id "first_operation")
     with
     | Error Conflict -> ()
     | _ -> failwith "operation collision published");
    assert (
      Option.is_some
        (Intents.lookup reopened ~principal ~key ~method_name:"provider.logout" ~params
         |> ok));
    let result =
      P.Command_receipt.Provider_logout
        { profile = DTO.Profile_id.of_string "codex" |> ok
        ; auth_epoch = 2L
        ; drain = Drained
        ; cleanup_pending = false
        }
    in
    (match
       Intents.complete
         intents
         original
         (P.Command_receipt.Provider_selection
            { profile = DTO.Profile_id.of_string "codex" |> ok
            ; revision = DTO.Revision.of_string "selection" |> ok
            })
     with
     | Error Conflict -> ()
     | _ -> failwith "wrong result method accepted");
    Intents.complete intents original result |> ok;
    let restored =
      Intents.lookup reopened ~principal ~key ~method_name:"provider.logout" ~params
      |> ok
      |> Option.value_exn
    in
    assert (Option.is_some (Intents.Intent.committed restored));
    print_endline
      "original intent and finite receipt survive restart; changed args refused");
  [%expect {| original intent and finite receipt survive restart; changed args refused |}]
;;
