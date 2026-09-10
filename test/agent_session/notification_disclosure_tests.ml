open Core
open Fixtures
module P = Agent_protocol
module C = Chat_response.Tool_capability
module S = Agent_session.Script_notification_service
module A = Agent_session.Session_actor
module B = Chat_response.Background_request
module Setup = Subscription_transaction_tests
module Timers = Schedule_transaction_tests

let cap result =
  Result.map_error result ~f:(fun error -> error.C.message) |> Result.ok_or_failwith
;;

let registry ?(resources = "resources-v1") names calls =
  let implementations =
    List.map names ~f:(fun name ->
      let module Definition = struct
        type input = string

        let name = name
        let description = Some "disclosure fixture"
        let type_ = "function"
        let parameters = `Object [ "type", `String "object" ]
        let input_of_string value = value
      end
      in
      ( Chatmd_shell_spec.Source_ref.digest ("implementation:" ^ name)
      , Ochat_function.create_function
          (module Definition)
          (fun _ ->
             Int.incr calls;
             Openai.Responses.Tool_output.Output.Text "PRIVATE") ))
  in
  C.create
    ~owner:"disclosure-fixture"
    ~resource_fingerprint:(Chatmd_shell_spec.Source_ref.digest resources)
    implementations
  |> cap
;;

let%expect_test
    "publisher selection persists independently of creator identity and fresh live \
     capability IDs"
  =
  Job_fixtures.with_actor (fun _ _ actor _ backend ->
    A.change_moderator actor (Some (Setup.encode Setup.before)) |> protocol_ok |> ignore;
    let calls = ref 0 in
    let original = registry [ "read_file"; "private_tool" ] calls in
    let selected = C.select original ~names:[ "read_file" ] |> cap in
    let parent = Job_fixtures.add_claimed_job actor in
    Timers.with_event actor parent (fun owner commit ->
      S.with_scope
        (Notification_access_tests.notices actor)
        ~owner
        ~source:Setup.source
        ~selected
        ~jobs:None
        ~error:P.Error.invalid_request
        (fun scope ->
           let open Result.Let_syntax in
           let tx = S.moderator_transaction scope in
           let error result = Result.map_error result ~f:P.Error.invalid_request in
           let%bind receipt, _ =
             tx.handlers.publish
               ~correlation:{ key = "result"; invocation_id = None; work = None }
               ~completion:(Succeeded (`String "ready"))
               ~wake:Request_turn
             |> error
           in
           let%bind acknowledge = tx.prepare [ receipt ] |> error in
           let%map () = Timers.save commit in
           acknowledge ()))
    |> protocol_ok
    |> ignore;
    let state = A.state actor |> protocol_ok in
    assert_same_session_snapshot state (Agent_session.Memory_backend.state backend);
    let delivery = List.hd_exn state.deliveries in
    let pins = Option.value_exn delivery.disclosure_pins in
    [%test_eq: string list] [ "read_file" ] (List.map pins ~f:fst);
    let restored =
      Agent_session.Session_persistence.restore_snapshot
        (Sexp.to_string_mach (Agent_session.Session_state.sexp_of_t state))
      |> store_ok
    in
    let restored = List.hd_exn restored.deliveries in
    assert (P.Delivery.equal delivery restored);
    let current = registry [ "extra_tool"; "read_file"; "private_tool" ] calls in
    let old_ref = List.hd_exn (C.references selected) in
    let current_ref = C.find current ~name:"read_file" |> cap |> C.reference in
    assert (not (P.Id.Capability.equal old_ref.id current_ref.id));
    let rebound =
      S.validate_disclosure ~current_capabilities:current restored |> protocol_ok
    in
    [%test_eq: string list]
      [ "read_file" ]
      (List.map (C.references rebound) ~f:(fun reference -> reference.name));
    List.iter
      [ C.select current ~names:[ "private_tool" ] |> cap
      ; registry ~resources:"revoked-resources" [ "read_file"; "private_tool" ] calls
      ]
      ~f:(fun revoked ->
        assert (
          Result.is_error (S.validate_disclosure ~current_capabilities:revoked restored)));
    let historical = P.Delivery.create delivery.context |> protocol_ok in
    assert (
      Result.is_error (S.validate_disclosure ~current_capabilities:current historical));
    let empty = P.Delivery.create ~disclosure_pins:[] delivery.context |> protocol_ok in
    let empty_scope =
      S.validate_disclosure ~current_capabilities:current empty |> protocol_ok
    in
    assert (List.is_empty (C.references empty_scope));
    assert (
      Result.is_error
        (P.Delivery.validate_transition ~previous:(Some historical) delivery));
    assert (
      Result.is_error
        (P.Delivery.validate_transition ~previous:(Some delivery) historical));
    let frame = notification_entry delivery in
    let committed =
      P.Delivery.commit ~track_wake:true delivery ~history_id:frame.id ~now:timestamp
      |> protocol_ok
    in
    List.iter [ delivery; committed; empty ] ~f:(fun value ->
      assert (
        P.Delivery.equal
          value
          (P.Delivery.of_json (P.Delivery.to_json value) |> protocol_ok)));
    let fields =
      match P.Delivery.to_json delivery with
      | `Object fields -> fields
      | _ -> assert false
    in
    let without =
      List.filter fields ~f:(fun (name, _) -> not (String.equal name "disclosure_pins"))
    in
    assert (Result.is_error (P.Delivery.of_json (`Object without)));
    let downgraded =
      `Object
        (List.map without ~f:(fun (name, value) ->
           name, if String.equal name "schema_version" then `Number "2" else value))
    in
    let downgraded = P.Delivery.of_json downgraded |> protocol_ok in
    assert (
      Result.is_error
        (P.Delivery.validate_transition ~previous:(Some delivery) downgraded));
    let invalid pins_json =
      `Object
        (List.map fields ~f:(fun (name, value) ->
           name, if String.equal name "disclosure_pins" then pins_json else value))
    in
    List.iter
      [ invalid (`Object [ "read_file", `String "not-a-pin" ])
      ; invalid
          (`Object
              [ "read_file", `String (snd (List.hd_exn pins))
              ; "read_file", `String (snd (List.hd_exn pins))
              ])
      ]
      ~f:(fun bad -> assert (Result.is_error (P.Delivery.of_json bad)));
    [%test_eq: int] 0 !calls;
    print_endline
      "exact read_file ceiling persisted and rebound across fresh IDs; extra tools \
       excluded";
    print_endline
      "revocation, absent metadata, downgrade and forged pins rejected; empty ceiling \
       stays empty; no execution");
  [%expect
    {|
    exact read_file ceiling persisted and rebound across fresh IDs; extra tools excluded
    revocation, absent metadata, downgrade and forged pins rejected; empty ceiling stays empty; no execution
    |}]
;;
