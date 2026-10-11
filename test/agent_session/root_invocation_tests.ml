open Core
open Fixtures
module S = Agent_session.Session_state
module D = Agent_session.Session_state_document
module Doc = Document_schema.Document
module Role = Chat_response.Script_contract_plan.Program.Role.Moderator_invocation

let replace_spec_field payload name value =
  match payload with
  | `Object fields ->
    `Object
      (List.map fields ~f:(fun (key, original) ->
         if not (String.equal key "spec")
         then key, original
         else (
           match original with
           | `Object spec ->
             let spec =
               List.filter spec ~f:(fun (key, _) -> not (String.equal key name))
             in
             ( key
             , `Object
                 (spec @ Option.to_list (Option.map value ~f:(fun value -> name, value)))
             )
           | _ -> assert false)))
  | _ -> assert false
;;

let%test_unit
    "root invocation is a retained native contract with explicit document migration"
  =
  with_actor_workspace (fun _env workspace_instance ->
    let initial =
      actor_state ~workspace_instance ~liveness:Process_bound ~start_immediately:false
    in
    List.iter [ Role.Ui; Headless ] ~f:(fun role ->
      let state = { initial with spec = { initial.spec with root_invocation = role } } in
      S.validate state |> protocol_ok;
      let document = D.encode (D.authored state) ~limits:document_limits |> document_ok in
      [%test_eq: int] 10 (Doc.version document);
      let with_future =
        Doc.create
          ~limits:document_limits
          ~kind:"session.state"
          ~version:10
          ~payload:
            (replace_spec_field
               (Doc.payload document)
               "future_root_metadata"
               (Some (`Object [ "exact", `String "retained" ])))
        |> document_ok
      in
      let carrier = D.decode ~limits:document_limits with_future |> document_ok in
      assert (Role.equal role (D.value carrier).spec.root_invocation);
      let roundtrip = D.encode carrier ~limits:document_limits |> document_ok in
      assert (Jsonaf.exactly_equal (Doc.payload with_future) (Doc.payload roundtrip));
      List.iter
        [ None; Some `Null; Some (`String "inferred"); Some (`Number "1") ]
        ~f:(fun malformed ->
          let malformed =
            Doc.create
              ~limits:document_limits
              ~kind:"session.state"
              ~version:10
              ~payload:
                (replace_spec_field (Doc.payload document) "root_invocation" malformed)
            |> document_ok
          in
          assert (Result.is_error (D.decode ~limits:document_limits malformed)));
      let legacy =
        Doc.create
          ~limits:document_limits
          ~kind:"session.state"
          ~version:9
          ~payload:(replace_spec_field (Doc.payload document) "root_invocation" None)
        |> document_ok
      in
      let migrated =
        D.upgrade legacy ~limits:document_limits
        |> document_ok
        |> D.decode ~limits:document_limits
        |> document_ok
        |> D.value
      in
      assert (Role.equal Ui migrated.spec.root_invocation);
      [%test_eq: int] 25 migrated.schema_version;
      let old_native = S.upgrade_schema { state with schema_version = 24 } in
      match role, old_native with
      | Ui, Ok restored -> assert (Role.equal Ui restored.spec.root_invocation)
      | Headless, Error error ->
        assert (Agent_protocol.Error.equal_code error.code Migration_required)
      | Ui, Error _ | Headless, Ok _ -> assert false))
;;
