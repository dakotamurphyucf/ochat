open Core
open Fixtures
module P = Agent_protocol
module G = P.Authoring_guidance
module Presence = Chat_response.Authoring_presence
module Policy = Chat_response.Authoring_policy
module State = Agent_session.Session_state
module A = Agent_session.Session_actor
module Codec = Agent_session.History_codec

let digest = Chatmd_shell_spec.Source_ref.digest

let manual () =
  let ceiling =
    Chat_response.Tool_capability.create
      ~owner:"authoring-history"
      ~resource_fingerprint:(digest "resources")
      []
    |> function
    | Ok value -> value
    | Error e -> raise_s [%sexp (e : Chat_response.Tool_capability.error)]
  in
  Policy.resolve ~policy:Manual ~ceiling ~selected_names:[] ()
  |> function
  | Ok value -> value
  | Error e -> raise_s [%sexp (e : Policy.error)]
;;

let context_identity = digest "runtime-language-target-capabilities"

let entry policy =
  let id =
    History_entry.Id.create ~namespace:"authoring-fixture" ~sequence:0
    |> Result.ok_or_failwith
  in
  let entry =
    Codec.user_text ~id "Installed ChatML guidance: use explicit calls and let*."
    |> Codec.to_protocol
  in
  let guidance =
    G.create
      ~context_identity
      ~policy_fingerprint:(Policy.fingerprint policy)
      ~purpose:Reference
      ~payload:entry.payload
      ~topics:
        [ { id = "chatml.tasks"
          ; document_sha256 = digest "tasks-v1"
          ; source = Installed (digest "installed-corpus")
          ; complete = true
          }
        ]
    |> protocol_ok
  in
  { entry with provenance = Runtime_authoring guidance }
;;

let report policy known effective =
  Presence.inspect ~policy ~context_identity ~known ~effective |> protocol_ok
;;

let statuses report =
  List.map report.Presence.observations ~f:(fun observation -> observation.presence)
;;

let restore state =
  State.sexp_of_t state
  |> Sexp.to_string_mach
  |> Agent_session.Session_persistence.restore_snapshot
  |> store_ok
;;

let%expect_test
    "guidance provenance survives projections and compaction archives without claiming \
     retained content"
  =
  let policy = manual () in
  let guidance_entry = entry policy in
  let initial (state : State.t) =
    { state with
      State.conversation =
        { state.conversation with canonical_history = [ guidance_entry ] }
    ; moderator =
        Some
          (Agent_session.Runtime_builder.encode_moderator_snapshot (handoff_snapshot 0))
    }
  in
  Job_fixtures.with_actor ~prepare_state:initial (fun _ _ actor writer backend ->
    let before = A.state actor |> protocol_ok in
    let restored = restore before in
    let projected state =
      let snapshot = State.snapshot ~now:timestamp state in
      P.Snapshot.to_json snapshot |> P.Snapshot.of_json |> protocol_ok
    in
    let known =
      Presence.remember ~previous:[] ~history:restored.conversation.canonical_history
      |> protocol_ok
    in
    let known =
      [%sexp_of: Presence.receipt list] known
      |> Sexp.to_string_mach
      |> Sexp.of_string
      |> [%of_sexp: Presence.receipt list]
    in
    let effective = (Option.value_exn (projected restored).effective_history).entries in
    assert (P.History.equal_entry guidance_entry (List.hd_exn effective));
    [%test_eq: Presence.presence list]
      [ Present ]
      (statuses (report policy known effective));
    let exported = Agent_session.Chatmd_export.render_protocol effective |> protocol_ok in
    assert (String.is_substring exported ~substring:"ochat-runtime-authoring");
    assert (String.is_substring exported ~substring:"chatml.tasks");
    (* An actual moderator overlay changes the effective occurrence's provenance,
       even if its replacement happens to repeat the same text. *)
    let snapshot = handoff_snapshot 0 in
    let replacement =
      Session.Moderator_state.Identity_snapshot.Replacement.
        { target_id = guidance_entry.id
        ; change_id = 0
        ; script_label = None
        ; value =
            guidance_entry.payload
            |> Chatml.Chatml_value_codec.import_json
            |> Session.Snapshot.of_value
            |> Result.ok_or_failwith
        }
    in
    let changed =
      { restored with
        moderator =
          Some
            (Agent_session.Runtime_builder.encode_moderator_snapshot
               { snapshot with
                 revision = 1
               ; next_change_id = 1
               ; replacements = [ replacement ]
               })
      }
    in
    let effective = (Option.value_exn (projected changed).effective_history).entries in
    [%test_eq: Presence.presence list]
      [ Modified ]
      (statuses (report policy known effective));
    assert (
      P.History.equal_entry
        guidance_entry
        (List.hd_exn changed.conversation.canonical_history));
    A.compact
      actor
      ~attachment_id:writer.id
      ~expected_revision:(Some before.counters.revision)
    |> protocol_ok
    |> ignore;
    let compacted = await_idle actor in
    let archive = List.hd_exn compacted.conversation.compaction_archives in
    let archived =
      Agent_session.Memory_backend.archived_state backend ~revision:archive.revision
      |> Option.value_exn
      |> restore
    in
    let index =
      Presence.remember ~previous:known ~history:archived.conversation.canonical_history
      |> protocol_ok
    in
    assert (List.equal Presence.equal_receipt known index);
    let compacted = restore compacted in
    let snapshot = projected compacted in
    let effective =
      Option.value_map
        snapshot.effective_history
        ~default:snapshot.canonical_history
        ~f:Fn.id
    in
    let absent = report policy index effective.entries in
    [%test_eq: Presence.presence list] [ Absent ] (statuses absent);
    assert ((not absent.refresh_primer) && List.is_empty absent.missing_preload);
    assert (Chatmd_shell_spec.Extension_spec.equal_policy (Policy.policy policy) Manual);
    let without_provenance =
      Codec.of_protocol guidance_entry |> protocol_ok |> Codec.to_protocol
    in
    [%test_eq: Presence.presence list]
      [ Modified ]
      (statuses (report policy known [ without_provenance ]));
    let preserved =
      Codec.all_to_protocol
        ~previous:[ guidance_entry ]
        [ Codec.of_protocol guidance_entry |> protocol_ok ]
    in
    [%test_eq: Presence.presence list]
      [ Present ]
      (statuses (report policy known preserved));
    print_s
      [%sexp
        "present; same-text overlay modified; archive remembered; compacted content \
         absent; manual unchanged"]);
  [%expect
    {| "present; same-text overlay modified; archive remembered; compacted content absent; manual unchanged" |}]
;;

let%expect_test "guidance requires schema 10 and rejects forged codec identities" =
  let policy = manual () in
  let value = entry policy in
  Job_fixtures.with_actor (fun _ _ actor _ _ ->
    let empty = A.state actor |> protocol_ok in
    let older = { empty with schema_version = 9 } in
    let upgraded = State.upgrade_schema older |> protocol_ok in
    assert_same_session_snapshot empty upgraded;
    let state =
      { empty with
        conversation = { empty.conversation with canonical_history = [ value ] }
      }
    in
    State.validate state |> protocol_ok;
    let restored = restore state in
    assert (
      P.History.equal_entry value (List.hd_exn restored.conversation.canonical_history));
    assert (Result.is_error (State.upgrade_schema { state with schema_version = 9 }));
    let guidance =
      match value.provenance with
      | Runtime_authoring guidance -> guidance
      | _ -> assert false
    in
    let fields =
      match G.to_json guidance with
      | `Object fields -> fields
      | _ -> assert false
    in
    assert (Result.is_error (G.of_json (`Object (fields @ [ "extra", `True ]))));
    assert (
      Result.is_error
        (G.of_json
           (`Object (List.Assoc.add fields ~equal:String.equal "version" (`Number "2")))));
    let forged =
      G.sexp_of_t guidance
      |> function
      | Sexp.List fields ->
        Sexp.List
          (List.map fields ~f:(function
             | Sexp.List [ Atom "version"; _ ] -> Sexp.List [ Atom "version"; Atom "2" ]
             | field -> field))
      | _ -> assert false
    in
    let forged = G.t_of_sexp forged in
    let invalid =
      { state with
        conversation =
          { state.conversation with
            canonical_history = [ { value with provenance = Runtime_authoring forged } ]
          }
      }
    in
    assert (Result.is_error (State.validate invalid));
    assert (
      Result.is_error
        (Agent_session.Session_persistence.restore_snapshot
           (State.sexp_of_t invalid |> Sexp.to_string_mach)));
    print_s
      [%sexp
        "schema 9 migrates empty; cannot smuggle provenance; future metadata fails closed"]);
  [%expect
    {| "schema 9 migrates empty; cannot smuggle provenance; future metadata fails closed" |}]
;;
