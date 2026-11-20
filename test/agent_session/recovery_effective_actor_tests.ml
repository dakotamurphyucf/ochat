open! Core
open Fixtures
open Recovery_window_actor_tests
module Effective_capture = A.Recovery_effective_capture
module Effective_ledger = A.Recovery_effective_ledger
module Effective_record = A.Recovery_effective_record
module Runtime = Inference_runtime

exception Stop_before_attempt

let effective_records (state : State.t) =
  Option.value_map state.recovery_effective ~default:[] ~f:Effective_ledger.records
;;

let set_effective_replacement actor (source : P.History.entry) ~change ~text =
  let snapshot = handoff_snapshot 0 in
  let value = A.History_codec.user_text ~id:source.id text |> History_entry.payload in
  let replacement =
    Session.Moderator_state.Identity_snapshot.Replacement.
      { target_id = source.id; change_id = change; value; script_label = None }
  in
  let snapshot =
    { snapshot with
      revision = change + 1
    ; next_change_id = change + 1
    ; replacements = [ replacement ]
    }
  in
  Actor.change_moderator
    actor
    (Some (A.Runtime_builder.encode_moderator_snapshot snapshot))
  |> protocol_ok
  |> ignore;
  snapshot
;;

module Source = A.Recovery_source_reader
module Effective_archive = A.Recovery_effective_archive

let journal_transactions journal =
  let scan = Store.Journal.scan journal |> store_ok in
  List.filter_map scan.entries ~f:(fun entry ->
    match Store.Frame.flags entry.frame with
    | 0 -> Some (Store.Transaction.decode (Store.Frame.payload entry.frame) |> store_ok)
    | _ -> None)
;;

let member_for record id =
  List.find_exn (Effective_record.members record) ~f:(fun member ->
    P.History.Id.equal id (Citation.occurrence (Effective_record.Member.citation member)))
;;

let read_effective ?allowance fixture record member ~current ~project =
  Source.read_effective
    ?allowance
    fixture.sessions
    ~handle:fixture.handle
    ~current
    ~max_frame_bytes:payload_limit
    ~source_revision:(Effective_record.source_revision record)
    (Effective_record.Member.citation member)
    ~project
;;

let archived_location record =
  match Effective_record.custody record with
  | Archived location -> location
  | Live | Lost ->
    failwith "an eligible preparation did not move into the verified archive"
;;

let prepend_before_archive actor (snapshot : Session.Moderator_state.Identity_snapshot.t) =
  let entry = history 90 "inserted after preparation" in
  let value = A.History_codec.of_protocol entry |> protocol_ok |> History_entry.payload in
  let item =
    Session.Moderator_state.Identity_snapshot.Inserted.
      { entry_id = entry.id
      ; change_id = snapshot.next_change_id
      ; value
      ; script_label = None
      }
  in
  let new_snapshot =
    { snapshot with
      revision = snapshot.revision + 1
    ; next_change_id = snapshot.next_change_id + 1
    ; prepended_items = [ item ]
    }
  in
  Actor.change_moderator
    actor
    (Some (A.Runtime_builder.encode_moderator_snapshot new_snapshot))
  |> protocol_ok
  |> ignore
;;

let check_rollover_source fixture (prestate : State.t) =
  let predecessor = scope prestate in
  let successor =
    Scope.create
      ~session:fixture.owner
      ~generation:prestate.identity.generation
      ~window:(P.Id.Context_window.create ())
    |> protocol_ok
  in
  let document = A.Session_state_document.authored prestate in
  let archive =
    Archive.reference_for
      document
      ~limits:document_limits
      ~kind:Rollover
      (P.Id.Operation.create ())
    |> protocol_ok
  in
  Archive.write
    ~env:fixture.env
    ~handle:fixture.handle
    ~max_payload_length:payload_limit
    archive
    document
  |> protocol_ok;
  let recovery_windows =
    A.Recovery_window_seal.prepare prestate ~predecessor ~successor ~archive ~retained:[]
    |> protocol_ok
  in
  let recovery_effective = Effective_capture.relocate prestate ~archive |> protocol_ok in
  let counters =
    { prestate.counters with revision = Int64.succ prestate.counters.revision }
  in
  let conversation =
    { prestate.conversation with
      canonical_history = []
    ; compaction_archives = [ archive ]
    }
  in
  let sealed =
    { prestate with
      counters
    ; conversation
    ; recovery_windows = Some recovery_windows
    ; recovery_effective
    }
  in
  let record = List.last_exn (effective_records sealed) in
  let target = (history 50 "").id in
  let member = member_for record target in
  assert (
    Effective_archive.equal_kind
      (Effective_archive.kind (archived_location record))
      Rollover);
  (match
     read_effective
       fixture
       record
       member
       ~current:(fun () -> Ok sealed)
       ~project:(fun row -> Ok row.id)
   with
   | Error failure -> failwith (Sexp.to_string_hum (Source.sexp_of_error failure))
   | Ok id -> assert (P.History.Id.equal id target));
  let expired =
    { sealed with
      recovery_windows = Some (Ledger.expire recovery_windows predecessor |> protocol_ok)
    }
  in
  let projected = ref false in
  let expire_during_projection () = Ok (if !projected then expired else sealed) in
  (match
     read_effective
       fixture
       record
       member
       ~current:expire_during_projection
       ~project:(fun row ->
         projected := true;
         Ok row.id)
   with
   | Error Expired -> assert !projected
   | Error _ | Ok _ -> failwith "expiry did not revoke the selected source");
  let file =
    Filename.concat
      (Store.Session_store.Handle.archive_directory fixture.handle)
      (Archive.filename archive)
  in
  Eio.Path.unlink Eio.Path.(Eio.Stdenv.fs fixture.env / file);
  match
    read_effective
      fixture
      record
      member
      ~current:(fun () -> Ok expired)
      ~project:(fun _ -> failwith "expired source reached projection")
  with
  | Error Expired -> ()
  | Error _ | Ok _ -> failwith "expiry did not precede physical archive access"
;;

let check_snapshot (state : State.t) =
  let module Codec = A.Session_state_document in
  let module Raw = Recovery_window_ledger_tests in
  let marker = P.Session_storage_semantics.recovery_effective_archive in
  let document =
    Codec.encode (Codec.authored state) ~limits:document_limits |> document_ok
  in
  assert (List.mem (D.Document.required_semantics document) marker ~equal:String.equal);
  let read = Raw.decode document |> document_ok |> Codec.value in
  let shelf state =
    Option.value_exn state.State.recovery_effective |> Effective_ledger.to_jsonaf
  in
  assert (D.Json.equal (shelf read) (shelf state));
  let missing_fence =
    Raw.update_object (D.Document.json document) ~field:"required_semantics" ~f:(function
      | `Array values ->
        `Array
          (List.filter values ~f:(function
             | `String value -> not (String.equal value marker)
             | _ -> true))
      | _ -> assert false)
    |> Raw.inspect
  in
  assert (Result.is_error (Raw.decode missing_fence))
;;

let check_mixed_allowance fixture (replayed : State.t) latest selected =
  let module Text = A.Recovery_item_text in
  let citation = Effective_record.Member.citation selected in
  let scope = Citation.scope citation in
  let window =
    ledger replayed
    |> Ledger.windows
    |> List.find_exn ~f:(fun window -> Scope.equal (Ledger.Window.scope window) scope)
  in
  let canonical =
    Ledger.Window.fold_members window ~init:None ~f:(fun found member ->
      let candidate = Ledger.Member.citation member in
      if P.History.Id.equal (Citation.occurrence candidate) (Citation.occurrence citation)
      then (
        match Ledger.Member.source member with
        | Archive _ -> Ok (Some candidate)
        | Live _ -> failwith "fixture canonical source did not use a physical archive")
      else Ok found)
    |> protocol_ok
    |> Option.value_exn
  in
  let read allowance selection =
    Text.resolve
      ~allowance
      fixture.sessions
      ~handle:fixture.handle
      ~current:(fun _ _ -> Ok replayed)
      ~max_frame_bytes:payload_limit
      ~max_payload_bytes:payload_limit
      selection
      ~view:Ordinary
  in
  let live_text = "live after compaction" in
  let live =
    membership replayed
    |> List.find_exn ~f:(fun member ->
      Ledger.Member.matches member (history 97 live_text))
  in
  (match Ledger.Member.source live with
   | Live _ -> ()
   | Archive _ -> failwith "the newly appended canonical source must still be live");
  let live = Text.Canonical (Ledger.Member.citation live) in
  let canonical_selection = Text.Canonical canonical in
  let effective =
    Text.Prepared { citation; source_revision = Effective_record.source_revision latest }
  in
  let expect_error reason result =
    assert (Option.equal Source.equal_error (Result.error result) (Some reason))
  in
  let admitted text result =
    let item = Result.ok result |> Option.value_exn in
    [%test_eq: string list]
      [ text ]
      (List.map (Text.parts item) ~f:(fun part -> part.text))
  in
  let original = "original canonical body" in
  let projected = "new version" in
  let exact_bytes =
    String.length live_text + (2 * String.length original) + String.length projected
  in
  let whole_call ~opens ~bytes f =
    Source.with_allowance
      fixture.sessions
      ~handle:fixture.handle
      ~max_source_opens:opens
      ~max_text_bytes:bytes
      ~f
    |> Result.ok
    |> Option.value_exn
  in
  whole_call ~opens:2 ~bytes:exact_bytes (fun allowance ->
    admitted live_text (read allowance live);
    admitted original (read allowance canonical_selection);
    admitted projected (read allowance effective);
    expect_error Budget_exhausted (read allowance canonical_selection);
    Ok ());
  whole_call ~opens:2 ~bytes:(exact_bytes - 1) (fun allowance ->
    admitted live_text (read allowance live);
    admitted original (read allowance canonical_selection);
    expect_error Budget_exhausted (read allowance effective);
    Ok ());
  let expired = whole_call ~opens:2 ~bytes:exact_bytes Result.return in
  expect_error Denied (read expired canonical_selection);
  let with_source_bytes max_source_bytes f =
    Source.with_allowance fixture.sessions ~handle:fixture.handle ~max_source_bytes ~f
  in
  let archive_read ?(max_frame_bytes = payload_limit) allowance =
    Source.read
      ~allowance
      fixture.sessions
      ~handle:fixture.handle
      ~current:(fun () -> Ok replayed)
      ~max_frame_bytes
      canonical
      ~project:Result.return
  in
  (* Find the exact physical read threshold without depending on the random
     archived IDs or duplicating the decoder's measured contributions. The
     second exact read must pay the same full source work again. *)
  let rec threshold lower upper =
    if lower >= upper
    then lower
    else (
      let midpoint = lower + ((upper - lower) / 2) in
      match with_source_bytes midpoint (archive_read ?max_frame_bytes:None) with
      | Ok _ -> threshold lower midpoint
      | Error Budget_exhausted -> threshold (midpoint + 1) upper
      | Error error -> failwith (Sexp.to_string_hum (Source.sexp_of_error error)))
  in
  let single = threshold 1 (32 * 1024 * 1024) in
  let borrowed =
    with_source_bytes (2 * single) (fun allowance ->
      Source.read
        ~allowance
        fixture.sessions
        ~handle:fixture.handle
        ~current:(fun () -> Ok replayed)
        ~max_frame_bytes:payload_limit
        canonical
        ~project:(fun row ->
          expect_error Denied (archive_read allowance);
          Ok row))
  in
  assert (Result.is_ok borrowed);
  let read_twice max_source_bytes expected =
    let result =
      with_source_bytes max_source_bytes (fun allowance ->
        assert (Result.is_ok (archive_read allowance));
        archive_read allowance)
    in
    match expected with
    | None -> assert (Result.is_ok result)
    | Some expected -> expect_error expected result
  in
  read_twice (2 * single) None;
  read_twice ((2 * single) - 1) (Some Budget_exhausted);
  expect_error Limit (with_source_bytes (2 * single) (archive_read ~max_frame_bytes:1));
  let checkpoint () = raise Eio.Time.Timeout in
  match
    Source.with_allowance
      fixture.sessions
      ~handle:fixture.handle
      ~checkpoint
      ~f:(fun allowance -> read allowance canonical_selection)
  with
  | exception Eio.Time.Timeout -> ()
  | _ -> failwith "the host deadline did not cancel the source read"
;;

let check_archived_source fixture (replayed : State.t) =
  let target = (history 50 "").id in
  let records = effective_records replayed in
  let first = List.hd_exn records in
  let live_member = member_for first target in
  let latest = List.last_exn records in
  let selected = member_for latest target in
  let current () = Ok replayed in
  let project row = Ok row in
  let expect reason = function
    | Error actual when Source.equal_error actual reason -> ()
    | Error actual -> failwith (Sexp.to_string_hum (Source.sexp_of_error actual))
    | Ok _ -> failwith "an unavailable archive yielded a source"
  in
  read_effective fixture first live_member ~current ~project |> expect Unavailable;
  let recovered =
    match read_effective fixture latest selected ~current ~project with
    | Ok row -> row
    | Error failure -> failwith (Sexp.to_string_hum (Source.sexp_of_error failure))
  in
  assert (
    String.is_substring (Jsonaf.to_string recovered.payload) ~substring:"new version");
  assert (P.History.equal_provenance recovered.provenance (Moderator_replaced target));
  let module Text = A.Recovery_item_text in
  let citation = Effective_record.Member.citation selected in
  let source_revision = Effective_record.source_revision latest in
  let item =
    Text.resolve
      fixture.sessions
      ~handle:fixture.handle
      ~current:(fun _ _ -> Ok replayed)
      ~max_frame_bytes:payload_limit
      ~max_payload_bytes:payload_limit
      (Prepared { citation; source_revision })
      ~view:Ordinary
    |> Result.ok
    |> Option.value_exn
  in
  [%test_eq: string list]
    [ "new version" ]
    (List.map (Text.parts item) ~f:(fun part -> part.text));
  Source.read_effective
    ~check_replaced_source:(fun original ->
      assert (P.History.Id.equal original.id target);
      assert (
        String.is_substring
          (Jsonaf.to_string original.payload)
          ~substring:"original canonical body");
      Error Source.Unavailable)
    fixture.sessions
    ~handle:fixture.handle
    ~current
    ~max_frame_bytes:payload_limit
    ~source_revision
    citation
    ~project:(fun _ -> failwith "denied original reached projection")
  |> expect Unavailable;
  check_mixed_allowance fixture replayed latest selected;
  let module Query = Recovery_history_query_tests in
  let ordinal = Effective_record.Member.request_ordinal selected in
  let query =
    Query.make
      fixture
      ~family:Effective
      ~ordinals:(ordinal, ordinal + 1)
      ~scope:(Window (Citation.scope citation))
      (Query.search "new version")
  in
  let snippets, coverage = Query.all_snippets fixture query ~current in
  [%test_eq: string list] snippets [ "new version" ];
  [%test_eq: string] coverage "source_unavailable";
  let location = archived_location latest in
  let position =
    Effective_archive.find
      location
      ~request_ordinal:(Effective_record.Member.request_ordinal selected)
    |> Option.value_exn
  in
  [%test_eq: int]
    (Effective_record.Member.effective_ordinal selected + 1)
    (Effective_archive.Position.effective_ordinal position);
  let suppressed =
    let checkpoint = Option.value_exn replayed.moderator in
    let snapshot =
      A.Moderator_checkpoint.decode (Some checkpoint) |> protocol_ok |> Option.value_exn
    in
    let tombstone =
      Session.Moderator_state.Identity_snapshot.Tombstone.
        { target_id = target; change_id = snapshot.next_change_id }
    in
    let snapshot =
      { snapshot with
        revision = snapshot.revision + 1
      ; next_change_id = snapshot.next_change_id + 1
      ; replacements = []
      ; tombstones = [ tombstone ]
      }
    in
    { replayed with
      moderator = Some (A.Runtime_builder.encode_moderator_snapshot snapshot)
    }
  in
  let current_after_projection =
    let projected = ref false in
    let current () = Ok (if !projected then suppressed else replayed) in
    let project row =
      projected := true;
      Ok row
    in
    read_effective fixture latest selected ~current ~project |> expect Withdrawn;
    !projected
  in
  assert current_after_projection;
  let reference =
    List.find_exn replayed.conversation.compaction_archives ~f:(fun reference ->
      P.Id.Operation.equal reference.operation_id (Effective_archive.operation location))
  in
  let path_for reference =
    Filename.concat
      (Store.Session_store.Handle.archive_directory fixture.handle)
      (Archive.filename reference)
  in
  let path = path_for reference in
  let adjacent = List.hd_exn replayed.conversation.compaction_archives in
  assert (not (reference_eq reference adjacent));
  let replacement =
    Eio.Path.load Eio.Path.(Eio.Stdenv.fs fixture.env / path_for adjacent)
  in
  let file = Eio.Path.(Eio.Stdenv.fs fixture.env / path) in
  let saved = Eio.Path.(Eio.Stdenv.fs fixture.env / (path ^ ".fixture-backup")) in
  Eio.Path.rename file saved;
  Exn.protect
    ~finally:(fun () -> Eio.Path.rename saved file)
    ~f:(fun () ->
      Source.with_allowance
        ~max_source_opens:2
        fixture.sessions
        ~handle:fixture.handle
        ~f:(fun allowance ->
          let read () =
            read_effective ~allowance fixture latest selected ~current ~project
          in
          read () |> expect Missing;
          read_effective
            ~allowance
            fixture
            latest
            selected
            ~current:(fun () -> Ok suppressed)
            ~project:(fun _ -> failwith "withdrawn source reached projection")
          |> expect Withdrawn;
          Eio.Path.save ~create:(`Exclusive 0o600) file replacement;
          read () |> expect Corrupt;
          read () |> expect Budget_exhausted;
          Ok ())
      |> Result.ok
      |> Option.value_exn)
;;

let%test_unit "actual root preparation retains only exact neutral sources and replays" =
  with_actor_workspace (fun env workspace_instance ->
    Eio.Switch.run (fun sw ->
      let post_stream ~sw:_ ~inputs:_ = failwith "preparation reached the provider" in
      let prepared_models = ref [] in
      let on_prepare request =
        if not (List.is_empty (Inference.Request.history request))
        then
          prepared_models
          := Inference.Request.Target.model (Inference.Request.target request)
             :: !prepared_models
      in
      let runtime_fixture =
        Inference_fixture.create_with_observer
          ~on_prepare
          ~namespace:"recovery-preparation"
          ~default_model:"model"
          ~post_stream
      in
      let runtime_ok result =
        result
        |> Result.map_error ~f:(fun e ->
          Sexp.to_string_hum (Runtime.Preparation_error.sexp_of_t e))
        |> Result.ok_or_failwith
      in
      let selected =
        Inference_fixture.capture_config runtime_fixture Chat_response.Config.default
        |> runtime_ok
      in
      let previous = Inference_fixture.resolve runtime_fixture selected |> runtime_ok in
      let inference_ok result =
        result
        |> Result.map_error ~f:(fun e ->
          Sexp.to_string_hum (Inference.Request.Error.sexp_of_t e))
        |> Result.ok_or_failwith
      in
      let selection =
        Inference.Selection.captured selected ~limits:document_limits |> inference_ok
      in
      let state_before =
        actor_state ~workspace_instance ~liveness:Detached ~start_immediately:false
      in
      let state_before =
        { state_before with
          spec = { state_before.spec with inference_target = selection }
        }
      in
      let storage = Job_artifact_fixtures.create env sw state_before in
      let owner =
        P.Session_ref.create
          ~server_id:(Store.Session_store.server_id storage.sessions)
          ~session_id
      in
      let fixture : fixture =
        { env; sessions = storage.sessions; handle = storage.session; owner }
      in
      let initial = Persistence.Restored.authored state_before in
      let active_journal = journal fixture Store.Journal.create in
      let write_archive =
        Archive.write_document
          ~env
          ~handle:fixture.handle
          ~max_payload_length:payload_limit
      in
      let fail_next_archive = ref false in
      let orphan = ref None in
      let archive reference document =
        let%bind.Result () = write_archive reference document in
        if !fail_next_archive
        then (
          fail_next_archive := false;
          orphan := Some reference;
          Error
            (P.Error.create
               Persistence_error
               ~message:"effective archive synced before the fixture stopped publication"
               ~retryable:false
               ()))
        else Ok ()
      in
      let after =
        Eio.Switch.run (fun sw ->
          with_actor
            fixture
            sw
            active_journal
            ~restored:initial
            ~previous_transaction_hash:None
            ~archive
            (fun actor _ ->
               let writer = attach actor in
               let policy =
                 A.Configuration_policy.
                   { select_profile =
                       (fun ~current:_ ~profile:_ ->
                         Error (P.Error.invalid_request "fixture has one target"))
                   ; approve = (fun ~current:_ ~proposed:_ -> Ok ())
                   ; resolve =
                       (fun target ->
                         Inference_fixture.resolve runtime_fixture target
                         |> Result.map_error ~f:(fun _ ->
                           P.Error.invalid_request "unavailable fixture target"))
                   }
               in
               Actor.set_configuration_policy actor policy |> protocol_ok;
               let canonical = history 50 "original canonical body" in
               let synthetic = history 51 "synthetic body must not survive" in
               let snapshot = ref None in
               let mismatch = ref false in
               let invalid_target = ref false in
               let cutover = ref false in
               let new_model = "model-after-cutover" in
               let switch_model () =
                 let module C = P.Session_configuration in
                 let request =
                   C.Update_request.
                     { session_id
                     ; attachment_id = writer
                     ; expected_generation = 0
                     ; expected_revision = 0L
                     ; patch =
                         C.Patch.create ~model:new_model ~settings:[] () |> protocol_ok
                     ; idempotency_key =
                         P.Idempotency_key.of_string "recovery-cutover" |> protocol_ok
                     }
                 in
                 let changed = Actor.update_configuration actor request |> protocol_ok in
                 assert changed.pending
               in
               let repeats = ref 1 in
               let allocated = ref 0 in
               let identity =
                 { (Inference_fixture.identity runtime_fixture) with
                   with_attempt =
                     (fun _ ~relation:_ ~f:_ ->
                       incr allocated;
                       raise Stop_before_attempt)
                 }
               in
               let worker =
                 A.Operation_worker.create ~run:(fun ~sw ~input caps ->
                   let history =
                     Option.value_map !snapshot ~default:input.history ~f:(fun snapshot ->
                       Chat_response.Moderator_manager.effective_entries_of_snapshot
                         snapshot
                         input.history
                       |> Result.map_error ~f:(fun _ -> "invalid moderator fixture")
                       |> Result.ok_or_failwith
                       |> List.map ~f:(fun row ->
                         row.Chat_response.Moderation.Effective_entry.entry))
                   in
                   let history =
                     if !mismatch
                     then
                       (A.History_codec.of_protocol synthetic |> protocol_ok)
                       :: List.map history ~f:(fun entry ->
                         if P.History.Id.equal (History_entry.id entry) canonical.id
                         then
                           A.History_codec.user_text
                             ~id:canonical.id
                             "changed bytes under the same occurrence"
                         else entry)
                     else history
                   in
                   let root = Option.value_exn caps.root_context in
                   for _ = 1 to !repeats do
                     try
                       root.with_context
                         ~previous
                         ~history
                         (fun context ~on_prepared ~on_dispatch:_ ->
                            let target = Runtime.Context.target context in
                            if !cutover
                            then (
                              cutover := false;
                              switch_model ();
                              assert (
                                String.equal
                                  (Inference.Request.Target.model target)
                                  "model"));
                            let target =
                              if !invalid_target
                              then
                                Inference.Request.Target.with_model
                                  target
                                  ~model:"different"
                                  ~limits:document_limits
                                |> inference_ok
                              else target
                            in
                            let request =
                              Inference.Request.create
                                ~target
                                ~history
                                ~tools:[]
                                ~assets:[]
                                ~limits:Transcript.Admission.default
                              |> inference_ok
                            in
                            match
                              Inference_client.run
                                context
                                ~sw
                                ~identity
                                ~relation:Root
                                ~request
                                ~before_dispatch:on_prepared
                                ~on_attempt:(fun _ -> assert false)
                                ~on_completion:(fun _ -> assert false)
                                ~on_event:ignore
                                ~on_observation:ignore
                            with
                            | Error (Preparation Target_mismatch) when !invalid_target ->
                              ()
                            | Ok _ | Error _ -> assert false)
                     with
                     | Stop_before_attempt -> ()
                   done;
                   Completed
                     { final_history = input.history
                     ; runtime_requests = []
                     ; moderator_snapshot = None
                     })
               in
               Actor.set_operation_worker actor (Some worker) |> protocol_ok;
               Actor.start actor ~attachment_id:writer |> protocol_ok |> ignore;
               Actor.append_history actor ~attachment_id:writer [ canonical ]
               |> protocol_ok
               |> ignore;
               let turn sequence =
                 Actor.submit_message
                   ~submitting_principal:principal_id
                   actor
                   ~attachment_id:writer
                   (history sequence "turn request")
                 |> protocol_ok
                 |> ignore;
                 Eio.Time.with_timeout_exn (Eio.Stdenv.clock env) 5. (fun () ->
                   await_idle actor)
               in
               let before = turn 52 in
               assert (Option.is_none before.recovery_effective);
               assert (Int.equal !allocated 1);
               let enabled =
                 Actor.enable_recovery_windows
                   actor
                   ~actor:(actor_proof ())
                   ~attachment_id:writer
                   ~authorize:(fun _ -> Ok ())
                 |> protocol_ok
               in
               let overlay_text = "moderator exact projection body" in
               snapshot
               := Some
                    (set_effective_replacement
                       actor
                       canonical
                       ~change:0
                       ~text:overlay_text);
               cutover := true;
               let prepared = turn 53 in
               [%test_eq: string list] [ "model"; "model" ] (List.rev !prepared_models);
               let first = List.hd_exn (effective_records prepared) in
               assert (Scope.equal enabled (Effective_record.scope first));
               let selected_member =
                 List.find_exn (Effective_record.members first) ~f:(fun member ->
                   P.History.Id.equal
                     canonical.id
                     (Citation.occurrence (Effective_record.Member.citation member)))
               in
               let citation = Effective_record.Member.citation selected_member in
               (match Citation.version citation with
                | Effective { origin = Replaced { change_id = 0; _ }; _ } -> ()
                | _ -> assert false);
               let resolved state =
                 Effective_capture.resolve_live
                   state
                   ~source_revision:(Effective_record.source_revision first)
                   citation
                 |> protocol_ok
               in
               let selected = resolved prepared |> Option.value_exn in
               assert (
                 String.is_substring
                   (Jsonaf.to_string selected.payload)
                   ~substring:overlay_text);
               assert (
                 P.History.equal_provenance
                   selected.provenance
                   (Moderator_replaced canonical.id));
               assert (
                 not
                   (String.is_substring
                      (Jsonaf.to_string (Effective_record.to_jsonaf first))
                      ~substring:overlay_text));
               mismatch := true;
               let mismatched_state = turn 54 in
               let records = effective_records mismatched_state in
               [%test_eq: int] 2 (List.length records);
               let latest = List.last_exn records in
               let members = Effective_record.members latest in
               let omitted id =
                 not
                   (List.exists members ~f:(fun member ->
                      P.History.Id.equal
                        id
                        (Citation.occurrence (Effective_record.Member.citation member))))
               in
               assert (omitted canonical.id && omitted synthetic.id);
               assert (Effective_record.input_count latest > List.length members);
               assert (
                 List.for_all members ~f:(fun m ->
                   Effective_record.Member.request_ordinal m > 0));
               assert (Option.is_some (resolved mismatched_state));
               snapshot
               := Some
                    (set_effective_replacement
                       actor
                       canonical
                       ~change:1
                       ~text:"new version");
               invalid_target := true;
               let after = turn 55 in
               [%test_eq: int] 2 (List.length (effective_records after));
               assert (Option.is_none (resolved after));
               [%test_eq: string list]
                 [ "model"; "model"; new_model ]
                 (List.rev !prepared_models);
               [%test_eq: int] 3 !allocated;
               invalid_target := false;
               mismatch := false;
               repeats := 64;
               let saturated = turn 56 in
               [%test_eq: int] 67 (List.length !prepared_models);
               [%test_eq: int] 67 !allocated;
               let records = effective_records saturated in
               [%test_eq: int] 64 (List.length records);
               let module Query = Recovery_history_query_tests in
               let latest = List.last_exn records in
               let ordinal =
                 Effective_record.Member.request_ordinal (member_for latest canonical.id)
               in
               let query =
                 Query.make
                   fixture
                   ~family:Effective
                   ~ordinals:(ordinal, ordinal + 1)
                   ~scope:(Window (scope saturated))
                   (Query.search "new version")
               in
               let snippets, coverage =
                 Query.all_snippets fixture query ~current:(fun () -> Ok saturated)
               in
               [%test_eq: string list] snippets [ "new version" ];
               [%test_eq: string] coverage "source_unavailable";
               assert (
                 Effective_record.to_jsonaf (List.hd_exn records)
                 |> D.Json.equal (Effective_record.to_jsonaf first));
               prepend_before_archive actor (Option.value_exn !snapshot);
               check_rollover_source fixture (state actor);
               Inference_ports.install_compaction_runtime actor |> protocol_ok;
               fail_next_archive := true;
               let unchanged = compact fixture actor writer in
               let orphan = Option.value_exn !orphan in
               let written = read_archive fixture orphan |> protocol_ok in
               let retained state = Option.value_exn state.State.recovery_effective in
               assert (
                 D.Json.equal
                   (Effective_ledger.to_jsonaf (retained unchanged))
                   (Effective_ledger.to_jsonaf (retained saturated)));
               assert (
                 D.Json.equal
                   (Effective_ledger.to_jsonaf (retained written))
                   (Effective_ledger.to_jsonaf (retained saturated)));
               assert (List.is_empty unchanged.conversation.compaction_archives);
               let compacted = compact fixture actor writer in
               let terminal =
                 List.find_exn
                   (journal_transactions active_journal)
                   ~f:(fun transaction ->
                     Int64.equal
                       transaction.transaction_sequence
                       compacted.counters.transaction_sequence)
               in
               assert (
                 List.mem
                   (delta_kinds terminal)
                   "recovery_summary_compacted"
                   ~equal:String.equal);
               assert (
                 List.mem
                   (D.Document.required_semantics terminal.delta)
                   P.Session_storage_semantics.recovery_effective_archive
                   ~equal:String.equal);
               let compacted_again = compact fixture actor writer in
               [%test_eq: int]
                 2
                 (List.length compacted_again.conversation.compaction_archives);
               assert (
                 D.Json.equal
                   (Effective_ledger.to_jsonaf (retained compacted))
                   (Effective_ledger.to_jsonaf (retained compacted_again)));
               Actor.append_history
                 actor
                 ~attachment_id:writer
                 [ history 97 "live after compaction" ]
               |> protocol_ok
               |> ignore;
               state actor))
      in
      let replay_journal = journal fixture Store.Journal.open_existing in
      let prepared_deltas =
        journal_transactions replay_journal
        |> List.filter ~f:(fun transaction ->
          List.mem
            (delta_kinds transaction)
            "recovery_effective_prepared"
            ~equal:String.equal)
      in
      [%test_eq: int] 64 (List.length prepared_deltas);
      assert (
        List.for_all prepared_deltas ~f:(fun transaction ->
          List.mem
            (D.Document.required_semantics transaction.delta)
            P.Session_storage_semantics.recovery_effective
            ~equal:String.equal));
      let recovered =
        Store.Recovery.load
          ~env
          ~journal:replay_journal
          ~snapshot_directory:
            (Store.Session_store.Handle.snapshot_directory fixture.handle)
          ~max_snapshot_payload_length:payload_limit
          ~session_id
          ~initial
          ~restore_snapshot:(Persistence.restore_snapshot ~limits:document_limits)
          ~apply:(Persistence.apply_transaction ~limits:document_limits)
          ~validate_transaction:(Persistence.validate_transaction ~limits:document_limits)
          ~validate:Persistence.validate
        |> store_ok
      in
      let shelf (state : State.t) =
        Option.value_exn state.recovery_effective |> Effective_ledger.to_jsonaf
      in
      let replayed = Persistence.Restored.state recovered.state in
      assert (D.Json.equal (shelf after) (shelf replayed));
      check_snapshot replayed;
      check_archived_source fixture replayed))
;;

let%test_unit
    "effective coordinate projection defers unrelated canonical payload decoding"
  =
  let module Projection = A.Recovery_effective_projection in
  let oversized =
    { (history 60 "") with payload = `String (String.make (100 * 1024) 'x') }
  in
  let visible = history 61 "eligible exact neutral row" in
  let create history =
    Projection.create ~source_revision:1L ~overlay_incarnation:0L ~history ~moderator:None
    |> protocol_ok
  in
  let projection = create [ oversized; visible ] |> Option.value_exn in
  let row id = Projection.find projection id |> Option.value_exn in
  let no_version, remaining_bytes =
    Projection.Row.version
      (row oversized.id)
      ~remaining_bytes:Runtime.Recovery_input.max_projection_bytes
  in
  assert (Option.is_none no_version);
  let version, _ = Projection.Row.version (row visible.id) ~remaining_bytes in
  assert (Option.is_some version);
  assert (
    Option.is_none
      (create
         (List.init (A.Recovery_window_record.max_members + 1) ~f:(fun _ -> visible))))
;;

let%test_unit "single-item recovery discloses only the selected semantic view" =
  let module Text = A.Recovery_item_text in
  let module Payload = History_entry.Payload in
  let message role text : Payload.Semantic.view =
    Message
      { form = Input
      ; role
      ; content =
          [ Text
              { text; annotations = [ `String "private annotation" ]; logprobs = Absent }
          ]
      ; phase = Absent
      }
  in
  let cases : (Payload.Semantic.view * (Text.view * string) option) list =
    [ message Developer "developer text", Some (Selected_developer, "developer text")
    ; ( Result { kind = Function; relation = Unresolved; output = Text "tool result" }
      , Some (Selected_tool_result, "tool result") )
    ; message Assistant "answer", Some (Ordinary, "answer")
    ; message User "<system-reminder>host instruction", None
    ; message System "system instruction", None
    ; Reasoning { readable_summary = [ "reasoning summary" ] }, None
    ; ( Call
          { kind = Function
          ; name = "tool"
          ; namespace = Absent
          ; input_bytes = "private argument"
          ; async = Absent
          }
      , None )
    ]
  in
  with_actor_workspace (fun env workspace_instance ->
    Eio.Switch.run (fun sw ->
      let rows =
        List.mapi cases ~f:(fun i (semantic, _) ->
          let semantic =
            Payload.Semantic.create semantic ~metadata:Payload.Metadata.empty
            |> Result.ok_or_failwith
          in
          Payload.captured
            semantic
            ~origin:Payload.Origin.unavailable
            ~raw:(`Object [ "private", `String "raw provider data" ])
          |> Result.ok_or_failwith
          |> History_entry.create_with_id ~id:(history i "").id
          |> A.History_codec.to_protocol)
      in
      let base =
        actor_state ~workspace_instance ~liveness:Detached ~start_immediately:false
      in
      let base =
        { base with
          conversation =
            { base.conversation with
              canonical_history = rows
            ; next_history_sequence = Int64.of_int (List.length rows)
            ; reserved_history_through = Int64.of_int (List.length rows)
            }
        }
      in
      let storage = Job_artifact_fixtures.create env sw base in
      let owner =
        P.Session_ref.create
          ~server_id:(Store.Session_store.server_id storage.sessions)
          ~session_id
      in
      let ledger =
        A.Recovery_window_capture.bootstrap
          base
          ~owner
          ~window:(P.Id.Context_window.create ())
        |> protocol_ok
      in
      let source = { base with recovery_windows = Some ledger } in
      List.iter2_exn (membership source) cases ~f:(fun member (_, expected) ->
        let citation = Ledger.Member.citation member in
        let resolve view ~current =
          Text.resolve
            storage.sessions
            ~handle:storage.session
            ~current
            ~max_frame_bytes:payload_limit
            ~max_payload_bytes:payload_limit
            (Canonical citation)
            ~view
        in
        List.iter
          [ Text.Ordinary; Selected_developer; Selected_tool_result ]
          ~f:(fun view ->
            let current selection requested =
              (match selection with
               | Text.Canonical selected -> assert (Citation.equal selected citation)
               | Prepared _ -> assert false);
              assert (Text.equal_view requested view);
              Ok source
            in
            match expected, resolve view ~current with
            | Some (allowed, content), Ok item when Text.equal_view allowed view ->
              [%test_eq: string list]
                [ content ]
                (List.map (Text.parts item) ~f:(fun p -> p.text))
            | None, Error Source.Unavailable -> ()
            | Some (allowed, _), Error Source.Unavailable
              when not (Text.equal_view allowed view) -> ()
            | _ ->
              failwith "recovery exposed an ineligible part or missed an eligible one");
        Option.iter expected ~f:(fun (view, _) ->
          let calls = ref 0 in
          let current _ _ =
            Int.incr calls;
            if !calls >= 3
            then Error (P.Error.invalid_request "authority withdrawn")
            else Ok source
          in
          match resolve view ~current with
          | Error Denied -> ()
          | _ -> failwith "a revoked selection escaped at release"))))
;;
