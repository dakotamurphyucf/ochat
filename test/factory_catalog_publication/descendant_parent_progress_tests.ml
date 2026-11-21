open! Core
open Agent_server_test_support
open Retry_support
open Descendant_turn_adapter_tests
module Pending = A.Goal_descendant_pending
module Occurrence = A.Goal_descendant_occurrence
module Store = Agent_store.Session_store

let transaction env path =
  let scan =
    Eio.Path.load Eio.Path.(Eio.Stdenv.fs env / path)
    |> Agent_store.Journal_segment.scan_contents ~max_payload_length:(64 * 1024 * 1024)
    |> Source_preparation_tests.store_ok
  in
  match List.last scan.entries with
  | Some frame when Int.equal (Agent_store.Frame.flags frame.frame) 0 ->
    Some
      (Agent_store.Transaction.decode (Agent_store.Frame.payload frame.frame)
       |> Source_preparation_tests.store_ok)
  | Some _ | None -> None
;;

let reasons transaction =
  A.Session_delta_document.decode
    ~limits:Document_schema.Limits.default
    transaction.Agent_store.Transaction.delta
  |> document_ok
  |> A.Session_delta_document.value
  |> Child_usage_fault_tests.Fault.leaves
  |> List.filter_map ~f:(function
    | A.Session_delta.Goal_descendants_changed update -> Some (GD.Update.reason update)
    | _ -> None)
;;

let is_attempt_of occurrence child =
  P.Session_ref.equal (Occurrence.Frame.session (Occurrence.frame occurrence)) child
  &&
  match Occurrence.kind occurrence with
  | Attempt _ -> true
  | Host_turn _ -> false
;;

let is_child_input text inputs =
  List.exists inputs ~f:(fun item ->
    let json = Openai.Responses.Item.jsonaf_of_t item in
    match Document_schema.Json.field json ~name:"role" with
    | Value (`String "user") ->
      String.is_substring (Jsonaf.to_string json) ~substring:text
    | Value _ | Absent | Null -> false)
;;

let owns_input text request =
  let module Payload = History_entry.Payload in
  List.exists (Inference.Request.history request) ~f:(fun entry ->
    match Payload.Semantic.view (Payload.semantic (History_entry.payload entry)) with
    | Message { role = User; content; _ } ->
      List.exists content ~f:(function
        | Payload.Content.Text value -> String.equal text value.text
        | _ -> false)
    | _ -> false)
;;

type selected =
  { parent : P.Id.Session.t
  ; child : P.Session_ref.t
  ; journal : string
  ; delegation_record : string
  }

let%expect_test "ordinary parent progress closes the exact unstarted child Attempt" =
  let text = "741 ordinary managed provider request" in
  let queued = ref None in
  let selected = ref None in
  let armed = ref false in
  let closed = ref None in
  let first_reserve, signal_reserve = Eio.Promise.create () in
  let native_send, signal_native_send = Eio.Promise.create () in
  let child_preparing, signal_child_preparing = Eio.Promise.create () in
  let parent_running, signal_parent = Eio.Promise.create () in
  let finish_parent, release_parent = Eio.Promise.create () in
  let before_read, signal_reader = Eio.Promise.create () in
  let finish_read, release_reader = Eio.Promise.create () in
  let probe = ref None in
  let dispatches = ref 0 in
  let release resolver promise =
    if Option.is_none (Eio.Promise.peek promise) then Eio.Promise.resolve resolver ()
  in
  let capacity_source request =
    if owns_input text request
    then (
      release signal_child_preparing child_preparing;
      Eio.Promise.await parent_running);
    Inference_runtime.Context_capacity.Source.create
      ~encoder:Openai_responses_v1
      ~read:(fun () -> Incomplete Counter_unavailable)
  in
  let budget =
    P.Goal.Budget.create
      ~admitted_turns:None
      ~admitted_attempts:(Some 3L)
      ~observed_tokens:None
      ~deadline:None
    |> protocol_ok
  in
  Birth.with_native_goal_child
    ~timeout:180.
    ~budget
    ~capacity_source
    ~child_sources:Birth.attempt_sources
    ~parent_extra_tools:[ "agent_send" ]
    ~wrap_env:(fun env ->
      let native_env = env in
      let env =
        Crash_fault_io.wrap
          env
          ~boundary:After_sync
          ~matches:(fun path ->
            Option.is_none !closed
            && String.is_suffix path ~suffix:".log"
            && Option.exists !selected ~f:(fun target ->
              String.equal (Filename.dirname path) target.journal))
          ~reached:(fun path ->
            let target = Option.value_exn !selected in
            Option.iter (transaction native_env path) ~f:(fun transaction ->
              if P.Id.Session.equal transaction.session_id target.parent
              then
                List.iter (reasons transaction) ~f:(function
                  | Reserve { occurrence; _ }
                    when (not (Eio.Promise.is_resolved first_reserve))
                         && is_attempt_of occurrence target.child ->
                    armed := true;
                    Eio.Promise.resolve signal_reserve occurrence
                  | Close_before_writer pending
                    when Option.exists
                           (Eio.Promise.peek first_reserve)
                           ~f:(fun occurrence ->
                             Occurrence.equal (Pending.occurrence pending) occurrence) ->
                    closed := Some pending
                  | _ -> ())))
      in
      Crash_fault_io.wrap
        env
        ~boundary:Before_open_in
        ~matches:(fun path ->
          !armed
          && Option.exists !selected ~f:(fun target ->
            String.equal path target.delegation_record))
        ~reached:(fun _ ->
          armed := false;
          Eio.Promise.resolve signal_reader ();
          Eio.Promise.await finish_read))
    ~post_creation_stream:(fun inputs ->
      if is_child_input text inputs
      then (
        Int.incr dispatches;
        Birth.Provider_probe.before_submit (Option.value_exn !probe);
        completed_response "741 original managed child completed")
      else (
        match !queued with
        | Some arguments ->
          queued := None;
          Eio.Promise.resolve signal_native_send ();
          function_response "agent_send" arguments
        | None
          when Option.is_some !selected && not (Eio.Promise.is_resolved parent_running) ->
          fun () ->
            Eio.Promise.resolve signal_parent ();
            Eio.Promise.await finish_parent;
            completed_response "741 ordinary parent progress" ()
        | None -> completed_response "741 ordinary parent finished"))
    (fun env _sw daemon parent child scope reference _ ->
       Descendant_ancestry_support.start_legacy daemon child;
       let parent_id = (state parent).identity.session_id in
       let journal =
         Store.Handle.journal_directory (Option.value_exn parent.store_handle)
       in
       let child_owner =
         A.Goal_descendants.session (Option.value_exn (state child).goal_descendants)
       in
       let delegation_record =
         let filename =
           D.Key.sexp_of_t reference.key
           |> Sexp.to_string_mach
           |> Digestif.SHA256.digest_string
           |> Digestif.SHA256.to_hex
         in
         let root = Agent_store.Data_root.path (Store.data_root (Daemon.store daemon)) in
         Filename.concat (Filename.concat root "delegations") (filename ^ ".frame")
       in
       let root_before = branch parent scope reference in
       [%test_eq: int64] 0L (GD.Branch.confirmed_prefix root_before);
       let key = P.Idempotency_key.of_string "741-managed" |> protocol_ok in
       protect_cleanup
         ~finally:(fun () ->
           release release_parent finish_parent;
           release release_reader finish_read;
           selected := None)
         (fun () ->
            selected
            := Some
                 { parent = parent_id; child = child_owner; journal; delegation_record };
            queued
            := Some
                 (`Object
                     [ "session_id", P.Id.Session.to_json reference.child_session_id
                     ; "idempotency_key", P.Idempotency_key.to_json key
                     ; "message", `String text
                     ]);
            Goal.ordinary
              daemon
              parent
              (request parent "741-parent-progress" "Send once.")
            |> admitted
            |> ignore;
            (* The managed child can start preparing before the parent follows
               its published native result. Preserve both witnesses and bound
               each wait. The parent follow-up releases child preparation. *)
            within env "parent adapter selected native send" (fun () ->
              Eio.Promise.await native_send);
            within env "managed child reached capacity preparation" (fun () ->
              Eio.Promise.await child_preparing);
            within env "parent follow-up provider request" (fun () ->
              Eio.Promise.await parent_running);
            let first_occurrence =
              within env "child Attempt reservation fsync" (fun () ->
                Eio.Promise.await first_reserve)
            in
            within env "reserved child delegation read" (fun () ->
              Eio.Promise.await before_read);
            let first =
              within env "existing reserved parent branch" (fun () ->
                GD.Branch.pending (branch parent scope reference) |> Option.value_exn)
            in
            assert (Occurrence.equal (Pending.occurrence first) first_occurrence);
            let sent = managed child reference key in
            let operation =
              match sent.status with
              | Assigned operation -> operation
              | status -> raise_s [%sexp (status : M.status)]
            in
            [%test_eq: int64] 0L (marks child).attempt;
            let captured_parent_revision = (state parent).counters.revision in
            release release_parent finish_parent;
            within env "ordinary parent terminal publication" (fun () ->
              Goal_native_test_support.await_quiet parent)
            |> ignore;
            assert (Int64.(captured_parent_revision < (state parent).counters.revision));
            probe
            := Some
                 (Birth.Provider_probe.create
                    ~parent
                    ~child
                    ~scope
                    ~reference
                    ~parent_attempts_before:
                      (A.Goal_accounting.admitted_attempts
                         (Goal.accounting (state parent))));
            release release_reader finish_read;
            within env "ordinary managed child terminal" (fun () ->
              Goal_native_test_support.await_quiet child)
            |> ignore;
            assert (Pending.equal first (Option.value_exn !closed));
            let last = Birth.Provider_probe.included (branch parent scope reference) in
            assert (
              Int64.(
                succ (Pending.nonce first)
                = Pending.nonce (A.Goal_descendant_included.pending last)));
            Birth.Provider_probe.after_terminal (Option.value_exn !probe);
            assert (
              M.equal_status
                (Terminal (Some operation, Completed))
                (managed child reference key).status);
            [%test_eq: int] 1 !dispatches));
  print_endline
    "exact unstarted reservation closed; ordinary managed child used one attempt";
  [%expect
    {| exact unstarted reservation closed; ordinary managed child used one attempt |}]
;;
