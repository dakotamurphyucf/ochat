open! Core
open Fixtures
module A = Agent_session
module P = Agent_protocol
module D = Document_schema

let limits = document_limits

let set json name value =
  match json with
  | `Object fields -> `Object (List.Assoc.add fields ~equal:String.equal name value)
  | _ -> failwith "expected fixture object"
;;

let remove json name =
  match json with
  | `Object fields -> `Object (List.Assoc.remove fields ~equal:String.equal name)
  | _ -> failwith "expected fixture object"
;;

let field json name =
  match D.Json.field json ~name with
  | Value value -> value
  | Absent | Null -> failwith "missing fixture field"
;;

let get = function
  | Ok value -> value
  | Error (error : P.Error.t) -> failwith error.message
;;

let encode document = A.Session_state_document.encode document ~limits |> document_ok

let decode payload =
  D.Document.create ~limits ~kind:"session.state" ~version:9 ~payload
  |> document_ok
  |> A.Session_state_document.decode ~limits
  |> document_ok
;;

let unrelated_write document =
  let before = A.Session_state_document.value document in
  A.Session_state_document.with_value document { before with stop_epoch = 1L }
  |> encode
  |> D.Document.payload
;;

let%expect_test "unrelated state writes preserve absent and explicit null run custody" =
  with_actor_workspace (fun _ workspace_instance ->
    let initial =
      actor_state ~workspace_instance ~liveness:Detached ~start_immediately:false
    in
    let payload =
      A.Session_state_document.authored initial |> encode |> D.Document.payload
    in
    let historical_payload =
      let conversation = field payload "conversation" in
      let conversation =
        remove (remove conversation "pending_revision") "pending_dispositions"
      in
      let deferred =
        match field conversation "deferred_user_entries" with
        | `Array entries ->
          `Array (List.map entries ~f:(fun entry -> field entry "entry"))
        | _ -> assert false
      in
      set payload "conversation" (set conversation "deferred_user_entries" deferred)
    in
    (* Capture the exact historical frames before conversion. The v7 payload has
       no run custody; conversion and unrelated writes cannot invent that field. *)
    let original_frames_unchanged =
      List.for_all [ 7; 8 ] ~f:(fun version ->
        List.for_all
          [ remove historical_payload "run_state"
          ; set historical_payload "run_state" `Null
          ]
          ~f:(fun payload ->
            let payload = set payload "future_run_capture" (`String "legacy-kept") in
            let original =
              D.Document.create ~limits ~kind:"session.state" ~version ~payload
              |> document_ok
            in
            let bytes = D.Document.to_string original in
            let rewritten =
              A.Session_state_document.decode ~limits original
              |> document_ok
              |> unrelated_write
            in
            let presence_preserved =
              match
                ( D.Json.field payload ~name:"run_state"
                , D.Json.field rewritten ~name:"run_state" )
              with
              | Absent, Absent | Null, Null -> true
              | (Absent | Null | Value _), (Absent | Null | Value _) -> false
            in
            presence_preserved
            && String.equal bytes (D.Document.to_string original)
            && D.Json.equal (field rewritten "future_run_capture") (`String "legacy-kept")))
    in
    let absent =
      payload |> fun payload -> remove payload "run_state" |> decode |> unrelated_write
    in
    let explicit_null = set payload "run_state" `Null |> decode |> unrelated_write in
    print_s
      [%sexp
        { original_frames_unchanged : bool
        ; absent_preserved =
            ((match D.Json.field absent ~name:"run_state" with
              | Absent -> true
              | Null | Value _ -> false)
             : bool)
        ; null_preserved =
            ((match D.Json.field explicit_null ~name:"run_state" with
              | Null -> true
              | Absent | Value _ -> false)
             : bool)
        }]);
  [%expect
    {|
    ((original_frames_unchanged true) (absent_preserved true)
     (null_preserved true))
    |}]
;;

let observer = { P.Invocation.script_id = "workflow"; source_sha256 = String.make 64 'a' }

let installed =
  A.Run_source_installation.apply
    A.Run_source_installation.initial
    ~change:(Replace observer)
  |> get
;;

let index =
  A.Run_state.replace_installation
    A.Run_state.empty
    ~installation:installed
    ~retired_runs:[]
  |> get
;;

let%expect_test "unrelated writes preserve future run and nested source metadata" =
  with_actor_workspace (fun _ workspace_instance ->
    let initial =
      actor_state ~workspace_instance ~liveness:Detached ~start_immediately:false
    in
    let source =
      A.Run_source_installation.captured installed ~generation:initial.identity.generation
      |> get
    in
    let run =
      P.Run.create
        ~id:(P.Id.Run.of_string "run_custody_fixture" |> get)
        ~session:
          (P.Session_ref.create
             ~server_id:(P.Id.Server.of_string "srv_fixture" |> get)
             ~session_id:initial.identity.session_id)
        ~principal_id:(P.Id.Principal.of_string "pri_fixture" |> get)
        ~source
        ~mode:Workflow
        ~lifecycle:Admitted
        ~revision:0L
        ~owned_work:[]
        ~relinquished_work:[]
        ~terminal_work:[]
        ~created_at:initial.identity.created_at
        ~updated_at:initial.identity.updated_at
      |> get
    in
    let receipt =
      P.Run_receipt.create
        ~run_id:run.id
        ~principal_id:run.principal_id
        ~source
        ~key:(P.Idempotency_key.of_string "admission" |> get)
        ~request_sha256:(String.make 64 'b')
        ~kind:Admission
        ~run_revision:0L
        ~session_revision:initial.counters.revision
        ~committed_at:initial.identity.updated_at
      |> get
    in
    let index = A.Run_state.commit index ~run ~receipt ~intent:None |> get in
    let payload =
      A.Session_state_document.authored { initial with run_state = Some index }
      |> encode
      |> D.Document.payload
    in
    let run_state = field payload "run_state" in
    let runs =
      match field run_state "runs" with
      | `Array [ run ] ->
        `Array
          [ set
              (set run "future_run" (`String "retained"))
              "source"
              (set (field run "source") "future_source" (`String "retained"))
          ]
      | _ -> failwith "expected one run"
    in
    let payload =
      set payload "run_state" (set run_state "runs" runs) |> decode |> unrelated_write
    in
    let run =
      match field (field payload "run_state") "runs" with
      | `Array [ run ] -> run
      | _ -> failwith "expected run"
    in
    print_s
      [%sexp
        { run_metadata = (Jsonaf.to_string (field run "future_run") : string)
        ; source_metadata =
            (Jsonaf.to_string (field (field run "source") "future_source") : string)
        }]);
  [%expect {| ((run_metadata "\"retained\"") (source_metadata "\"retained\"")) |}]
;;

let%expect_test "run evidence changes notify existing session subscribers once" =
  with_actor_workspace (fun _ workspace_instance ->
    let initial =
      actor_state ~workspace_instance ~liveness:Detached ~start_immediately:false
    in
    let source =
      A.Run_source_installation.captured installed ~generation:initial.identity.generation
      |> get
    in
    let run_id = P.Id.Run.of_string "run_observation_fixture" |> get in
    let principal_id = P.Id.Principal.of_string "pri_fixture" |> get in
    let run lifecycle revision =
      P.Run.create
        ~id:run_id
        ~session:
          (P.Session_ref.create
             ~server_id:(P.Id.Server.of_string "srv_fixture" |> get)
             ~session_id:initial.identity.session_id)
        ~principal_id
        ~source
        ~mode:Workflow
        ~lifecycle
        ~revision
        ~owned_work:[]
        ~relinquished_work:[]
        ~terminal_work:[]
        ~created_at:initial.identity.created_at
        ~updated_at:initial.identity.updated_at
      |> get
    in
    let receipt kind revision key =
      P.Run_receipt.create
        ~run_id
        ~principal_id
        ~source
        ~key:(P.Idempotency_key.of_string key |> get)
        ~request_sha256:(String.make 64 'b')
        ~kind
        ~run_revision:revision
        ~session_revision:revision
        ~committed_at:initial.identity.updated_at
      |> get
    in
    let admitted =
      A.Run_state.commit
        index
        ~run:(run Admitted 0L)
        ~receipt:(receipt Admission 0L "admission")
        ~intent:None
      |> get
    in
    let apply state next payloads =
      A.Session_transition.apply
        ~now:initial.identity.updated_at
        state
        ~delta:(A.Session_delta.Run_state_changed next)
        ~payloads
      |> get
    in
    let admission = apply initial admitted [] in
    let terminal =
      A.Run_state.commit
        admitted
        ~run:(run (Terminal (Completed None)) 1L)
        ~receipt:(receipt Terminal 1L "terminal")
        ~intent:None
      |> get
    in
    let finished = apply admission.state terminal [] in
    let unchanged = apply finished.state terminal [] in
    let existing_event =
      apply
        initial
        admitted
        [ P.Event.Durable.Payload.Session_updated (A.Session_state.summary initial) ]
    in
    let private_presence =
      A.Run_state.to_jsonaf terminal
      |> fun json -> set json "job_deliveries" (`Array []) |> A.Run_state.of_jsonaf |> get
    in
    let private_only = apply finished.state private_presence [] in
    let updated_once (transition : A.Session_transition.t) =
      match transition.events with
      | [ event ] ->
        (match
           P.Event.Durable.Payload.of_json ~kind:event.P.Event.Durable.kind event.payload
           |> get
         with
         | Session_updated session ->
           Int64.equal session.revision transition.state.counters.revision
           && Int64.equal event.sequence transition.state.counters.event_sequence
         | _ -> false)
      | [] | _ :: _ :: _ -> false
    in
    print_s
      [%sexp
        { admission = (updated_once admission : bool)
        ; terminal = (updated_once finished : bool)
        ; existing_event_not_duplicated = (updated_once existing_event : bool)
        ; no_change_no_event = (List.is_empty unchanged.events : bool)
        ; private_presence_no_event = (List.is_empty private_only.events : bool)
        ; ordered =
            (Int64.equal
               finished.state.counters.event_sequence
               (Int64.succ admission.state.counters.event_sequence)
             : bool)
        }]);
  [%expect
    {|
    ((admission true) (terminal true) (existing_event_not_duplicated true)
     (no_change_no_event true) (private_presence_no_event true) (ordered true))
    |}]
;;
