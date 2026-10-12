open! Core
module P = Agent_protocol
module C = Agent_client
module T = Transcript
module R = History_chatmd.Rendering
module M = Managed_completion_presentation

let ok = function
  | Ok value -> value
  | Error failure -> raise_s [%sexp (failure : P.Error.t)]
;;

let session_id = P.Id.Session.of_string "ses_presentation" |> ok
let operation_id = P.Id.Operation.of_string "op_presentation" |> ok
let timestamp = P.Timestamp.of_string "2026-10-12T12:00:00Z" |> ok

let operation state =
  P.Operation.
    { id = operation_id
    ; generation = 0
    ; kind = Turn Administrative
    ; state
    ; started_at = timestamp
    ; updated_at = timestamp
    }
;;

let snapshot () =
  let spec =
    P.Session.Spec.create
      ~execution_host:Embedded
      ~prompt:(Local_path "/definition.chatmd")
      ~workspace:Current
      ~liveness:Process_bound
      ~persistence:Transient
      ~start_immediately:false
      ~labels:[]
      ()
    |> ok
  in
  let session =
    P.Session.
      { id = session_id
      ; creator = None
      ; created_at = timestamp
      ; updated_at = timestamp
      ; generation = 0
      ; spec
      ; desired_state = Running
      ; observed_state = Idle
      ; prompt_revision = None
      ; workspace_instance = None
      ; active_operation = Some (operation Running)
      ; revision = 1L
      ; metadata_revision = 0L
      ; organization = P.Session_organization.Values.empty
      ; latest_event_sequence = 1L
      ; inference_summary = History_entry.Payload.Presence.Absent
      }
  in
  P.Public.Snapshot.create
    { session
    ; lifecycle = None
    ; canonical_history =
        { entries = []
        ; previous_cursor = None
        ; next_cursor = None
        ; reached_start = true
        ; reached_end = true
        ; structurally_complete = true
        }
    ; archived_revisions = []
    ; effective_history = None
    ; deferred_entries = []
    ; permissions = []
    ; grants = []
    ; jobs = []
    ; extension_status = []
    ; schedules = []
    ; active_tool_calls = []
    ; active_agent_calls = []
    ; halted = false
    ; halt_reason = None
    ; failure = None
    ; revision = 1L
    ; latest_event_sequence = 1L
    }
  |> ok
;;

let root =
  T.Scope.create
    ~source:(T.Source_id.of_string "parent" |> Result.ok_or_failwith)
    ~attempt:(T.Attempt_id.of_string "attempt" |> Result.ok_or_failwith)
    ~relation:Root
  |> Result.ok_or_failwith
;;

let child =
  T.Scope.create
    ~source:(T.Source_id.of_string "child" |> Result.ok_or_failwith)
    ~attempt:root.key.attempt
    ~relation:
      (Nested { scope = root.key; call_entry_id = None; call_alias = Some "worker" })
  |> Result.ok_or_failwith
;;

let item scope =
  T.Item.create
    ~scope
    ~id:(T.Item_id.of_string "message" |> Result.ok_or_failwith)
    ~entry_id:None
    ~header:(Some (Message Assistant))
    ~call_name:None
  |> Result.ok_or_failwith
;;

let part item =
  T.Part.create
    ~item
    ~id:(T.Part_id.of_string "text" |> Result.ok_or_failwith)
    ~index:(Some 0)
    ~kind:Text
  |> Result.ok_or_failwith
;;

let transcript view =
  T.Stream.create view ~limits:T.Admission.default
  |> Result.ok_or_failwith
  |> fun event -> P.Event.Recoverable.Transcript event
;;

let live projection sequence payload =
  let event =
    P.Event.Recoverable.create
      ~session_id
      ~operation_id
      ~operation_sequence:(Int64.of_int sequence)
      ~anchor_sequence:1L
      ~timestamp
      ~invocation_id:None
      ~parent_invocation_id:None
      payload
    |> ok
  in
  C.Projection.apply_live_event projection event |> ok
;;

let changed scope change =
  transcript (Changed { target = Content (part (item scope)); change })
;;

let tool_key =
  P.Activity.Key.create ~scope:child.key ~call_alias:"tool" ~parent:None |> ok
;;

let progress update =
  P.Event.Recoverable.Tool_activity
    (Progress { key = tool_key; progress = { channel = Stdout; update } })
;;

let terminal projection =
  let payload = P.Event.Durable.Payload.Operation_completed (operation Completed) in
  let internal =
    P.Event.Durable.of_payload ~session_id ~sequence:2L ~revision:2L ~timestamp payload
  in
  let event =
    P.Public.Durable.of_internal_envelope
      internal
      ~body:(Full (Shared (P.Public.Durable.Shared_payload.of_internal payload |> ok)))
      ~extension_status:None
      ~replacement_snapshot:None
    |> ok
  in
  C.Projection.apply_event projection event |> ok
;;

let limits ?(bytes = 65536) ?(work = 1048576) () =
  R.Limits.create ~max_bytes:bytes ~max_work_steps:work ~json_limits:T.Admission.default
  |> fun result ->
  match result with
  | Ok value -> value
  | Error error -> raise_s [%sexp (error : R.Error.t)]
;;

let observe presenter projection =
  match M.observe presenter projection ~checkpoint:ignore with
  | Ok value -> value
  | Error error -> raise_s [%sexp (error : R.Error.t)]
;;

let require_frame presenter projection = observe presenter projection |> Option.value_exn

let%expect_test "actual admitted live gaps, nested scope and unobserved tool outcome" =
  let presenter = M.create (limits ()) in
  let initial = C.Projection.install_snapshot (snapshot ()) in
  let text = live initial 1 (changed root (Append "hello")) in
  let first = require_frame presenter text in
  assert (String.is_substring first ~substring:"text \"hello\"");
  assert (Option.is_none (observe presenter text));
  let gap = live text 3 (changed child (Append "child")) in
  let tool = live gap 4 (progress (Append "tail")) in
  print_string (require_frame presenter tool);
  let replaced = live tool 5 (changed root (Replace "new\027[31m")) in
  let replacement = require_frame presenter replaced in
  assert (not (String.is_substring replacement ~substring:"hello"));
  assert (not (String.contains replacement '\027'));
  assert (String.is_substring replacement ~substring:"new\\027[31m");
  let ended = require_frame presenter (terminal replaced) in
  assert (String.is_substring ended ~substring:"terminal operation observation");
  assert (String.is_substring ended ~substring:"tool outcome not observed (running)");
  assert (not (String.is_substring ended ~substring:"tool returned"));
  let descriptor =
    P.Activity.Tool.descriptor
      tool_key
      ~call_entry_id:None
      ~name:"actual-worker"
      ~kind:Function
      ~input:"{}"
      ~classification:(Some Subagent)
    |> ok
  in
  let started = live replaced 6 (Tool_activity (Started descriptor)) in
  let returned =
    live
      started
      7
      (Tool_activity (Finished { key = tool_key; outcome = Returned; output = None }))
    |> require_frame presenter
  in
  assert (String.is_substring returned ~substring:"name \"actual-worker\"");
  assert (String.is_substring returned ~substring:"tool returned");
  let cleared = require_frame presenter (C.Projection.install_snapshot (snapshot ())) in
  assert (String.is_substring cleared ~substring:"no retained item or tool observations");
  print_endline
    "replacement quoted; terminal did not invent tool return; fresh snapshot cleared";
  [%expect
    {|
    [whole-session live observation; replaces previous observation]
    operation "op_presentation"
    [live gap from sequence 2]
    scope "parent"/"attempt" (root)
    item "message"
    message "assistant"
    [earlier item content unavailable]
    part "text"
    [earlier text unavailable]
    text "hello"
    scope "child"/"attempt" (nested; parent "parent"/"attempt"; call "worker")
    item "message"
    message "assistant"
    [earlier item content unavailable]
    part "text"
    [earlier text unavailable]
    text "child"
    tool scope "child"/"attempt"
    call "tool"
    [tool descriptor unavailable]
    [earlier activity unavailable]
    stdout progress "tail"
    tool outcome not observed (running)
    replacement quoted; terminal did not invent tool return; fresh snapshot cleared
    |}]
;;

let%test_unit "finite frame admission preserves prior view and original checkpoint" =
  let initial = C.Projection.install_snapshot (snapshot ()) in
  let small = live initial 1 (changed root (Append "small")) in
  let frame = require_frame (M.create (limits ())) small in
  let presenter = M.create (limits ~bytes:(String.length frame) ()) in
  [%test_eq: string] (require_frame presenter small) frame;
  let large = live small 2 (changed root (Replace (String.make 8192 'x'))) in
  (match M.observe presenter large ~checkpoint:ignore with
   | Error Output_limit -> ()
   | Error (Invalid_limits | Work_limit | Json_error _) | Ok _ ->
     failwith "oversized display did not reject at output admission");
  assert (Option.is_none (observe presenter small));
  let exhausted = M.create (limits ~work:1 ()) in
  (match M.observe exhausted small ~checkpoint:ignore with
   | Error Work_limit -> ()
   | Error (Invalid_limits | Output_limit | Json_error _) | Ok _ ->
     failwith "render work limit was not enforced");
  let reason = Failure "original presentation checkpoint" in
  let calls = ref 0 in
  (try
     M.observe presenter small ~checkpoint:(fun () ->
       Int.incr calls;
       if !calls = 2 then raise reason)
     |> ignore;
     failwith "checkpoint was swallowed"
   with
   | Failure _ as actual -> assert (phys_equal actual reason));
  assert (Option.is_none (observe presenter small))
;;
