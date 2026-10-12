open! Core
open Agent_server_test_support
module P = Agent_protocol
module C = Agent_client
module S = Agent_server
module A = Agent_session

let prompt =
  {|<developer>Retain this code, not transcript markup.</developer>
<script id="seed_run" language="chatml" kind="moderator" api="extensibility-v1">
let initial_state = []
let on_event = fun ctx state event -> match event with
| `Session_start -> Task.bind(Run.finish(`Object([
  { key = "kind"; value = `String("finish") },
  { key = "terminal"; value = `Object([{ key = "kind"; value = `String("completed") }]) },
  { key = "relinquish"; value = `Array([]) }])), fun ignored -> Task.pure(state))
| _ -> Task.pure(state)
</script>|}
;;

let transcript ?(additional_text = "") () =
  let module N = History_entry.Payload in
  let id sequence =
    History_entry.Id.create ~namespace:"original/transcript" ~sequence
    |> Result.ok_or_failwith
  in
  let retained sequence role kind view metadata =
    let semantic = N.Semantic.create view ~metadata |> Result.ok_or_failwith in
    let payload =
      N.captured
        semantic
        ~origin:N.Origin.unavailable
        ~raw:
          (`Object
              [ "opaque", `String "007\\caller"
              ; "number", `Number "1.2300e+02"
              ; "null", `Null
              ])
      |> Result.ok_or_failwith
    in
    P.History.
      { id = id sequence
      ; content_revision = Content_revision.zero
      ; role
      ; kind
      ; payload = N.to_json payload
      ; provenance = Canonical
      ; redacted = false
      }
  in
  let message form role text =
    N.Semantic.Message
      { form
      ; role
      ; content = [ Text { text; annotations = []; logprobs = Absent } ]
      ; phase = Absent
      }
  in
  let metadata =
    { N.Metadata.empty with call_id = Value "opaque:01"; item_id = Value "provider/17" }
  in
  let entries =
    [ retained
        1
        System
        Message
        (message Input Developer "literal <tool name=never/>")
        N.Metadata.empty
    ; retained
        2
        User
        Message
        (message Input User "<script>not executed</script>")
        N.Metadata.empty
    ; retained
        3
        Assistant
        Message
        (message Output Assistant "retained assistant")
        N.Metadata.empty
    ; retained
        4
        Assistant
        Tool_call
        (N.Semantic.Call
           { kind = Function
           ; name = "unexecuted"
           ; namespace = Absent
           ; input_bytes = "{\"n\":1e0}"
           ; async = Absent
           })
        metadata
    ; retained
        5
        Tool
        Tool_output
        (N.Semantic.Result
           { relation = Bound (id 4); kind = Function; output = Text "retained result" })
        metadata
    ]
  in
  let entries =
    if String.is_empty additional_text
    then entries
    else
      entries
      @ [ retained 6 User Message (message Input User additional_text) N.Metadata.empty ]
  in
  P.Session_seed.Transcript.create
    entries
    ~next_history_sequence:0L
    ~limits:Document_schema.Limits.default
    ~max_entries:32
    ~max_work_steps:100000
    ~checkpoint:Eio.Fiber.check
  |> protocol_ok
;;

let request ~code ~key transcript =
  P.Session_seed.Request.create
    (session_spec ())
    ~code
    ~transcript
    ~idempotency_key:(P.Idempotency_key.of_string key |> protocol_ok)
  |> protocol_ok
;;

let current_revision client =
  C.Connection.request_without_history
    client
    (Prompt_get { prompt_id = S.Catalog_identity.prompt_definition "restart.prompt" })
  |> protocol_ok
  |> function
  | P.Method_result.Prompt_get prompt -> Option.value_exn prompt.current_revision
  | _ -> failwith "prompt projection"
;;

let catalog_code client =
  P.Session_seed.Code_selection.Catalog
    { definition_id = S.Catalog_identity.prompt_definition "restart.prompt"
    ; expected_revision = current_revision client
    }
;;

let state daemon session_id =
  S.Session_registry.read_state
    (S.Daemon.registry daemon)
    ~authorize:(fun _ -> Ok ())
    session_id
  |> protocol_ok
;;

let entry daemon session_id =
  S.Session_registry.load (S.Daemon.registry daemon) session_id |> protocol_ok
;;

let expected daemon (state : A.Session_state.t) =
  P.Session_lifecycle.Expected.create
    ~reference:
      (P.Session_ref.create
         ~server_id:(Agent_store.Session_store.server_id (S.Daemon.store daemon))
         ~session_id:state.identity.session_id)
    ~generation:state.identity.generation
    ~session_revision:state.counters.revision
    ~lifecycle_revision:P.Session_lifecycle.Revision.zero
  |> protocol_ok
;;

let start ?(factory_limits = S.Daemon.default_options.factory_limits) sw env ~root config =
  S.Daemon.start
    ~sw
    ~env
    ~config
    ~tool_dir:root
    ~home:root
    ~process_start_identity:None
    ~options:
      { S.Daemon.default_options with
        startup_mode = On_demand
      ; factory_limits
      ; inference_policy =
          inference_policy
            ~default_model:"fixture-model"
            ~post_stream:(fun ~sw:_ ~inputs:_ -> failwith "seed invoked provider")
      }
    ()
  |> protocol_ok
;;

let with_root f =
  Eio_main.run (fun env ->
    Mirage_crypto_rng_unix.use_default ();
    let root = temporary_root env in
    Exn.protect
      ~finally:(fun () ->
        Eio.Cancel.protect (fun () ->
          Eio.Path.rmtree ~missing_ok:true Eio.Path.(Eio.Stdenv.fs env / root)))
      ~f:(fun () ->
        let workspace = Filename.concat root "workspace" in
        Eio.Path.mkdir ~perm:0o700 Eio.Path.(Eio.Stdenv.fs env / workspace);
        let prompt_file = Filename.concat root "root.chatmd" in
        Eio.Path.save
          ~create:(`Exclusive 0o600)
          Eio.Path.(Eio.Stdenv.fs env / prompt_file)
          prompt;
        f env ~root ~prompt_file (config root workspace prompt_file)))
;;

let connection daemon principal =
  let notifications = Eio.Stream.create 256 in
  let actor = Operator_authorization.guarded ~principal ~is_current:(fun () -> true) in
  let context =
    S.Connection_context.create_authenticated
      ~actor
      ~connection_id:(P.Id.Attachment.create () |> P.Id.Attachment.to_string)
      ~transport:In_memory
      ~publish_notification:(Eio.Stream.add notifications)
      ~max_attachments:64
  in
  C.In_memory.create
    ~notifications
    ~request:(fun command ->
      S.Dispatcher.dispatch_command (S.Daemon.dispatcher daemon) ~context command)
    ~close:(fun () -> S.Daemon.close_connection daemon context)
;;

let with_host env ~root config f =
  Eio.Switch.run (fun sw ->
    let daemon = start sw env ~root config in
    let client = connection daemon (principal ()) in
    initialize client;
    Exn.protect
      ~f:(fun () -> f sw daemon client)
      ~finally:(fun () ->
        Eio.Cancel.protect (fun () ->
          C.Connection.close client;
          S.Daemon.shutdown daemon |> protocol_ok)))
;;

let assert_stopped daemon seeded transcript =
  let session_id = seeded.P.Session_seed.Result.session_id in
  let state = state daemon session_id in
  Option.iter
    (S.Session_registry.find (S.Daemon.registry daemon) session_id)
    ~f:(fun entry -> assert (not (S.Runtime_owner.is_loaded entry.runtime)));
  assert (P.Session.equal_desired_state state.lifecycle.desired Stopped);
  (match state.lifecycle.observed with
   | Stopped -> ()
   | Queued_for_slot
   | Starting
   | Recovering
   | Idle
   | Running_turn _
   | Compacting _
   | Waiting_for_permission _
   | Stopping
   | Failed _ -> failwith "seed is not stopped");
  assert (
    List.equal
      P.History.equal_entry
      state.conversation.canonical_history
      (P.Session_seed.Transcript.entries transcript));
  assert (List.is_empty state.invocations);
  assert (Option.is_none state.moderator);
  assert (Int.equal state.conversation.initial_prompt_entry_count 0);
  (match state.runtime_initialization with
   | Pending { fresh_history = false } -> ()
   | Pending { fresh_history = true } | Ready -> failwith "seed initialization state");
  state
;;

let store_ok = function
  | Ok value -> value
  | Error error -> raise_s [%sexp (error : Agent_store.Store_error.t)]
;;
