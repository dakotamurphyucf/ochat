open! Core
module A = Agent_session
module P = Agent_protocol
module C = Support.Config_fixture
module E = Recovery_checkpoint_scenario
module F = Crash_recovery_fixture
module Notes = Recovery_note_provider_scenario
module Provider = Support.Tui_stream_provider
module Scope = P.Recovery_window.Scope
module Citation = P.Recovery_window.Citation
module Stock = Recovery_capacity_scenario
module Rollover = A.Recovery_rollover

let require = F.require
let field = Jsonaf.member_exn
let string value = `String value
let number value = `Number (Int.to_string value)

let expect_json actual expected =
  if not (Document_schema.Json.equal actual expected)
  then
    raise_s
      [%sexp
        "unexpected example tool result"
      , (Jsonaf.to_string expected : string)
      , (Jsonaf.to_string actual : string)]
;;

let page = [ "limit", number 1; "cursor", `Null ]
let term = "α-lantern"
let original = "Decision α-lantern belongs to the blue review."
let note = "Search for α-lantern in the preceding window. Resume the review."
let path = "handoff/review"

let checked_in_sources =
  [ ( "learning/context-recovery/agent.chatmd"
    , [%blob "../../../docs-src/examples/learning/context-recovery/agent.chatmd"] )
  ; ( "learning/context-recovery/chatml.project.json"
    , [%blob "../../../docs-src/examples/learning/context-recovery/chatml.project.json"] )
  ; ( "learning/context-recovery/moderator.chatml"
    , [%blob "../../../docs-src/examples/learning/context-recovery/moderator.chatml"] )
  ; ( "libraries/recovery-rollover/chatml.project.json"
    , [%blob "../../../docs-src/examples/libraries/recovery-rollover/chatml.project.json"]
    )
  ; ( "libraries/recovery-rollover/src/recovery_rollover.chatml"
    , [%blob
        "../../../docs-src/examples/libraries/recovery-rollover/src/recovery_rollover.chatml"]
    )
  ; ( "libraries/recovery-rollover/src/recovery_rollover.chatmli"
    , [%blob
        "../../../docs-src/examples/libraries/recovery-rollover/src/recovery_rollover.chatmli"]
    )
  ; ( "libraries/lifecycle/chatml.project.json"
    , [%blob "../../../docs-src/examples/libraries/lifecycle/chatml.project.json"] )
  ; ( "libraries/lifecycle/src/lifecycle.chatml"
    , [%blob "../../../docs-src/examples/libraries/lifecycle/src/lifecycle.chatml"] )
  ; ( "libraries/lifecycle/src/lifecycle.chatmli"
    , [%blob "../../../docs-src/examples/libraries/lifecycle/src/lifecycle.chatmli"] )
  ; ( "learning/lifecycle-composition/components/review/chatml.project.json"
    , [%blob
        "../../../docs-src/examples/learning/lifecycle-composition/components/review/chatml.project.json"]
    )
  ; ( "learning/lifecycle-composition/components/review/src/review.chatml"
    , [%blob
        "../../../docs-src/examples/learning/lifecycle-composition/components/review/src/review.chatml"]
    )
  ; ( "learning/lifecycle-composition/components/review/src/review.chatmli"
    , [%blob
        "../../../docs-src/examples/learning/lifecycle-composition/components/review/src/review.chatmli"]
    )
  ]
;;

let configure env fixture =
  let root = Filename.dirname (C.config_path fixture) in
  List.iter checked_in_sources ~f:(fun (relative, bytes) ->
    let path = Filename.concat root relative in
    Eio.Path.mkdirs ~exists_ok:true ~perm:0o700 (F.path env (Filename.dirname path));
    F.write env path bytes);
  let name = function
    | Sexp.List (Atom key :: _) -> key
    | _ -> F.fail "invalid example configuration field"
  in
  let find fields key =
    List.find_exn fields ~f:(fun field -> String.equal (name field) key)
  in
  let fields = function
    | Sexp.List [ Atom _; List fields ] -> fields
    | _ -> F.fail "invalid example configuration group"
  in
  let test_config = Sexp.of_string_many (C.configuration fixture ()) in
  let transport = fields (find test_config "server") in
  let http =
    Sexp.List
      [ Atom "http"
      ; List
          (List.map
             (fields (find transport "http"))
             ~f:(fun field ->
               if String.equal (name field) "idle_connection_timeout_ms"
               then Sexp.List [ Atom "idle_connection_timeout_ms"; Atom "60000" ]
               else field))
      ]
  in
  let sample =
    [%blob "../../../docs-src/examples/learning/context-recovery/server.sexp"]
    |> Sexp.of_string_many
  in
  (* Keep the actual example policy, workspace and source boundaries. Adapt
     socket/store paths and HTTP only for the process fixture's client. *)
  List.map sample ~f:(fun group ->
    match name group with
    | "server" ->
      Sexp.List
        [ Atom "server"
        ; List
            (List.map (fields group) ~f:(fun field ->
               match name field with
               | ("unix_socket" | "data_dir") as key -> find transport key
               | "http" -> http
               | _ -> field))
        ]
    | _ -> group)
  |> List.map ~f:Sexp.to_string_hum
  |> String.concat ~sep:"\n"
  |> C.write_configuration fixture ~name:"learning/context-recovery/server.sexp"
;;

let enabled_session client connection ~server_id =
  let page = P.Page.Request.create ~limit:10 () |> F.protocol_ok in
  let prompt =
    match
      F.request client (Prompt_list { page; enabled = Some true; available = Some true })
    with
    | Prompt_list { items = [ prompt ]; _ } -> prompt.id
    | _ -> F.fail "example prompt did not appear in the ready catalog"
  in
  let workspace =
    match
      F.request
        client
        (Workspace_list { page; kind = None; access = None; available = Some true })
    with
    | Workspace_list { items = [ workspace ]; _ } -> workspace.id
    | _ -> F.fail "example workspace did not appear in the ready catalog"
  in
  let spec =
    P.Session.Spec.create
      ~execution_host:Daemon
      ~prompt:(Catalog prompt)
      ~workspace:(Configured workspace)
      ~liveness:Detached
      ~persistence:Durable
      ~start_immediately:false
      ~labels:[]
      ()
    |> F.protocol_ok
  in
  let created =
    Support.Http_driver.request
      client
      (P.Command.Session_create
         { spec
         ; requested_mode = Some Owner_read_write
         ; subscribe = false
         ; idempotency_key = F.key "composed-recovery"
         })
    |> F.protocol_ok
  in
  match created.result with
  | Session_create { session; attachment = Some { attachment; _ }; _ } ->
    let scope = Stock.enable connection session ~server_id attachment.id in
    Stock.start client session attachment.id "composed-recovery-start";
    session, attachment.id, scope
  | _ -> F.fail "could not acquire the example session writer"
;;

let complete request id =
  match Notes.output request ~id with
  | Complete value -> value
  | outcome ->
    raise_s [%sexp "example tool failed", (id : string), (outcome : P.Invocation.outcome)]
;;

let component_state (state : A.Session_state.t) ~scope ~finished =
  let snapshot =
    A.Moderator_checkpoint.decode state.moderator |> F.protocol_ok |> Option.value_exn
  in
  let get value key =
    match value with
    | Session.Snapshot.Record fields -> List.Assoc.find_exn fields key ~equal:String.equal
    | _ -> F.fail "example moderator did not retain typed component state"
  in
  let actual = snapshot.current_state in
  [%test_eq: Session.Snapshot.t]
    (get actual "successor")
    (Variant ("Some", [ String (P.Id.Context_window.to_string (Scope.window scope)) ]));
  [%test_eq: Session.Snapshot.t] (get (get actual "review") "seen") (Bool true);
  [%test_eq: Session.Snapshot.t] (get actual "review_finished") (Bool finished)
;;

let verify_rollover (state : A.Session_state.t) ~previous ~next =
  let ledger = Option.value_exn state.recovery_rollover in
  let scheduled = Rollover.foreground ledger ~scope:next |> Option.value_exn in
  let receipt = Rollover.find ledger ~id:scheduled.owner |> Option.value_exn in
  (match receipt.status with
   | Terminal (Committed { successor; checkpoint = Checkpointed _; _ })
     when Scope.equal successor next -> ()
   | _ -> F.fail "example did not commit its checkpointed successor");
  let hooks = Option.value_exn receipt.hooks in
  match A.Recovery_rollover_hooks.before hooks, A.Recovery_rollover_hooks.after hooks with
  | ( Completed (before, Proceed { hints = Some hints; continuation = None })
    , Completed (after, Continue) ) ->
    require
      (Scope.equal before.window previous
       && Scope.equal after.window next
       && List.length (A.Recovery_rollover_hint_ref.Selection.references hints) = 1)
      "example hooks did not select the exact checkpoint and successor"
  | _ -> F.fail "example did not record ordered before and after hook decisions"
;;

let run env environment =
  let fixture = F.fixture env environment "context-recovery-example" in
  let config_path = configure env fixture in
  Eio.Switch.run (fun sw ->
    let port =
      let reservation = Support.Port_reservation.create ~sw ~env in
      let port = Support.Port_reservation.port reservation in
      Support.Port_reservation.release reservation;
      port
    in
    let provider = Provider.start ~sw ~env ~port in
    Stock.with_stock env fixture ~port ~config_path (fun client connection server_id ->
      let session, attachment, previous = enabled_session client connection ~server_id in
      let durable () =
        Support.Background_fixture.checkpoint
          env
          fixture
          { summary = session; attachment_id = attachment }
        |> Option.value_exn
      in
      let index = ref 0 in
      let await () = Provider.await_request provider env !index in
      let next_request () =
        Int.incr index;
        await ()
      in
      let call ~id name fields =
        Notes.function_call (await ()) ~id ~name (`Object fields);
        complete (next_request ()) id
      in
      let seeded =
        E.submit
          client
          session
          attachment
          ~timing:Safe_boundary
          ~key:"earlier-decision"
          original
      in
      let first = await () in
      require
        (String.is_substring
           (Provider.body first |> Jsonaf.to_string)
           ~substring:"# Recovering a local session")
        "real loader did not capture builtin context-recovery";
      require
        (Set.mem (E.tools first) "context_read_text")
        "example tools were not selected";
      Provider.reply_summary first "I recorded the earlier decision.";
      E.await_idle env client session;
      E.send
        client
        session
        attachment
        ~key:"start-recovery"
        "Checkpoint this review, open a new window, and recover the earlier decision.";
      ignore (next_request () : Provider.request);
      let listed = call ~id:"initial-windows" "context_list_windows" page in
      let row = field "items" listed |> Jsonaf.list_exn |> List.hd_exn in
      expect_json (field "scope" row) (Scope.to_json previous);
      let empty = call ~id:"note-list-before" "recovery_note_list" page in
      expect_json
        (call
           ~id:"note-write"
           "recovery_note_write"
           [ "kind", string "create"
           ; "expected", `Null
           ; "expected_catalog_revision", field "catalog_revision" empty
           ; "path", string path
           ; "content", string note
           ])
        `Null;
      let listed = call ~id:"note-list-after" "recovery_note_list" page in
      let item = field "items" listed |> Jsonaf.list_exn |> List.hd_exn in
      expect_json (field "path" item) (string path);
      let selection = field "selection" item in
      let expected_window = P.Id.Context_window.to_json (Scope.window previous) in
      expect_json
        (call
           ~id:"checkpoint"
           "context_checkpoint"
           [ "expected_window", expected_window
           ; "key", string "example-cut"
           ; "mode", string "register"
           ; "notes", `Array [ selection ]
           ])
        (`Object [ "status", string "accepted" ]);
      E.request_transition (await ()) previous ~id:"start-new-window";
      let successor_request = next_request () in
      E.require_successor connection previous;
      let next =
        E.list_windows connection previous
        |> List.find_exn ~f:(fun row -> P.Recovery_query.State.equal row.state Open)
        |> fun (row : P.Recovery_query.Summary.t) -> row.scope
      in
      let state = durable () in
      verify_rollover state ~previous ~next;
      component_state state ~scope:next ~finished:false;
      let windows =
        call
          ~id:"successor-windows"
          "context_list_windows"
          [ "limit", number 2; "cursor", `Null ]
      in
      let has_window ~scope ~state =
        field "items" windows
        |> Jsonaf.list_exn
        |> List.exists ~f:(fun row ->
          Document_schema.Json.equal (field "scope" row) (Scope.to_json scope)
          && Document_schema.Json.equal (field "state" row) (string state))
      in
      require
        (has_window ~scope:previous ~state:"sealed"
         && has_window ~scope:next ~state:"open")
        "selected window list did not distinguish predecessor and successor";
      let input = field "input" (Provider.body successor_request) |> Jsonaf.list_exn in
      E.require_hints input [ E.excerpt "local_note" note ];
      require
        (not
           (String.is_substring
              (Provider.body successor_request |> Jsonaf.to_string)
              ~substring:original))
        "first successor request repeated the old original instead of retrieving it";
      let note_result =
        call
          ~id:"read-note"
          "recovery_note_read"
          [ ( "selection"
            , `Object
                [ "kind", string "local"
                ; "id", field "id" selection
                ; "revision", field "revision" selection
                ] )
          ; "range", `Object [ "start", number 0; "stop", number (String.length note) ]
          ]
      in
      expect_json (field "text" note_result) (string note);
      let matches =
        call
          ~id:"search-sealed"
          "context_search"
          ([ "window", Scope.to_json previous
           ; "family", string "canonical"
           ; "role", string "user"
           ; "term", string term
           ]
           @ page)
      in
      let hit =
        match field "rows" matches |> Jsonaf.list_exn with
        | [ row ] -> row
        | _ -> F.fail "bounded search did not produce one exact predecessor result"
      in
      let citation = field "citation" hit |> Citation.of_json |> F.protocol_ok in
      require
        (Scope.equal (Citation.scope citation) previous
         && P.History.Id.equal (Citation.occurrence citation) seeded.history_id)
        "bounded search found a different predecessor occurrence";
      let read ~id ~start ~max_bytes =
        call
          ~id
          "context_read_text"
          [ "citation", Citation.to_json citation
          ; "preparation_revision", `Null
          ; "part", field "part" hit
          ; "start", start
          ; "stop", field "stop" hit
          ; "max_bytes", number max_bytes
          ]
      in
      let chunk = read ~id:"read-first-scalar" ~start:(field "start" hit) ~max_bytes:2 in
      expect_json (field "text" chunk) (string "α");
      let rest =
        read
          ~id:"read-rest"
          ~start:(field "next_offset" chunk)
          ~max_bytes:(String.length term)
      in
      [%test_eq: string]
        (Jsonaf.string_exn (field "text" chunk) ^ Jsonaf.string_exn (field "text" rest))
        term;
      require
        (Option.is_none (Jsonaf.member "next_offset" rest))
        "final exact text range supplied a cursor";
      Provider.reply_summary
        (await ())
        "The preceding window contains the exact decision marker α-lantern.";
      E.await_idle env client session;
      component_state (durable ()) ~scope:next ~finished:true))
;;
