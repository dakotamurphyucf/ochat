open! Core
open Fixtures
open Recovery_window_actor_tests
module Query = A.Recovery_history_query
module Cursor = Query.Continuation

let checked = function
  | Ok value -> value
  | Error (Query.Source cause) ->
    failwith (Sexp.to_string_hum (A.Recovery_source_reader.sexp_of_error cause))
  | Error Refresh_required -> failwith "query unexpectedly requested refresh"
  | Error Reply_limit -> failwith "query unexpectedly exceeded reply limit"
  | Error Invalid_cursor -> failwith "query unexpectedly rejected its own cursor"
;;

let field json name =
  P.Json_codec.fields json
  |> protocol_ok
  |> fun fields -> P.Json_codec.required fields name |> protocol_ok
;;

let rows page = P.Json_codec.list Result.return (field page "rows") |> protocol_ok
let coverage page = P.Json_codec.string (field page "coverage") |> protocol_ok

let snippets page =
  List.map (rows page) ~f:(fun row ->
    P.Search_snippet.of_json (field row "snippet") |> protocol_ok |> P.Search_snippet.text)
;;

let make
      fixture
      ?(family = Query.Canonical)
      ?(view = A.Recovery_item_text.Ordinary)
      ?ordinals
      ?limit
      ?max_reply_bytes
      ~scope
      action
  =
  Query.create
    ~source:fixture.owner
    ~scope
    ~family
    ~view
    ?ordinals
    ?limit
    ?max_reply_bytes
    action
  |> protocol_ok
;;

let run
      fixture
      query
      ~current
      ?(authorize = fun _ _ _ -> Ok ())
      ?(on_sign = fun () -> ())
      ?continuation
      ()
  =
  let next = ref None in
  let sign cursor =
    on_sign ();
    next := Some (Cursor.of_json (Cursor.to_json cursor) |> protocol_ok);
    Ok "fixture-signed-continuation"
  in
  let%map.Result page =
    Query.run
      query
      fixture.sessions
      ~handle:fixture.handle
      ~current
      ~authorize
      ~max_frame_bytes:payload_limit
      ~max_payload_bytes:payload_limit
      ?continuation
      ~sign
      ()
  in
  let next =
    match field page "next" with
    | `Null -> None
    | `String token when String.equal token "fixture-signed-continuation" -> !next
    | _ -> failwith "unexpected continuation wire value"
  in
  page, next
;;

let all_snippets fixture query ~current =
  let rec loop remaining reversed continuation =
    if Int.equal remaining 0 then failwith "history paging made no bounded progress";
    let page, next = run fixture query ~current ?continuation () |> checked in
    let reversed = List.rev_append (snippets page) reversed in
    match next with
    | None -> List.rev reversed, coverage page
    | Some cursor -> loop (remaining - 1) reversed (Some cursor)
  in
  loop 64 [] None
;;

let with_store entries f =
  with_actor_workspace (fun env workspace_instance ->
    Eio.Switch.run (fun sw ->
      let base =
        actor_state ~workspace_instance ~liveness:Detached ~start_immediately:false
      in
      let count = Int64.of_int (List.length entries) in
      let base : State.t =
        { base with
          conversation =
            { base.conversation with
              canonical_history = entries
            ; next_history_sequence = count
            ; reserved_history_through = count
            }
        }
      in
      let storage = Job_artifact_fixtures.create env sw base in
      let owner =
        P.Session_ref.create
          ~server_id:(Store.Session_store.server_id storage.sessions)
          ~session_id
      in
      let fixture : fixture =
        { env; sessions = storage.sessions; handle = storage.session; owner }
      in
      let ledger =
        A.Recovery_window_capture.bootstrap
          base
          ~owner
          ~window:(P.Id.Context_window.create ())
        |> protocol_ok
      in
      f fixture sw { base with recovery_windows = Some ledger }))
;;

let with_rollover fixture (before : State.t) retained =
  let source_scope = scope before in
  let successor =
    Scope.create
      ~generation:before.identity.generation
      ~session:fixture.owner
      ~window:(P.Id.Context_window.create ())
    |> protocol_ok
  in
  let document = A.Session_state_document.authored before in
  let archive =
    Archive.reference_for document ~kind:Rollover ~limits:document_limits operation_id
    |> protocol_ok
  in
  Archive.write
    ~env:fixture.env
    ~handle:fixture.handle
    ~max_payload_length:payload_limit
    archive
    document
  |> protocol_ok;
  let ledger =
    A.Recovery_window_seal.prepare
      before
      ~predecessor:source_scope
      ~successor
      ~archive
      ~retained
    |> protocol_ok
  in
  let sealed : State.t =
    { before with
      recovery_windows = Some ledger
    ; counters = { before.counters with revision = Int64.succ before.counters.revision }
    ; conversation =
        { before.conversation with
          canonical_history = retained
        ; compaction_archives = [ archive ]
        }
    }
  in
  sealed, archive, source_scope
;;

let search text = Query.Search (P.Search_term.create text |> protocol_ok)

let%expect_test
    "physical archive work is shared and search keeps the earliest exact window across \
     pages"
  =
  let originals = List.init 10 ~f:(fun i -> history i (sprintf "a needle %d" i)) in
  with_store originals (fun fixture sw source ->
    let first = List.hd_exn originals in
    let carried, archive, predecessor = with_rollover fixture source [ first ] in
    let journal = journal fixture Store.Journal.create in
    with_actor
      fixture
      sw
      journal
      ~restored:(Persistence.Restored.authored carried)
      ~previous_transaction_hash:None
      ~archive:(fun _ _ -> failwith "the query must not write an archive")
      (fun actor _ ->
         let writer = attach actor in
         let append entries =
           Actor.append_history actor ~attachment_id:writer entries
           |> protocol_ok
           |> ignore
         in
         append [ history 10 "b needle live" ];
         let live ?continuation query =
           let observation = Actor.observe_resource_state actor |> protocol_ok in
           run
             fixture
             query
             ~current:(fun () -> Actor.Resource_observation.check_current observation)
             ?continuation
             ()
         in
         let query = make fixture ~scope:Current_generation (search "NEEDLE") in
         let page, next = live query |> checked in
         [%test_eq: string] (coverage page) "work_limit";
         [%test_eq: int] (List.length (rows page)) 8;
         let next = Option.value_exn next in
         [%test_eq: int] next.next_position 8;
         [%test_eq: int] next.highwater.last_window_members 2;
         append [ history 11 "native caller appended needle" ];
         let done_, next_ = live ~continuation:next query |> checked in
         [%test_eq: string] (coverage done_) "complete";
         [%test_eq: string list]
           (snippets done_)
           [ "a needle 8"; "a needle 9"; "b needle live" ];
         assert (Option.is_none next_);
         let listed, _ =
           live (make fixture ~scope:(Window (scope carried)) List_items) |> checked
         in
         [%test_eq: int] (List.length (rows listed)) 3;
         let list_original = List.hd_exn (rows listed) in
         let citation =
           Citation.of_json (field list_original "citation") |> protocol_ok
         in
         assert (Scope.equal (Citation.scope citation) (scope carried));
         assert (P.History.Id.equal (Citation.occurrence citation) first.id);
         let manager = actor_proof ~scopes:[ Send_messages; Manage_grants ] () in
         let file =
           Eio.Path.(
             Eio.Stdenv.fs fixture.env
             / Filename.concat
                 (Store.Session_store.Handle.archive_directory fixture.handle)
                 (Archive.filename archive))
         in
         Eio.Path.unlink file;
         (match live query with
          | Error (Source Missing) -> ()
          | _ -> failwith "a missing promised archive became a successful miss");
         Eio.Path.save ~create:(`Exclusive 0o600) file "broken frame";
         (match live query with
          | Error (Source Corrupt) -> ()
          | _ -> failwith "a corrupt promised archive became a successful miss");
         Actor.expire_recovery_window
           actor
           ~actor:manager
           ~attachment_id:writer
           ~authorize:(fun _ -> Ok ())
           predecessor
         |> protocol_ok
         |> ignore;
         (match live ~continuation:next query with
          | Error Refresh_required -> ()
          | _ -> failwith "expiry did not invalidate an earlier cursor");
         let after_expiry, _ = live query |> checked in
         [%test_eq: string list]
           (snippets after_expiry)
           [ "a needle 0"; "b needle live"; "native caller appended needle" ]));
  [%expect {| |}]
;;

let%expect_test
    "empty pages advance, reply bounds include the cursor, and revocation wins at release"
  =
  with_store
    (List.init 263 ~f:(fun i -> history i (sprintf "needle %d" i)))
    (fun fixture _ state ->
       let current () = Ok state in
       let query =
         make fixture ~ordinals:(260, 261) ~scope:Current_generation (search "needle")
       in
       let empty, next = run fixture query ~current () |> checked in
       [%test_eq: int] (List.length (rows empty)) 0;
       [%test_eq: string] (coverage empty) "position_limit";
       let next = Option.value_exn next in
       [%test_eq: int] next.next_position 256;
       let done_, next_ = run fixture query ~current ~continuation:next () |> checked in
       [%test_eq: string list] (snippets done_) [ "needle 260" ];
       assert (Option.is_none next_);
       let query =
         make fixture ~max_reply_bytes:600 ~scope:Current_generation (search "needle")
       in
       let clipped, next = run fixture query ~current () |> checked in
       assert (String.length (Jsonaf.to_string clipped) <= 600);
       [%test_eq: string] (coverage clipped) "reply_limit";
       let count = List.length (rows clipped) in
       assert (count > 0);
       let next = Option.value_exn next in
       [%test_eq: int] next.next_position count;
       let query = make fixture ~limit:1 ~scope:Current_generation (search "needle") in
       let live = ref true in
       let current () =
         if !live then Ok state else Error (P.Error.invalid_request "caller revoked")
       in
       (match run fixture query ~current ~on_sign:(fun () -> live := false) () with
        | Error (Source Denied) -> ()
        | _ -> failwith "result escaped after its host authority changed");
       let malformed =
         make fixture ~max_reply_bytes:20 ~scope:Current_generation (search "needle")
       in
       match run fixture malformed ~current:(fun () -> Ok state) () with
       | Error Reply_limit -> ()
       | _ -> failwith "an impossible encoded response did not report its limit");
  [%expect {| |}]
;;
