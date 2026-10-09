open! Core
open Fixtures
module A = Agent_session.Session_actor
module P = Agent_protocol

let user_entry actor text =
  let ids =
    Agent_session.History_id_source.create
      ~namespace:(P.Id.Session.to_string session_id)
      ~block_size:1
      ~reserve:(fun ~count -> A.reserve_history_block actor ~count)
    |> protocol_ok
  in
  let id = Agent_session.History_id_source.allocate ids |> protocol_ok in
  Agent_session.History_codec.user_text ~id text
  |> Agent_session.History_codec.to_protocol
;;

let conflict = function
  | Error { P.Error.code = Conflict; _ } -> true
  | Error failure -> raise_s [%sexp (failure : P.Error.t)]
  | Ok _ -> false
;;

let%expect_test
    "lifecycle exclusion guards canonical edits and admission and can abort before \
     retirement"
  =
  Job_fixtures.with_actor (fun _env _sw actor writer _backend ->
    let entry = user_entry actor "retained input" in
    let original = A.state actor |> protocol_ok in
    let stale =
      A.begin_lifecycle
        actor
        ~attachment_id:(Some writer.id)
        ~expected_generation:original.identity.generation
        ~expected_revision:Int64.(original.counters.revision + 1L)
    in
    let fence =
      A.begin_lifecycle
        actor
        ~attachment_id:(Some writer.id)
        ~expected_generation:original.identity.generation
        ~expected_revision:original.counters.revision
      |> protocol_ok
    in
    let patch =
      P.Session_metadata.Patch.create
        ~name:(Set "fenced")
        ~set_labels:[]
        ~remove_labels:[]
      |> protocol_ok
    in
    let edit_blocked =
      A.update_metadata
        actor
        ~attachment_id:writer.id
        ~expected_metadata_revision:original.identity.metadata_revision
        ~patch
        ()
      |> conflict
    in
    let history_blocked =
      A.append_history actor ~attachment_id:writer.id [ entry ] |> conflict
    in
    let start_blocked = A.start actor ~attachment_id:writer.id |> conflict in
    let observed = A.lifecycle_state actor fence |> protocol_ok in
    print_s
      [%sexp
        (( conflict stale
         , edit_blocked
         , history_blocked
         , start_blocked
         , Int64.equal original.counters.revision observed.counters.revision )
         : bool * bool * bool * bool * bool)];
    A.abort_lifecycle actor fence |> protocol_ok;
    A.append_history actor ~attachment_id:writer.id [ entry ] |> protocol_ok |> ignore;
    let current = A.state actor |> protocol_ok in
    print_s [%sexp (List.length current.conversation.canonical_history : int)]);
  [%expect
    {|
    (true true true true true)
    1
    |}]
;;

let%expect_test
    "permanent lifecycle retirement preserves reads and actual joins and cannot reopen"
  =
  Job_fixtures.with_actor (fun _env _sw actor writer _backend ->
    let state = A.state actor |> protocol_ok in
    let fence =
      A.begin_lifecycle
        actor
        ~attachment_id:(Some writer.id)
        ~expected_generation:state.identity.generation
        ~expected_revision:state.counters.revision
      |> protocol_ok
    in
    A.retire_lifecycle actor fence |> protocol_ok;
    let join = A.retire_runtime_worker actor ~closing:true |> protocol_ok in
    A.Runtime_retirement.await join |> protocol_ok;
    A.set_runtime_worker actor ~worker:None ~inference:None |> protocol_ok;
    let abort_blocked = A.abort_lifecycle actor fence |> conflict in
    let start_blocked = A.start actor ~attachment_id:writer.id |> conflict in
    let second_blocked =
      A.begin_lifecycle
        actor
        ~attachment_id:None
        ~expected_generation:state.identity.generation
        ~expected_revision:state.counters.revision
      |> conflict
    in
    let observed = A.lifecycle_state actor fence |> protocol_ok in
    print_s
      [%sexp
        (( abort_blocked
         , start_blocked
         , second_blocked
         , A.Runtime_retirement.is_finished join
         , P.Id.Session.equal observed.identity.session_id state.identity.session_id )
         : bool * bool * bool * bool * bool)]);
  [%expect {| (true true true true true) |}]
;;
