open! Core

type t =
  { writer : Agent_store.Commit_writer.t
  ; durability : Agent_store.Journal_segment.durability
  ; command_accepted : string -> int64 -> unit
  ; archive :
      Session_state.Compaction_archive.t
      -> Session_state.t
      -> (unit, Agent_protocol.Error.t) result
  ; mutable previous_transaction_hash : string option
  }

let create ~archive ~command_accepted ~writer ~durability ~previous_transaction_hash =
  { archive; writer; durability; command_accepted; previous_transaction_hash }
;;

let protocol_error error =
  Agent_protocol.Error.create
    Persistence_error
    ~message:(Sexp.to_string_hum ([%sexp_of: Agent_store.Store_error.t] error))
    ~retryable:true
    ()
;;

let event_range events =
  match events with
  | [] -> None, None
  | first :: rest ->
    let last = List.last rest |> Option.value ~default:first in
    Some first.Agent_protocol.Event.Durable.sequence, Some last.sequence
;;

let accepted_at_ns state =
  state.Session_state.identity.updated_at
  |> Agent_protocol.Timestamp.to_time_ns
  |> Time_ns.to_int_ns_since_epoch
  |> Int64.of_int
;;

let transaction t ~command_audit previous transition =
  let first_event_sequence, last_event_sequence =
    event_range transition.Session_transition.events
  in
  Agent_store.Transaction.create
    ~session_id:previous.Session_state.identity.session_id
    ~generation:transition.state.identity.generation
    ~transaction_sequence:transition.state.counters.transaction_sequence
    ~previous_transaction_hash:t.previous_transaction_hash
    ~session_revision:transition.state.counters.revision
    ~first_event_sequence
    ~last_event_sequence
    ~accepted_at_ns:(accepted_at_ns transition.state)
    ~command_audit
    ~delta:(Sexp.to_string_mach ([%sexp_of: Session_delta.t] transition.delta))
    ~durable_events:
      (List.map transition.events ~f:(fun event ->
         Sexp.to_string_mach ([%sexp_of: Agent_protocol.Event.Durable.t] event)))
;;

let commit t ~command_audit ~(previous : Session_state.t) transition =
  let open Result.Let_syntax in
  let%bind () =
    List.fold_result
      transition.Session_transition.state.conversation.compaction_archives
      ~init:()
      ~f:(fun () archive ->
        if
          List.exists previous.conversation.compaction_archives ~f:(fun old ->
            Int64.equal old.revision archive.revision)
        then Ok ()
        else t.archive archive previous)
  in
  let%bind transaction =
    transaction t ~command_audit previous transition |> Result.map_error ~f:protocol_error
  in
  let%map committed =
    Agent_store.Commit_writer.commit t.writer ~durability:t.durability transaction
    |> Result.map_error ~f:protocol_error
  in
  t.previous_transaction_hash <- Some committed.transaction_hash;
  Option.iter command_audit ~f:(fun encoded ->
    t.command_accepted encoded committed.transaction_sequence)
;;

let actor_persistence t =
  Session_actor.
    { commit =
        (fun ~command_audit ~previous transition ->
          commit t ~command_audit ~previous transition)
    }
;;

let transaction_hash t = t.previous_transaction_hash

let install_snapshot ~env ~handle ~max_payload_length ~transaction_hash state =
  let snapshot =
    Agent_store.Snapshot.
      { schema_version = Session_state.current_schema_version
      ; transaction_sequence = state.Session_state.counters.transaction_sequence
      ; transaction_hash
      ; event_sequence = state.counters.event_sequence
      ; created_at = state.identity.updated_at
      ; prompt_artifact =
          Agent_protocol.Id.Prompt_revision.to_string state.spec.prompt_revision_id
      ; workspace_identity = state.spec.workspace_instance.conflict_domain
      ; payload = Sexp.to_string_mach ([%sexp_of: Session_state.t] state)
      }
  in
  Agent_store.Snapshot.install
    ~env
    ~directory:(Agent_store.Session_store.Handle.snapshot_directory handle)
    ~max_payload_length
    snapshot
;;

let restore_snapshot payload =
  try
    let state = Sexp.of_string payload |> [%of_sexp: Session_state.t] in
    Session_state.validate state
    |> Result.map_error ~f:(fun error ->
      Agent_store.Store_error.Corrupt error.Agent_protocol.Error.message)
    |> Result.map ~f:(fun () -> state)
  with
  | exn ->
    Error
      (Agent_store.Store_error.Corrupt
         ("session snapshot decode failed: " ^ Exn.to_string exn))
;;

let timestamp_of_ns value =
  Time_ns.of_int_ns_since_epoch (Int64.to_int_exn value)
  |> Agent_protocol.Timestamp.of_time_ns
;;

let apply_transaction state transaction =
  try
    let delta =
      Sexp.of_string transaction.Agent_store.Transaction.delta
      |> [%of_sexp: Session_delta.t]
    in
    Session_delta.apply state delta
    |> Result.map_error ~f:(fun error ->
      Agent_store.Store_error.Corrupt error.Agent_protocol.Error.message)
    |> Result.map ~f:(fun state ->
      { state with
        identity =
          { state.identity with updated_at = timestamp_of_ns transaction.accepted_at_ns }
      ; counters =
          { state.counters with
            revision = transaction.session_revision
          ; transaction_sequence = transaction.transaction_sequence
          ; event_sequence =
              Option.value
                transaction.last_event_sequence
                ~default:state.counters.event_sequence
          }
      })
  with
  | exn ->
    Error
      (Agent_store.Store_error.Corrupt
         ("session delta decode failed: " ^ Exn.to_string exn))
;;

let durable_events transaction =
  let decode encoded =
    try Ok (Sexp.of_string encoded |> [%of_sexp: Agent_protocol.Event.Durable.t]) with
    | exn ->
      Error
        (Agent_store.Store_error.Corrupt
           ("durable event decode failed: " ^ Exn.to_string exn))
  in
  let open Result.Let_syntax in
  let%bind events =
    Result.all (List.map transaction.Agent_store.Transaction.durable_events ~f:decode)
  in
  if
    List.for_all events ~f:(fun (event : Agent_protocol.Event.Durable.t) ->
      Agent_protocol.Id.Session.compare
        event.session_id
        transaction.Agent_store.Transaction.session_id
      = 0)
  then Ok events
  else Error (Agent_store.Store_error.Corrupt "durable event belongs to another session")
;;
