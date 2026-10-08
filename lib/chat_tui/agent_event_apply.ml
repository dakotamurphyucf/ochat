open! Core
module P = Agent_protocol

type t = { mutable agent_operation : P.Id.Operation.t option }

let create () = { agent_operation = None }

let sync_operation t model projection =
  let fields = Agent_projection.fields projection in
  match fields.session.active_operation with
  | Some operation
    when not (Option.exists t.agent_operation ~f:(P.Id.Operation.equal operation.id)) ->
    Model.clear_agent_calls model;
    t.agent_operation <- Some operation.id
  | None
    when Option.is_some t.agent_operation
         && Option.is_none (Agent_projection.terminal_operation projection)
         && Option.is_none
              (Agent_client.Live_projection.terminal_view
                 (Agent_projection.live projection)) ->
    Model.clear_agent_calls model;
    t.agent_operation <- None
  | Some _ | None -> ()
;;

let apply t ~model ~viewport_height projection =
  sync_operation t model projection;
  let live = Agent_projection.live projection in
  let terminal_view = Agent_client.Live_projection.terminal_view live in
  let operation_views =
    Agent_client.Live_projection.operations live @ Option.to_list terminal_view
  in
  let canonical_rows = Agent_projection.rows projection in
  let known =
    Hash_set.of_list
      (module Projected_message.Id)
      (List.map canonical_rows ~f:(fun row -> row.Projected_message.id))
  in
  let drafts =
    operation_views
    |> List.concat_map ~f:(fun operation ->
      let rows = Stream.rows_of_draft operation.drafts in
      match operation.continuity with
      | Complete -> rows
      | Incomplete { first_missing_sequence } ->
        let local_id = P.Id.Operation.to_string operation.operation_id in
        let id =
          Projected_message.Id.local ~namespace:"live-gap" ~local_id
          |> Result.ok_or_failwith
        in
        let gap =
          Projected_message.
            { id
            ; entry_id = None
            ; message =
                ( "system"
                , Printf.sprintf
                    "[Live transcript incomplete from sequence %Ld]"
                    first_missing_sequence )
            ; provenance = Placeholder
            ; source = Placeholder { local_id; kind = "live-gap" }
            ; editing_text = None
            ; revision = 0
            }
        in
        gap :: rows)
    |> List.filter ~f:(fun row -> not (Hash_set.mem known row.Projected_message.id))
  in
  let rows = canonical_rows @ drafts in
  (* Public read state cannot become standalone writable canonical context. *)
  Model.set_history_items model [];
  Model.rebuild_tool_output_index_for_public
    model
    (Agent_projection.visible_history projection);
  let damage =
    Model.reconcile_projected_messages_with_damage
      model
      ~viewport_height
      ~rows
      ~messages:(List.map rows ~f:(fun row -> row.Projected_message.message))
  in
  let fields = Agent_projection.fields projection in
  Model.reconcile_agent_activity
    model
    (fields.active_tool_calls
     @ fields.active_agent_calls
     @ Agent_client.Live_projection.activities live
     @ Option.value_map terminal_view ~default:[] ~f:(fun view -> view.activities))
    ~operation_ended:(Option.is_some terminal_view);
  (match Agent_projection.synchronization projection with
   | Current -> ()
   | Snapshot_required error ->
     Model.set_connection_status model (Some (Connection_status.failed error)));
  Agent_projection.snapshot projection
  |> Agent_work_view.of_snapshot
  |> Model.update_session_work model
  |> ignore;
  Ok damage
;;
