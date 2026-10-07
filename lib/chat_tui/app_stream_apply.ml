open! Core

let reconcile_target_width runtime =
  let model = runtime.App_runtime.model in
  if Option.is_some (Model.width_preparation model)
  then (
    Model.reconcile_width_preparation model;
    App_runtime.reprioritize_target_width_batches runtime;
    App_runtime.pump_target_width_completion runtime)
;;

let apply_transcript_event runtime throttler ~viewport_height event =
  let open Result.Let_syntax in
  let%map drafts = Stream.apply runtime.App_runtime.transcript_drafts event in
  let drafts =
    match Transcript.Stream.view event with
    | Item_finalized { item; entry } ->
      (match item.scope.relation with
       | Root
         when List.exists (Model.history_items runtime.model) ~f:(fun existing ->
                History_entry.Id.equal
                  (History_entry.id existing)
                  (History_entry.id entry)) ->
         Stream.remove_committed drafts (History_entry.id entry)
       | Root | Nested _ -> drafts)
    | Source_started _
    | Item_announced _
    | Part_announced _
    | Changed _
    | Source_finished _
    | Unknown_event _ -> drafts
  in
  runtime.transcript_drafts <- drafts;
  let damage = App_runtime.refresh_messages ~viewport_height runtime in
  reconcile_target_width runtime;
  match damage with
  | Model.No_damage | Below_viewport -> ()
  | Visible_damage | Above_viewport | Unknown_damage ->
    Redraw_throttle.request_redraw throttler
;;

let apply_history_committed runtime throttler entry =
  let model = runtime.App_runtime.model in
  let id = History_entry.id entry in
  if
    not
      (List.exists (Model.history_items model) ~f:(fun existing ->
         History_entry.Id.equal id (History_entry.id existing)))
  then ignore (Model.add_history_item model entry : Model.t);
  runtime.transcript_drafts <- Stream.remove_committed runtime.transcript_drafts id;
  ignore (App_runtime.refresh_messages runtime : Model.projection_damage);
  reconcile_target_width runtime;
  Redraw_throttle.request_redraw throttler
;;

let apply_tool_output = apply_history_committed

let replace_history runtime redraw_immediate items =
  Model.set_history_items runtime.App_runtime.model items;
  runtime.transcript_drafts <- Stream.create ();
  ignore (App_runtime.refresh_messages runtime : Model.projection_damage);
  reconcile_target_width runtime;
  redraw_immediate ()
;;
