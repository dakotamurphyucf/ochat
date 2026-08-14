open Core
open Eio.Std
module Model = Model
module Redraw_throttle = Redraw_throttle
module Runtime = App_runtime

module Context = struct
  type t =
    { shared : App_context.Resources.t
    ; runtime : Runtime.t
    }
end

let start (ctx : Context.t) =
  let env = ctx.shared.services.env in
  let ui_sw = ctx.shared.services.ui_sw in
  let session = ctx.shared.services.session in
  let runtime = ctx.runtime in
  let internal_stream = ctx.shared.streams.internal in
  let throttler = ctx.shared.ui.throttler in
  let model = runtime.Runtime.model in
  let op_id = Runtime.alloc_op_id runtime in
  runtime.Runtime.op <- Some (Runtime.Starting_compaction { id = op_id });
  runtime.Runtime.cancel_compaction_on_start <- false;
  Model.set_activity model (Some Model.Compacting);
  Log.emit `Info "Compacting history items…";
  Redraw_throttle.request_redraw throttler;
  let history_snapshot = Model.history_items model in
  let session_snapshot : Session.t option =
    match session with
    | None -> None
    | Some s ->
      let s =
        Option.value_map runtime.Runtime.session_state ~default:s ~f:(fun state -> !state)
      in
      Some
        Session.
          { s with
            history = history_snapshot
          ; next_history_sequence =
              History_entry.Allocator.next_sequence runtime.Runtime.history_allocator
          ; tasks = Model.tasks model
          ; kv_store = Hashtbl.to_alist (Model.kv_store model)
          }
  in
  Fiber.fork ~sw:ui_sw (fun () ->
    match
      Switch.run
      @@ fun sw ->
      Eio.Stream.add internal_stream (`Compaction_started (op_id, sw));
      (match session_snapshot with
       | None -> ()
       | Some s ->
         Session_store.save ~env s
         (* Session_store.reset_session ~env ~id:s.id ~keep_history:false () *));
      Context_compaction.Compactor.compact_entries
        ~allocator:runtime.Runtime.history_allocator
        ~env:(Some env)
        ~history:history_snapshot
    with
    | Ok history' -> Eio.Stream.add internal_stream (`Compaction_done (op_id, history'))
    | Error exn -> Eio.Stream.add internal_stream (`Compaction_error (op_id, exn))
    | exception exn -> Eio.Stream.add internal_stream (`Compaction_error (op_id, exn)))
;;
