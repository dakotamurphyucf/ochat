open! Core
open Support
module Store = Agent_store
module Origin = A.Recurrence_run_origin
module Occurrence = A.Recurrence_identity.Occurrence
module Definition = A.Recurrence_identity.Definition
module Owner = Agent_server.Runtime_owner

type t =
  { synced : Origin.t Eio.Promise.t
  ; acknowledge : unit -> unit
  }

let rec admitted_origin ~controller (transaction : Store.Transaction.t) = function
  | A.Session_delta.Batch changes ->
    List.find_map changes ~f:(admitted_origin ~controller transaction)
  | Run_state_changed index ->
    List.find_map (A.Run_state.runs index) ~f:(fun run ->
      let open Option.Let_syntax in
      let%bind origin = A.Run_state.recurrence_origin index run.id in
      let admission = Origin.admission origin in
      let definition = Occurrence.definition (Origin.occurrence origin) in
      if
        P.Id.Session.equal
          controller
          (P.Session_ref.session_id (Definition.controller definition))
        && P.Id.Recurrence.equal recurrence_id (Definition.recurrence_id definition)
        && P.Id.Session.equal
             transaction.session_id
             (P.Session_ref.session_id run.session)
        && P.Run.Lifecycle.equal run.lifecycle Admitted
        && List.is_empty run.owned_work
        && Int64.equal admission.session_revision transaction.session_revision
      then (
        Origin.validate_run origin ~run ~admission |> protocol_ok;
        Some origin)
      else None)
  | _ -> None
;;

let sync_boundary env ~controller =
  let synced, signal_synced = Eio.Promise.create () in
  let may_acknowledge, signal_acknowledge = Eio.Promise.create () in
  let acknowledge () =
    if Option.is_none (Eio.Promise.peek may_acknowledge)
    then Eio.Promise.resolve signal_acknowledge ()
  in
  let reached path =
    let scan =
      Eio.Path.load Eio.Path.(Eio.Stdenv.fs env / path)
      |> Store.Journal_segment.scan_contents ~max_payload_length:(64 * 1024 * 1024)
      |> store_ok
    in
    assert (not scan.crash_tail);
    match List.last scan.entries with
    | Some entry when Store.Frame.flags entry.frame = 0 ->
      let tx = Store.Transaction.decode (Store.Frame.payload entry.frame) |> store_ok in
      let delta =
        A.Session_delta_document.decode ~limits:D.Limits.default tx.delta
        |> document_ok
        |> A.Session_delta_document.value
      in
      Option.iter (admitted_origin ~controller tx delta) ~f:(fun origin ->
        (* The data is physically durable. This only delays the writer's successful
           reply to its Actor and never manufactures or withholds an ACK. *)
        Eio.Promise.resolve signal_synced origin;
        Eio.Promise.await may_acknowledge)
    | Some _ | None -> ()
  in
  let wrapped =
    Crash_fault_io.wrap env ~boundary:After_sync ~reached ~matches:(fun path ->
      Option.is_none (Eio.Promise.peek synced)
      && (not
            (String.is_substring
               path
               ~substring:("/" ^ P.Id.Session.to_string controller ^ "/")))
      && String.is_substring path ~substring:"/journal/"
      && String.is_suffix path ~suffix:".log")
  in
  wrapped, { synced; acknowledge }
;;

let prove t ~clock ~daemon ~parent ~provider_count =
  let origin = Eio.Promise.await t.synced in
  let registry = Daemon.registry daemon in
  let child = loaded daemon (P.Session_ref.session_id (Origin.session origin)) in
  let await_displaced () =
    let rec loop () =
      if Owner.source_lane_idle child.runtime
      then failwith "scheduler abandoned the acknowledged Run during loaded retention";
      Eio.Time.Mono.sleep clock 0.05;
      loop ()
    in
    loop ()
  in
  Registry.For_testing.with_loaded_retention_wait_observer registry parent (fun waiter ->
    Eio.Switch.run (fun sw ->
      Controller_retention_fixture.with_retention sw registry parent (fun () ->
        t.acknowledge ();
        let current = state child in
        let index = Option.value_exn current.run_state in
        let run = A.Run_state.find index (Origin.run_id origin) |> Option.value_exn in
        let retained = A.Run_state.recurrence_origin index run.id |> Option.value_exn in
        assert (Origin.equal origin retained);
        Origin.validate_run origin ~run ~admission:(Origin.admission origin)
        |> protocol_ok;
        assert (P.Run.Lifecycle.equal run.lifecycle Admitted);
        assert (List.is_empty run.owned_work);
        assert (List.is_empty current.jobs);
        [%test_eq: int] 0 (provider_count ());
        Eio.Fiber.first (fun () -> Eio.Promise.await waiter) await_displaced;
        [%test_eq: int] 0 (provider_count ()))));
  origin
;;

let release_writer t = t.acknowledge ()
