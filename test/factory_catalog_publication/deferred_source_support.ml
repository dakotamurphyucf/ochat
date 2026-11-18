open! Core
open Agent_server_test_support
module A = Agent_session
module Actor = A.Session_actor
module Owner = Agent_server.Runtime_owner
module Daemon = Agent_server.Daemon
module Factory = Agent_server.Session_factory
module Registry = Agent_server.Session_registry
module P = Agent_protocol
module Existing = Source_preparation_tests

let prompt_text =
  {|<developer>Actual source owner with a loaded, quiescent runtime.</developer>
<script id="owner" language="chatml" kind="moderator" api="extensibility-v1">
let initial_state = 0
let on_event ctx state event = Task.pure(state)
</script>|}
;;

let protect_cleanup = Creation_tests.protect_cleanup

type cleanup =
  | Direct of
      { before_close : unit -> unit
      ; observe_close : (Agent_server.Session_disposal_report.t -> unit) option
      }
  | Daemon_shutdown

let direct_cleanup ?observe_close ?(before_close = ignore) () =
  Direct { before_close; observe_close }
;;

(* One actual Factory-created runtime, with its own hidden issuer association.
   The loaded resource snapshot is DATA; tests obtain admission only through the
   real Factory/Actor APIs. Every user callback stays inside this joined lifetime. *)
let with_loaded_entry_using ?(drive_startup = false) ~cleanup daemon f =
  let factory = Daemon.factory daemon in
  let entry =
    Factory.create_session
      factory
      ~command_audit:None
      ~principal:(principal ())
      (create_request ~start_immediately:true ())
    |> protocol_ok
  in
  let close ~before_close ~observe_close () =
    protect_cleanup
      ~finally:(fun () ->
        match observe_close with
        | None -> Registry.close_entry entry
        | Some observe ->
          let report = Registry.close_entry_reported entry in
          assert (Agent_server.Session_disposal_report.disposed report);
          observe report)
      before_close
  in
  let registry = Daemon.registry daemon in
  let session_id = (Existing.state entry).identity.session_id in
  let register () = Registry.add registry ~session_id entry in
  let with_cleanup use_entry =
    match cleanup with
    | Direct { before_close; observe_close } ->
      let close = close ~before_close ~observe_close in
      (match register () with
       | Ok () -> ()
       | Error error ->
         close ();
         raise_s [%sexp (error : P.Error.t)]);
      protect_cleanup ~finally:close use_entry
    | Daemon_shutdown ->
      (* After publication, the Daemon joins schedulers and all dependent runtimes
         before closing this entry's Actor and Store handle. *)
      Registry.close_provisional registry ~session_id entry register |> protocol_ok;
      use_entry ()
  in
  with_cleanup (fun () ->
    (* A parked scheduler cannot consume the pending initial lifecycle. Use its
       exact Factory driver when the caller explicitly owns that scheduling cut. *)
    if drive_startup
    then Factory.drain_idle_moderator factory entry |> protocol_ok |> ignore;
    (* Join real startup, including silent factual ACKs, before selecting
    an admission basis. This does not run another lifecycle event. *)
    let rec await_idle () =
      let current =
        Actor.with_moderator_checkpoint entry.actor (fun () -> Actor.state entry.actor)
        |> protocol_ok
      in
      (* A terminal startup failure cannot satisfy the successful-idle predicate.
         Report it directly instead of spinning until an enclosing deadline. *)
      Option.iter current.failure ~f:(fun failure ->
        raise_s [%sexp "source startup failed", (failure : P.Error.t)]);
      List.iter current.moderator_executions ~f:(fun receipt ->
        match receipt.P.Moderator_execution.status with
        | Running | Completed _ -> ()
        | Failed failure ->
          raise_s
            [%sexp "source startup callback failed", (failure : P.Invocation.tool_error)]
        | Interrupted reason ->
          raise_s [%sexp "source startup callback interrupted", (reason : string)]);
      match
        ( current.active_operation
        , current.lifecycle.observed
        , current.runtime_initialization )
      with
      | None, Idle, Ready
        when Option.is_some current.moderator
             && (not (List.is_empty current.moderator_executions))
             && List.for_all current.moderator_executions ~f:(fun receipt ->
               match receipt.P.Moderator_execution.status with
               | Completed _ -> true
               | Running | Failed _ | Interrupted _ -> false) -> ()
      | _ ->
        Eio.Fiber.yield ();
        await_idle ()
    in
    await_idle ();
    let actual_runtime =
      Owner.with_loaded_runtime entry.runtime (fun runtime ->
        let native = Option.value_exn runtime.A.Runtime_builder.native_runtime in
        assert (Lazy.is_val native.capabilities);
        Ok runtime)
      |> protocol_ok
    in
    let revision =
      match
        (List.hd_exn (A.Prompt_catalog.entries (Daemon.prompts daemon))).availability
      with
      | Ready revision -> revision
      | Disabled | Unavailable _ -> failwith "source owner has no captured revision"
    in
    f ~entry ~runtime:actual_runtime ~revision)
;;

let with_loaded_entry ?observe_close ?before_close ?drive_startup daemon f =
  with_loaded_entry_using
    ?drive_startup
    ~cleanup:(direct_cleanup ?observe_close ?before_close ())
    daemon
    f
;;
