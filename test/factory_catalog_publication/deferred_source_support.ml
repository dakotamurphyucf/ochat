open! Core
open Agent_server_test_support
module A = Agent_session
module Actor = A.Session_actor
module Owner = Agent_server.Runtime_owner
module Daemon = Agent_server.Daemon
module Factory = Agent_server.Session_factory
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

(* One actual Factory-created runtime, with its own hidden issuer association.
   The loaded resource snapshot is DATA; tests obtain admission only through the
   real Factory/Actor APIs. Every user callback stays inside this joined lifetime. *)
let with_loaded_entry daemon f =
  let factory = Daemon.factory daemon in
  let entry =
    Factory.create_session
      factory
      ~command_audit:None
      ~principal:(principal ())
      (create_request ~start_immediately:true ())
    |> protocol_ok
  in
  (match
     Agent_server.Session_registry.add
       (Daemon.registry daemon)
       ~session_id:(Existing.state entry).identity.session_id
       entry
   with
   | Ok () -> ()
   | Error error ->
     entry.close ();
     raise_s [%sexp (error : P.Error.t)]);
  protect_cleanup ~finally:entry.close (fun () ->
    (* Join real startup, including silent factual ACKs, before selecting
    an admission basis. This does not run another lifecycle event. *)
    let rec await_idle () =
      let current =
        Actor.with_moderator_checkpoint entry.actor (fun () -> Actor.state entry.actor)
        |> protocol_ok
      in
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
