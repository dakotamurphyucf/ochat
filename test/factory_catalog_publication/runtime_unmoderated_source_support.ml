open! Core
open Agent_server_test_support
open Retry_support
module B = A.Runtime_builder
module N = A.Native_tool_invocation
module C = Chat_response.Tool_capability
module I = P.Invocation
module Warm = Delegated_warm_resource_tests
module Failure = Delegated_policy_caller_support.Failure
module Store = Agent_store

let prompt =
  {|<authoring_context policy="manual"/><config model="fixture-model"/>
<developer>Actual registered root without a moderator.</developer>
<tool name="read_file"><read id="data" path="${workspace}"/></tool>|}
;;

module Native_host = struct
  type t =
    { mutable selected : (string * (unit -> unit)) option
    ; mutable reads : int
    ; failure : Failure.t
    }

  let create () = { selected = None; reads = 0; failure = Failure.create () }

  let wrap t env =
    Crash_fault_io.wrap
      env
      ~boundary:Before_open_in
      ~matches:(fun path ->
        Option.exists t.selected ~f:(fun (selected, _) -> String.equal selected path))
      ~reached:(fun _ ->
        let _, run = Option.value_exn t.selected in
        t.selected <- None;
        t.reads <- t.reads + 1;
        Failure.guard t.failure run)
  ;;
end

type t =
  { env : Eio_unix.Stdenv.base
  ; sw : Eio.Switch.t
  ; daemon : Daemon.t
  ; entry : Registry.entry
  ; writer : P.Id.Attachment.t
  ; first : B.t
  ; host : Native_host.t
  ; fault : Inference_close_tests.Fault.t
  ; cache : Warm.Handoff_io.t
  ; counters : counters
  ; next : Delegated_policy_caller_support.Provider.call option ref
  ; callers : A.Delegated_policy_caller.t list ref
  ; phase : string ref
  }

let within t phase f =
  t.phase := phase;
  Eio.Time.with_timeout_exn t.env#clock 30. f
;;

let state t = Actor.state t.entry.actor |> protocol_ok

let ledger_json (current : A.Session_state.t) =
  A.Inference_ledger.to_document current.inference_ledger
  |> Result.map_error ~f:(fun error ->
    Sexp.to_string_hum (A.Inference_ledger.Error.sexp_of_t error))
  |> Result.ok_or_failwith
  |> Document_schema.Document.json
;;

let rec settle t =
  let current = Warm.settled t.entry in
  match current.active_operation, current.lifecycle.observed, current.failure with
  | None, Idle, None -> current
  | _, _, Some failure -> raise_s [%sexp (failure : P.Error.t)]
  | _ ->
    Eio.Fiber.yield ();
    settle t
;;

let await_idle t =
  let rec loop () =
    Failure.raise_if_present t.host.failure;
    let current = Warm.settled t.entry in
    match current.active_operation, current.lifecycle.observed, current.failure with
    | None, Idle, None -> current
    | _, _, Some failure -> raise_s [%sexp (failure : P.Error.t)]
    | _ ->
      Eio.Fiber.yield ();
      loop ()
  in
  loop ()
;;

let assert_unmoderated (runtime : B.t) (current : A.Session_state.t) =
  assert (Option.is_some runtime.unmoderated_source);
  assert (Option.is_none runtime.moderator_manager);
  assert (Option.is_none current.moderator);
  assert (List.is_empty current.moderator_executions);
  assert (Option.is_none current.spec.delegation);
  match current.runtime_initialization with
  | Ready -> ()
  | Pending _ -> failwith "actual plain root initialization is not ready"
;;

let with_fixture ?test_root ?(wrap_env = Fn.id) f =
  let host = Native_host.create () in
  let fault = Inference_close_tests.Fault.create () in
  let cache = Warm.Handoff_io.create () in
  let counters = { inference_ports = ref 0; providers = ref 0; transcripts = ref [] } in
  let next = ref None in
  let phase = ref "initial public plain root" in
  let base =
    inference_policy ~default_model:"fixture-model" ~post_stream:(fun ~sw:_ ~inputs ->
      Int.incr counters.providers;
      counters.transcripts
      := !(counters.transcripts)
         @ [ `Array (List.map inputs ~f:Res.Item.jsonaf_of_t) |> Jsonaf.to_string ];
      match !next with
      | Some call ->
        next := None;
        Delegated_policy_caller_support.Provider.function_response call
      | None -> completed_response "597 actual unmoderated native completed")
  in
  let policy =
    { base with
      runtime_inference_ports =
        (fun actor ->
          Int.incr counters.inference_ports;
          base.runtime_inference_ports actor)
    }
  in
  Creation_tests.with_daemon
    ?test_root
    ~qualify_chatml_extensions:true
    ~prompt_text:prompt
    ~sources:[ "value.txt", "597 unmoderated actual file" ]
    ~wrap_env:(fun env ->
      wrap_env
        (Native_host.wrap
           host
           (Inference_close_tests.Fault.wrap_env fault (Warm.Handoff_io.wrap cache env))))
    ~policy
    (fun env sw _root daemon ->
       Eio.Time.with_timeout_exn env#clock 90. (fun () ->
         let entry =
           Factory.create_session
             (Daemon.factory daemon)
             ~command_audit:None
             ~principal
             (create_request ~start_immediately:true ())
           |> protocol_ok
         in
         let current = Actor.state entry.actor |> protocol_ok in
         let session = current.identity.session_id in
         let registry = Daemon.registry daemon in
         (match Registry.add registry ~session_id:session entry with
          | Ok () -> ()
          | Error error ->
            Agent_server.Session_registry.close_entry entry;
            raise_s [%sexp (error : P.Error.t)]);
         protect_cleanup
           ~finally:(fun () ->
             protect_cleanup
               ~finally:(fun () -> Agent_server.Session_registry.close_entry entry)
               (fun () ->
                  let actual = Registry.remove registry session |> Option.value_exn in
                  assert (phys_equal actual.actor entry.actor);
                  assert (phys_equal actual.runtime entry.runtime)))
           (fun () ->
              let writer, _ =
                Actor.attach entry.actor ~mode:Read_write ~subscribe:false |> protocol_ok
              in
              let callers = ref [] in
              let first = Warm.loaded entry in
              let t =
                { env :> Eio_unix.Stdenv.base
                ; sw
                ; daemon
                ; entry
                ; writer = writer.id
                ; first
                ; host
                ; fault
                ; cache
                ; counters
                ; next
                ; callers
                ; phase
                }
              in
              protect_cleanup
                ~finally:(fun () ->
                  host.selected <- None;
                  fault.phase <- Quiet;
                  List.iter !callers ~f:A.Delegated_policy_caller.close;
                  protect_cleanup
                    ~finally:(fun () -> Actor.detach entry.actor writer.id |> protocol_ok)
                    (fun () ->
                       match (state t).active_operation with
                       | None -> ()
                       | Some operation ->
                         Actor.cancel_operation
                           entry.actor
                           ~attachment_id:writer.id
                           ~operation_id:operation.id
                         |> protocol_ok
                         |> ignore;
                         within t "join actual plain root cleanup" (fun () ->
                           settle t |> ignore)))
                (fun () ->
                   try
                     let initial =
                       within t "join initial actual plain root" (fun () -> await_idle t)
                     in
                     assert_unmoderated first initial;
                     [%test_eq: int] 0 !(counters.providers);
                     let handle = Option.value_exn entry.store_handle in
                     fault.marker
                     <- Some
                          (Filename.concat
                             (Store.Data_root.indexes_path
                                (Store.Session_store.data_root (Daemon.store daemon)))
                             "sessions.recovery-required");
                     cache.cache
                     <- Some
                          (Filename.concat
                             (Store.Session_store.Handle.cache_directory handle)
                             "cache.bin");
                     f t
                   with
                   | exn ->
                     let backtrace = Stdlib.Printexc.get_raw_backtrace () in
                     (try Eio.traceln "597 unmoderated runtime phase: %s" !phase with
                      | _ -> ());
                     Exn.raise_with_original_backtrace exn backtrace))))
;;

let request t borrowed ~candidate caller =
  let invocation = N.borrowed_invocation borrowed in
  assert (P.Id.Session.equal invocation.context.session_id (state t).identity.session_id);
  assert (I.equal_origin invocation.context.origin Model);
  assert (String.equal invocation.context.tool_name "read_file");
  (match N.borrowed_agent_actor borrowed with
   | Ok (Some original_actor) ->
     assert (Operator_authorization.is_current original_actor);
     assert (
       P.Id.Principal.equal
         (Operator_authorization.principal original_actor).id
         principal.id)
   | Ok None | Error _ -> failwith "actual unmoderated native lost original submission");
  let selected = N.borrowed_capabilities borrowed |> protocol_ok in
  let reference =
    C.references selected
    |> List.find_exn ~f:(fun reference -> String.equal reference.C.name "read_file")
  in
  let call : Chat_response.Moderation.Tool_call.t =
    { id = P.Id.Invocation.to_string candidate
    ; name = reference.name
    ; args = `Object [ "root", `String "data"; "file", `String "value.txt" ]
    ; kind = Function
    ; payload_text = {|{"root":"data","file":"value.txt"}|}
    ; meta = `Null
    }
  in
  ({ invocation_id = candidate
   ; session_id = invocation.context.session_id
   ; generation = invocation.context.generation
   ; owner = Native_call invocation.context.id
   ; caller = Some caller
   ; selected
   ; call
   }
   : A.Script_tool_calls.preparation)
;;

let caller t borrowed =
  let actual = N.borrowed_policy_caller borrowed |> protocol_ok in
  t.callers := actual :: !(t.callers);
  actual
;;

let registered t request =
  Actor.check_delegated_policy_caller t.entry.actor request |> protocol_ok;
  let captured = Actor.Causal_governance.capture t.entry.actor ~request |> protocol_ok in
  match Actor.Causal_governance.bind_standalone t.entry.actor captured with
  | Error { code = Permission_denied; message; _ }
    when String.equal message "registered policy governance needs private ancestry" -> ()
  | Error failure ->
    raise_s [%sexp "unexpected registered root result", (failure : P.Error.t)]
  | Ok _ -> failwith "registered plain root was misclassified as standalone"
;;

let run t ~id f =
  assert (Option.is_none !(t.next));
  let entered = ref false in
  t.next
  := Some
       { Delegated_policy_caller_support.Provider.name = "read_file"
       ; file = "value.txt"
       ; id
       };
  t.host.selected
  <- Some
       ( Filename.concat
           (state t).spec.workspace_instance.canonical_root.native_path
           "value.txt"
       , fun () ->
           entered := true;
           f (N.borrow () |> protocol_ok) );
  let message, _operation =
    Delegated_policy_caller_support.submit
      t.entry
      t.writer
      ~actor:(Some original)
      ("597 unmoderated " ^ id)
  in
  let after = within t ("join actual native " ^ id) (fun () -> await_idle t) in
  Failure.raise_if_present t.host.failure;
  assert !entered;
  assert (Option.is_none !(t.next));
  let actual =
    List.filter after.invocations ~f:(fun invocation ->
      Option.equal String.equal invocation.I.context.provider_call_id (Some id))
    |> Delegated_policy_claim_tests.one "actual plain-root native invocation"
  in
  (match actual.status with
   | Published (Complete (`String value))
     when String.is_substring value ~substring:"597 unmoderated actual file" -> ()
   | status -> raise_s [%sexp "actual plain-root native failed", (status : I.status)]);
  assert (
    List.exists after.conversation.canonical_history ~f:(P.History.equal_entry message));
  after
;;
