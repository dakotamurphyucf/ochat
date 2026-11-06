open! Core
open Agent_server_test_support
open Retry_support
module Native = Goal_native_start_tests
module Goal = Goal_service_tests
module G = P.Goal
module Request = P.Goal_native_start_request
module Intent = P.Goal_native_start_intent
module Store = Agent_store
module Service = Agent_server.Goal_run_service
module Driver = Agent_server.Goal_native_resume_dispatcher

module Fault = struct
  module Cut = struct
    type t =
      | Unknown of Crash_fault_io.boundary
      | Revoke_original of (unit -> unit)
  end

  module Arm = struct
    type t =
      { session : P.Id.Session.t
      ; invocation : P.Id.Invocation.t
      ; request : Request.t
      ; principal : P.Id.Principal.t
      }
  end

  module Candidate = struct
    type t =
      { transaction : Store.Transaction.t
      ; invocation : P.Invocation.t
      ; intent : Intent.t
      ; index : A.Run_state.t
      ; catalog : A.Goal_catalog.t
      ; admission : P.Run_receipt.t
      }
  end

  type t =
    { cut : Cut.t
    ; mutable arm : Arm.t option
    ; mutable candidate : Candidate.t option
    ; mutable hits : int
    }

  let create cut = { cut; arm = None; candidate = None; hits = 0 }

  let candidate (arm : Arm.t) (transaction : Store.Transaction.t) atoms =
    let open Option.Let_syntax in
    let%bind invocation, intent, admission =
      List.find_map atoms ~f:(function
        | A.Session_delta.Invocation_changed invocation
          when P.Id.Invocation.equal invocation.context.id arm.invocation ->
          (match invocation.native_goal_action with
           | Some (Start intent)
             when Request.equal (Intent.request intent) arm.request
                  && P.Id.Principal.equal (Intent.principal_id intent) arm.principal ->
             (match Intent.phase intent with
              | Admitted value -> Some (invocation, intent, value.admission)
              | Staged | Accepted _ | Dispatch_claimed _ | Discarded _ | Rejected _ ->
                None)
           | Some (Start _ | Resume _) | None -> None)
        | _ -> None)
    in
    let%bind index =
      List.find_map atoms ~f:(function
        | A.Session_delta.Run_state_changed index
          when List.exists (A.Run_state.receipts index) ~f:(P.Run_receipt.equal admission)
               && Option.is_some (A.Run_state.find index admission.run_id)
               && Option.is_some (A.Run_state.goal_link index admission.run_id) ->
          Some index
        | _ -> None)
    in
    let%map catalog =
      List.find_map atoms ~f:(function
        | A.Session_delta.Goal_catalog_changed catalog
          when Option.exists
                 (A.Goal_catalog.find catalog (Request.goal_id arm.request))
                 ~f:(fun goal ->
                   Option.exists (G.episode goal) ~f:(fun episode ->
                     P.Run_receipt.equal admission (P.Goal_episode.admission episode))) ->
          Some catalog
        | _ -> None)
    in
    { Candidate.transaction; invocation; intent; index; catalog; admission }
  ;;

  let reached t env path =
    match t.arm with
    | None -> ()
    | Some arm ->
      let scan =
        Eio.Path.load Eio.Path.(Eio.Stdenv.fs env / path)
        |> Store.Journal_segment.scan_contents ~max_payload_length:(64 * 1024 * 1024)
        |> Existing.store_ok
      in
      (match List.last scan.entries with
       | Some frame when Int.equal (Store.Frame.flags frame.frame) 0 ->
         let transaction =
           Store.Transaction.decode (Store.Frame.payload frame.frame) |> Existing.store_ok
         in
         if P.Id.Session.equal transaction.session_id arm.session
         then (
           let atoms =
             A.Session_delta_document.decode ~limits:D.Limits.default transaction.delta
             |> document_ok
             |> A.Session_delta_document.value
             |> Goal_admission_fault_tests.atoms
           in
           Option.iter (candidate arm transaction atoms) ~f:(fun candidate ->
             t.arm <- None;
             t.candidate <- Some candidate;
             t.hits <- t.hits + 1;
             match t.cut with
             | Revoke_original revoke -> revoke ()
             | Unknown _ ->
               raise (Core_unix.Unix_error (EIO, "557 atomic native Start sync", path))))
       | Some _ | None -> ())
  ;;

  let wrap t env =
    let boundary =
      match t.cut with
      | Unknown boundary -> boundary
      | Revoke_original _ -> Crash_fault_io.After_sync
    in
    Crash_fault_io.wrap
      env
      ~boundary
      ~matches:(fun path ->
        String.is_substring path ~substring:"/journal/"
        && String.is_suffix path ~suffix:".log")
      ~reached:(reached t env)
  ;;

  let arm t (fixture : Native.accepted_start) =
    assert (Option.is_none t.arm);
    t.arm
    <- Some
         { Arm.session = fixture.before.identity.session_id
         ; invocation = fixture.invocation.context.id
         ; request = fixture.request
         ; principal = Intent.principal_id fixture.accepted
         }
  ;;

  let selected t =
    [%test_eq: int] 1 t.hits;
    Option.value_exn t.candidate
  ;;
end

let assert_candidate (fixture : Native.accepted_start) (candidate : Fault.Candidate.t) =
  let admission = candidate.admission in
  assert (P.Id.Principal.equal principal.id admission.principal_id);
  assert (P.Run_receipt.Kind.equal Admission admission.kind);
  assert (P.Run_source.equal (Request.source fixture.request) admission.source);
  assert (P.Idempotency_key.equal (Request.key fixture.request) admission.key);
  [%test_eq: string]
    (Request.digest fixture.request |> protocol_ok)
    admission.request_sha256;
  [%test_eq: int64] candidate.transaction.session_revision admission.session_revision;
  [%test_eq: int64] 0L admission.run_revision;
  assert (Native.same_invocation fixture.invocation candidate.invocation);
  let run = A.Run_state.find candidate.index admission.run_id |> Option.value_exn in
  let link = A.Run_state.goal_link candidate.index admission.run_id |> Option.value_exn in
  let goal =
    A.Goal_catalog.find candidate.catalog (Request.goal_id fixture.request)
    |> Option.value_exn
  in
  let episode = G.episode goal |> Option.value_exn in
  assert (P.Run.Mode.equal Workflow run.mode);
  assert (P.Run_source.equal admission.source run.source);
  assert (P.Id.Principal.equal admission.principal_id run.principal_id);
  [%test_eq: int64] 0L run.revision;
  (match run.lifecycle with
   | Admitted -> ()
   | Active | Waiting _ | Terminal _ -> failwith "atomic Start admission already executed");
  assert (List.is_empty run.owned_work);
  assert (P.Goal_episode.matches_run episode run);
  assert (P.Run_receipt.equal admission (P.Goal_episode.admission episode));
  assert (P.Goal_episode.equal episode (A.Run_goal_link.episode link));
  assert (P.Id.Goal.equal (Request.goal_id fixture.request) (A.Run_goal_link.goal_id link));
  [%test_eq: string]
    (A.Goal_native_start_association.intent fixture.request
     |> protocol_ok
     |> A.Goal_episode_admission.digest)
    (A.Run_goal_link.intent_sha256 link)
;;

let assert_no_execution providers current =
  [%test_eq: int] 0 !providers;
  [%test_eq: int] 0 (Native.user_input_count current);
  assert (Option.is_none current.A.Session_state.active_operation);
  let accounting = Goal.accounting current in
  [%test_eq: int64 * int64]
    (0L, 0L)
    ( A.Goal_accounting.admitted_attempts accounting
    , A.Goal_accounting.admitted_turns accounting );
  assert (List.is_empty (A.Goal_accounting.bindings accounting));
  assert (List.is_empty (A.Inference_ledger.rows current.inference_ledger))
;;

let attempted = function
  | Service.Outcome.Attempted attempt -> attempt
  | Retained _ -> failwith "new native Start returned historical DATA before attempting"
;;

let policy providers ports =
  let base =
    inference_policy ~default_model:"fixture-model" ~post_stream:(fun ~sw:_ ~inputs:_ ->
      incr providers;
      failwith "failed native Start admission dispatched a provider")
  in
  { base with
    runtime_inference_ports =
      (fun actor ->
        incr ports;
        base.runtime_inference_ports actor)
  }
;;

let assert_recovered request session (candidate : Fault.Candidate.t) current =
  let invocation, intent = Native.start_invocation current in
  assert (Native.same_invocation candidate.invocation invocation);
  (match Intent.phase intent with
   | Admitted value -> assert (P.Run_receipt.equal candidate.admission value.admission)
   | Staged | Accepted _ | Dispatch_claimed _ | Discarded _ | Rejected _ ->
     failwith "physical admitted Start frame was not retained");
  assert (Request.equal request (Intent.request intent));
  A.Goal_native_start_association.check_admission
    intent
    ~state:current
    ~session
    ~admission:candidate.admission
  |> protocol_ok
;;

let reopen root session request candidate providers ports policy =
  Eio_main.run (fun env ->
    Eio.Time.with_timeout_exn (Eio.Stdenv.clock env) 30. (fun () ->
      Eio.Switch.run (fun sw ->
        Mirage_crypto_rng_unix.use_default ();
        let daemon =
          Daemon.start
            ~sw
            ~env:(env :> Eio_unix.Stdenv.base)
            ~config:(config root root (Filename.concat root "parent.chatmd"))
            ~tool_dir:root
            ~home:root
            ~process_start_identity:None
            ~options:
              { Daemon.default_options with
                inference_policy = policy
              ; qualify_chatml_extensions = true
              }
            ()
          |> protocol_ok
        in
        protect_cleanup
          ~finally:(fun () -> Daemon.shutdown daemon |> protocol_ok)
          (fun () ->
             (* Fresh root and SAME actor locks, after the old host scope joined.
                Execute recovery may legitimately prepare its ordinary runtime;
                it cannot decode a native original or replay Start from DATA.
                No explicit-close flag substitutes for physical release. *)
             let loaded =
               Registry.load (Daemon.registry daemon) (P.Session_ref.session_id session)
               |> protocol_ok
             in
             Owner.drain_idle_moderator loaded.runtime |> protocol_ok |> ignore;
             let current =
               Actor.with_moderator_checkpoint loaded.actor (fun () ->
                 Actor.state loaded.actor)
               |> protocol_ok
             in
             assert_recovered request session candidate current;
             assert_no_execution providers current;
             assert (
               Option.is_none (Actor.select_native_goal_action loaded.actor |> protocol_ok));
             let frames = Existing.frames env loaded in
             let counts = !ports, !providers in
             let driver = Driver.create ~factory:(Daemon.factory daemon) in
             protect_cleanup
               ~finally:(fun () -> Driver.close driver)
               (fun () -> Driver.process_entry driver loaded |> protocol_ok);
             Existing.assert_same current loaded;
             [%test_eq: string list] frames (Existing.frames env loaded);
             [%test_eq: int * int] counts (!ports, !providers)))))
;;

exception Admission_failure_observed of unit ref

let unknown boundary =
  let root =
    Eio_main.run (fun env ->
      Mirage_crypto_rng_unix.use_default ();
      temporary_root env)
  in
  let providers = ref 0
  and ports = ref 0 in
  let policy = policy providers ports in
  let fault = Fault.create (Unknown boundary) in
  let retained = ref None in
  let primary = Admission_failure_observed (ref ()) in
  Exn.protect
    ~finally:(fun () ->
      Eio_main.run (fun env -> Eio.Path.rmtree Eio.Path.(Eio.Stdenv.fs env / root)))
    ~f:(fun () ->
      let escaped =
        match
          Native.with_accepted_start
            ~test_root:root
            ~wrap_env:(Fault.wrap fault)
            ~v1:false
            ~policy
            ~providers
            (fun _env _sw _root daemon entry fixture ->
               Fault.arm fault fixture;
               let ports_before = !ports in
               let attempt =
                 Service.start_native_episode
                   (Daemon.factory daemon)
                   ~entry
                   ~selection:fixture.selection
                 |> protocol_ok
                 |> attempted
               in
               let error =
                 match Service.Attempt.result attempt |> protocol_ok with
                 | Admission_uncertain error -> error
                 | Activated _
                 | Controlled _
                 | Settled _
                 | Retained _
                 | Activation_failed _ ->
                   failwith "uncertain native Start write invented certainty"
               in
               [%test_eq: P.Error.code] Persistence_error error.code;
               assert (Option.is_none (Service.Attempt.no_effects attempt));
               let candidate = Fault.selected fault in
               assert_candidate fixture candidate;
               let current = state entry in
               let _, intent = Native.start_invocation current in
               (match Intent.phase intent with
                | Dispatch_claimed _ -> ()
                | Staged | Accepted _ | Admitted _ | Discarded _ | Rejected _ ->
                  failwith "unknown native admission rewrote durable claim");
               assert_no_execution providers current;
               [%test_eq: int] ports_before !ports;
               assert (Option.is_none (G.episode (Goal.goal current)));
               retained := Some (fixture.session, fixture.request, candidate);
               raise primary)
        with
        | () -> failwith "native Start sync cut was not reached"
        | exception exn -> exn
      in
      if
        not
          (List.exists
             (Goal_control_publication_tests.failures escaped)
             ~f:(phys_equal primary))
      then raise escaped;
      let session, request, candidate = Option.value_exn !retained in
      (* Both cuts follow a complete append. Neither claims absence merely from
         missing sync/ACK; fresh physical recovery decides the retained DATA. *)
      reopen root session request candidate providers ports policy);
  [%test_eq: int] 1 fault.hits
;;

let revoked_original () =
  let live = ref true in
  let actor = Goal_resume_tests.guarded principal live in
  let fault = Fault.create (Revoke_original (fun () -> live := false)) in
  let providers = ref 0
  and ports = ref 0 in
  Native.with_accepted_start
    ~actor
    ~wrap_env:(Fault.wrap fault)
    ~v1:false
    ~policy:(policy providers ports)
    ~providers
    (fun env _sw _root daemon entry fixture ->
       Fault.arm fault fixture;
       let ports_before = !ports in
       let attempt =
         Service.start_native_episode
           (Daemon.factory daemon)
           ~entry
           ~selection:fixture.selection
         |> protocol_ok
         |> attempted
       in
       let admission, error =
         match Service.Attempt.result attempt |> protocol_ok with
         | Activation_failed { admission; error } ->
           [%test_eq: P.Error.code] Permission_denied error.code;
           assert (not error.retryable);
           admission, error
         | Activated _ | Controlled _ | Settled _ | Retained _ | Admission_uncertain _ ->
           failwith "post-ACK revocation lost known admission or invented handoff"
       in
       let candidate = Fault.selected fault in
       assert_candidate fixture candidate;
       assert (P.Run_receipt.equal candidate.admission admission);
       assert (Option.is_none (Service.Attempt.no_effects attempt));
       let current = state entry in
       assert_no_execution providers current;
       [%test_eq: int] ports_before !ports;
       Native.assert_admission_ack env entry fixture.invocation admission;
       assert_recovered fixture.request fixture.session candidate current;
       let frames = Existing.frames env entry in
       let counts = !ports, !providers in
       Actor.observe_native_goal_start_dispatch
         entry.actor
         ~selection:fixture.selection
         ~failure:(Some error)
       |> protocol_ok;
       (match
          Service.start_native_episode
            (Daemon.factory daemon)
            ~entry
            ~selection:fixture.selection
          |> protocol_ok
        with
        | Retained actual -> assert (P.Run_receipt.equal admission actual)
        | Attempted _ ->
          failwith "known native admission retried fresh source after revocation");
       Existing.assert_same current entry;
       [%test_eq: string list] frames (Existing.frames env entry);
       [%test_eq: int * int] counts (!ports, !providers))
;;

let%expect_test "native Start atomic admission uncertainty retains DATA without replay" =
  unknown Before_sync;
  unknown After_sync;
  print_endline
    "same actual Start/Run/Goal frame; missing ACK stays uncertain; physical recovery \
     never restores custody";
  [%expect
    {| same actual Start/Run/Goal frame; missing ACK stays uncertain; physical recovery never restores custody |}]
;;

let%expect_test "native Start known admission survives original revocation without input" =
  revoked_original ();
  print_endline
    "known native admission remains exact after original revocation; no input, provider \
     or replay";
  [%expect
    {| known native admission remains exact after original revocation; no input, provider or replay |}]
;;

let%expect_test "accepted native Start cannot reacquire a displaced runtime source" =
  let providers = ref 0
  and ports = ref 0 in
  Native.with_accepted_start
    ~v1:false
    ~policy:(policy providers ports)
    ~providers
    (fun env _sw _root daemon entry fixture ->
       let first_runtime =
         Owner.with_loaded_runtime entry.runtime (fun runtime -> Ok runtime)
         |> protocol_ok
       in
       let first_manager =
         Option.value_exn first_runtime.A.Runtime_builder.moderator_manager
       in
       Owner.unload_and_wait entry.runtime |> protocol_ok;
       assert (not (Owner.is_loaded entry.runtime));
       Owner.ensure_loaded_authenticated entry.runtime ~actor:original |> protocol_ok;
       Owner.with_loaded_runtime entry.runtime (fun runtime ->
         assert (not (phys_equal first_runtime runtime));
         let manager = Option.value_exn runtime.A.Runtime_builder.moderator_manager in
         assert (not (phys_equal first_manager manager));
         Ok ())
       |> protocol_ok;
       let current =
         Actor.with_moderator_checkpoint entry.actor (fun () -> Actor.state entry.actor)
         |> protocol_ok
       in
       Native.completed_callbacks current;
       assert_no_execution providers current;
       assert (List.is_empty (A.Run_state.runs (Option.value_exn current.run_state)));
       assert (Option.is_none (G.episode (Goal.goal current)));
       let _, retained = Native.start_invocation current in
       assert (Request.equal fixture.request (Intent.request retained));
       (match Intent.phase retained with
        | Accepted _ | Discarded _ | Rejected _ -> ()
        | Staged | Dispatch_claimed _ | Admitted _ ->
          failwith "source displacement invented a native Start dispatch");
       assert (Option.is_none (Actor.select_native_goal_action entry.actor |> protocol_ok));
       let frames = Existing.frames env entry in
       let counts = !ports, !providers in
       (match
          Service.start_native_episode
            (Daemon.factory daemon)
            ~entry
            ~selection:fixture.selection
        with
        | Error error ->
          (match error.P.Error.code with
           | Permission_denied | Conflict -> assert (not error.retryable)
           | _ ->
             raise_s [%sexp "unexpected displaced Start failure", (error : P.Error.t)])
        | Ok _ -> failwith "displaced native Start acquired a fresh source owner");
       Existing.assert_same current entry;
       [%test_eq: string list] frames (Existing.frames env entry);
       [%test_eq: int * int] counts (!ports, !providers));
  print_endline
    "actual runtime and Manager replacement retires old accepted Start custody; no \
     claim, Run, input or provider";
  [%expect
    {| actual runtime and Manager replacement retires old accepted Start custody; no claim, Run, input or provider |}]
;;

let%expect_test
    "native Start caller cancellation retains its ACK and joins source cleanup"
  =
  let providers = ref 0
  and ports = ref 0 in
  Native.with_accepted_start
    ~v1:false
    ~policy:(policy providers ports)
    ~providers
    (fun env _sw _root daemon entry fixture ->
       let admitted = ref None
       and prepared_source = ref None in
       let cancellation =
         Failure "native Start actual caller cancelled after admission ACK"
       in
       let ports_before = !ports in
       (match
          Eio.Cancel.sub (fun context ->
            Factory.with_native_goal_start_source
              (Daemon.factory daemon)
              ~entry
              ~selection:fixture.selection
              (fun _deferred prepared ->
                 prepared_source := Some prepared;
                 let intent =
                   A.Goal_native_start_association.intent fixture.request |> protocol_ok
                 in
                 (match
                    Actor.admit_held_goal_run
                      entry.actor
                      ~binding:(Owner.Prepared.binding prepared)
                      ~intent
                      ~session:fixture.session
                    |> protocol_ok
                  with
                  | Admitted activation ->
                    admitted
                    := Some
                         (Actor.Held_run.admission
                            (Actor.Goal_activation.held activation))
                  | Retained _ | Rejected _ | Uncertain _ | Activation_unavailable _ ->
                    failwith
                      "cancellation cut did not follow an actual native admission ACK");
                 Eio.Cancel.cancel context cancellation;
                 Eio.Fiber.check ();
                 failwith "cancelled caller reached native first input"))
        with
        | exception Eio.Cancel.Cancelled cause -> assert (phys_equal cause cancellation)
        | exception exn -> raise exn
        | Ok _ | Error _ -> failwith "native source cleanup replaced caller cancellation");
       let admission = Option.value_exn !admitted in
       let prepared = Option.value_exn !prepared_source in
       assert (Result.is_error (Owner.Prepared.check_basis prepared));
       let current = state entry in
       assert_no_execution providers current;
       [%test_eq: int] ports_before !ports;
       Native.assert_admission_ack env entry fixture.invocation admission;
       let _, intent = Native.start_invocation current in
       (match Intent.phase intent with
        | Admitted value -> assert (P.Run_receipt.equal admission value.admission)
        | Staged | Accepted _ | Dispatch_claimed _ | Discarded _ | Rejected _ ->
          failwith "caller cancellation discarded the known native admission");
       A.Goal_native_start_association.check_admission
         intent
         ~state:current
         ~session:fixture.session
         ~admission
       |> protocol_ok;
       assert (Option.is_none (Actor.select_native_goal_action entry.actor |> protocol_ok));
       let frames = Existing.frames env entry in
       (match
          Service.start_native_episode
            (Daemon.factory daemon)
            ~entry
            ~selection:fixture.selection
          |> protocol_ok
        with
        | Retained actual -> assert (P.Run_receipt.equal admission actual)
        | Attempted _ -> failwith "cancelled native input was replayed from its receipt");
       Existing.assert_same current entry;
       [%test_eq: string list] frames (Existing.frames env entry);
       [%test_eq: int] ports_before !ports;
       (* Borrow the SAME already-loaded runtime through its actual lease and
          source-exclusion checks. A new Headless issuance would correctly reject
          this retained nonterminal Run, independently of source cleanup. *)
       let acquired = ref false in
       Owner.with_loaded_runtime entry.runtime (fun runtime ->
         assert (Option.is_some runtime.A.Runtime_builder.moderator_manager);
         acquired := true;
         Ok ())
       |> protocol_ok;
       assert !acquired;
       let after_probe = state entry in
       Existing.assert_same current entry;
       [%test_eq: string list] frames (Existing.frames env entry);
       assert_no_execution providers after_probe;
       [%test_eq: int] ports_before !ports;
       let index = Option.value_exn after_probe.run_state in
       [%test_eq: int] 1 (List.length (A.Run_state.runs index));
       assert (List.exists (A.Run_state.receipts index) ~f:(P.Run_receipt.equal admission)));
  print_endline
    "same cancellation cause escapes joined source cleanup; exact admission remains; \
     input is never replayed and actual Owner is reusable";
  [%expect
    {| same cancellation cause escapes joined source cleanup; exact admission remains; input is never replayed and actual Owner is reusable |}]
;;
