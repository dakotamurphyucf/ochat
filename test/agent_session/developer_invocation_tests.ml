open Core
open Fixtures
module O = Agent_protocol.Execution_owner
module H = Agent_protocol.Host_invocation
module I = Agent_protocol.Invocation
module E = Agent_protocol.Error
module D = Agent_session.Developer_invocation_owner
module C = Chat_response.Tool_capability

let start = Agent_protocol.Timestamp.of_string "2026-10-13T00:00:00Z" |> protocol_ok
let future = Agent_protocol.Timestamp.of_string "2026-10-14T00:00:00Z" |> protocol_ok
let expired = Agent_protocol.Timestamp.of_string "2026-10-15T00:00:00Z" |> protocol_ok

let test_owner () =
  match
    O.developer
      ~host_id:(Agent_protocol.Id.Server.create ())
      ~run_id:(Agent_protocol.Id.Developer_run.create ())
      ~epoch:0
      ~principal_id:(Agent_protocol.Id.Principal.create ())
  with
  | Ok (Developer owner) -> owner
  | Ok (Conversation _) -> assert false
  | Error error -> raise_s [%sexp "owner", (error : E.t)]
;;

let%test_unit "owner constructors reject decoded malformed IDs and coordinates" =
  let module Id = Agent_protocol.Id in
  let bad_host = Id.Server.t_of_sexp (Sexp.Atom "foreign-host") in
  let bad_session = Id.Session.t_of_sexp (Sexp.Atom "foreign-session") in
  assert (
    Result.is_error
      (O.developer
         ~host_id:bad_host
         ~run_id:(Id.Developer_run.create ())
         ~epoch:0
         ~principal_id:(Id.Principal.create ())));
  assert (Result.is_error (O.conversation ~session_id:bad_session ~generation:0));
  assert (
    Result.is_error (O.conversation ~session_id:(Id.Session.create ()) ~generation:(-1)))
;;

let test_invocation owner selected ?parent_invocation ?(tool_name = "read_file") () =
  H.create_developer
    { H.id = Agent_protocol.Id.Invocation.create ()
    ; owner = O.Developer owner
    ; origin = Script
    ; parent_invocation
    ; parent_job = None
    ; tool_name
    ; implementation_revision = "revision"
    ; capability_fingerprint = C.fingerprint selected
    ; input = `Object []
    ; created_at = start
    ; deadline = Some future
    }
  |> protocol_ok
;;

type journal =
  { record : (int * H.t) option ref
  ; reads : int ref
  ; writes : int ref
  ; before_read_return : (unit -> unit) ref
  ; before_write_return : (int -> unit) ref
  ; reject_terminal : bool ref
  }

let journal () =
  { record = ref None
  ; reads = ref 0
  ; writes = ref 0
  ; before_read_return = ref (fun () -> ())
  ; before_write_return = ref (fun _ -> ())
  ; reject_terminal = ref false
  }
;;

let storage journal : D.storage =
  { read =
      (fun id ->
        Int.incr journal.reads;
        let snapshot = !(journal.record) in
        Option.iter snapshot ~f:(fun (_, value) ->
          assert (Agent_protocol.Id.Invocation.equal id (H.context value).id));
        !(journal.before_read_return) ();
        Ok snapshot)
  ; compare_and_set =
      (fun ~id ~expected_revision ~after ->
        assert (Agent_protocol.Id.Invocation.equal id (H.context after).id);
        if
          not
            (Option.equal
               Int.equal
               expected_revision
               (Option.map !(journal.record) ~f:fst))
        then Error (E.invalid_request "revision conflict")
        else if !(journal.reject_terminal) && Option.is_some expected_revision
        then Error (E.invalid_request "terminal storage unavailable")
        else (
          Int.incr journal.writes;
          let revision = !(journal.writes) in
          journal.record := Some (revision, after);
          !(journal.before_write_return) revision;
          Ok revision))
  }
;;

let test_actor ~owner ~selected ~journal ~current_valid ~now =
  D.create
    ~owner
    ~capture_fingerprint:
      (Chatml_source.Digest.of_contents "admitted capture" ~checkpoint:ignore)
    ~ceiling:selected
    ~selected:(fun () -> selected)
    ~current:(fun ~owner:_ ~capture_fingerprint:_ ->
      if !current_valid then Ok () else Error (E.invalid_request "run revoked"))
    ~now:(fun () -> !now)
    ~storage:(storage journal)
  |> protocol_ok
;;

let expect_no_callback actor invocation callback_count =
  match
    D.with_invocation actor ~parent:Root ~invocation (fun ~active:_ ~dispatched:_ ->
      Int.incr callback_count;
      Ok (I.Complete `Null))
  with
  | Error _ -> assert (Int.equal !callback_count 0)
  | Ok value ->
    (match H.status value with
     | Resolved (Fail _) -> assert (Int.equal !callback_count 0)
     | _ -> failwith "invalid currentness reached callback")
;;

let%test_unit "developer actor rechecks delayed read, CAS and current callback" =
  Eio_main.run (fun _env ->
    let selected = native_registry (ref 0) ~raises:false in
    List.iter [ `After_read; `After_dispatch; `During_current; `Deadline ] ~f:(fun mode ->
      let owner = test_owner () in
      let invocation = test_invocation owner selected () in
      let journal = journal () in
      let current_valid = ref true in
      let now = ref start in
      let actor = test_actor ~owner ~selected ~journal ~current_valid ~now in
      (match mode with
       | `After_read -> journal.before_read_return := fun () -> current_valid := false
       | `After_dispatch ->
         journal.before_write_return
         := fun revision -> if Int.equal revision 1 then current_valid := false
       | `During_current ->
         (* A successful current callback may yield and advance the clock. The
            final non-yielding deadline check must observe that new value. *)
         ()
       | `Deadline -> now := expired);
      let callback_count = ref 0 in
      (match mode with
       | `During_current ->
         let calls = ref 0 in
         let actor =
           D.create
             ~owner
             ~capture_fingerprint:
               (Chatml_source.Digest.of_contents "admitted capture" ~checkpoint:ignore)
             ~ceiling:selected
             ~selected:(fun () -> selected)
             ~current:(fun ~owner:_ ~capture_fingerprint:_ ->
               Int.incr calls;
               if !calls > 4 then now := expired;
               Ok ())
             ~now:(fun () -> !now)
             ~storage:(storage journal)
           |> protocol_ok
         in
         expect_no_callback actor invocation callback_count
       | `After_read | `After_dispatch | `Deadline ->
         expect_no_callback actor invocation callback_count);
      match mode, !(journal.record) with
      | (`After_dispatch | `During_current), Some (_, value) ->
        (match H.status value with
         | Resolved (Fail _) -> ()
         | _ -> failwith "post-CAS failure was not terminally recorded")
      | (`After_read | `Deadline), None -> ()
      | _ -> failwith "unexpected durable state"))
;;

let%test_unit "developer actor retains effects, terminal record and original exception" =
  Eio_main.run (fun _env ->
    let selected = native_registry (ref 0) ~raises:false in
    let owner = test_owner () in
    let invocation = test_invocation owner selected () in
    let journal = journal () in
    let actor =
      test_actor ~owner ~selected ~journal ~current_valid:(ref true) ~now:(ref start)
    in
    let effects = ref 0 in
    let original = Failure "original callback failure" in
    (match
       D.with_invocation actor ~parent:Root ~invocation (fun ~active:_ ~dispatched:_ ->
         Int.incr effects;
         raise original)
     with
     | exception error -> assert (phys_equal error original)
     | Ok _ | Error _ -> failwith "unexpected exception was swallowed");
    assert (Int.equal !effects 1);
    (match !(journal.record) with
     | Some (_, value) ->
       (match H.status value with
        | Resolved (Fail { code = "invocation.callback_failed"; _ }) -> ()
        | _ -> failwith "partial effect lacked terminal failure")
     | None -> failwith "missing durable record");
    match
      D.with_invocation actor ~parent:Root ~invocation (fun ~active:_ ~dispatched:_ ->
        Int.incr effects;
        Ok (I.Complete `Null))
    with
    | Error _ -> assert (Int.equal !effects 1)
    | Ok _ -> failwith "terminal invocation replayed")
;;

let%test_unit "uncertain terminal save leaves dispatching record without replay" =
  Eio_main.run (fun _env ->
    let selected = native_registry (ref 0) ~raises:false in
    let owner = test_owner () in
    let invocation = test_invocation owner selected () in
    let journal = journal () in
    journal.reject_terminal := true;
    let actor =
      test_actor ~owner ~selected ~journal ~current_valid:(ref true) ~now:(ref start)
    in
    let effects = ref 0 in
    let invoke () =
      D.with_invocation actor ~parent:Root ~invocation (fun ~active:_ ~dispatched:_ ->
        Int.incr effects;
        Ok (I.Complete `Null))
    in
    (match invoke () with
     | Error error -> assert (E.equal_code error.code Persistence_error)
     | Ok _ -> failwith "uncertain terminal save acknowledged");
    (match !(journal.record) with
     | Some (_, value) -> assert (I.equal_status (H.status value) Dispatching)
     | None -> failwith "missing dispatching record");
    assert (Result.is_error (invoke ()));
    assert (Int.equal !effects 1))
;;

let multi_storage () : D.storage =
  let records = Hashtbl.create (module Agent_protocol.Id.Invocation) in
  { read = (fun id -> Ok (Hashtbl.find records id))
  ; compare_and_set =
      (fun ~id ~expected_revision ~after ->
        let actual = Hashtbl.find records id |> Option.map ~f:fst in
        if not (Option.equal Int.equal expected_revision actual)
        then Error (E.invalid_request "revision conflict")
        else (
          let next = Option.value_map actual ~default:1 ~f:(fun value -> value + 1) in
          Hashtbl.set records ~key:id ~data:(next, after);
          Ok next))
  }
;;

let%test_unit "cancellation after an effect saves terminal state and retains its cause" =
  Eio_main.run (fun _env ->
    let selected = native_registry (ref 0) ~raises:false in
    let owner = test_owner () in
    let invocation = test_invocation owner selected () in
    let journal = journal () in
    let actor =
      test_actor ~owner ~selected ~journal ~current_valid:(ref true) ~now:(ref start)
    in
    let effects = ref 0 in
    let active = ref None in
    let saved_after_cancel = ref false in
    (journal.before_write_return
     := fun revision ->
          if Int.equal revision 2
          then (
            Eio.Fiber.yield ();
            saved_after_cancel := true));
    let reason = Failure "developer callback cancelled" in
    (match
       Eio.Cancel.sub (fun cancel ->
         D.with_invocation
           actor
           ~parent:Root
           ~invocation
           (fun ~active:claim ~dispatched:_ ->
              active := Some claim;
              Int.incr effects;
              Eio.Cancel.cancel cancel reason;
              Eio.Fiber.check ();
              Ok (I.Complete `Null)))
     with
     | exception Eio.Cancel.Cancelled actual -> assert (phys_equal actual reason)
     | Ok _ | Error _ -> failwith "cancellation disappeared");
    assert (Int.equal !effects 1 && !saved_after_cancel);
    (match !(journal.record) with
     | Some (_, value) ->
       (match H.status value with
        | Resolved (Cancelled _) -> ()
        | _ -> failwith "cancellation lacked terminal evidence")
     | None -> failwith "missing cancellation record");
    assert (
      Result.is_error
        (D.with_invocation actor ~parent:Root ~invocation (fun ~active:_ ~dispatched:_ ->
           Int.incr effects;
           Ok (I.Complete `Null))));
    assert (Int.equal !effects 1);
    let parent = Option.value_exn !active in
    let child =
      test_invocation owner selected ~parent_invocation:(H.context invocation).id ()
    in
    assert (Result.is_error (D.check_invocation actor ~parent:(Child parent) child)))
;;

let%test_unit "yielding current check cannot retain a completed parent claim" =
  Eio_main.run (fun _env ->
    let selected = native_registry (ref 0) ~raises:false in
    let owner = test_owner () in
    let entered, entered_u = Eio.Promise.create () in
    let release, release_u = Eio.Promise.create () in
    let child_check = ref false in
    let actor =
      D.create
        ~owner
        ~capture_fingerprint:
          (Chatml_source.Digest.of_contents "admitted capture" ~checkpoint:ignore)
        ~ceiling:selected
        ~selected:(fun () -> selected)
        ~current:(fun ~owner:_ ~capture_fingerprint:_ ->
          if !child_check
          then (
            child_check := false;
            Eio.Promise.resolve entered_u ();
            Eio.Promise.await release);
          Ok ())
        ~now:(fun () -> start)
        ~storage:(multi_storage ())
      |> protocol_ok
    in
    let parent = test_invocation owner selected () in
    let child =
      test_invocation owner selected ~parent_invocation:(H.context parent).id ()
    in
    let child_result = ref None in
    let child_callbacks = ref 0 in
    Eio.Switch.run (fun sw ->
      let _parent =
        D.with_invocation
          actor
          ~parent:Root
          ~invocation:parent
          (fun ~active ~dispatched:_ ->
             child_check := true;
             Eio.Fiber.fork ~sw (fun () ->
               child_result
               := Some
                    (D.with_invocation
                       actor
                       ~parent:(Child active)
                       ~invocation:child
                       (fun ~active:_ ~dispatched:_ ->
                          Int.incr child_callbacks;
                          Ok (I.Complete `Null))));
             Eio.Promise.await entered;
             Ok (I.Complete `Null))
        |> protocol_ok
      in
      Eio.Promise.resolve release_u ());
    (match !child_result with
     | Some (Error _) -> ()
     | Some (Ok _) | None ->
       failwith "completed parent was accepted after current yielded");
    assert (Int.equal !child_callbacks 0))
;;

let two_native_registry on_call =
  let make name =
    let module Definition = struct
      type input = string

      let name = name
      let description = None
      let type_ = "function"
      let parameters = `Object [ "type", `String "object" ]
      let input_of_string input = input
    end
    in
    Ochat_function.create_function
      (module Definition)
      (fun _ ->
         on_call name;
         Openai.Responses.Tool_output.Output.Text "output")
  in
  C.create
    ~owner:"developer-fixture"
    ~resource_fingerprint:(Chatmd_shell_spec.Source_ref.digest "resources")
    [ Chatmd_shell_spec.Source_ref.digest "read-revision", make "read_file"
    ; Chatmd_shell_spec.Source_ref.digest "write-revision", make "write_file"
    ]
  |> Result.map_error ~f:(fun error -> error.C.message)
  |> Result.ok_or_failwith
;;

let%test_unit "narrowed owned child resolves only its live parent selection" =
  Eio_main.run (fun _env ->
    let module N = Agent_session.Native_tool_invocation in
    let calls = ref [] in
    let on_call = ref (fun _ -> ()) in
    let initial =
      two_native_registry (fun name ->
        calls := name :: !calls;
        !on_call name)
    in
    let current_registry = ref initial in
    let owner = test_owner () in
    let actor =
      D.create
        ~owner
        ~capture_fingerprint:
          (Chatml_source.Digest.of_contents "admitted capture" ~checkpoint:ignore)
        ~ceiling:initial
        ~selected:(fun () -> !current_registry)
        ~current:(fun ~owner:_ ~capture_fingerprint:_ -> Ok ())
        ~now:(fun () -> start)
        ~storage:(multi_storage ())
      |> protocol_ok
    in
    let binding name =
      C.find initial ~name
      |> Result.map_error ~f:(fun error -> error.C.message)
      |> Result.ok_or_failwith
    in
    let reference name = C.reference (binding name) in
    let root =
      H.create_developer
        { (H.context (test_invocation owner initial ())) with
          implementation_revision = (reference "read_file").implementation_revision
        }
      |> protocol_ok
    in
    let authorize _ _ = Ok () in
    let prepare_output _ = Ok (`String "disclosed") in
    (on_call
     := fun name ->
          if String.equal name "read_file" && Int.equal (List.length !calls) 1
          then (
            let parent = N.borrow_owned () |> protocol_ok in
            let narrowed =
              N.select_owned_tools parent ~names:[ "read_file" ] |> protocol_ok
            in
            let selected = N.owned_borrowed_capabilities narrowed |> protocol_ok in
            let child name =
              let parent_context = H.context (N.owned_borrowed_invocation narrowed) in
              H.create_developer
                { parent_context with
                  id = Agent_protocol.Id.Invocation.create ()
                ; parent_invocation = Some parent_context.id
                ; tool_name = name
                ; implementation_revision = (reference name).implementation_revision
                ; capability_fingerprint = C.fingerprint selected
                }
              |> protocol_ok
            in
            let execute name ~authorize =
              N.run_owned_child
                ~parent:narrowed
                ~managed:None
                ~reference:(reference name)
                ~invocation:(child name)
                ~authorize
                ~prepare_output
              |> protocol_ok
              |> H.status
            in
            (match execute "read_file" ~authorize with
             | Resolved (Complete _) -> ()
             | _ -> failwith "selected child did not run");
            (match execute "write_file" ~authorize with
             | Resolved (Fail { code = "invocation.stale_binding"; _ }) -> ()
             | _ -> failwith "excluded child ran");
            (match
               execute "read_file" ~authorize:(fun _ _ ->
                 current_registry
                 := C.select initial ~names:[]
                    |> Result.map_error ~f:(fun error -> error.C.message)
                    |> Result.ok_or_failwith;
                 Ok ())
             with
             | Resolved (Fail { code = "invocation.stale_binding"; _ }) -> ()
             | _ -> failwith "revoked child ran");
            current_registry := initial;
            let replacement = two_native_registry (fun _ -> failwith "replacement ran") in
            match
              execute "read_file" ~authorize:(fun _ _ ->
                current_registry := replacement;
                Ok ())
            with
            | Resolved (Fail { code = "invocation.stale_binding"; _ }) -> ()
            | _ -> failwith "replaced child ran"));
    let resolved =
      N.run_owned_root
        ~owner:actor
        ~managed:None
        ~reference:(reference "read_file")
        ~invocation:root
        ~authorize
        ~prepare_output
      |> protocol_ok
    in
    (match H.status resolved with
     | Resolved (Complete _) -> ()
     | _ -> failwith "root failed");
    assert (List.equal String.equal (List.rev !calls) [ "read_file"; "read_file" ]))
;;

let%test_unit "developer custom native uses the shared string-input policy" =
  Eio_main.run (fun _env ->
    let module N = Agent_session.Native_tool_invocation in
    let calls = ref 0 in
    let selected = native_registry ~custom:true calls ~raises:false in
    let reference = List.hd_exn (C.references selected) in
    let owner = test_owner () in
    let actor =
      test_actor
        ~owner
        ~selected
        ~journal:(journal ())
        ~current_valid:(ref true)
        ~now:(ref start)
    in
    let invocation =
      H.create_developer
        { (H.context (test_invocation owner selected ())) with
          implementation_revision = reference.implementation_revision
        ; input = `String "{}"
        }
      |> protocol_ok
    in
    let resolved =
      N.run_owned_root
        ~owner:actor
        ~managed:None
        ~reference
        ~invocation
        ~authorize:(fun _ _ -> Ok ())
        ~prepare_output:(fun _ -> Ok (`String "disclosed"))
      |> protocol_ok
    in
    (match H.status resolved with
     | Resolved (Complete (`String "disclosed")) -> ()
     | _ -> failwith "custom native was not admitted");
    assert (Int.equal !calls 1))
;;

let%test_unit "managed admission preserves conversation Function and Custom routing" =
  with_temp_directory (fun env _ ->
    let module Managed = Chat_response.Managed_tool_registry in
    let base =
      C.create
        ~owner:"managed-parity"
        ~resource_fingerprint:(Chatmd_shell_spec.Source_ref.digest "resources")
        []
      |> Result.map_error ~f:(fun error -> error.C.message)
      |> Result.ok_or_failwith
    in
    let dir = Eio.Stdenv.cwd env in
    let source_loader =
      Source_loader.captured_filesystem
        ~root:dir
        ~sources:
          [ "input.json", {|{"type":"object"}|}; "output.json", {|{"type":"object"}|} ]
    in
    let elements =
      Prompt.Chat_markdown.parse_chat_inputs
        ~dir
        ~source_loader
        {|<script id="owned" language="chatml" kind="tool">
let run ctx input = Task.pure(`Complete(input))
</script>
<tool name="summary" type="chatml" script="owned" entrypoint="run" input_schema="input.json" output_schema="output.json"/>|}
    in
    let managed =
      Managed.prepare ~env ~owner:"managed-parity" ~capabilities:base elements
      |> Result.map_error ~f:(fun errors ->
        String.concat
          ~sep:"; "
          (List.map errors ~f:Chatmd_shell_spec.Diagnostic.to_string))
      |> Result.ok_or_failwith
    in
    let current = Managed.capabilities managed in
    let selected =
      C.select current ~names:[ "summary" ]
      |> Result.map_error ~f:(fun error -> error.C.message)
      |> Result.ok_or_failwith
    in
    let binding =
      C.find selected ~name:"summary"
      |> Result.map_error ~f:(fun error -> error.C.message)
      |> Result.ok_or_failwith
    in
    let reference = C.reference binding in
    let revision =
      Managed.execution_revision managed binding
      |> Result.map_error ~f:(fun error -> error.C.message)
      |> Result.ok_or_failwith
    in
    List.iter [ None; Some I.Function; Some I.Custom ] ~f:(fun kind ->
      let payload : I.payload_fingerprint =
        { sha256 = Chatmd_shell_spec.Source_ref.digest "{}"; byte_length = 2 }
      in
      let routing : I.routing option =
        Option.map kind ~f:(fun kind ->
          ({ kind
           ; original_name = "summary"
           ; original_payload = payload
           ; final_payload = payload
           ; canonical_payload = None
           ; preparation = Passed
           }
           : I.routing))
      in
      let original = invocation_fixture () in
      let conversation =
        I.create
          ?routing
          { original.context with
            id = Agent_protocol.Id.Invocation.create ()
          ; tool_name = "summary"
          ; implementation_revision = revision
          ; capability_fingerprint = C.fingerprint selected
          ; input = `Object []
          }
        |> protocol_ok
        |> I.dispatch
        |> protocol_ok
      in
      let envelope = H.of_conversation conversation |> protocol_ok in
      assert (Option.exists (H.to_conversation envelope) ~f:(phys_equal conversation));
      let legacy =
        Managed.admit managed ~current ~selected ~reference ~invocation:conversation
      in
      let owned =
        Managed.admit_owned managed ~current ~selected ~reference ~invocation:envelope
      in
      assert (Bool.equal (Result.is_ok legacy) (Result.is_ok owned));
      assert (
        Bool.equal
          (Result.is_ok legacy)
          (not (Option.equal I.equal_call_kind kind (Some I.Custom)))));
    let owner = test_owner () in
    let developer =
      H.create_developer
        { (H.context (test_invocation owner selected ())) with
          tool_name = "summary"
        ; implementation_revision = revision
        }
      |> protocol_ok
      |> H.dispatch
      |> protocol_ok
    in
    assert (
      Result.is_ok
        (Managed.admit_owned managed ~current ~selected ~reference ~invocation:developer)))
;;
