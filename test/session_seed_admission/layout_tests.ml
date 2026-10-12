open! Core
open Admission_tests
module Store = Agent_store
module Sessions = Store.Session_store

exception Initializer_failure

let with_store f =
  Journal_tests.with_directory (fun env root ->
    Eio.Switch.run (fun sw ->
      let store =
        Sessions.create
          ~env
          ~sw
          ~root:Eio.Path.(native_exn (root / "data"))
          ~server_id:(P.Id.Server.of_string "srv_seed_layout" |> ok)
          ~process_start_identity:None
          ~lock_nonce:"seed-layout"
        |> store_ok
      in
      Exn.protect
        ~finally:(fun () ->
          Eio.Cancel.protect (fun () -> Sessions.close store |> store_ok))
        ~f:(fun () -> f env sw store)))
;;

let staging env store transaction =
  Eio.Path.(
    env#fs
    / Store.Data_root.sessions_path (Sessions.data_root store)
    / (".creating-" ^ P.Id.Transaction.to_string transaction))
;;

let create store sw transaction initialize =
  Sessions.create_session_initialized
    store
    ~sw
    ~transaction_id:transaction
    ~actor_lock_nonce:"seed-layout-actor"
    ~initialize
;;

let is_absent path =
  match Eio.Path.kind ~follow:false path with
  | `Not_found -> true
  | _ -> false
;;

let is_directory path =
  match Eio.Path.kind ~follow:false path with
  | `Directory -> true
  | _ -> false
;;

let with_clock env now : Eio_unix.Stdenv.base =
  let module Clock = struct
    type t = unit
    type time = float

    let now () = now ()
    let sleep_until () at = Eio.Time.sleep_until env#clock at
  end
  in
  let clock = Eio.Resource.T ((), Eio.Time.Pi.clock (module Clock)) in
  object
    method clock = clock
    method mono_clock = env#mono_clock
    method domain_mgr = env#domain_mgr
    method stdin = env#stdin
    method stdout = env#stdout
    method stderr = env#stderr
    method net = env#net
    method process_mgr = env#process_mgr
    method fs = env#fs
    method cwd = env#cwd
    method secure_random = env#secure_random
    method debug = env#debug
    method backend_id = env#backend_id
  end
;;

let temporary_owner_invariant env store marker =
  let managed =
    Eio.Path.(env#fs / Store.Data_root.path (Sessions.data_root store) / "managed")
  in
  Eio.Path.mkdir ~perm:0o700 managed;
  let definition =
    Agent_session.Workspace_definition.create
      ~id:(P.Id.Workspace_definition.of_string "wsd_seed_temporary" |> ok)
      ~config_name:"seed temporary"
      ~source:
        (Temporary
           { location = System_tmp
           ; cleanup = Retain
           ; managed_root = Some (Eio.Path.native_exn managed)
           })
      ~access:Exclusive
      ~conflict_domain:None
      ~prompt_limits:[]
    |> store_ok
  in
  let instance_id = P.Id.Workspace_instance.create () in
  let path = Eio.Path.(managed / P.Id.Workspace_instance.to_string instance_id) in
  let resolve env =
    Agent_session.Workspace_resolver.resolve
      ~env
      ~instance_id
      ~session_directory:"unused"
      definition
  in
  (match resolve (with_clock env (fun () -> raise marker)) with
   | _ -> assert false
   | exception exn -> assert (phys_equal exn marker));
  assert (is_absent path);
  (match
     Eio.Cancel.sub (fun context ->
       resolve
         (with_clock env (fun () ->
            Eio.Cancel.cancel context marker;
            Eio.Fiber.check ();
            assert false)))
   with
   | _ -> assert false
   | exception Eio.Cancel.Cancelled reason -> assert (phys_equal reason marker));
  assert (is_absent path);
  Eio.Path.mkdir ~perm:0o700 path;
  Eio.Path.save ~create:(`Exclusive 0o600) Eio.Path.(path / "sentinel") "existing";
  assert (Result.is_error (resolve env));
  assert (String.equal (Eio.Path.load Eio.Path.(path / "sentinel")) "existing")
;;

let projection (state : State.t) =
  let summary = State.summary state in
  let entry =
    Store.Session_index.Entry.
      { session = summary
      ; runnable_job_count = 0
      ; deliverable_job_count = 0
      ; earliest_schedule_due = None
      ; owner_grace_deadline = None
      ; pending_initial_start = false
      ; archived = false
      ; lifecycle_revision = Store.Session_archive_record.Revision.zero
      ; admission = Automatic
      }
  in
  let metadata =
    Sessions.Metadata.
      { schema_version = Sessions.current_metadata_schema_version
      ; session = summary
      ; prompt_artifact = P.Id.Prompt_revision.to_string state.spec.prompt_revision_id
      ; workspace_identity = state.spec.workspace_instance.conflict_domain
      ; data_schema_version = State.current_schema_version
      }
  in
  Sessions.Initial_projection.create ~metadata ~entry |> store_ok
;;

let%test_unit "native workspace ownership follows actual one-shot layout publication" =
  with_store (fun env sw store ->
    let managed =
      Eio.Path.(
        env#fs / Store.Data_root.path (Sessions.data_root store) / "owned-managed")
    in
    Eio.Path.mkdir ~perm:0o700 managed;
    let definition =
      Agent_session.Workspace_definition.create
        ~id:(P.Id.Workspace_definition.of_string "wsd_owned_publication" |> ok)
        ~config_name:"owned publication"
        ~source:
          (Temporary
             { location = System_tmp
             ; cleanup = Retain
             ; managed_root = Some (Eio.Path.native_exn managed)
             })
        ~access:Exclusive
        ~conflict_domain:None
        ~prompt_limits:[]
      |> store_ok
    in
    List.iter [ false; true ] ~f:(fun after_rename ->
      let owned = ref None in
      let initial = ref None in
      let session_id = P.Id.Session.create () in
      let create_result =
        create store sw (P.Id.Transaction.create ()) (fun ~staging_directory ->
          let lease =
            Agent_session.Workspace_resolver.resolve_owned
              ~sw
              ~env
              ~physical_roots:[]
              ~instance_id:(P.Id.Workspace_instance.create ())
              ~session_directory:staging_directory
              definition
            |> store_ok
          in
          owned := Some lease;
          let original = Receipt_tests.created_state () in
          let state =
            { original with
              identity = { original.identity with session_id }
            ; spec =
                { original.spec with
                  workspace_instance =
                    Agent_session.Workspace_resolver.Owned.instance lease
                }
            }
          in
          let selected = projection state in
          initial := Some selected;
          (* Real native obstruction, with no production callback: metadata replace
           before rename, or actor lock acquisition after the actual rename. *)
          Eio.Path.mkdir
            ~perm:0o700
            Eio.Path.(
              env#fs
              / staging_directory
              / if after_rename then "actor.lock" else "metadata.sexp");
          Ok selected)
      in
      assert (Result.is_error create_result);
      let lease = Option.value_exn !owned in
      let selected = Option.value_exn !initial in
      [%test_eq: bool] after_rename (Sessions.Initial_projection.installed selected);
      let instance = Agent_session.Workspace_resolver.Owned.instance lease in
      let path = Eio.Path.(env#fs / instance.canonical_root.native_path) in
      if Sessions.Initial_projection.installed selected
      then Agent_session.Workspace_resolver.Owned.transfer lease;
      Agent_session.Workspace_resolver.Owned.close lease |> store_ok;
      [%test_eq: bool] after_rename (is_directory path);
      if after_rename
      then (
        let final =
          Eio.Path.(
            env#fs / Store.Data_root.session_path (Sessions.data_root store) session_id)
        in
        assert (is_directory final);
        let transaction = P.Id.Transaction.create () in
        assert (
          Result.is_error
            (create store sw transaction (fun ~staging_directory:_ -> Ok selected)));
        assert (is_absent (staging env store transaction));
        assert (Sessions.Initial_projection.installed selected);
        assert (is_directory final))))
;;

let%expect_test "creation owns only acquired staging and preserves installed evidence" =
  with_store (fun env sw store ->
    let marker = Initializer_failure in
    temporary_owner_invariant env store marker;
    let transaction = P.Id.Transaction.create () in
    (match create store sw transaction (fun ~staging_directory:_ -> raise marker) with
     | _ -> assert false
     | exception exn -> assert (phys_equal exn marker));
    assert (is_absent (staging env store transaction));
    let transaction = P.Id.Transaction.create () in
    (match
       Eio.Cancel.sub (fun context ->
         create store sw transaction (fun ~staging_directory:_ ->
           Eio.Cancel.cancel context marker;
           Eio.Fiber.check ();
           assert false))
     with
     | _ -> assert false
     | exception Eio.Cancel.Cancelled reason -> assert (phys_equal reason marker));
    assert (is_absent (staging env store transaction));
    let transaction = P.Id.Transaction.create () in
    let existing = staging env store transaction in
    Eio.Path.mkdir ~perm:0o700 existing;
    Eio.Path.save ~create:(`Exclusive 0o600) Eio.Path.(existing / "sentinel") "existing";
    assert (
      Result.is_error
        (create store sw transaction (fun ~staging_directory:_ -> assert false)));
    assert (String.equal (Eio.Path.load Eio.Path.(existing / "sentinel")) "existing");
    let state = Receipt_tests.created_state () in
    let summary = State.summary state in
    let entry =
      Store.Session_index.Entry.
        { session = summary
        ; runnable_job_count = 0
        ; deliverable_job_count = 0
        ; earliest_schedule_due = None
        ; owner_grace_deadline = None
        ; pending_initial_start = false
        ; archived = false
        ; lifecycle_revision = Store.Session_archive_record.Revision.zero
        ; admission = Automatic
        }
    in
    let metadata =
      Sessions.Metadata.
        { schema_version = Sessions.current_metadata_schema_version
        ; session = summary
        ; prompt_artifact = P.Id.Prompt_revision.to_string state.spec.prompt_revision_id
        ; workspace_identity = state.spec.workspace_instance.conflict_domain
        ; data_schema_version = State.current_schema_version
        }
    in
    let transaction = P.Id.Transaction.create () in
    let result =
      create store sw transaction (fun ~staging_directory ->
        (* An actual storage obstruction makes actor-lock acquisition fail AFTER
         the directory rename, without any production fault callback. *)
        Eio.Path.mkdir ~perm:0o700 Eio.Path.(env#fs / staging_directory / "actor.lock");
        Sessions.Initial_projection.create ~metadata ~entry)
    in
    assert (Result.is_error result);
    assert (is_absent (staging env store transaction));
    let final = Store.Data_root.session_path (Sessions.data_root store) summary.id in
    assert (is_directory Eio.Path.(env#fs / final));
    assert (
      String.is_substring
        (Eio.Path.load Eio.Path.(env#fs / final / "metadata.sexp"))
        ~substring:(P.Id.Session.to_string summary.id));
    print_endline
      "throw/cancel clean owned staging; pre-existing staging and published identity \
       survive");
  [%expect
    {| throw/cancel clean owned staging; pre-existing staging and published identity survive |}]
;;
