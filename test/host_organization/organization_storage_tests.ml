open Core
open Agent_server_test_support
module P = Agent_protocol
module Store = Agent_store
module S = Store.Organization_state
module D = Document_schema

let ok = function
  | Ok value -> value
  | Error _ -> failwith "storage fixture rejected"
;;

let host = P.Id.Server.of_string "srv_organization_storage" |> ok
let now = P.Timestamp.of_string "2026-10-08T12:00:00Z" |> ok

let owner =
  P.Principal.create
    ~id:(P.Id.Principal.of_string "pri_storage_owner" |> ok)
    ~authentication_kind:"test"
    ~scopes:(P.Scope.Set.of_list [ View_organization; Manage_organization ])
    ~attributes:[]
  |> ok
;;

let create_request =
  P.Organization_request.Create.
    { host_id = host
    ; name = P.Organization_group.Name.create "Retained" |> ok
    ; idempotency_key = P.Idempotency_key.of_string "create" |> ok
    }
;;

let mutation = S.Mutation.Create_project create_request

let audit =
  Store.Idempotency_store.Command_audit.
    { key =
        { principal_id = owner.id
        ; session_id = None
        ; method_name = "project.create"
        ; idempotency_key = create_request.idempotency_key
        }
    ; request_digest = S.request_digest mutation |> ok
    ; protected_record = false
    }
;;

let with_root f =
  Eio_main.run (fun env ->
    Mirage_crypto_rng_unix.use_default ();
    let temporary = temporary_root env in
    Exn.protect
      ~finally:(fun () ->
        Eio.Path.rmtree ~missing_ok:true Eio.Path.(Eio.Stdenv.fs env / temporary))
      ~f:(fun () ->
        Eio.Switch.run (fun sw -> f env sw (Filename.concat temporary "data"))))
;;

let create env sw root =
  Store.Session_store.create
    ~env
    ~sw
    ~root
    ~server_id:host
    ~process_start_identity:None
    ~lock_nonce:"organization-create"
  |> ok
;;

let reopen env sw root =
  Store.Session_store.open_existing
    ~env
    ~sw
    ~root
    ~process_start_identity:None
    ~lock_nonce:"organization-reopen"
;;

let%expect_test
    "root1 upgrade preserves authority and unknowns; root2 missing authority rejects"
  =
  with_root (fun env sw root ->
    let store = create env sw root in
    let organizations = Store.Session_store.organizations store in
    let id = P.Id.Project.of_string "prj_retained_storage" |> ok in
    Store.Organization_store.mutate
      organizations
      ~principal:owner
      ~audit
      ~now
      ~candidate:(Some (S.Candidate.Project id))
      mutation
    |> ok
    |> ignore;
    Store.Session_store.close store |> ok;
    let schema = Eio.Path.(Eio.Stdenv.fs env / root / "schema.sexp") in
    let organization =
      Eio.Path.(Eio.Stdenv.fs env / root / "indexes" / "organization.json")
    in
    let original_organization = Eio.Path.load organization in
    let document =
      D.Document.decode ~limits:Store.Store_schema_document.limits (Eio.Path.load schema)
      |> ok
    in
    let original_created_at =
      D.Json.field (D.Document.payload document) ~name:"created_at"
    in
    let legacy =
      match D.Document.json document with
      | `Object fields ->
        `Object
          (("future_root", `String "keep")
           :: List.Assoc.add fields ~equal:String.equal "schema_version" (`Number "1"))
      | _ -> failwith "schema object"
    in
    Eio.Path.save ~create:(`Or_truncate 0o600) schema (Jsonaf.to_string legacy);
    let store = reopen env sw root |> ok in
    let current =
      Store.Organization_store.snapshot_checked (Store.Session_store.organizations store)
      |> ok
    in
    print_s
      [%sexp
        (List.length (S.projects current) : int), (List.length (S.receipts current) : int)];
    print_s
      [%sexp (String.equal original_organization (Eio.Path.load organization) : bool)];
    let upgraded =
      D.Document.decode ~limits:Store.Store_schema_document.limits (Eio.Path.load schema)
      |> ok
    in
    let created_equal =
      match
        original_created_at, D.Json.field (D.Document.payload upgraded) ~name:"created_at"
      with
      | Value before, Value after -> D.Json.equal before after
      | _ -> false
    in
    print_s
      [%sexp
        (D.Document.version upgraded : int)
      , (created_equal : bool)
      , (String.is_substring (D.Document.to_string upgraded) ~substring:"future_root"
         : bool)];
    Store.Session_store.close store |> ok;
    let schema_bytes = Eio.Path.load schema in
    Eio.Path.unlink organization;
    print_s
      [%sexp
        (Result.is_error (reopen env sw root) : bool)
      , (String.equal schema_bytes (Eio.Path.load schema) : bool)];
    print_s [%sexp (Result.is_error (reopen env sw root) : bool)]);
  [%expect
    {|
    (1 1)
    true
    (2 true true)
    (true true)
    true
    |}]
;;

let%expect_test
    "failed publication closes authority until validated reopen without partial mutation"
  =
  with_root (fun env sw root ->
    let store = create env sw root in
    let organizations = Store.Session_store.organizations store in
    let organization =
      Eio.Path.(Eio.Stdenv.fs env / root / "indexes" / "organization.json")
    in
    let backup =
      Eio.Path.(Eio.Stdenv.fs env / root / "indexes" / "organization.before")
    in
    Eio.Path.rename organization backup;
    Eio.Path.mkdir ~perm:0o700 organization;
    let result =
      Store.Organization_store.mutate
        organizations
        ~principal:owner
        ~audit
        ~now
        ~candidate:
          (Some (S.Candidate.Project (P.Id.Project.of_string "prj_failed_storage" |> ok)))
        mutation
    in
    print_s
      [%sexp
        (Result.is_error result : bool)
      , (Result.is_error (Store.Organization_store.snapshot_checked organizations) : bool)];
    Eio.Path.rmtree organization;
    Eio.Path.rename backup organization;
    print_s
      [%sexp
        (Result.is_error (Store.Organization_store.snapshot_checked organizations) : bool)];
    Store.Session_store.close store |> ok;
    let store = reopen env sw root |> ok in
    let state =
      Store.Organization_store.snapshot_checked (Store.Session_store.organizations store)
      |> ok
    in
    print_s
      [%sexp
        (List.length (S.projects state) : int), (List.length (S.receipts state) : int)];
    Store.Session_store.close store |> ok);
  [%expect
    {|
    (true true)
    true
    (0 0)
    |}]
;;

module Startup_fault = struct
  type boundary =
    | Organization_rename
    | Organization_directory_sync
    | Schema_rename
    | Schema_directory_sync
    | Release_only

  type failure =
    | Timeout
    | Unexpected
    | Cancelled

  type t =
    { boundary : boundary
    ; failure : failure
    ; secondary_timeout : bool
    }

  exception Primary_failure

  let raise_failure = function
    | Timeout -> raise Eio.Time.Timeout
    | Unexpected -> raise Primary_failure
    | Cancelled -> raise (Eio.Cancel.Cancelled Primary_failure)
  ;;

  let wrap env ~root ~armed ~secondary ~schema_installed =
    let reached selected =
      armed := None;
      secondary := selected.secondary_timeout;
      raise_failure selected.failure
    in
    let wrap_lock (Eio.Resource.T (file, handler)) =
      let module Original = (val Eio.Resource.get handler Eio.File.Pi.Write) in
      let module File = struct
        include Original

        let truncate file size =
          Original.truncate file size;
          if !secondary
          then (
            secondary := false;
            raise Eio.Time.Timeout)
        ;;
      end
      in
      Eio.Resource.T
        ( file
        , Eio.Resource.handler
            (H (Eio.File.Pi.Write, (module File)) :: Eio.Resource.bindings handler) )
    in
    let rec wrap_directory : 'a. ([> `Dir ] as 'a) Eio.Resource.t -> 'a Eio.Resource.t =
      fun (Eio.Resource.T (native, handler) as native_directory) ->
      let module Original = (val Eio.Resource.get handler Eio.Fs.Pi.Dir) in
      let module Directory = struct
        include Original

        let open_dir directory ~sw path =
          wrap_directory (Original.open_dir directory ~sw path)
        ;;

        let rename directory source _destination target =
          Original.rename directory source native_directory target;
          let basename = Filename.basename target in
          if String.equal basename "schema.sexp" then schema_installed := true;
          match !armed with
          | Some ({ boundary = Organization_rename; _ } as selected)
            when String.equal basename "organization.json" -> reached selected
          | Some ({ boundary = Schema_rename; _ } as selected)
            when String.equal basename "schema.sexp" -> reached selected
          | Some _ | None -> ()
        ;;

        let open_in directory ~sw path =
          let file = Original.open_in directory ~sw path in
          (match !armed with
           | Some { boundary = Release_only; _ }
             when String.equal (Filename.basename path) "schema.sexp" ->
             armed := None;
             secondary := true
           | Some ({ boundary = Organization_directory_sync; _ } as selected)
             when String.equal path (Filename.concat (Filename.concat root "indexes") ".")
             ->
             Store.Durable_file.sync_directory ~env ~path:(Filename.concat root "indexes")
             |> ok;
             reached selected
           | Some ({ boundary = Schema_directory_sync; _ } as selected)
             when String.equal path (Filename.concat root ".") && !schema_installed ->
             (* Directory syncing uses the native descriptor directly. Execute
                the real sync through the original capability before injecting
                lost acknowledgement; a File.sync wrapper would never run. *)
             Store.Durable_file.sync_directory ~env ~path:root |> ok;
             reached selected
           | Some _ | None -> ());
          file
        ;;

        let open_out directory ~sw ~append ~create path =
          let file = Original.open_out directory ~sw ~append ~create path in
          if String.equal (Filename.basename path) "daemon.lock"
          then wrap_lock file
          else file
        ;;
      end
      in
      Eio.Resource.T
        ( native
        , Eio.Resource.handler
            (H (Eio.Fs.Pi.Dir, (module Directory)) :: Eio.Resource.bindings handler) )
    in
    let directory, path = Eio.Stdenv.fs env in
    let fs = wrap_directory directory, path in
    object
      method fs = fs
      method cwd = env#cwd
      method stdin = env#stdin
      method stdout = env#stdout
      method stderr = env#stderr
      method net = env#net
      method domain_mgr = env#domain_mgr
      method process_mgr = env#process_mgr
      method clock = env#clock
      method mono_clock = env#mono_clock
      method secure_random = env#secure_random
      method debug = env#debug
      method backend_id = env#backend_id
    end
  ;;
end

let%expect_test "root upgrade faults release ownership in the same live switch" =
  let cases =
    [ Startup_fault.Organization_rename, Startup_fault.Timeout, false, 1
    ; Organization_directory_sync, Timeout, false, 1
    ; Schema_rename, Timeout, false, 2
    ; Schema_directory_sync, Timeout, false, 2
    ; Schema_rename, Unexpected, true, 2
    ; Organization_rename, Cancelled, true, 1
    ]
  in
  List.iter cases ~f:(fun (boundary, failure, secondary_timeout, installed_version) ->
    with_root (fun native_env sw root ->
      let store = create native_env sw root in
      Store.Session_store.close store |> ok;
      let schema = Eio.Path.(Eio.Stdenv.fs native_env / root / "schema.sexp") in
      let organization =
        Eio.Path.(Eio.Stdenv.fs native_env / root / "indexes" / "organization.json")
      in
      let current =
        D.Document.decode
          ~limits:Store.Store_schema_document.limits
          (Eio.Path.load schema)
        |> ok
      in
      let original_created =
        D.Json.field (D.Document.payload current) ~name:"created_at"
      in
      let legacy =
        match D.Document.json current with
        | `Object fields ->
          `Object
            (("future_root", `String "retain")
             :: List.Assoc.add fields ~equal:String.equal "schema_version" (`Number "1"))
        | _ -> assert false
      in
      Eio.Path.save ~create:(`Or_truncate 0o600) schema (Jsonaf.to_string legacy);
      Eio.Path.unlink organization;
      let armed = ref (Some Startup_fault.{ boundary; failure; secondary_timeout }) in
      let secondary = ref false
      and schema_installed = ref false in
      let env = Startup_fault.wrap native_env ~root ~armed ~secondary ~schema_installed in
      let caught =
        try
          ignore
            (reopen env sw root : (Store.Session_store.t, Store.Store_error.t) result);
          false
        with
        | Eio.Time.Timeout ->
          (match failure with
           | Timeout -> true
           | Unexpected | Cancelled -> false)
        | Startup_fault.Primary_failure ->
          (match failure with
           | Unexpected -> true
           | Timeout | Cancelled -> false)
        | Eio.Cancel.Cancelled Startup_fault.Primary_failure ->
          (match failure with
           | Cancelled -> true
           | Timeout | Unexpected -> false)
      in
      assert caught;
      assert (Option.is_none !armed);
      assert (not !secondary);
      let installed =
        D.Document.decode
          ~limits:Store.Store_schema_document.limits
          (Eio.Path.load schema)
        |> ok
      in
      assert (Int.equal (D.Document.version installed) installed_version);
      let installed_authority = Eio.Path.load organization in
      let reopened = reopen native_env sw root |> ok in
      assert (String.equal installed_authority (Eio.Path.load organization));
      let upgraded =
        D.Document.decode
          ~limits:Store.Store_schema_document.limits
          (Eio.Path.load schema)
        |> ok
      in
      assert (Int.equal (D.Document.version upgraded) 2);
      assert (String.is_substring (D.Document.to_string upgraded) ~substring:"future_root");
      (match
         original_created, D.Json.field (D.Document.payload upgraded) ~name:"created_at"
       with
       | Value before, Value after -> assert (D.Json.equal before after)
       | _ -> assert false);
      let state =
        Store.Organization_store.snapshot_checked
          (Store.Session_store.organizations reopened)
        |> ok
      in
      assert (List.is_empty (S.projects state));
      assert (List.is_empty (S.receipts state));
      Store.Session_store.close reopened |> ok));
  print_endline
    "six real publication cuts preserved authority and primary exceptions; same-switch \
     reopen succeeded";
  [%expect
    {|
    six real publication cuts preserved authority and primary exceptions; same-switch reopen succeeded
    |}]
;;

let%expect_test "migration preserves primary exception over lock cleanup failure" =
  with_root (fun native_env sw root ->
    let store = create native_env sw root in
    Store.Session_store.close store |> ok;
    let schema = Eio.Path.(Eio.Stdenv.fs native_env / root / "schema.sexp") in
    let organization =
      Eio.Path.(Eio.Stdenv.fs native_env / root / "indexes" / "organization.json")
    in
    let current =
      D.Document.decode ~limits:Store.Store_schema_document.limits (Eio.Path.load schema)
      |> ok
    in
    let legacy =
      match D.Document.json current with
      | `Object fields ->
        `Object
          (("future_migration", `String "retain")
           :: List.Assoc.add fields ~equal:String.equal "schema_version" (`Number "1"))
      | _ -> assert false
    in
    Eio.Path.save ~create:(`Or_truncate 0o600) schema (Jsonaf.to_string legacy);
    Eio.Path.unlink organization;
    let armed =
      ref
        (Some
           Startup_fault.
             { boundary = Schema_rename; failure = Unexpected; secondary_timeout = true })
    in
    let secondary = ref false
    and schema_installed = ref false in
    let env = Startup_fault.wrap native_env ~root ~armed ~secondary ~schema_installed in
    let caught =
      try
        ignore
          (Store.Migration.run
             ~env
             ~sw
             ~root
             ~server_id:host
             ~process_start_identity:None
             ~lock_nonce:"migration-fault"
             ~mode:Apply
           : (Store.Migration.plan, Store.Store_error.t) result);
        false
      with
      | Startup_fault.Primary_failure -> true
    in
    assert caught;
    assert (Option.is_none !armed);
    assert (not !secondary);
    let authority_bytes = Eio.Path.load organization in
    let reopened = reopen native_env sw root |> ok in
    assert (String.equal authority_bytes (Eio.Path.load organization));
    let installed =
      D.Document.decode ~limits:Store.Store_schema_document.limits (Eio.Path.load schema)
      |> ok
    in
    assert (Int.equal (D.Document.version installed) 2);
    assert (
      String.is_substring (D.Document.to_string installed) ~substring:"future_migration");
    Store.Session_store.close reopened |> ok);
  print_endline
    "original migration exception survived secondary cleanup Timeout; same-switch reopen \
     succeeded";
  [%expect
    {|
    original migration exception survived secondary cleanup Timeout; same-switch reopen succeeded
    |}]
;;

let%expect_test "successful migration reports exceptional release failure" =
  with_root (fun native_env sw root ->
    let store = create native_env sw root in
    Store.Session_store.close store |> ok;
    let armed =
      ref
        (Some
           Startup_fault.
             { boundary = Release_only; failure = Timeout; secondary_timeout = true })
    in
    let secondary = ref false
    and schema_installed = ref false in
    let env = Startup_fault.wrap native_env ~root ~armed ~secondary ~schema_installed in
    let caught =
      try
        ignore
          (Store.Migration.run
             ~env
             ~sw
             ~root
             ~server_id:host
             ~process_start_identity:None
             ~lock_nonce:"migration-release-fault"
             ~mode:Validate_only
           : (Store.Migration.plan, Store.Store_error.t) result);
        false
      with
      | Eio.Time.Timeout -> true
    in
    assert caught;
    assert (Option.is_none !armed);
    assert (not !secondary);
    let reopened = reopen native_env sw root |> ok in
    Store.Session_store.close reopened |> ok);
  print_endline
    "successful inspection surfaced release Timeout and released the owned lock";
  [%expect
    {|
    successful inspection surfaced release Timeout and released the owned lock
    |}]
;;
