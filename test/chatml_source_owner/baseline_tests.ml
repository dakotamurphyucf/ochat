open Core
open Support
module B = Agent_session.Prompt_project_baseline
module Store = Agent_session.Prompt_project_baseline_store
module Revision = Agent_session.Prompt_revision
module A = Agent_store.Prompt_artifact_store
module CM = Prompt.Chat_markdown
module C = Chatml_prompt_programs
module Plan = Chat_response.Script_contract_plan
module Fault = Crash_fault_io

let limits ?(definitions = 8) ?(references = 16) ?(bytes = 100_000) ?(work = 1_000_000) ()
  =
  let document =
    Document_schema.Limits.create
      ~max_bytes:bytes
      ~max_depth:128
      ~max_fields:100_000
      ~max_nodes:1_000_000
    |> ok
  in
  B.Limits.create
    ~max_definitions:definitions
    ~max_references:references
    ~max_identity_bytes:4096
    ~max_work_steps:work
    ~document
  |> Or_error.ok_exn
;;

let with_success f =
  with_workspace (fun env parent root ->
    Script_tests.populate root;
    let definition = definition root in
    let artifacts =
      A.create ~env ~root:(Eio.Path.native_exn Eio.Path.(parent / "artifacts")) |> ok
    in
    Eio.Switch.run (fun sw ->
      let owner = open_owner sw root definition in
      Owner.with_scope owner ~f:(fun scope ->
        let captured = capture env scope definition |> ok in
        let actual = Owner.Capture.value captured in
        let parsed = Builder.Capture.root actual in
        let plan = Script_tests.plan parsed in
        let custody = Script_tests.prepare env scope captured [ plan ] |> ok in
        let prepared =
          B.Prepared.create
            custody
            ~capture:actual
            ~plans:[ plan ]
            ~limits:(limits ())
            ~checkpoint:ignore
          |> ok
        in
        let revision =
          Builder.build_captured
            ~capture:actual
            ~source_receipt:(receipt definition)
            ~prepare_programs:(fun received ->
              assert (phys_equal received actual);
              Ok custody)
            ~artifact_store:artifacts
            ~transaction_id:(Agent_protocol.Id.Transaction.create ())
            ~created_at:(Agent_protocol.Timestamp.of_string "2026-10-11T12:00:00Z" |> ok)
            definition
          |> ok
        in
        let success =
          B.Success.finalize prepared ~revision ~limits:(limits ()) ~checkpoint:ignore
          |> ok
        in
        f env parent root definition plan artifacts revision success;
        Ok ())
      |> ok))
;;

let document success =
  B.Document.empty ~limits:(limits ())
  |> ok
  |> B.Document.replace_successes
       ~entries:[ success ]
       ~limits:(limits ())
       ~checkpoint:ignore
  |> ok
;;

let bytes document =
  B.Document.encode document ~limits:(limits ()) ~checkpoint:ignore |> ok
;;

let basename = "script-project-baselines.json"
let read directory = Eio.Path.load Eio.Path.(directory / basename)

let write directory bytes =
  Agent_store.Durable_file.replace_in
    ~directory
    ~basename
    ~durability:Flush_file_and_directory
    bytes
  |> ok
;;

let error result f =
  match result with
  | Error problem -> assert (f problem)
  | Ok _ -> failwith "invalid baseline accepted"
;;

let%expect_test "last successful exact project selections survive host restart" =
  with_success (fun _env parent root definition plan artifacts revision success ->
    let directory = Eio.Path.(parent / "baseline") in
    Eio.Path.mkdir ~perm:0o700 directory;
    Eio.Path.with_open_dir directory (fun directory ->
      Eio.Switch.run (fun sw ->
        let owner =
          Store.create_new directory ~sw ~limits:(limits ()) ~checkpoint:ignore |> ok
        in
        let initial = Store.snapshot owner ~checkpoint:ignore |> ok in
        (match
           Store.Snapshot.previous
             initial
             ~receipt:(receipt definition)
             ~checkpoint:ignore
           |> ok
         with
         | First_admission -> ()
         | Established _ | Untracked _ -> failwith "new owned root not authoritative");
        let candidate =
          Store.prepare owner ~snapshot:initial ~successes:[ success ] ~checkpoint:ignore
          |> ok
        in
        ignore (Store.publish owner candidate ~checkpoint:ignore |> ok : Store.Snapshot.t));
      Eio.Path.rmtree Eio.Path.(root / "pkg");
      Eio.Path.unlink Eio.Path.(root / "root.chatmd");
      Eio.Switch.run (fun sw ->
        let owner =
          Store.open_existing directory ~sw ~limits:(limits ()) ~checkpoint:ignore |> ok
        in
        let snapshot = Store.snapshot owner ~checkpoint:ignore |> ok in
        [%test_eq: int option] (Store.Snapshot.generation snapshot) (Some 1);
        [%test_eq: int] (List.length (Store.Snapshot.protected_revisions snapshot)) 1;
        let entry =
          match
            Store.Snapshot.previous
              snapshot
              ~receipt:(receipt definition)
              ~checkpoint:ignore
            |> ok
          with
          | Established entry -> entry
          | First_admission | Untracked _ -> failwith "history lost"
        in
        let artifact = A.load artifacts (Revision.id revision) |> ok in
        let prior =
          B.Entry.reconcile
            entry
            ~artifact
            ~custody_limits:(Script_tests.custody_limits ())
            ~limits:(limits ())
            ~checkpoint:ignore
          |> ok
        in
        List.iter (Plan.programs plan) ~f:(fun program ->
          let found =
            B.Entry.Prior.find
              prior
              ~slot:(C.Binding.slot (Plan.Program.binding program))
              ~role:(Plan.Program.role program)
              ~limits:(limits ())
              ~checkpoint:ignore
            |> ok
          in
          match found with
          | Selected source ->
            printf
              "%s: exact captured lock\n"
              (C.Binding.script_id (Plan.Program.binding program));
            let retained =
              C.retained
                (Revision.programs revision |> Option.value_exn)
                ~max_work_steps:1_000_000
                ~checkpoint:ignore
              |> ok
            in
            let expected =
              List.find_exn (C.Retained.programs retained) ~f:(fun saved ->
                C.Binding.equal
                  (Plan.Program.binding program)
                  (C.Retained.Program.binding saved)
                && Chatml_source.Digest.equal
                     (Plan.Program.compiler_contract program)
                     (C.Retained.Program.compiler_contract saved))
            in
            (match C.Retained.Program.input expected with
             | Provided -> failwith "selected fixture lost its project lock"
             | Selected original ->
               assert (
                 Chatml_source.Id.equal
                   (Chatml_source.id source)
                   (Chatml_source.id original));
               [%test_eq: string]
                 (Chatml_source.contents source)
                 (Chatml_source.contents original))
          | Provided ->
            printf
              "%s: known Provided\n"
              (C.Binding.script_id (Plan.Program.binding program))
          | New_slot -> failwith "known role became new");
        let program = List.hd_exn (Plan.programs plan) in
        let found =
          B.Entry.Prior.find
            prior
            ~slot:(C.Binding.slot (Plan.Program.binding program))
            ~role:(Plan.Program.Role.Standard One_off_v1)
            ~limits:(limits ())
            ~checkpoint:ignore
          |> ok
        in
        (match found with
         | New_slot -> ()
         | Provided | Selected _ -> failwith "new role borrowed sibling");
        let different_receipt =
          Agent_store.Prompt_source_receipt.create
            ~definition_id:definition.id
            ~root_file:"/different/root.chatmd"
            ~max_path_bytes:4096
            ~checkpoint:ignore
          |> Or_error.ok_exn
        in
        error
          (Store.Snapshot.previous snapshot ~receipt:different_receipt ~checkpoint:ignore)
          (function
          | Baseline B.Error.Foreign_operator_intent -> true
          | _ -> false);
        let empty_update =
          Store.prepare owner ~snapshot ~successes:[] ~checkpoint:ignore |> ok
        in
        let next = Store.publish owner empty_update ~checkpoint:ignore |> ok in
        [%test_eq: int] (List.length (Store.Snapshot.protected_revisions next)) 1)));
  [%expect
    {|
    lookup: exact captured lock
    spare: known Provided
    |}]
;;

let modify json field f =
  match json with
  | `Object fields ->
    `Object
      (List.map fields ~f:(fun (key, value) ->
         key, if String.equal key field then f value else value))
  | _ -> failwith "independent object fixture"
;;

let append json key value =
  match json with
  | `Object fields -> `Object (fields @ [ key, value ])
  | _ -> failwith "independent object fixture"
;;

let%test_unit
    "complete envelope, unknown retention, association tampering and cooperative \
     admission"
  =
  with_success (fun _ _ _ _ _ _ revision success ->
    let empty_wire =
      {|{"format":"ochat.document","schema_version":1,"kind":"ochat.prompt-project-baselines","payload":{"generation":0,"entries":[]}}|}
    in
    List.iter
      [ String.length empty_wire - 1; String.length empty_wire ]
      ~f:(fun ceiling ->
        let admitted = B.Document.empty ~limits:(limits ~bytes:ceiling ()) in
        if ceiling < String.length empty_wire
        then assert (Result.is_error admitted)
        else [%test_eq: string] (bytes (ok admitted)) empty_wire);
    let doc = document success in
    let encoded = bytes doc in
    let length = String.length encoded in
    List.iter
      [ length - 1; length ]
      ~f:(fun maximum ->
        let policy = limits ~bytes:maximum () in
        let expect result =
          if maximum < length then assert (Result.is_error result) else ignore (ok result)
        in
        expect (B.Document.encode doc ~limits:policy ~checkpoint:ignore);
        expect (B.Document.decode encoded ~limits:policy ~checkpoint:ignore));
    let json = Jsonaf.of_string encoded in
    let multiple =
      modify json "payload" (fun payload ->
        modify payload "entries" (function
          | `Array [ entry ] ->
            let second =
              modify entry "definition_id" (fun _ -> `String "prd_second")
              |> fun entry ->
              modify entry "source_receipt" (fun receipt ->
                modify receipt "definition_id" (fun _ -> `String "prd_second"))
            in
            `Array [ entry; second ]
          | _ -> failwith "one definition fixture"))
      |> Jsonaf.to_string
    in
    assert (
      Result.is_error
        (B.Document.decode multiple ~limits:(limits ~references:3 ()) ~checkpoint:ignore));
    assert (
      Result.is_error
        (B.Document.decode multiple ~limits:(limits ~definitions:1 ()) ~checkpoint:ignore));
    let admitted_multiple =
      B.Document.decode
        multiple
        ~limits:(limits ~references:4 ~definitions:2 ())
        ~checkpoint:ignore
      |> ok
    in
    [%test_eq: int] (List.length (B.Document.entries admitted_multiple)) 2;
    let unknown =
      json
      |> fun json ->
      append json "future_index" (`Array [ `String "retained" ])
      |> fun json ->
      modify json "payload" (fun payload ->
        modify payload "entries" (function
          | `Array [ entry ] ->
            `Array [ append entry "future_definition" (`String "kept") ]
          | _ -> failwith "single definition fixture"))
    in
    let admitted =
      B.Document.decode (Jsonaf.to_string unknown) ~limits:(limits ()) ~checkpoint:ignore
      |> ok
    in
    let replaced =
      B.Document.replace_successes
        admitted
        ~entries:[ success ]
        ~limits:(limits ())
        ~checkpoint:ignore
      |> ok
    in
    let after = bytes replaced |> Jsonaf.of_string in
    let field json name = Document_schema.Json.field json ~name in
    assert (Document_schema.Json.equal unknown after |> not);
    (* only generation changes *)
    [%test_eq: string]
      (Jsonaf.to_string
         (match field after "future_index" with
          | Value value -> value
          | _ -> failwith "unknown dropped"))
      {|["retained"]|};
    assert (String.is_substring (Jsonaf.to_string after) ~substring:"future_definition");
    let role_only =
      modify json "payload" (fun payload ->
        modify payload "entries" (function
          | `Array [ entry ] ->
            `Array
              [ modify entry "references" (function
                  | `Array (reference :: rest) ->
                    `Array
                      (modify reference "role" (fun _ ->
                         `Object
                           [ "kind", `String "standard"; "target", `String "one_off_v1" ])
                       :: rest)
                  | _ -> failwith "role references fixture")
              ]
          | _ -> failwith "definition fixture"))
    in
    assert (
      Result.is_error
        (B.Document.decode
           (Jsonaf.to_string role_only)
           ~limits:(limits ())
           ~checkpoint:ignore));
    let wrong_slot =
      modify json "payload" (fun payload ->
        modify payload "entries" (function
          | `Array [ entry ] ->
            `Array
              [ modify entry "references" (function
                  | `Array (first :: second :: rest) ->
                    let foreign_key =
                      match Document_schema.Json.field second ~name:"program_key" with
                      | Value value -> value
                      | _ -> failwith "independent program key fixture"
                    in
                    `Array
                      (modify first "program_key" (fun _ -> foreign_key) :: second :: rest)
                  | _ -> failwith "two actual slots fixture")
              ]
          | _ -> failwith "definition fixture"))
    in
    let claimed =
      B.Document.decode
        (Jsonaf.to_string wrong_slot)
        ~limits:(limits ())
        ~checkpoint:ignore
      |> ok
    in
    let entry = List.hd_exn (B.Document.entries claimed) in
    error
      (B.Entry.reconcile
         entry
         ~artifact:(Revision.artifact revision)
         ~custody_limits:(Script_tests.custody_limits ())
         ~limits:(limits ())
         ~checkpoint:ignore)
      (function
        | B.Error.Invalid_association _ -> true
        | _ -> false);
    let tamper =
      modify json "payload" (fun payload ->
        modify payload "entries" (function
          | `Array [ entry ] ->
            `Array
              [ modify entry "references" (function
                  | `Array (reference :: rest) ->
                    `Array
                      (modify reference "association_key" (fun _ ->
                         `String (String.make 64 '0'))
                       :: rest)
                  | _ -> failwith "role references fixture")
              ]
          | _ -> failwith "definition fixture"))
    in
    assert (
      Result.is_error
        (B.Document.decode
           (Jsonaf.to_string tamper)
           ~limits:(limits ())
           ~checkpoint:ignore));
    assert (
      Result.is_error
        (B.Document.decode encoded ~limits:(limits ~references:1 ()) ~checkpoint:ignore));
    assert (
      Result.is_error
        (B.Document.replace_successes
           doc
           ~entries:[ success; success ]
           ~limits:(limits ())
           ~checkpoint:ignore));
    assert (
      Result.is_error
        (B.Document.replace_successes
           doc
           ~entries:[]
           ~limits:(limits ~definitions:0 ())
           ~checkpoint:ignore));
    let checkpoints = ref 0 in
    ignore
      (B.Document.encode admitted ~limits:(limits ()) ~checkpoint:(fun () ->
         incr checkpoints)
       |> ok
       : string);
    let sentinel = Failure "borrowed checkpoint" in
    for interruption = 1 to !checkpoints do
      let calls = ref 0 in
      let raised =
        try
          ignore
            (B.Document.encode admitted ~limits:(limits ()) ~checkpoint:(fun () ->
               incr calls;
               if !calls = interruption then raise sentinel)
             : (string, B.Error.t) Result.t);
          false
        with
        | exn ->
          assert (phys_equal exn sentinel);
          true
      in
      assert raised
    done;
    assert (
      Result.is_error
        (B.Document.encode
           admitted
           ~limits:(limits ~work:(!checkpoints - 1) ())
           ~checkpoint:ignore));
    ignore
      (B.Document.encode
         admitted
         ~limits:(limits ~work:!checkpoints ())
         ~checkpoint:ignore
       |> ok
       : string))
;;

let%test_unit
    "untracked historical data, intent mismatch and stale/foreign snapshots never \
     authorize new admission"
  =
  with_workspace (fun _ parent root ->
    let definition = definition root in
    let directory = Eio.Path.(parent / "baseline") in
    Eio.Path.mkdir ~perm:0o700 directory;
    Eio.Path.with_open_dir directory (fun directory ->
      Eio.Switch.run (fun sw ->
        let owner =
          Store.open_existing directory ~sw ~limits:(limits ()) ~checkpoint:ignore |> ok
        in
        let snapshot = Store.snapshot owner ~checkpoint:ignore |> ok in
        (match
           Store.Snapshot.previous
             snapshot
             ~receipt:(receipt definition)
             ~checkpoint:ignore
           |> ok
         with
         | Untracked Missing_index -> ()
         | _ -> failwith "missing history inferred Initial");
        error (Store.prepare owner ~snapshot ~successes:[] ~checkpoint:ignore) (function
          | Untracked Missing_index -> true
          | _ -> false));
      write directory "not JSON";
      Eio.Switch.run (fun sw ->
        let owner =
          Store.open_existing directory ~sw ~limits:(limits ()) ~checkpoint:ignore |> ok
        in
        let snapshot = Store.snapshot owner ~checkpoint:ignore |> ok in
        match
          Store.Snapshot.previous
            snapshot
            ~receipt:(receipt definition)
            ~checkpoint:ignore
          |> ok
        with
        | Untracked (Invalid_index _) -> ()
        | _ -> failwith "invalid history inferred Initial");
      Eio.Path.unlink Eio.Path.(directory / basename);
      Eio.Switch.run (fun sw ->
        let first =
          Store.create_new directory ~sw ~limits:(limits ()) ~checkpoint:ignore |> ok
        in
        let snapshot = Store.snapshot first ~checkpoint:ignore |> ok in
        let second =
          Store.open_existing directory ~sw ~limits:(limits ()) ~checkpoint:ignore |> ok
        in
        error (Store.prepare second ~snapshot ~successes:[] ~checkpoint:ignore) (function
          | Foreign_snapshot -> true
          | _ -> false);
        let candidate =
          Store.prepare first ~snapshot ~successes:[] ~checkpoint:ignore |> ok
        in
        ignore (Store.publish first candidate ~checkpoint:ignore |> ok : Store.Snapshot.t);
        error (Store.publish first candidate ~checkpoint:ignore) (function
          | Stale_snapshot -> true
          | _ -> false);
        write directory "external";
        error (Store.snapshot first ~checkpoint:ignore) (function
          | Stale_snapshot -> true
          | _ -> false));
      Eio.Path.unlink Eio.Path.(directory / basename);
      Eio.Path.mkdir ~perm:0o700 Eio.Path.(directory / basename);
      error
        (Eio.Switch.run (fun sw ->
           Store.open_existing directory ~sw ~limits:(limits ()) ~checkpoint:ignore))
        (function
          | Store (Io _) -> true
          | _ -> false)))
;;

let fault_directory directory boundary reached =
  let resource, relative = directory in
  ( Fault.wrap_directory resource ~boundary ~reached ~matches:(fun path ->
      String.is_prefix path ~prefix:(basename ^ ".tmp-") || String.equal path ".")
  , relative )
;;

let%test_unit
    "writer failures and cancellation leave exact old/new bytes with conservative \
     uncertainty"
  =
  with_workspace (fun _ parent _ ->
    let directory = Eio.Path.(parent / "baseline") in
    Eio.Path.mkdir ~perm:0o700 directory;
    Eio.Path.with_open_dir directory (fun directory ->
      let old = B.Document.empty ~limits:(limits ()) |> ok |> bytes in
      List.iter
        [ Fault.After_create, false
        ; Before_sync, false
        ; Before_rename, false
        ; After_rename, true
        ; Before_directory_sync, true
        ]
        ~f:(fun (boundary, renamed) ->
          write directory old;
          let hit = ref false in
          let sentinel = Failure "index writer boundary" in
          let wrapped =
            fault_directory directory boundary (fun _ ->
              hit := true;
              raise sentinel)
          in
          Eio.Switch.run (fun sw ->
            let owner =
              Store.open_existing wrapped ~sw ~limits:(limits ()) ~checkpoint:ignore |> ok
            in
            let snapshot = Store.snapshot owner ~checkpoint:ignore |> ok in
            let candidate =
              Store.prepare owner ~snapshot ~successes:[] ~checkpoint:ignore |> ok
            in
            let expected =
              B.Document.empty ~limits:(limits ())
              |> ok
              |> B.Document.replace_successes
                   ~entries:[]
                   ~limits:(limits ())
                   ~checkpoint:ignore
              |> ok
              |> bytes
            in
            let raised =
              try
                ignore (Store.publish owner candidate ~checkpoint:ignore);
                false
              with
              | exn ->
                assert (phys_equal exn sentinel);
                true
            in
            assert raised;
            assert !hit;
            assert (Store.equal_state (Store.state owner) Uncertain);
            [%test_eq: string] (read directory) (if renamed then expected else old);
            error (Store.snapshot owner ~checkpoint:ignore) (function
              | Uncertain -> true
              | _ -> false)));
      write directory old;
      Eio.Switch.run (fun sw ->
        let owner =
          Store.open_existing directory ~sw ~limits:(limits ()) ~checkpoint:ignore |> ok
        in
        let snapshot = Store.snapshot owner ~checkpoint:ignore |> ok in
        let candidate =
          Store.prepare owner ~snapshot ~successes:[] ~checkpoint:ignore |> ok
        in
        let polls = ref 0 in
        let result =
          Eio.Cancel.sub (fun context ->
            try
              ignore
                (Store.publish owner candidate ~checkpoint:(fun () ->
                   incr polls;
                   Eio.Cancel.cancel context (Failure "checkpoint cancellation")));
              false
            with
            | Eio.Cancel.Cancelled _ -> true)
        in
        assert result;
        [%test_eq: string] (read directory) old;
        assert (Store.equal_state (Store.state owner) Available))))
;;

let%test_unit
    "closing joins an admitted worker; undelivered committed output remains uncertain"
  =
  with_workspace (fun _ parent _ ->
    let directory = Eio.Path.(parent / "baseline") in
    Eio.Path.mkdir ~perm:0o700 directory;
    Eio.Path.with_open_dir directory (fun directory ->
      let old = B.Document.empty ~limits:(limits ()) |> ok |> bytes in
      write directory old;
      Eio.Switch.run (fun outer ->
        let admitted, resolve_admitted = Eio.Promise.create () in
        let close_requested, resolve_close = Eio.Promise.create () in
        let finished, resolve_finished = Eio.Promise.create () in
        let worker_cleaned = ref false in
        Eio.Fiber.fork ~sw:outer (fun () ->
          Eio.Switch.run (fun sw ->
            let owner =
              Store.open_existing directory ~sw ~limits:(limits ()) ~checkpoint:ignore
              |> ok
            in
            Eio.Promise.resolve resolve_admitted owner;
            Eio.Promise.await close_requested));
        let owner = Eio.Promise.await admitted in
        Eio.Fiber.fork ~sw:outer (fun () ->
          let cancelled =
            try
              ignore
                (Store.snapshot owner ~checkpoint:(fun () ->
                   Eio.Promise.resolve resolve_close ();
                   Fun.protect
                     ~finally:(fun () -> worker_cleaned := true)
                     Eio.Fiber.await_cancel));
              false
            with
            | Eio.Cancel.Cancelled _ -> true
          in
          assert cancelled;
          Eio.Promise.resolve resolve_finished ());
        Eio.Promise.await finished;
        assert !worker_cleaned;
        assert (Store.equal_state (Store.state owner) Closed));
      let sentinel = Failure "owner already cancelling" in
      let rejected_open = ref false in
      let open_checkpoints = ref 0 in
      let raised =
        try
          Eio.Switch.run (fun sw ->
            let owner =
              Store.open_existing directory ~sw ~limits:(limits ()) ~checkpoint:ignore
              |> ok
            in
            Eio.Switch.fail sw sentinel;
            Eio.Cancel.protect (fun () ->
              error (Store.snapshot owner ~checkpoint:ignore) (function
                | Closed -> true
                | _ -> false);
              rejected_open
              := try
                   ignore
                     (Store.open_existing
                        directory
                        ~sw
                        ~limits:(limits ())
                        ~checkpoint:(fun () -> incr open_checkpoints));
                   false
                 with
                 | Eio.Cancel.Cancelled _ -> true));
          false
        with
        | exn ->
          assert (phys_equal exn sentinel);
          true
      in
      assert raised;
      assert !rejected_open;
      [%test_eq: int] !open_checkpoints 0;
      write directory old;
      Eio.Switch.run (fun sw ->
        Eio.Cancel.sub (fun caller ->
          let hit = ref false in
          let wrapped =
            fault_directory directory After_rename (fun _ ->
              hit := true;
              Eio.Cancel.cancel caller (Failure "waiter cancelled after commit"))
          in
          let owner =
            Store.open_existing wrapped ~sw ~limits:(limits ()) ~checkpoint:ignore |> ok
          in
          let snapshot = Store.snapshot owner ~checkpoint:ignore |> ok in
          let candidate =
            Store.prepare owner ~snapshot ~successes:[] ~checkpoint:ignore |> ok
          in
          let cancelled =
            try
              ignore (Store.publish owner candidate ~checkpoint:ignore);
              false
            with
            | Eio.Cancel.Cancelled _ -> true
          in
          assert cancelled;
          assert !hit;
          assert (Store.equal_state (Store.state owner) Uncertain);
          Eio.Cancel.protect (fun () -> assert (not (String.equal (read directory) old)))))))
;;
