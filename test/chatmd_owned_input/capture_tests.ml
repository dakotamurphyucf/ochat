open! Core

let () = Mirage_crypto_rng_unix.use_default ()

module B = Agent_session.Prompt_revision_builder
module CM = Prompt.Chat_markdown

let with_directory f =
  Eio_main.run (fun env ->
    let name = "ochat-input." ^ Agent_protocol.Id.Transaction.(create () |> to_string) in
    let root =
      Eio.Path.(
        Eio.Stdenv.fs env / (Sys.getenv "TMPDIR" |> Option.value ~default:"/tmp") / name)
    in
    Eio.Path.mkdir ~perm:0o700 root;
    Exn.protect
      ~f:(fun () -> f env root)
      ~finally:(fun () -> Eio.Cancel.protect (fun () -> Eio.Path.rmtree root)))
;;

let definition root =
  Agent_session.Prompt_definition.create
    ~root_invocation:Ui
    ~id:
      (Agent_protocol.Id.Prompt_definition.of_string "prd_owned_input"
       |> Result.map_error ~f:(fun error ->
         Sexp.to_string_hum (Agent_protocol.Error.sexp_of_t error))
       |> Result.ok_or_failwith)
    ~config_name:"owned-input"
    ~root_file:(Eio.Path.native_exn Eio.Path.(root / "root.chatmd"))
    ~allowed_workspaces:[]
    ~permission_profile:"interactive"
    ~runtime_policy:None
    ~enabled:true
    ~description:None
  |> Result.map_error ~f:(fun error ->
    Sexp.to_string_hum (Agent_store.Store_error.sexp_of_t error))
  |> Result.ok_or_failwith
;;

let save root path text =
  Eio.Path.save ~create:(`Or_truncate 0o600) Eio.Path.(root / path) text
;;

let capture ?(checkpoint = ignore) env root preprocess =
  B.capture ~env ~preprocess ~limits:(Input_tests.limits ()) ~checkpoint (definition root)
  |> Result.map_error ~f:(fun errors ->
    Sexp.to_string_hum ([%sexp_of: B.Diagnostic.t list] errors))
  |> Result.ok_or_failwith
;;

let%expect_test "selected capture narrows root and distinct dependency bytes" =
  with_directory (fun env root ->
    let entry = Source.Portable_path.create "root.chatmd" |> Or_error.ok_exn in
    let capture ?max_total_source_bytes () =
      B.capture_with_loader
        ~loader:(Source_loader.filesystem ~root)
        ~entry
        ?max_total_source_bytes
        ~env
        ~preprocess:Fn.id
        ~limits:(Input_tests.limits ~bytes:(16 * 1024 * 1024) ())
        ~checkpoint:ignore
        (definition root)
    in
    let rejects ?max_total_source_bytes () =
      match capture ?max_total_source_bytes () with
      | exception Failure _ -> true
      | Error _ -> true
      | Ok _ -> false
    in
    let captured ?max_total_source_bytes () =
      capture ?max_total_source_bytes ()
      |> Result.map_error ~f:(fun diagnostics ->
        Sexp.to_string_hum ([%sexp_of: B.Diagnostic.t list] diagnostics))
      |> Result.ok_or_failwith
    in
    let authored =
      {|<tool name="first" agent="child.chatmd" local/><tool name="second" agent="child.chatmd" local/>|}
    in
    let child = "<user>retained dependency</user>" in
    save root "root.chatmd" authored;
    save root "child.chatmd" child;
    let total = String.length authored + String.length child in
    assert (rejects ~max_total_source_bytes:(total - 1) ());
    let exact = captured ~max_total_source_bytes:total () in
    [%test_eq: int] total (B.Capture.source_bytes exact);
    let wider_request = captured ~max_total_source_bytes:(16 * 1024 * 1024) () in
    [%test_eq: int] total (B.Capture.source_bytes wider_request);
    save root "root.chatmd" ("<user>" ^ String.make (8 * 1024 * 1024) 'x' ^ "</user>");
    assert (rejects ~max_total_source_bytes:(16 * 1024 * 1024) ());
    print_endline "root and dependency caps held");
  [%expect {| root and dependency caps held |}]
;;

let%test_unit "retained generated loader reuses first exact bytes and preflights files" =
  with_directory (fun _env dir ->
    Eio.Path.save ~create:(`Or_truncate 0o600) Eio.Path.(dir / "part.chatmd") "first";
    let result =
      Eio.Path.with_open_dir dir (fun opened ->
        Chatml_local_source.with_root
          opened
          ~max_path_components:8
          ~checkpoint:ignore
          ~f:(fun owner ->
            let module Retained = Source_loader.Generated_retained in
            let limits =
              Retained.Limits.create
                ~max_source_bytes:16
                ~max_bundle_bytes:20
                ~max_files:2
              |> Or_error.ok_exn
            in
            let reads = ref 0 in
            let retained =
              Retained.create
                ~root:dir
                ~owner
                ~root_file:(Source.Portable_path.create "root.chatmd" |> Or_error.ok_exn)
                ~root_contents:"unsaved"
                ~limits
                ~before_import_read:(fun ~member:_ ->
                  incr reads;
                  16)
                ~on_import_read:(fun ~member:_ ~contents:_ -> ())
                ~checkpoint:ignore
              |> Or_error.ok_exn
            in
            let loader = Retained.loader retained in
            let root =
              Source_loader.root loader ~file:"root.chatmd" |> Result.ok_or_failwith
            in
            [%test_eq: string]
              (Source_loader.read_bounded ~max_bytes:16 loader root
               |> Result.ok_or_failwith)
              "unsaved";
            let part =
              Source_loader.root loader ~file:"part.chatmd" |> Result.ok_or_failwith
            in
            [%test_eq: string]
              (Source_loader.read_bounded ~max_bytes:16 loader part
               |> Result.ok_or_failwith)
              "first";
            Eio.Path.save
              ~create:(`Or_truncate 0o600)
              Eio.Path.(dir / "part.chatmd")
              "later";
            [%test_eq: string]
              (Source_loader.read_bounded ~max_bytes:16 loader part
               |> Result.ok_or_failwith)
              "first";
            [%test_eq: int] !reads 1;
            [%test_eq: int] (List.length (Retained.captured_sources retained)) 2;
            let absent =
              Source_loader.root loader ~file:"absent.chatmd" |> Result.ok_or_failwith
            in
            (match Source_loader.read_bounded ~max_bytes:16 loader absent with
             | exception exn ->
               (match Source_loader.Generated_error.of_exn exn with
                | Some Limit_exceeded -> ()
                | _ -> raise exn)
             | _ -> failwith "file cap did not reject before a native read");
            [%test_eq: int] !reads 1;
            Ok ()))
    in
    match result with
    | Ok () -> ()
    | Error _ -> failwith "retained root fixture failed")
;;

let%test_unit
    "actual queued Roots preprocess once and retained replay uses captured bytes only"
  =
  with_directory (fun env root ->
    let authored =
      {|<tool name="first" agent="child.chatmd" local/><tool name="second" agent="child.chatmd" local/>|}
    in
    let child = {|<user>child</user><tool name="back" agent="root.chatmd" local/>|} in
    save root "root.chatmd" authored;
    save root "child.chatmd" child;
    let calls = ref 0 in
    let captured =
      capture env root (fun text ->
        incr calls;
        text ^ "\n")
    in
    assert (Int.equal !calls 2);
    assert (Int.equal (List.length (B.Capture.agents captured)) 1);
    assert (
      List.equal
        (fun (path, contents) (expected_path, expected_contents) ->
           String.equal path expected_path && String.equal contents expected_contents)
        (B.Capture.sources captured)
        [ "child.chatmd", child ]);
    let polls = ref 0 in
    ignore (capture ~checkpoint:(fun () -> incr polls) env root Fn.id : B.Capture.t);
    assert (!polls > 0);
    let returned = ref false in
    (match
       Eio.Cancel.sub (fun context ->
         let count = ref 0 in
         ignore
           (capture
              ~checkpoint:(fun () ->
                incr count;
                if Int.equal !count !polls
                then Eio.Cancel.cancel context (Input_tests.Interrupted !count))
              env
              root
              Fn.id
            : B.Capture.t);
         returned := true)
     with
     | () -> assert false
     | exception Eio.Cancel.Cancelled _ -> ());
    assert (not !returned);
    let input = CM.Parsed.root_input (B.Capture.root captured) in
    let loader =
      Source_loader.captured_filesystem
        ~root
        ~sources:(("root.chatmd", authored) :: B.Capture.sources captured)
    in
    Eio.Path.unlink Eio.Path.(root / "root.chatmd");
    Eio.Path.unlink Eio.Path.(root / "child.chatmd");
    let parsed =
      CM.replay_chat_inputs
        input
        ~source_loader:loader
        ~dir:root
        ~limits:(Input_tests.limits ())
        ~checkpoint:ignore
    in
    assert (Int.equal !calls 2);
    assert (
      List.equal
        (fun left right -> Int.equal (CM.compare_top_level_elements left right) 0)
        (CM.Parsed.elements parsed)
        (CM.Parsed.elements (B.Capture.root captured))))
;;

let%test_unit
    "joint capture model charges distinct transformed payloads and reuses equal payloads"
  =
  with_directory (fun env root ->
    let authored = {|<tool name="child" agent="child.chatmd" local/>|} in
    save root "root.chatmd" authored;
    save root "child.chatmd" "<user>raw</user>";
    let effective = "<!--" ^ String.make (4 * 1024 * 1024) 'x' ^ "-->" ^ authored in
    let same = capture env root (fun _ -> effective) in
    assert (Int.equal (List.length (B.Capture.agents same)) 1);
    let raw_bytes = String.length authored + String.length "<user>raw</user>" in
    assert (raw_bytes + String.length effective < 8 * 1024 * 1024);
    assert (raw_bytes + (2 * String.length effective) > 8 * 1024 * 1024);
    let calls = ref 0 in
    (match
       capture env root (fun _ ->
         incr calls;
         if Int.equal !calls 1 then effective else " " ^ effective)
     with
     | _ -> assert false
     | exception Failure _ -> ());
    assert (Int.equal !calls 2);
    (* Failure publishes no retained Capture and leaves the source reusable. *)
    ignore (capture env root Fn.id : B.Capture.t))
;;

let%test_unit
    "fresh artifact publication replays exact retained declarations against installed \
     tree"
  =
  with_directory (fun env root ->
    let prefix = "<!-- retained -->\r\n" in
    let tag =
      {|<script id="main" language="chatml" kind="moderator">let initial_state = 0</script>|}
    in
    let authored = prefix ^ tag in
    save root "root.chatmd" authored;
    let definition = definition root in
    let store =
      Agent_store.Prompt_artifact_store.create
        ~env
        ~root:(Eio.Path.native_exn Eio.Path.(root / "artifacts"))
      |> Result.map_error ~f:(fun error ->
        Sexp.to_string_hum (Agent_store.Store_error.sexp_of_t error))
      |> Result.ok_or_failwith
    in
    let created_at =
      Agent_protocol.Timestamp.of_string "2026-10-11T12:00:00Z"
      |> Result.map_error ~f:(fun error ->
        Sexp.to_string_hum (Agent_protocol.Error.sexp_of_t error))
      |> Result.ok_or_failwith
    in
    let revision =
      B.build
        ~env
        ~artifact_store:store
        ~transaction_id:(Agent_protocol.Id.Transaction.create ())
        ~created_at
        definition
      |> Result.map_error ~f:(fun errors ->
        Sexp.to_string_hum ([%sexp_of: B.Diagnostic.t list] errors))
      |> Result.ok_or_failwith
    in
    let artifact = Agent_session.Prompt_revision.artifact revision in
    assert (String.equal artifact.root_chatmd authored);
    let reference =
      match Agent_session.Prompt_revision.elements revision with
      | [ CM.Script { source_ref = Some reference; _ } ] -> reference
      | _ -> assert false
    in
    assert (Int.equal reference.start_pos.offset (String.length prefix));
    assert (Int.equal reference.end_pos.offset (String.length authored));
    assert (
      String.equal
        reference.source_dir
        (Eio.Path.native_exn (Agent_session.Prompt_revision.materialized_tree revision)));
    Eio.Path.unlink Eio.Path.(root / "root.chatmd");
    (* Existing raw-only durable restoration remains compatible; exact effective
       input/range associations are deliberately the206 custody consumer seam. *)
    let restored =
      B.restore
        ~artifact_store:store
        definition
        (Agent_session.Prompt_revision.id revision)
      |> Result.map_error ~f:(fun errors ->
        Sexp.to_string_hum ([%sexp_of: B.Diagnostic.t list] errors))
      |> Result.ok_or_failwith
    in
    assert (
      Agent_protocol.Id.Prompt_revision.equal
        (Agent_session.Prompt_revision.id revision)
        (Agent_session.Prompt_revision.id restored));
    match Agent_session.Prompt_revision.elements restored with
    | [ CM.Script { source = Inline body; _ } ] ->
      assert (String.equal body "let initial_state = 0")
    | _ -> assert false)
;;
