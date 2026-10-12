open Core
open Support
module Entry = Agent_session.Prompt_source_entry
module Revision = Agent_session.Prompt_revision
module Store = Agent_store.Prompt_artifact_store
module CM = Prompt.Chat_markdown

let timestamp = Agent_protocol.Timestamp.of_string "2026-10-11T12:00:00Z" |> ok
let content_limits = Captured_child_tests.content_limits

let with_fixture f =
  with_workspace (fun env directory root ->
    List.iter [ "definitions"; "nested" ] ~f:(fun name ->
      Eio.Path.mkdir ~perm:0o700 Eio.Path.(root / name));
    save root "root.chatmd" {|<import src="definitions/tools.chatmd"/>|};
    save
      root
      "definitions/tools.chatmd"
      {|<tool name="child" agent="../nested/child.chatmd" local/>
<tool name="external" agent="/tmp/unadmitted-child.chatmd" local/>
<tool name="remote" agent="https://example.invalid/child.chatmd"/>
<user source_context="/tmp/forged-context.chatmd"><agent src="../nested/child.chatmd" local/></user>|};
    save root "shared.chatmd" "<developer>shared A</developer>";
    save root "guide.md" "guide A";
    save
      root
      "nested/child.chatmd"
      {|<import src="../shared.chatmd"/>
<tool name="back" agent="../root.chatmd" local/>
<user><doc src="../guide.md" local capture/></user>|};
    Eio.Switch.run (fun sw ->
      let definition = definition root in
      let owner = open_owner sw root definition in
      let store =
        Store.create ~env ~root:(Eio.Path.native_exn Eio.Path.(directory / "artifacts"))
        |> ok
      in
      let captured, documents =
        Owner.with_scope owner ~f:(fun scope ->
          let open Result.Let_syntax in
          let%bind captured = capture env scope definition in
          let%map documents =
            Owner.Scope.capture_documents
              scope
              ~capture:captured
              ~limits:content_limits
              ~checkpoint:ignore
          in
          captured, documents)
        |> ok
      in
      let parent =
        Builder.build_captured
          ~capture:(Owner.Capture.value captured)
          ~source_receipt:(receipt definition)
          ~prepare_content:(fun _ -> Ok documents)
          ~artifact_store:store
          ~transaction_id:(Agent_protocol.Id.Transaction.create ())
          ~created_at:timestamp
          definition
        |> ok
      in
      f env root sw owner store captured documents parent))
;;

let references parent =
  Revision.parser_context parent |> Option.value_exn |> CM.Parsed.agent_references
;;

let reference parent =
  references parent
  |> List.find_exn ~f:(fun reference ->
    Option.is_some (CM.Agent_reference.queued_key reference))
;;

let admit parent reference ~max_work_steps ~checkpoint =
  Entry.of_reference ~parent reference ~max_work_steps ~checkpoint
;;

let capture_entry env owner entry ?(preprocess = Fn.id) ?(checkpoint = ignore) () =
  Owner.with_scope owner ~f:(fun scope ->
    Owner.Scope.capture_entry
      scope
      ~entry
      ~env
      ~preprocess
      ~limits:(parser_limits ())
      ~checkpoint)
;;

let%expect_test
    "selected member uses current original-root bytes and publishes its capture"
  =
  with_fixture (fun env root _ owner store original _ parent ->
    let entry =
      admit parent (reference parent) ~max_work_steps:100 ~checkpoint:ignore |> ok
    in
    [%test_eq: string]
      "nested/child.chatmd"
      (Source.Portable_path.to_string (Entry.member entry));
    assert (Receipt.equal (Entry.receipt entry) (receipt (Revision.definition parent)));
    save root "shared.chatmd" "<developer>shared B</developer>";
    save root "guide.md" "guide B";
    let selected, documents =
      Owner.with_scope owner ~f:(fun scope ->
        let open Result.Let_syntax in
        let%bind selected =
          Owner.Scope.capture_entry
            scope
            ~entry
            ~env
            ~preprocess:Fn.id
            ~limits:(parser_limits ())
            ~checkpoint:ignore
        in
        let%map documents =
          Owner.Scope.capture_documents
            scope
            ~capture:selected
            ~limits:content_limits
            ~checkpoint:ignore
        in
        selected, documents)
      |> ok
    in
    let hooks = ref 0 in
    let hook _ =
      incr hooks;
      Ok documents
    in
    assert (
      Result.is_error
        (Builder.build_captured_entry
           ~entry
           ~capture:(Owner.Capture.value original)
           ~prepare_content:hook
           ~artifact_store:store
           ~transaction_id:(Agent_protocol.Id.Transaction.create ())
           ~created_at:timestamp
           ()));
    assert (
      Result.is_error
        (Builder.build_captured
           ~capture:(Owner.Capture.value selected)
           ~source_receipt:(Entry.receipt entry)
           ~prepare_content:hook
           ~artifact_store:store
           ~transaction_id:(Agent_protocol.Id.Transaction.create ())
           ~created_at:timestamp
           (Entry.definition entry)));
    [%test_eq: int] 0 !hooks;
    save root "guide.md" "guide C";
    Owner.close owner;
    List.iter
      [ "root.chatmd"
      ; "definitions/tools.chatmd"
      ; "nested/child.chatmd"
      ; "shared.chatmd"
      ; "guide.md"
      ]
      ~f:(fun path -> Eio.Path.unlink Eio.Path.(root / path));
    let published =
      Builder.build_captured_entry
        ~entry
        ~capture:(Owner.Capture.value selected)
        ~prepare_content:hook
        ~artifact_store:store
        ~transaction_id:(Agent_protocol.Id.Transaction.create ())
        ~created_at:timestamp
        ()
      |> ok
    in
    let restored =
      Builder.restore
        ~content_limits
        ~artifact_store:store
        (Entry.definition entry)
        (Revision.id published)
      |> ok
    in
    [%test_eq: int] 1 !hooks;
    [%test_eq: string] "nested/child.chatmd" (Revision.root_relative_path restored);
    assert (
      Receipt.equal
        (Option.value_exn (Revision.artifact restored).source_receipt)
        (Entry.receipt entry));
    let back =
      Revision.parser_context restored
      |> Option.value_exn
      |> CM.Parsed.agent_references
      |> List.find_exn ~f:(fun reference ->
        Option.equal
          String.equal
          (CM.Agent_reference.queued_key reference)
          (Some "root.chatmd"))
    in
    (match CM.Agent_reference.invocation back with
     | Tool tool ->
       [%test_eq: string]
         tool.agent
         (Eio.Path.native_exn
            Eio.Path.(Revision.materialized_tree restored / "nested" / "../root.chatmd"))
     | Persistent_tool _ | Content _ -> assert false);
    let texts =
      List.filter_map (Revision.elements restored) ~f:(function
        | CM.Developer { content = Some (Text text); _ } -> Some text
        | CM.User { content = Some (Items [ Basic { text = Some text; _ } ]); _ } ->
          Some text
        | _ -> None)
    in
    print_s [%sexp (Revision.root_relative_path restored : string), (texts : string list)]);
  [%expect {| (nested/child.chatmd ("shared B" "guide B")) |}]
;;

exception Stop of int ref

let[@inline never] raise_checkpoint marker = raise marker

let%test_unit
    "entry admission bounds every reference and rejects foreign or unavailable provenance"
  =
  with_fixture (fun _ _ _ _ store original documents parent ->
    let own = reference parent in
    let expected = List.length (references parent) + 8 in
    let polls = ref 0 in
    let admitted =
      admit parent own ~max_work_steps:expected ~checkpoint:(fun () -> incr polls) |> ok
    in
    [%test_eq: int] expected (Entry.work_steps admitted);
    for maximum = 0 to expected - 1 do
      assert (
        Result.is_error (admit parent own ~max_work_steps:maximum ~checkpoint:ignore))
    done;
    assert (
      Result.is_error
        (admit parent own ~max_work_steps:0 ~checkpoint:(fun () -> assert false)));
    let other =
      Builder.build_captured
        ~capture:(Owner.Capture.value original)
        ~source_receipt:(receipt (Revision.definition parent))
        ~prepare_content:(fun _ -> Ok documents)
        ~artifact_store:store
        ~transaction_id:(Agent_protocol.Id.Transaction.create ())
        ~created_at:timestamp
        (Revision.definition parent)
      |> ok
    in
    assert (
      Result.is_error
        (admit parent (reference other) ~max_work_steps:expected ~checkpoint:ignore));
    List.iter (references parent) ~f:(fun candidate ->
      match CM.Agent_reference.queued_key candidate with
      | None ->
        assert (
          Result.is_error
            (admit parent candidate ~max_work_steps:expected ~checkpoint:ignore))
      | Some queued_key ->
        let selected =
          admit parent candidate ~max_work_steps:expected ~checkpoint:ignore |> ok
        in
        [%test_eq: string]
          queued_key
          (Source.Portable_path.to_string (Entry.member selected)));
    [%test_eq: int]
      2
      (List.count (references parent) ~f:(fun candidate ->
         Option.is_some (CM.Agent_reference.queued_key candidate)));
    let legacy =
      Revision.create
        ~definition:(Revision.definition parent)
        ~artifact:(Revision.artifact parent)
        ~materialized_tree:(Revision.materialized_tree parent)
        ~elements:(Revision.elements parent)
    in
    assert (Result.is_error (admit legacy own ~max_work_steps:expected ~checkpoint:ignore));
    let previous = Stdlib.Printexc.backtrace_status () in
    Fun.protect
      ~finally:(fun () -> Stdlib.Printexc.record_backtrace previous)
      (fun () ->
         Stdlib.Printexc.record_backtrace true;
         for stop = 1 to !polls do
           let marker = Stop (ref stop) in
           let reached = ref 0 in
           match
             admit parent own ~max_work_steps:expected ~checkpoint:(fun () ->
               incr reached;
               if !reached = stop then raise_checkpoint marker)
           with
           | exception exn ->
             assert (phys_equal exn marker);
             assert (
               String.is_substring
                 (Stdlib.Printexc.raw_backtrace_to_string
                    (Stdlib.Printexc.get_raw_backtrace ()))
                 ~substring:"raise_checkpoint")
           | Ok _ | Error _ -> assert false
         done))
;;

let%test_unit "selected captures keep native affinity and callback lifecycle" =
  with_fixture (fun env root sw owner _ _ _ parent ->
    let entry =
      admit parent (reference parent) ~max_work_steps:100 ~checkpoint:ignore |> ok
    in
    let selected = capture_entry env owner entry () |> ok in
    let foreign = open_owner sw root (Entry.definition entry) in
    error
      (Owner.with_scope foreign ~f:(fun scope -> Owner.Scope.check_capture scope selected))
      Foreign_capture;
    let lease = Owner.lease owner ~sw ~checkpoint:ignore |> ok in
    Owner.with_scope lease ~f:(fun scope -> Owner.Scope.check_capture scope selected)
    |> ok;
    let before = descriptors env in
    let marker = Stop (ref 1) in
    List.iter [ `Checkpoint; `Preprocess ] ~f:(fun callback ->
      match
        capture_entry
          env
          owner
          entry
          ~checkpoint:(fun () ->
            match callback with
            | `Checkpoint -> raise marker
            | `Preprocess -> ())
          ~preprocess:(fun text ->
            match callback with
            | `Preprocess -> raise marker
            | `Checkpoint -> text)
          ()
      with
      | exception exn ->
        assert (phys_equal exn marker);
        [%test_eq: int] before (descriptors env)
      | Ok _ | Error _ -> assert false);
    capture_entry env owner entry () |> ok |> ignore;
    error
      (capture_entry
         env
         owner
         entry
         ~checkpoint:(fun () -> Owner.Policy.revoke (Owner.policy owner))
         ())
      Revoked;
    assert (String.is_prefix (root_text selected) ~prefix:"<import"))
;;
