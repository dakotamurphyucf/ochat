open Core
module Captured = Chatml_source
module Diagnostic = Chatml_diagnostic

let checkpoint () = ()

let capture ?(namespace = "test") path contents =
  let id =
    Captured.Id.create ~package_namespace:namespace ~relative_path:path |> Or_error.ok_exn
  in
  Captured.create id ~contents ~expected_digest:None ~max_bytes:None ~checkpoint
  |> Or_error.ok_exn
;;

let site source =
  let location =
    Captured.locate
      source
      ~start_byte:0
      ~end_byte:(Captured.byte_length source)
      ~checkpoint
    |> Or_error.ok_exn
  in
  Diagnostic.Site.create source ~span:(Captured.Location.span location) ~checkpoint
  |> Or_error.ok_exn
;;

let same_site left right =
  let left = Diagnostic.Site.location left in
  let right = Diagnostic.Site.location right in
  Captured.Id.equal (Captured.Location.source left) (Captured.Location.source right)
  && Captured.Digest.equal
       (Captured.Location.source_digest left)
       (Captured.Location.source_digest right)
  && Int.equal
       (Captured.Location.span left).left.offset
       (Captured.Location.span right).left.offset
  && Int.equal
       (Captured.Location.span left).right.offset
       (Captured.Location.span right).right.offset
;;

let%expect_test "ownership follows the original failure under fallback composition" =
  (* Same offsets, different files and different revisions of the same file.
     The independent model is left-biased option choice, not source lookup. *)
  let sources =
    [ capture "a.chatml" "alpha_failure"
    ; capture "b.chatml" "bravo_failure"
    ; capture "a.chatml" "gamma_failure"
    ]
  in
  let sites = None :: List.map sources ~f:(fun source -> Some (site source)) in
  List.iter sites ~f:(fun initial ->
    List.iter sites ~f:(fun first ->
      List.iter sites ~f:(fun second ->
        let diagnostic = Diagnostic.create ?site:initial "failure" in
        let actual =
          Diagnostic.with_fallback (Diagnostic.with_fallback diagnostic first) second
        in
        let expected = List.find_map [ initial; first; second ] ~f:Fn.id in
        assert (Option.equal same_site actual.site expected);
        assert (String.equal actual.message "failure");
        Option.iter expected ~f:(fun site ->
          let span = Captured.Location.span (Diagnostic.Site.location site) in
          assert (
            Option.value_map actual.span ~default:false ~f:(fun actual ->
              Int.equal actual.left.offset span.left.offset
              && Int.equal actual.right.offset span.right.offset))))));
  [%expect {| |}]
;;

let%expect_test "admission rejects inconsistent coordinates instead of clipping" =
  let source = capture "range.chatml" "a\r\nb" in
  let valid = Captured.Location.span (Diagnostic.Site.location (site source)) in
  List.iter
    [ { valid with left = { valid.left with offset = -1 } }
    ; { valid with right = { valid.right with offset = 99 } }
    ; { valid with right = { valid.right with line = 1 } }
    ; { valid with right = { valid.right with column = 99 } }
    ]
    ~f:(fun span ->
      assert (Result.is_error (Diagnostic.Site.create source ~span ~checkpoint)));
  [%expect {| |}]
;;

let%expect_test
    "bounded diagnostics retain exact ownership without live text substitution"
  =
  let namespace = String.make 65536 'n' ^ "\027[2J\000\n" in
  let source = capture ~namespace "dependency.chatml" "alpha_failure" in
  let primary = site source in
  let huge = String.make (128 * 1024) 'm' in
  let related =
    List.init 100 ~f:(fun _ -> Diagnostic.Related.create primary ~message:huge)
  in
  let diagnostic = Diagnostic.create ~site:primary ~related huge in
  assert (String.length diagnostic.message <= 16 * 1024);
  assert (List.length diagnostic.related <= 8);
  List.iter diagnostic.related ~f:(fun related ->
    assert (String.length (Diagnostic.Related.message related) <= 16 * 1024));
  let check_rendered rendered =
    assert (String.length rendered <= 16 * 1024);
    assert (
      not
        (String.exists rendered ~f:(fun c -> Char.equal c '\000' || Char.equal c '\027')))
  in
  check_rendered (Diagnostic.render diagnostic ~label:huge ~checkpoint);
  let raw_source = capture "bytes.chatml" "raw\000\027bytes" in
  let raw = Diagnostic.create ~site:(site raw_source) "invalid source bytes" in
  check_rendered (Diagnostic.render raw ~label:"Parse error" ~checkpoint);
  assert (String.equal (Captured.contents raw_source) "raw\000\027bytes");
  let multiline = capture "lines.chatml" "first\nlast" in
  let multiline = Diagnostic.create ~site:(site multiline) "failure" in
  let rendered = Diagnostic.render multiline ~label:"Runtime error" ~checkpoint in
  assert (String.is_substring rendered ~substring:"line 1, column 0 to line 2, column 4");
  let long_multiline = capture "long-lines.chatml" (String.make 5000 'x' ^ "\nlast") in
  let diagnostic = Diagnostic.create ~site:(site long_multiline) "failure" in
  let rendered = Diagnostic.render diagnostic ~label:"Runtime error" ~checkpoint in
  assert (not (String.mem rendered '^'));
  let readable = Diagnostic.create ~site:primary "division failed" in
  let rendered =
    Diagnostic.render_legacy
      readable
      ~source:"poisoned_current_file"
      ~label:"Runtime error"
      ~checkpoint
  in
  check_rendered rendered;
  assert (String.is_substring rendered ~substring:"alpha_failure");
  assert (not (String.is_substring rendered ~substring:"poisoned_current_file"));
  assert (
    Captured.Digest.equal
      (Captured.digest (Diagnostic.Site.source primary))
      (Captured.digest source));
  let legacy_source = capture "legacy.chatml" "legacy_marker" in
  let span = Captured.Location.span (Diagnostic.Site.location (site legacy_source)) in
  let legacy span =
    Diagnostic.render_legacy
      (Diagnostic.create ~span "failure")
      ~source:(Captured.contents legacy_source)
      ~label:"Runtime error"
      ~checkpoint
  in
  assert (String.is_substring (legacy span) ~substring:"legacy_marker");
  let forged = { span with right = { span.right with offset = 99 } } in
  assert (not (String.is_substring (legacy forged) ~substring:"legacy_marker"));
  [%expect {| |}]
;;

exception Interrupted

let interrupt_at_each_checkpoint f =
  let calls = ref 0 in
  f (fun () -> incr calls);
  assert (!calls > 0);
  for stop = 1 to !calls do
    let current = ref 0 in
    match
      f (fun () ->
        incr current;
        if !current = stop then raise Interrupted)
    with
    | () -> assert false
    | exception Interrupted -> ()
  done
;;

let%expect_test "diagnostic checkpoints propagate rather than convert interruption" =
  let source = capture "cancel.chatml" (String.make 8192 'x') in
  let primary = site source in
  let span = Captured.Location.span (Diagnostic.Site.location primary) in
  interrupt_at_each_checkpoint (fun checkpoint ->
    ignore
      (Diagnostic.Site.create source ~span ~checkpoint : Diagnostic.Site.t Or_error.t));
  let diagnostic = Diagnostic.create ~site:primary "failure" in
  interrupt_at_each_checkpoint (fun checkpoint ->
    ignore (Diagnostic.render diagnostic ~label:"Runtime error" ~checkpoint : string));
  let escaped_source = capture "escaped.chatml" (String.make 512 '\000') in
  let escaped = Diagnostic.create ~site:(site escaped_source) "failure" in
  interrupt_at_each_checkpoint (fun checkpoint ->
    ignore (Diagnostic.render escaped ~label:"Runtime error" ~checkpoint : string));
  let legacy = Diagnostic.create ~span "failure" in
  interrupt_at_each_checkpoint (fun checkpoint ->
    ignore
      (Diagnostic.render_legacy
         legacy
         ~source:(Captured.contents source)
         ~label:"Runtime error"
         ~checkpoint
       : string));
  [%expect {| |}]
;;

let%expect_test "a caller fallback cannot relabel a dependency failure" =
  let dependency = site (capture "graph.chatml" "1 / 0") in
  let caller = site (capture "main.chatml" "Graph.run()") in
  let diagnostic =
    Diagnostic.with_fallback
      (Diagnostic.create ~site:dependency "Division by zero")
      (Some caller)
  in
  Option.iter diagnostic.site ~f:(fun primary ->
    let source = Diagnostic.Site.source primary in
    printf
      "%s: %s\n%s\n"
      (Captured.Id.relative_path (Captured.id source))
      diagnostic.message
      (Captured.contents source));
  [%expect
    {|
    graph.chatml: Division by zero
    1 / 0
    |}]
;;
