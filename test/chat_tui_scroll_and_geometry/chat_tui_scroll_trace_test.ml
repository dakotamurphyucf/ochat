open Core

let%expect_test "event names omit input contents" =
  List.iter
    [ `Key (`Arrow `Up, [])
    ; `Key (`Arrow `Up, [ `Ctrl ])
    ; `Mouse (`Press (`Scroll `Down), (10, 20), [])
    ; `Paste `Start
    ]
    ~f:(fun event -> print_endline (Chat_tui.Live_scroll_trace.event_name event));
  [%expect
    {|
    key-arrow-up[]
    key-arrow-up[ctrl]
    mouse-scroll-down[]
    paste-start
    |}]
;;

let%expect_test "buffer preserves earliest records and flushes once" =
  let writes = ref [] in
  Chat_tui.Live_scroll_trace.For_testing.install
    ~max_records:2
    ~max_bytes:10_000
    ~write:(fun contents -> writes := contents :: !writes);
  List.iter [ "one"; "two"; "three" ] ~f:(fun value ->
    Chat_tui.Live_scroll_trace.emit ~phase:value [ "value", `String value ]);
  print_s
    [%sexp
      (Chat_tui.Live_scroll_trace.For_testing.retained_records () : int)
    , (Chat_tui.Live_scroll_trace.For_testing.dropped_records () : int)];
  Chat_tui.Live_scroll_trace.flush ();
  let output = List.hd_exn !writes in
  print_s
    [%sexp
      (List.length !writes : int)
    , (String.is_substring output ~substring:{|"phase":"one"|} : bool)
    , (String.is_substring output ~substring:{|"phase":"two"|} : bool)
    , (String.is_substring output ~substring:{|"phase":"three"|} : bool)
    , (String.is_substring output ~substring:{|"phase":"trace_dropped"|} : bool)
    , (Chat_tui.Live_scroll_trace.For_testing.retained_records () : int)];
  [%expect
    {|
    (2 1)
    (1 true true false true 0)
    |}]
;;

let%expect_test "byte bound drops oversized records" =
  let writes = ref [] in
  Chat_tui.Live_scroll_trace.For_testing.install
    ~max_records:10
    ~max_bytes:1
    ~write:(fun contents -> writes := contents :: !writes);
  Chat_tui.Live_scroll_trace.emit ~phase:"oversized" [];
  print_s
    [%sexp
      (Chat_tui.Live_scroll_trace.For_testing.retained_records () : int)
    , (Chat_tui.Live_scroll_trace.For_testing.dropped_records () : int)];
  Chat_tui.Live_scroll_trace.flush ();
  print_s
    [%sexp
      (List.length !writes : int)
    , (String.is_substring (List.hd_exn !writes) ~substring:{|"phase":"trace_dropped"|}
       : bool)];
  [%expect
    {|
    (0 1)
    (1 true)
    |}]
;;
