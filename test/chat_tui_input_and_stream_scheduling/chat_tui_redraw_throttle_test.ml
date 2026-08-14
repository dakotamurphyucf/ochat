open Core

let%expect_test "request/tick coalesces and pending guard works" =
  let enq = ref 0 in
  let t = Chat_tui.Redraw_throttle.create ~fps:60. ~enqueue_redraw:(fun () -> incr enq) in
  (* Burst of requests before any tick -> still only one enqueue after tick *)
  for _ = 1 to 100 do
    Chat_tui.Redraw_throttle.request_redraw t
  done;
  Chat_tui.Redraw_throttle.tick t;
  printf "enq=%d\n" !enq;
  [%expect {| enq=1 |}];
  (* Further ticks without handling keep pending; no new enqueues *)
  for _ = 1 to 10 do
    Chat_tui.Redraw_throttle.tick t
  done;
  printf "enq=%d\n" !enq;
  [%expect {| enq=1 |}];
  (* Mark handled, then another burst; next tick enqueues one more *)
  Chat_tui.Redraw_throttle.on_redraw_handled t;
  for _ = 1 to 50 do
    Chat_tui.Redraw_throttle.request_redraw t
  done;
  Chat_tui.Redraw_throttle.tick t;
  printf "enq=%d\n" !enq;
  [%expect {| enq=2 |}]
;;

let%expect_test "startup animation request seeds the first throttled frame" =
  let enqueued = ref 0 in
  let throttler =
    Chat_tui.Redraw_throttle.create ~fps:60. ~enqueue_redraw:(fun () -> incr enqueued)
  in
  Chat_tui.Redraw_throttle.tick throttler;
  printf "before=%d\n" !enqueued;
  Chat_tui.Redraw_throttle.request_redraw throttler;
  Chat_tui.Redraw_throttle.tick throttler;
  printf "after=%d\n" !enqueued;
  [%expect
    {|
    before=0
    after=1
    |}]
;;

let%expect_test "redraw_immediate clears pending/dirty" =
  let enq = ref 0 in
  let drew = ref 0 in
  let t = Chat_tui.Redraw_throttle.create ~fps:60. ~enqueue_redraw:(fun () -> incr enq) in
  Chat_tui.Redraw_throttle.request_redraw t;
  Chat_tui.Redraw_throttle.tick t;
  (* One enqueue now pending *)
  assert (Int.equal !enq 1);
  Chat_tui.Redraw_throttle.redraw_immediate t ~draw:(fun () -> incr drew);
  (* Immediate draw clears pending and dirty; tick won't enqueue until requested *)
  Chat_tui.Redraw_throttle.tick t;
  printf "enq=%d drew=%d\n" !enq !drew;
  [%expect {| enq=1 drew=1 |}];
  (* Request again and tick -> another enqueue *)
  Chat_tui.Redraw_throttle.request_redraw t;
  Chat_tui.Redraw_throttle.tick t;
  printf "enq=%d drew=%d\n" !enq !drew;
  [%expect {| enq=2 drew=1 |}]
;;

let%expect_test "progress bursts coalesce while immediate toggles and completion draw" =
  let enqueued = ref 0 in
  let drawn = ref 0 in
  let throttler =
    Chat_tui.Redraw_throttle.create ~fps:60. ~enqueue_redraw:(fun () -> incr enqueued)
  in
  for _ = 1 to 1_000 do
    Chat_tui.Redraw_throttle.request_redraw throttler
  done;
  Chat_tui.Redraw_throttle.tick throttler;
  Chat_tui.Redraw_throttle.redraw_immediate throttler ~draw:(fun () -> incr drawn);
  Chat_tui.Redraw_throttle.request_redraw throttler;
  Chat_tui.Redraw_throttle.redraw_immediate throttler ~draw:(fun () -> incr drawn);
  Chat_tui.Redraw_throttle.tick throttler;
  printf "enqueued=%d drawn=%d\n" !enqueued !drawn;
  [%expect {| enqueued=1 drawn=2 |}]
;;
