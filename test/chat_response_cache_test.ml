open Core

(* Ensure we can reference the nested [Cache] module inside
   [Chat_response]. *)
module Cache = Chat_response.Cache
module CM = Prompt.Chat_markdown

let make_key url : CM.agent_content = { url; is_local = false; items = [] }

let%expect_test "find_or_add returns cached value while still fresh" =
  let cache = Cache.create ~max_size:10 () in
  let calls = ref 0 in
  let default () =
    incr calls;
    Printf.sprintf "v%i" !calls
  in
  let key = make_key "foo" in
  let ttl = Time_ns.Span.of_int_sec 10 in
  let _v1 = Cache.find_or_add cache key ~ttl ~default in
  let _v2 = Cache.find_or_add cache key ~ttl ~default in
  (* The [default] callback should have been executed exactly once. *)
  print_s [%sexp (!calls : int)];
  [%expect {| 1 |}]
;;

let%expect_test "find_or_add recomputes after ttl expiry" =
  let cache = Cache.create ~max_size:10 () in
  let calls = ref 0 in
  let default () =
    incr calls;
    Printf.sprintf "v%i" !calls
  in
  let key = make_key "bar" in
  let ttl_zero = Time_ns.Span.zero in
  let _v1 = Cache.find_or_add cache key ~ttl:ttl_zero ~default in
  (* Immediately call again – because [ttl] was zero, the first value is
     already expired and the callback must run a second time. *)
  let _v2 = Cache.find_or_add cache key ~ttl:ttl_zero ~default in
  print_s [%sexp (!calls : int)];
  [%expect {| 2 |}]
;;
