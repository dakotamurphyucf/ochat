open Core

(* Ensure we can reference the nested [Cache] module inside
   [Chat_response]. *)
module Cache = Chat_response.Cache
module CM = Prompt.Chat_markdown

let make_key url : CM.agent_content = { url; is_local = false; items = [] }

let%expect_test "concurrent cache publication never exposes a partial binary file" =
  Eio_main.run (fun env ->
    let directory = Core_unix.mkdtemp "/tmp/ochat-cache-publication.XXXXXX" in
    let directory = Eio.Path.(Eio.Stdenv.fs env / directory) in
    Exn.protect
      ~finally:(fun () -> Eio.Path.rmtree ~missing_ok:true directory)
      ~f:(fun () ->
        let file = Eio.Path.(directory / "cache.bin") in
        let key = make_key "concurrent" in
        let ttl = Time_ns.Span.of_int_sec 3600 in
        let values = [ String.make 131_072 'a'; String.make 262_144 'b' ] in
        let caches =
          List.map values ~f:(fun value ->
            let cache = Cache.create ~max_size:10 () in
            Cache.find_or_add cache key ~ttl ~default:(fun () -> value) |> ignore;
            cache)
        in
        Cache.save ~file (List.hd_exn caches);
        let read () =
          let cache = Cache.load ~file ~max_size:10 () in
          let value =
            Cache.find_or_add cache key ~ttl ~default:(fun () ->
              failwith "publication lost a complete cache entry")
          in
          assert (List.mem values value ~equal:String.equal)
        in
        Eio.Fiber.all
          (List.map caches ~f:(fun cache () ->
             for _ = 1 to 40 do
               Cache.save ~file cache
             done)
           @ [ (fun () ->
                 for _ = 1 to 80 do
                   read ()
                 done)
             ]);
        read ();
        print_s [%sexp (Eio.Path.read_dir directory : string list)]));
  [%expect {| (cache.bin) |}]
;;

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
