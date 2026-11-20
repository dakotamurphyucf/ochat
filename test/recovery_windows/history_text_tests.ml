open! Core
module P = Agent_protocol
module T = P.History_text

let ok = function
  | Ok result -> result
  | Error (error : P.Error.t) -> failwith error.message
;;

let%test_unit "every bounded slice is a whole-scalar prefix and its continuation advances"
  =
  let scalars = [ "A"; "\r"; "\n"; "β"; "界"; "😀"; "e"; "́" ] in
  let text = String.concat scalars in
  let boundaries =
    List.folding_map scalars ~init:0 ~f:(fun n scalar ->
      let offset = n + String.length scalar in
      offset, offset)
    |> List.cons 0
  in
  let part = T.create ~index:3 ~text |> ok in
  for start = 0 to String.length text do
    for stop = start to String.length text do
      for max_bytes = 1 to 5 do
        let boundary i = List.mem boundaries i ~equal:Int.equal in
        let candidates =
          List.filter boundaries ~f:(fun n ->
            n >= start && n <= stop && n - start <= max_bytes)
        in
        match T.read ~max_bytes part ~start ~stop with
        | Error error when not (boundary start && boundary stop) ->
          assert (P.Error.equal_code error.code Invalid_request)
        | Error error ->
          assert (start < stop && List.equal Int.equal candidates [ start ]);
          assert (P.Error.equal_code error.code Resource_limit)
        | Ok slice ->
          assert (boundary start && boundary stop);
          let finish = List.last_exn candidates in
          assert (
            String.equal slice.text (String.sub text ~pos:start ~len:(finish - start)));
          assert (
            Option.equal
              Int.equal
              slice.next_offset
              (Option.some_if (finish < stop) finish));
          assert (finish > start || start = stop)
      done
    done
  done
;;

let%expect_test
    "search coordinates select the original match even after snippet truncation"
  =
  let text = String.make 600 'x' ^ "βNEEDle" ^ String.make 600 'y' in
  let part = T.create ~index:2 ~text |> ok in
  let matcher = P.Search_term.create "βneedle" |> ok |> T.Literal.create in
  let found = T.Literal.find matcher part |> ok |> Option.value_exn in
  let slice = T.read part ~start:found.start ~stop:found.stop |> ok in
  printf
    "part %d bytes [%d,%d) read %s; snippet starts at byte %d\n"
    part.index
    found.start
    found.stop
    slice.text
    (found.start - P.Search_snippet.highlight_start found.snippet);
  [%expect {| part 2 bytes [600,608) read βNEEDle; snippet starts at byte 348 |}]
;;
