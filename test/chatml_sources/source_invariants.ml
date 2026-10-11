open Core
module S = Chatml_source

let checkpoint () = ()
let id path = S.Id.create ~package_namespace:"test" ~relative_path:path |> Or_error.ok_exn

let capture ?(path = "main.chatml") contents =
  S.create (id path) ~contents ~expected_digest:None ~max_bytes:None ~checkpoint
  |> Or_error.ok_exn
;;

(* Independent finite-state recognizer for bounded ASCII portable paths. It does
   not split components or call the production path constructor. *)
let reference_path text =
  let rec scan index component =
    if Int.equal index (String.length text)
    then Int.equal component 3
    else (
      let next =
        match text.[index] with
        | '/' -> if Int.equal component 3 then Some 0 else None
        | ':' | '\\' -> None
        | '.' -> Some (Int.min 3 (component + 1))
        | _ -> Some 3
      in
      Option.value_map next ~default:false ~f:(scan (index + 1)))
  in
  scan 0 0
;;

let%test_unit "portable path constructor agrees with a bounded automaton" =
  let rec words remaining prefix =
    let expected = reference_path prefix in
    let result = Source.Portable_path.create prefix in
    [%test_eq: bool] (Result.is_ok result) expected;
    [%test_eq: bool]
      (Result.is_ok (S.Id.create ~package_namespace:"p" ~relative_path:prefix))
      expected;
    if remaining > 0
    then
      List.iter [ 'a'; '.'; '/'; ':'; '\\' ] ~f:(fun c ->
        words (remaining - 1) (prefix ^ String.of_char c))
  in
  words 5 "";
  List.iter
    [ "é/x", true
    ; "\255", false
    ; "a\000b", false
    ; "a\nb", false
    ; String.make 1024 'a', true
    ; String.make 1025 'a', false
    ]
    ~f:(fun (path, expected) ->
      [%test_eq: bool] (Result.is_ok (Source.Portable_path.create path)) expected);
  assert (Result.is_error (S.Id.create ~package_namespace:"" ~relative_path:"a"));
  let other =
    S.Id.create ~package_namespace:"other" ~relative_path:"main.chatml" |> Or_error.ok_exn
  in
  assert (not (S.Id.equal (id "main.chatml") other))
;;

(* Model positions by line-break endpoints in the WHOLE byte sequence. A byte
   position inside CRLF remains on its preceding line until LF is consumed. *)
let reference_position text offset =
  let rec endpoints i result =
    if i >= String.length text
    then List.rev result
    else (
      match text.[i] with
      | '\r' when i + 1 < String.length text && Char.equal text.[i + 1] '\n' ->
        endpoints (i + 2) ((i + 2) :: result)
      | '\r' | '\n' -> endpoints (i + 1) ((i + 1) :: result)
      | _ -> endpoints (i + 1) result)
  in
  let starts = 0 :: endpoints 0 [] |> List.filter ~f:(fun p -> p <= offset) in
  List.length starts, offset - List.last_exn starts
;;

let%test_unit "every bounded byte range has exact source revision and coordinates" =
  List.iter [ ""; "abc"; "é\r\nx\ny\rz"; "\r\n\r\n"; "\000\255" ] ~f:(fun bytes ->
    let source = capture bytes in
    let changed = capture (bytes ^ "!") in
    let foreign = capture ~path:"other.chatml" bytes in
    for left = -1 to String.length bytes + 1 do
      for right = -1 to String.length bytes + 1 do
        let range = S.locate source ~start_byte:left ~end_byte:right ~checkpoint in
        if left >= 0 && right >= left && right <= String.length bytes
        then (
          let location = Or_error.ok_exn range in
          let span = S.Location.span location in
          List.iter
            [ left, span.left; right, span.right ]
            ~f:(fun (offset, position) ->
              [%test_eq: int] position.Source.offset offset;
              [%test_eq: int * int]
                (position.line, position.column)
                (reference_position bytes offset));
          [%test_eq: string]
            (S.read source location |> Or_error.ok_exn)
            (String.sub bytes ~pos:left ~len:(right - left));
          assert (Result.is_ok (S.of_span source span ~checkpoint));
          assert (Result.is_error (S.read changed location));
          assert (Result.is_error (S.read foreign location));
          let forged =
            { span with left = { span.left with column = span.left.column + 1 } }
          in
          assert (Result.is_error (S.of_span source forged ~checkpoint)))
        else assert (Result.is_error range)
      done
    done)
;;

exception Checkpoint_interrupted of int

let interrupt_every_checkpoint run =
  let calls = ref 0 in
  run ~checkpoint:(fun () -> Int.incr calls);
  assert (!calls > 0);
  for interruption = 1 to !calls do
    let seen = ref 0 in
    match
      run ~checkpoint:(fun () ->
        Int.incr seen;
        if Int.equal !seen interruption then raise (Checkpoint_interrupted interruption))
    with
    | () -> failwith "checkpoint exception was swallowed"
    | exception Checkpoint_interrupted actual -> [%test_eq: int] actual interruption
  done
;;

let%test_unit "capture hashes exact bytes and propagates every checkpoint interruption" =
  List.iter
    [ "", "e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855"
    ; "abc", "ba7816bf8f01cfea414140de5dae2223b00361a396177a9cb410ff61f20015ad"
    ]
    ~f:(fun (contents, digest) ->
      let expected_digest = S.Digest.of_hex digest |> Or_error.ok_exn in
      let source =
        S.create
          (id "hash.chatml")
          ~contents
          ~expected_digest:(Some expected_digest)
          ~max_bytes:None
          ~checkpoint
        |> Or_error.ok_exn
      in
      [%test_eq: string] (S.Digest.to_hex (S.digest source)) digest;
      assert (
        Result.is_error
          (S.create
             (S.id source)
             ~contents:(contents ^ "x")
             ~expected_digest:(Some expected_digest)
             ~max_bytes:None
             ~checkpoint)));
  let source = capture "abc" in
  List.iter [ -1; 0; 1; 2; 3; 4 ] ~f:(fun limit ->
    [%test_eq: bool]
      (Result.is_ok
         (S.create
            (id "main.chatml")
            ~contents:"abc"
            ~expected_digest:None
            ~max_bytes:(Some limit)
            ~checkpoint))
      (limit >= 3));
  let bytes = String.make 16385 'x' ^ "\r\nend" in
  interrupt_every_checkpoint (fun ~checkpoint ->
    S.create
      (id "large.chatml")
      ~contents:bytes
      ~expected_digest:None
      ~max_bytes:None
      ~checkpoint
    |> Or_error.ok_exn
    |> ignore);
  let captured = capture bytes in
  interrupt_every_checkpoint (fun ~checkpoint ->
    S.locate captured ~start_byte:0 ~end_byte:(String.length bytes) ~checkpoint
    |> Or_error.ok_exn
    |> ignore);
  [%test_eq: string] (S.contents source) "abc"
;;

let%expect_test "source locations retain exact revision ownership" =
  let source = capture "let total = 3" in
  let location =
    S.locate source ~start_byte:4 ~end_byte:9 ~checkpoint |> Or_error.ok_exn
  in
  print_s [%sexp (S.read source location |> Or_error.ok_exn : string)];
  print_s [%sexp (Result.is_ok (S.read (capture "let other = 3") location) : bool)];
  [%expect
    {|
    total
    false
    |}]
;;
