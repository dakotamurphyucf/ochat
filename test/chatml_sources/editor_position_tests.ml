open Core
module Captured = Chatml_source
module Editor = Chatml_editor_position

let editor_ok = function
  | Ok value -> value
  | Error error -> Error.raise_s (Editor.Error.sexp_of_t error)
;;

let editor_error = function
  | Error error -> error
  | Ok _ -> failwith "expected editor-coordinate conversion error"
;;

let checkpoint () = ()

let capture ?(path = "main.chatml") contents =
  let id =
    Captured.Id.create ~package_namespace:"positions" ~relative_path:path
    |> Or_error.ok_exn
  in
  Captured.create id ~contents ~expected_digest:None ~max_bytes:None ~checkpoint
  |> Or_error.ok_exn
;;

let index ?(max_index_entries = 100_000) source =
  Editor.create source ~max_index_entries ~checkpoint |> editor_ok
;;

let position line column = Editor.Position.create ~line ~column |> editor_ok

type reference_position =
  { byte_offset : int
  ; line : int
  ; byte_column : int
  ; utf16_column : int
  }

(* Uutf supplies scalar boundaries independently of the production decoder;
   this model handles line endings and columns by walking those scalars. *)
let reference_positions contents =
  let scalars =
    Uutf.String.fold_utf_8
      (fun reversed offset decoded ->
         match decoded with
         | `Uchar uchar -> (offset, Stdlib.Uchar.to_int uchar) :: reversed
         | `Malformed _ -> failwith "reference received invalid UTF-8")
      []
      contents
    |> List.rev
  in
  let width codepoint =
    if codepoint < 0x80
    then 1
    else if codepoint < 0x800
    then 2
    else if codepoint < 0x10000
    then 3
    else 4
  in
  let rec walk line byte_column utf16_column reversed = function
    | [] ->
      List.rev
        ({ byte_offset = String.length contents; line; byte_column; utf16_column }
         :: reversed)
    | (byte_offset, 13) :: (_, 10) :: rest ->
      let point = { byte_offset; line; byte_column; utf16_column } in
      walk (line + 1) 0 0 (point :: reversed) rest
    | (byte_offset, codepoint) :: rest ->
      let point = { byte_offset; line; byte_column; utf16_column } in
      if Int.equal codepoint 13 || Int.equal codepoint 10
      then walk (line + 1) 0 0 (point :: reversed) rest
      else
        walk
          line
          (byte_column + width codepoint)
          (utf16_column + if codepoint > 0xffff then 2 else 1)
          (point :: reversed)
          rest
  in
  walk 0 0 0 [] scalars
;;

let check_reference contents =
  let source = capture contents in
  let editor = index source in
  let points = reference_positions contents in
  List.iter points ~f:(fun point ->
    List.iter
      [ Editor.Encoding.Utf8_bytes, point.byte_column
      ; Editor.Encoding.Utf16_units, point.utf16_column
      ]
      ~f:(fun (encoding, column) ->
        let editor_position = position point.line column in
        [%test_eq: int]
          (Editor.byte_offset_of_position editor ~encoding editor_position ~checkpoint
           |> editor_ok)
          point.byte_offset;
        [%test_eq: Editor.Position.t]
          (Editor.position_of_byte_offset
             editor
             ~encoding
             ~byte_offset:point.byte_offset
             ~checkpoint
           |> editor_ok)
          editor_position));
  for byte_offset = 0 to String.length contents do
    if not (List.exists points ~f:(fun point -> Int.equal point.byte_offset byte_offset))
    then
      List.iter
        [ Editor.Encoding.Utf8_bytes; Editor.Encoding.Utf16_units ]
        ~f:(fun encoding ->
          assert (
            Result.is_error
              (Editor.position_of_byte_offset editor ~encoding ~byte_offset ~checkpoint)))
  done;
  List.iter
    (List.zip_exn (List.take points (List.length points - 1)) (List.tl_exn points))
    ~f:(fun (first, last) ->
      List.iter
        [ Editor.Encoding.Utf8_bytes, first.byte_column, last.byte_column
        ; Editor.Encoding.Utf16_units, first.utf16_column, last.utf16_column
        ]
        ~f:(fun (encoding, first_column, last_column) ->
          let range =
            Editor.Range.create
              ~start:(position first.line first_column)
              ~finish:(position last.line last_column)
            |> editor_ok
          in
          let location =
            Editor.location_of_range editor ~encoding range ~checkpoint |> editor_ok
          in
          let span = Captured.Location.span location in
          [%test_eq: int] span.left.offset first.byte_offset;
          [%test_eq: int] span.right.offset last.byte_offset;
          [%test_eq: Editor.Range.t]
            (Editor.range_of_location editor ~encoding location ~checkpoint |> editor_ok)
            range))
;;

let%test_unit "generated valid sources match an independent scalar model" =
  let scalar =
    Int.gen_incl 0 0x10f7ff
    |> Quickcheck.Generator.map ~f:(fun value ->
      let codepoint = if value >= 0xd800 then value + 0x800 else value in
      let buffer = Stdlib.Buffer.create 4 in
      Uutf.Buffer.add_utf_8 buffer (Stdlib.Uchar.of_int codepoint);
      Stdlib.Buffer.contents buffer)
  in
  let token =
    Quickcheck.Generator.weighted_union
      [ 3., scalar
      ; 2., Quickcheck.Generator.of_list [ "a"; "\000"; "\r"; "\n"; "\r\n"; "é"; "😀" ]
      ]
  in
  let generator =
    Quickcheck.Generator.list_with_length 32 token
    |> Quickcheck.Generator.map ~f:(String.concat ~sep:"")
  in
  Quickcheck.test ~trials:100 generator ~f:check_reference
;;

let%expect_test "line endings, supplementary scalars and EOF" =
  let editor = index (capture "é😀\r\nx\r\n") in
  List.iter
    [ Editor.Encoding.Utf8_bytes, position 0 2
    ; Editor.Encoding.Utf8_bytes, position 0 6
    ; Editor.Encoding.Utf16_units, position 0 1
    ; Editor.Encoding.Utf16_units, position 0 3
    ; Editor.Encoding.Utf16_units, position 2 0
    ]
    ~f:(fun (encoding, position) ->
      let byte_offset =
        Editor.byte_offset_of_position editor ~encoding position ~checkpoint |> editor_ok
      in
      print_s [%sexp (encoding : Editor.Encoding.t), (byte_offset : int)]);
  [%expect
    {|
    (Utf8_bytes 2)
    (Utf8_bytes 6)
    (Utf16_units 2)
    (Utf16_units 6)
    (Utf16_units 11)
    |}]
;;

let%test_unit "malformed bytes retain byte positions but block UTF-16" =
  List.iter
    [ "\192\128" (* overlong *)
    ; "\237\160\128" (* surrogate *)
    ; "\244\144\128\128" (* above U+10FFFF *)
    ; "\255"
    ; "\240\159" (* truncated *)
    ]
    ~f:(fun bytes ->
      let editor = index (capture bytes) in
      for byte_offset = 0 to String.length bytes do
        [%test_eq: int]
          (Editor.byte_offset_of_position
             editor
             ~encoding:Utf8_bytes
             (position 0 byte_offset)
             ~checkpoint
           |> editor_ok)
          byte_offset;
        assert (
          Result.is_error
            (Editor.position_of_byte_offset
               editor
               ~encoding:Utf16_units
               ~byte_offset
               ~checkpoint))
      done);
  let bytes = "a©\255" in
  let editor = index (capture bytes) in
  assert (
    Result.is_error
      (Editor.position_of_byte_offset
         editor
         ~encoding:Utf8_bytes
         ~byte_offset:2
         ~checkpoint))
;;

let%test_unit "malformed-source availability agrees with an independent decoder" =
  let generator =
    Quickcheck.Generator.list_with_length
      12
      (Quickcheck.Generator.of_list (List.init 256 ~f:Char.of_int_exn))
    |> Quickcheck.Generator.map ~f:String.of_char_list
  in
  Quickcheck.test ~trials:200 generator ~f:(fun bytes ->
    let expected =
      Uutf.String.fold_utf_8
        (fun valid _ -> function
           | `Uchar _ -> valid
           | `Malformed _ -> false)
        true
        bytes
    in
    let editor = index (capture bytes) in
    [%test_eq: bool]
      (Result.is_ok
         (Editor.position_of_byte_offset
            editor
            ~encoding:Utf16_units
            ~byte_offset:0
            ~checkpoint))
      expected)
;;

let%test_unit "a supplementary scalar across a sparse checkpoint stays exact" =
  let source = capture (String.make 4093 'a' ^ "😀" ^ "z") in
  let editor = index ~max_index_entries:2 source in
  [%test_eq: int]
    (Editor.byte_offset_of_position
       editor
       ~encoding:Utf16_units
       (position 0 4095)
       ~checkpoint
     |> editor_ok)
    4097;
  [%test_eq: Editor.Error.t]
    (Editor.byte_offset_of_position
       editor
       ~encoding:Utf16_units
       (position 0 4094)
       ~checkpoint
     |> editor_error)
    (Inside_utf16_surrogate_pair { line = 0; column = 4094 });
  for byte_offset = 4094 to 4096 do
    [%test_eq: Editor.Error.t]
      (Editor.position_of_byte_offset editor ~encoding:Utf8_bytes ~byte_offset ~checkpoint
       |> editor_error)
      (Inside_utf8_scalar { byte_offset })
  done
;;

let%test_unit "foreign revisions, CRLF midpoint and bounds reject" =
  let source = capture "x\r\ny" in
  let editor = index source in
  let location =
    Captured.locate source ~start_byte:2 ~end_byte:2 ~checkpoint |> Or_error.ok_exn
  in
  [%test_eq: Editor.Error.t]
    (Editor.range_of_location editor ~encoding:Utf8_bytes location ~checkpoint
     |> editor_error)
    (Inside_crlf { byte_offset = 2 });
  List.iter
    [ capture ~path:"foreign.chatml" "x\r\ny"; capture "x\r\nz" ]
    ~f:(fun foreign ->
      let location =
        Captured.locate foreign ~start_byte:0 ~end_byte:1 ~checkpoint |> Or_error.ok_exn
      in
      [%test_eq: Editor.Error.t]
        (Editor.range_of_location editor ~encoding:Utf8_bytes location ~checkpoint
         |> editor_error)
        Foreign_location);
  [%test_eq: Editor.Error.t]
    (Editor.position_of_byte_offset
       editor
       ~encoding:Utf8_bytes
       ~byte_offset:(-1)
       ~checkpoint
     |> editor_error)
    (Byte_offset_out_of_bounds (-1));
  assert (Result.is_error (Editor.Position.create ~line:(-1) ~column:0));
  assert (
    Result.is_error
      (Editor.byte_offset_of_position
         editor
         ~encoding:Utf8_bytes
         (position 99 0)
         ~checkpoint))
;;

let%test_unit "entry bound includes the base line and limits sparse checkpoints" =
  assert (Result.is_ok (Editor.create (capture "") ~max_index_entries:1 ~checkpoint));
  assert (Result.is_error (Editor.create (capture "") ~max_index_entries:0 ~checkpoint));
  assert (Result.is_error (Editor.create (capture "x\n") ~max_index_entries:1 ~checkpoint));
  let source = capture (String.make 4097 'x') in
  assert (Result.is_error (Editor.create source ~max_index_entries:1 ~checkpoint));
  assert (Result.is_ok (Editor.create source ~max_index_entries:2 ~checkpoint))
;;

exception Interrupted

let%test_unit "construction and conversion propagate checkpoint exceptions" =
  let raises_interrupted f =
    try
      f ();
      false
    with
    | Interrupted -> true
  in
  let source = capture "😀" in
  assert (
    raises_interrupted (fun () ->
      Editor.create source ~max_index_entries:10 ~checkpoint:(fun () -> raise Interrupted)
      |> ignore));
  let polls = ref 0 in
  assert (
    raises_interrupted (fun () ->
      Editor.create (capture "\255") ~max_index_entries:10 ~checkpoint:(fun () ->
        Int.incr polls;
        if Int.equal !polls 3 then raise Interrupted)
      |> ignore));
  [%test_eq: int] !polls 3;
  let crossing_source = capture (String.make 4095 'a' ^ "😀z") in
  List.iter [ 3; 5 ] ~f:(fun interrupt_at ->
    let polls = ref 0 in
    assert (
      raises_interrupted (fun () ->
        Editor.create crossing_source ~max_index_entries:10 ~checkpoint:(fun () ->
          Int.incr polls;
          if Int.equal !polls interrupt_at then raise Interrupted)
        |> ignore));
    [%test_eq: int] !polls interrupt_at);
  let editor = index (capture (String.make 128 'a')) in
  let polls = ref 0 in
  assert (
    raises_interrupted (fun () ->
      Editor.position_of_byte_offset
        editor
        ~encoding:Utf16_units
        ~byte_offset:64
        ~checkpoint:(fun () ->
          Int.incr polls;
          if Int.equal !polls 2 then raise Interrupted)
      |> ignore));
  [%test_eq: int] !polls 2
;;
