open! Core
module F = Fixture
module S = Shell_access
module Stream = S.Sanitized_stream

let chunks text width =
  let rec loop index =
    if index >= String.length text
    then []
    else (
      let length = Int.min width (String.length text - index) in
      String.sub text ~pos:index ~len:length :: loop (index + length))
  in
  loop 0
;;

let sanitize ?(replacement = "[REDACTED]") ?(maximum = 65536) secrets parts =
  let output = Buffer.create 128 in
  let on_output text =
    F.check (Stdlib.String.is_valid_utf_8 text) "invalid filter UTF-8";
    F.check (String.length text <= 4096) "oversized filter event";
    Buffer.add_string output text
  in
  let stream =
    Stream.create
      ~secret_filter:(S.Secret_filter.create ~replacement secrets)
      ~max_bytes:maximum
      ~on_output
    |> Result.ok_or_failwith
  in
  List.iter parts ~f:(Stream.feed stream);
  Stream.finish stream;
  Stream.finish stream;
  Stream.feed stream "ignored";
  Buffer.contents output
;;

let partitions text ~check =
  for index = 0 to String.length text do
    check [ String.prefix text index; String.drop_prefix text index ]
  done;
  for width = 1 to Int.max 1 (String.length text) do
    check (chunks text width)
  done;
  for left = 0 to String.length text do
    let right = left + ((String.length text - left) / 2) in
    check
      [ String.prefix text left
      ; String.sub text ~pos:left ~len:(right - left)
      ; String.drop_prefix text right
      ]
  done
;;

let escaped_overlaps () =
  let input =
    "pre TO\027[31mKEN\027[0m abcde abccde 🙂 é\n"
    ^ "T\027]0;hidden\007O\027Pignored\027\\KEN"
    ^ " TO\194\155KEN TO\226\128\174KEN"
  in
  let expected =
    "pre [REDACTED] [REDACTED] [REDACTED] 🙂 é\n" ^ "[REDACTED] [REDACTED] [REDACTED]"
  in
  partitions input ~check:(fun parts ->
    F.equal (sanitize [ "TOKEN"; "abc"; "cde" ] parts) expected)
;;

let unicode_and_limits () =
  let input = "🙂aé\255z\240\159" in
  partitions input ~check:(fun parts -> F.equal (sanitize [] parts) "🙂aé�z�");
  let text = "🙂aéxyz" in
  for maximum = 1 to 12 do
    let output = sanitize ~maximum ~replacement:"" [] (chunks text 1) in
    F.check (String.length output <= maximum) "filter exceeded byte budget";
    F.check (String.is_prefix text ~prefix:output) "limit skipped a scalar"
  done;
  partitions "é🙂é" ~check:(fun parts -> F.equal (sanitize [ "é🙂" ] parts) "[REDACTED]é")
;;

let replacement_boundaries () =
  List.iter
    [ "", [ "ab" ]
    ; "aX", [ "ab" ]
    ; "Xb", [ "ab" ]
    ; "[ab]", [ "ab" ]
    ; "[REDACTED]", [ "[" ]
    ; "\027[31m", [ "ab" ]
    ; "[ok]", [ "a\000b" ]
    ; "[ok]", [ "\255" ]
    ]
    ~f:(fun (replacement, secrets) ->
      F.check
        (Result.is_error
           (Stream.support (S.Secret_filter.create ~replacement secrets) ~max_bytes:100))
        "unsafe filter accepted");
  F.equal (sanitize ~replacement:"<é>" [ "ab" ] [ "a"; "baba" ]) "<é>a";
  F.check
    (Result.is_error (Stream.support (S.Secret_filter.create [ "long" ]) ~max_bytes:3))
    "oversized secret accepted";
  F.check
    (Result.is_error
       (Stream.support (S.Secret_filter.create ~replacement:"long" []) ~max_bytes:3))
    "oversized replacement accepted"
;;

let discard_and_expansion () =
  let output = Buffer.create 32 in
  let stream =
    Stream.create
      ~secret_filter:(S.Secret_filter.create [ "TOKEN" ])
      ~max_bytes:100
      ~on_output:(Buffer.add_string output)
    |> Result.ok_or_failwith
  in
  Stream.feed stream "safe----TO";
  let before = Buffer.contents output in
  Stream.discard stream;
  Stream.finish stream;
  Stream.feed stream "KEN";
  F.equal (Buffer.contents output) before;
  let expanded =
    sanitize
      ~maximum:100
      ~replacement:(String.make 90 '#')
      [ "x" ]
      [ String.concat (List.init 10000 ~f:(fun _ -> "x ")) ]
  in
  F.check (String.length expanded = 100) "replacement expansion not bounded"
;;

let run () =
  escaped_overlaps ();
  unicode_and_limits ();
  replacement_boundaries ();
  discard_and_expansion ()
;;
