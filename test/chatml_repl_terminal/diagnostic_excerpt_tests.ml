open! Core
module Excerpt = Chatml_repl_terminal.Diagnostic_excerpt
module W = Agent_protocol.Developer_repl_wire

let ok = function
  | Ok value -> value
  | Error error -> raise_s [%sexp (error : Agent_protocol.Error.t)]
;;

let limits () =
  let document =
    Document_schema.Limits.create
      ~max_bytes:16_384
      ~max_depth:32
      ~max_fields:1_024
      ~max_nodes:2_048
    |> function
    | Ok limits -> limits
    | Error _ -> failwith "invalid fixture limits"
  in
  W.Limits.create
    ~request:document
    ~result:document
    ~page:document
    ~response:document
    ~max_source_bytes:512
    ~max_reference_bytes:384
    ~max_output_bytes:512
    ~max_page_items:2
    ~max_event_bytes:256
    ~max_registry_entries:16
  |> ok
;;

let digest source = Chatml_source.Digest.of_contents source ~checkpoint:Fn.id

let diagnostic source ~start ~stop =
  let limits = limits () in
  let location =
    W.Submission.Location.create
      ~limits
      ~source:
        (Chatml_source.Id.create
           ~package_namespace:"server"
           ~relative_path:"snippet.chatml"
         |> Or_error.ok_exn)
      ~digest:(digest source)
      ~start_byte:start
      ~end_byte:stop
    |> ok
  in
  W.Submission.Diagnostic.create
    ~limits
    ~message:"type mismatch"
    ~location:(Some location)
  |> ok
;;

let%expect_test "diagnostic byte authority maps to Unicode/tab terminal columns" =
  let source = "α\tbad" in
  let result =
    Excerpt.create
      (diagnostic source ~start:3 ~stop:6)
      ~submitted_source:(Some (source, digest source))
      ~max_line_bytes:256
  in
  (match result with
   | Some (Excerpt { location; prefix; before; marked; after; marker }) ->
     print_s
       [%sexp
         (location : string)
       , (prefix : string)
       , (before : string)
       , (marked : string)
       , (after : string)
       , (marker : string)]
   | Some (Location_only _) | None -> failwith "matching source lost its excerpt");
  [%expect
    {|
    ("server:snippet.chatml: bytes 3-6" "1 | " "\206\177    " bad ""
     "         ^^^")
    |}]
;;

let%expect_test "interior byte ranges expand to graphemes and EOF has a visible caret" =
  let show source start stop =
    match
      Excerpt.create
        (diagnostic source ~start ~stop)
        ~submitted_source:(Some (source, digest source))
        ~max_line_bytes:256
    with
    | Some (Excerpt { marked; marker; _ }) ->
      print_s [%sexp (marked : string), (marker : string)]
    | Some (Location_only _) | None -> failwith "valid source range lost excerpt"
  in
  show "é + bad" 1 2;
  (* The bounded context window is "...value = 42": a four-column gutter
     plus thirteen displayed columns puts the EOF caret at column seventeen. *)
  show "let value = 42" 14 14;
  [%expect
    {|
    ("e\204\129" "    ^")
    ("" "                 ^")
    |}]
;;

let%expect_test
    "unmatched, unavailable, oversized and invalid source ranges never guess local text"
  =
  let source = "let value = bad" in
  let location_only diagnostic submitted_source =
    match Excerpt.create diagnostic ~submitted_source ~max_line_bytes:64 with
    | Some (Location_only _) -> true
    | Some (Excerpt _) | None -> false
  in
  let diagnostic = diagnostic source ~start:12 ~stop:15 in
  let cases =
    [ None
    ; Some (source, digest "different")
    ; Some ("short", digest source)
    ; Some (String.make 65_537 'x', digest source)
    ]
  in
  print_s [%sexp (List.map cases ~f:(location_only diagnostic) : bool list)];
  [%expect {| (true true true true) |}]
;;
