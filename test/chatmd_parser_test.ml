open Core

let sexp_of_doc doc = [%sexp (doc : Chatmd_ast.node list)]

let parse_and_report str =
  let lexbuf = Lexing.from_string str in
  try
    let doc = Chatmd_parser.document Chatmd_lexer.token lexbuf in
    print_s (sexp_of_doc doc)
  with
  | exn -> printf "ERR: %s\n" (Exn.to_string exn)
;;

let%expect_test "ast output" =
  List.iter
    [ "<msg></msg>"
    ; "<msg>Hello</msg>"
    ; "<user>Hello</user>"
    ; "<assistant>Hello</assistant>"
    ; "<msg>Hello > < world</msg>"
    ; "<tool />"
    ; "<tool_call></tool_call>"
    ; "<tool_response></tool_response>"
    ; "<reasoning><summary>this is a summary</summary></reasoning>"
    ; "<summary>hello</summary>"
    ; "<msg><yo/><doc/><yo><doc/></yo><doc/></msg>"
    ; "<system>hello</system>"
    ; "<developer>hello</developer>"
    ; "<system><doc src=\"./a.txt\" /></system>"
    ; "<import src=\"./a.txt\" />"
    ; "<img src=\"https://example.com/image.png\" alt=\"wide \n icon\" local/>"
    ; {|<img src="https://example.com/image.png" alt="wide \" \"
     icon" local/>|}
    ; "<img src=\"https://example.com/image.png\" alt=\"wide \n icon />"
    ; "<system>RAW|<doc src=\"./a.txt\" />|RAW</system>"
    ; "<msg>RAW|Hello|RAW</msg>"
    ; "plain" (* should now fail – top-level TEXT is forbidden *)
    ; "<yo/>" (* should fail – unknown tag at top level *)
    ; "<user> the one issues is that it does not respect white space or new lines. So: \
       RAW| <doc/> |RAW will still output the the  tags </user>"
    ]
    ~f:parse_and_report;
  [%expect
    {|
    ((Element Msg () ()))
    ((Element Msg () ((Text Hello))))
    ((Element User () ((Text Hello))))
    ((Element Assistant () ((Text Hello))))
    ((Element Msg () ((Text "Hello > < world"))))
    ((Element Tool () ()))
    ((Element Tool_call () ()))
    ((Element Tool_response () ()))
    ((Element Reasoning () ((Element Summary () ((Text "this is a summary"))))))
    ((Element Summary () ((Text hello))))
    ((Element Msg ()
      ((Text <yo/>) (Element Doc () ()) (Text <yo>) (Element Doc () ())
       (Text </yo>) (Element Doc () ()))))
    ((Element System () ((Text hello))))
    ((Element Developer () ((Text hello))))
    ((Element System () ((Element Doc ((src (./a.txt))) ()))))
    ((Element Import ((src (./a.txt))) ()))
    ((Element Img
      ((src (https://example.com/image.png)) (alt ( "wide \
                                                   \n icon")) (local ()))
      ()))
    ((Element Img
      ((src (https://example.com/image.png))
       (alt ( "wide \\\" \\\"\
             \n     icon"))
       (local ()))
      ()))
    ERR: (Failure
      "Unterminated quoted attribute value for alt starting at offset 41 (expected \")")
    ((Element System () ((Text "<doc src=\"./a.txt\" />"))))
    ((Element Msg () ((Text Hello))))
    ERR: (Failure "Unexpected text at top level: \"plain\"")
    ERR: (Failure "Unexpected text at top level: \"<yo/>\"")
    ((Element User ()
      ((Text
        " the one issues is that it does not respect white space or new lines. So:  <doc/>  will still output the the  tags "))))
    |}]
;;
