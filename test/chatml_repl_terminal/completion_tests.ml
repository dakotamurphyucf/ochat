open! Core
module Completion = Chatml_repl_terminal.Completion
module Help = Chatml_repl_terminal.Help

let query ?(binding_names = []) ?cursor_byte source =
  Completion.query
    ~source
    ~cursor_byte:(Option.value cursor_byte ~default:(String.length source))
    ~binding_names
;;

let labels = function
  | None -> []
  | Some completion -> List.map (Completion.items completion) ~f:Completion.Item.label
;;

let%expect_test
    "local completion uses exact full-token replacement and rejects stale requests"
  =
  let source = "let answer = matZZ" in
  let cursor_byte = String.length source - 2 in
  let completion = query ~cursor_byte source |> Option.value_exn in
  print_s [%sexp (labels (Some completion) : string list)];
  print_s
    [%sexp (Completion.accept completion ~source ~cursor_byte : (string * int) option)];
  print_s
    [%sexp
      (Completion.accept completion ~source:(source ^ " ") ~cursor_byte
       : (string * int) option)];
  print_s
    [%sexp
      (Completion.accept completion ~source ~cursor_byte:(cursor_byte - 1)
       : (string * int) option)];
  [%expect
    {|
    (match)
    (("let answer = match" 18))
    ()
    ()
    |}]
;;

let%expect_test "commands and topics complete without evaluating or submitting" =
  print_s [%sexp (labels (query ":he") : string list)];
  print_s [%sexp (labels (query "let x = 3\n:help co") : string list)];
  let completion = query ":he" |> Option.value_exn in
  print_s
    [%sexp
      (Completion.accept completion ~source:":he" ~cursor_byte:3 : (string * int) option)];
  [%expect
    {|
    (:help)
    (commands completion)
    ((:help 5))
    |}]
;;

let%test_unit
    "opaque lexical contexts, qualifiers and oversized input have no local guesses"
  =
  List.iter
    [ "\"mat"
    ; "'mat"
    ; "`mat"
    ; "(* mat"
    ; "(* nested (* mat *) mat"
    ; "Task.mat"
    ; ""
    ; String.make 65_537 'm'
    ]
    ~f:(fun source ->
      if Option.is_some (query source)
      then failwith "suggested a local name without lexical evidence");
  let source = "\"match\"" in
  if Option.is_some (query ~cursor_byte:4 source)
  then failwith "completed inside a string";
  if Option.is_some (query ~cursor_byte:(-1) "mat")
  then failwith "accepted negative cursor";
  if Option.is_some (query ~cursor_byte:4 "mat") then failwith "accepted cursor past EOF"
;;

let%test_unit "bounded catalog, deterministic selection and supplied name validation" =
  let binding_names =
    "match"
    :: "match"
    :: "bad\027name"
    :: List.init 200 ~f:(fun i -> "m" ^ Int.to_string i)
  in
  let completion = query ~binding_names "m" |> Option.value_exn in
  let names = labels (Some completion) in
  if List.length names > 16 then failwith "unbounded completion catalog";
  if List.count names ~f:(String.equal "match") <> 1 then failwith "duplicate completion";
  if List.exists names ~f:(String.is_substring ~substring:"\027")
  then failwith "unsafe name";
  let last = Completion.previous completion in
  if Completion.selected_index last <> List.length names - 1
  then failwith "previous did not wrap";
  if Completion.selected_index (Completion.next last) <> 0
  then failwith "next did not wrap";
  let returned =
    List.fold names ~init:completion ~f:(fun state _ -> Completion.next state)
  in
  if Completion.selected_index returned <> 0
  then failwith "selection cycle did not return"
;;

let%test_unit "help topics are finite and unknown topics do not echo untrusted bytes" =
  List.iter Help.topics ~f:(fun topic ->
    if String.is_prefix (Help.text ~topic:(Some topic)) ~prefix:"Unknown"
    then failwith "advertised help topic is missing");
  let unknown = Help.text ~topic:(Some "\027[2J") in
  if String.is_substring unknown ~substring:"\027"
  then failwith "unknown help echoed controls";
  if not (String.is_substring (Help.text ~topic:(Some "commands")) ~substring:":help")
  then failwith "help omitted itself"
;;

let%expect_test
    "live completion sites are lexical qualified paths, including empty member prefix"
  =
  let site source =
    Completion.context ~source ~cursor_byte:(String.length source)
    |> Option.map ~f:(fun context ->
      Completion.Context.path context, Completion.Context.prefix context)
  in
  List.iter
    [ "List.ma"; "Outer.Inner."; "map"; ":help co"; "(* List.ma" ]
    ~f:(fun source -> print_s [%sexp (site source : (string list * string) option)]);
  let source = "List.ma" in
  let completion =
    Completion.query_with_session_names
      ~source
      ~cursor_byte:7
      ~binding_names:[ "match" ]
      ~session_names:[ "map"; "map2" ]
    |> Option.value_exn
  in
  print_s
    [%sexp (Completion.accept completion ~source ~cursor_byte:7 : (string * int) option)];
  [%expect
    {|
    (((List) ma))
    (((Outer Inner) ""))
    ((() map))
    ()
    ()
    ((List.map 8))
    |}]
;;

let%test_unit
    "context equality binds whole source and exact cursor; empty member names accept \
     safely"
  =
  let context source cursor_byte =
    Completion.context ~source ~cursor_byte |> Option.value_exn
  in
  let first = context "List.ma" 7 in
  if
    Completion.Context.equal first (context "x = List.ma" 11)
    || Completion.Context.equal first (context "List.ma" 6)
  then failwith "context identity ignored source or cursor";
  let source = "List." in
  let completion =
    Completion.query_with_session_names
      ~source
      ~cursor_byte:5
      ~binding_names:[]
      ~session_names:[ "map" ]
    |> Option.value_exn
  in
  match Completion.accept completion ~source ~cursor_byte:5 with
  | Some ("List.map", 8) -> ()
  | Some _ | None -> failwith "empty member prefix replacement failed"
;;

let%test_unit
    "qualified whitespace retains module context and opaque prefixes never become \
     session reads"
  =
  let source = "List . ma" in
  let context =
    Completion.context ~source ~cursor_byte:(String.length source) |> Option.value_exn
  in
  if not (List.equal String.equal (Completion.Context.path context) [ "List" ])
  then failwith "qualified whitespace was interpreted as an unqualified site";
  List.iter [ "'mat"; "`Mat" ] ~f:(fun source ->
    if Option.is_some (Completion.context ~source ~cursor_byte:(String.length source))
    then failwith "opaque identifier prefix became a session read")
;;

let%test_unit
    "no-op editor transitions retain session suggestions; history clears site notices"
  =
  let module State = Chatml_repl_terminal.State in
  let must = function
    | Ok value -> value
    | Error error -> raise_s [%sexp (error : Agent_protocol.Error.t)]
  in
  let limits =
    State.Limits.create
      ~max_source_bytes:4096
      ~max_source_lines:32
      ~max_history_entries:16
      ~max_history_bytes:4096
      ~max_notice_bytes:1024
    |> must
  in
  let state = State.create ~limits ~context:"completion fixture" |> must in
  let state = State.submitted state ~source:"let saved = 42" ~preserve_editor:false in
  let editor = State.Editor.insert (State.editor state) ~limits "List.ma" |> must in
  let state =
    State.with_editor state editor
    |> must
    |> fun state -> State.with_session_completion state ~names:[ "map" ] ~notice:None
  in
  List.iter
    [ State.Editor.delete; (fun editor -> State.Editor.move editor End) ]
    ~f:(fun noop ->
      let next = State.with_editor state (noop (State.editor state)) |> must in
      match State.completion next with
      | None -> failwith "no-op edit discarded session suggestions"
      | Some completion ->
        if
          not
            (Completion.Item.equal_origin
               (Completion.Item.origin (Completion.selected completion))
               Session_binding)
        then failwith "no-op edit substituted local suggestions");
  let state =
    State.with_session_completion
      state
      ~names:[]
      ~notice:(Some "Session completion unavailable")
  in
  let searched = State.start_history_search state in
  if
    Option.is_some (State.completion_notice (State.accept_history_search searched))
    || Option.is_some (State.completion_notice (State.cancel_history_search searched))
    || Option.is_some (State.completion_notice (State.history_previous state))
    || Option.is_some
         (State.completion_notice
            (State.submitted state ~source:"next" ~preserve_editor:false))
  then failwith "editor lifecycle retained a completion notice from another site"
;;
