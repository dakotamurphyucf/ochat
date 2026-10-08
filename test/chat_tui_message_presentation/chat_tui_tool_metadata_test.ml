open Core

let make_model () : Chat_tui.Model.t =
  let open Chat_tui in
  let scroll_box = Notty_scroll_box.create Notty.I.empty in
  Model.create
    ~history_items:[]
    ~messages:[]
    ~input_line:""
    ~auto_follow:true
    ~msg_buffers:(Hashtbl.create (module String))
    ~function_name_by_id:(Hashtbl.create (module String))
    ~reasoning_idx_by_id:(Hashtbl.create (module String))
    ~tool_output_by_index:(Hashtbl.create (module Int))
    ~tasks:[]
    ~kv_store:(Hashtbl.create (module String))
    ~fetch_sw:None
    ~scroll_box
    ~cursor_pos:0
    ~selection_anchor:None
    ~mode:Chat_tui.Model.Insert
    ~draft_mode:Chat_tui.Model.Plain
    ~selected_msg:None
    ~undo_stack:[]
    ~redo_stack:[]
    ~cmdline:""
    ~cmdline_cursor:0
;;

module Payload = History_entry.Payload
module Model = Chat_tui.Model
module Types = Chat_tui.Types

let ok = Result.ok_or_failwith
let id sequence = History_entry.Id.create ~namespace:"tool-metadata" ~sequence |> ok

let entry sequence semantic =
  History_entry.create_with_id
    ~id:(id sequence)
    (Payload.Semantic.create semantic ~metadata:Payload.Metadata.empty
     |> ok
     |> Payload.authored)
;;

let call sequence name arguments =
  entry
    sequence
    (Call
       { kind = Function
       ; name
       ; namespace = Absent
       ; input_bytes = arguments
       ; async = Absent
       })
;;

let output sequence target =
  entry
    sequence
    (Result { relation = Bound (id target); kind = Function; output = Text "result" })
;;

let install model entries =
  let projection = Chat_tui.Conversation.project_entries entries in
  Model.set_history_items model entries;
  Model.reconcile_projected_rows model (Chat_tui.Conversation.rows projection);
  Model.reconcile_messages model (Chat_tui.Conversation.messages projection);
  Model.rebuild_tool_output_index model
;;

let show model sequence =
  match
    Model.tool_output_for_row
      model
      ~id:(Chat_tui.Projected_message.Id.canonical (id sequence))
  with
  | None -> print_endline "none"
  | Some (Types.Read_file { path }) ->
    printf "Read_file %s\n" (Option.value path ~default:"<none>")
  | Some (Read_directory { path }) ->
    printf "Read_directory %s\n" (Option.value path ~default:"<none>")
  | Some Apply_patch -> print_endline "Apply_patch"
  | Some (Other { name }) -> printf "Other %s\n" (Option.value name ~default:"<none>")
;;

let%expect_test "actual committed neutral calls classify host-bound results" =
  let model = make_model () in
  install
    model
    [ call 0 "read_file" {|{"file":"lib/foo.ml"}|}
    ; output 1 0
    ; call 2 "read_directory" {|{"path":"/tmp"}|}
    ; output 3 2
    ; call 4 "apply_patch" "*** Begin Patch"
    ; output 5 4
    ; call 6 "custom" "opaque input"
    ; output 7 6
    ];
  List.iter [ 1; 3; 5; 7 ] ~f:(show model);
  [%expect
    {|
    Read_file lib/foo.ml
    Read_directory /tmp
    Apply_patch
    Other custom
  |}]
;;

let%expect_test "metadata reconstruction is independent of result arrival order" =
  let model = make_model () in
  install model [ output 3 2; output 1 0 ];
  show model 1;
  install
    model
    [ output 3 2
    ; output 1 0
    ; call 0 "read_file" {|{"file":"README.md"}|}
    ; call 2 "apply_patch" "patch"
    ];
  List.iter [ 3; 1 ] ~f:(show model);
  [%expect
    {|
    Other <none>
    Apply_patch
    Read_file README.md
  |}]
;;

let%expect_test "missing or malformed path stays unavailable without fabricated filename" =
  let model = make_model () in
  install
    model
    [ call 0 "read_file" "not-json"
    ; output 1 0
    ; call 2 "read_directory" "{}"
    ; output 3 2
    ];
  List.iter [ 1; 3 ] ~f:(show model);
  [%expect
    {|
    Read_file <none>
    Read_directory <none>
  |}]
;;

let%expect_test "duplicate provider aliases cannot redirect a host-bound result" =
  let metadata = { Payload.Metadata.empty with call_id = Value "same-alias" } in
  let make sequence name input =
    History_entry.create_with_id
      ~id:(id sequence)
      (Payload.Semantic.create
         (Call
            { kind = Function
            ; name
            ; namespace = Absent
            ; input_bytes = input
            ; async = Absent
            })
         ~metadata
       |> ok
       |> Payload.authored)
  in
  let model = make_model () in
  install
    model
    [ make 0 "read_file" {|{"file":"first.ml"}|}
    ; make 2 "apply_patch" "patch"
    ; output 3 0
    ; output 4 2
    ];
  ignore
    (Model.mark_tool_call_finished model ~call_id:"same-alias" ~outcome:Returned : bool);
  assert (Option.is_none (Model.tool_call_outcome_for_message model ~idx:0));
  assert (Option.is_none (Model.tool_call_outcome_for_message model ~idx:1));
  List.iter [ 3; 4 ] ~f:(show model);
  [%expect
    {|
    Read_file first.ml
    Apply_patch
  |}]
;;

let%expect_test "public redaction removes previously retained call metadata" =
  let model = make_model () in
  let entries = [ call 0 "read_file" {|{"file":"secret.ml"}|}; output 1 0 ] in
  install model entries;
  show model 1;
  let public =
    List.map entries ~f:(fun value ->
      Agent_protocol.Public.History.redacted
        (History_entry.id value)
        ~provenance:Canonical
        (Agent_protocol.Public.History.Redaction.create ~disclosed_header:None)
      |> Result.map_error ~f:(fun error -> error.Agent_protocol.Error.message)
      |> ok)
  in
  Model.rebuild_tool_output_index_for_public model public;
  show model 1;
  [%expect
    {|
    Read_file secret.ml
    none
  |}]
;;

let%expect_test "lang_of_path maps common extensions" =
  let open Chat_tui.Renderer in
  let cases =
    [ "foo.ml"
    ; "foo.mli"
    ; "main.py"
    ; "lib.rs"
    ; "app.js"
    ; "component.jsx"
    ; "app.ts"
    ; "component.tsx"
    ; "README.md"
    ; "data.json"
    ; "script.sh"
    ; "notes.txt"
    ; "noext"
    ; "UPPER.ML"
    ]
  in
  List.iter cases ~f:(fun path ->
    let lang = lang_of_path path |> Option.value ~default:"<none>" in
    Printf.printf "%s -> %s\n" path lang);
  [%expect
    {|
      foo.ml -> ocaml
      foo.mli -> ocaml
      main.py -> python
      lib.rs -> rust
      app.js -> javascript
      component.jsx -> jsx
      app.ts -> typescript
      component.tsx -> tsx
      README.md -> markdown
      data.json -> json
      script.sh -> bash
      notes.txt -> <none>
      noext -> <none>
      UPPER.ML -> ocaml
    |}]
;;
