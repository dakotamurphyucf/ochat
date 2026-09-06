open Core
module Value_codec = Chatml.Chatml_value_codec

let snapshot_of_item (item : Openai.Responses.Item.t) : Session.Snapshot.t =
  let value = Value_codec.jsonaf_to_value (Openai.Responses.Item.jsonaf_of_t item) in
  match Value_codec.Snapshot.of_value value with
  | Ok snapshot -> snapshot
  | Error msg -> failwith msg
;;

let moderator_snapshot () =
  let module Snapshot = Session.Snapshot in
  let module Moderator = Session.Moderator_snapshot in
  let item =
    { Moderator.Item.id = "host-message-1"
    ; value =
        snapshot_of_item
          (Openai.Responses.Item.Output_message
             { role = Openai.Responses.Output_message.Assistant
             ; id = "host-message-1"
             ; content =
                 [ Openai.Responses.Output_message.
                     { annotations = []
                     ; text = "synthetic moderation output"
                     ; _type = "output_text"
                     }
                 ]
             ; status = "completed"
             ; phase = None
             ; _type = "message"
             })
    }
  in
  let overlay =
    { Moderator.Overlay.empty with
      appended_items = [ item ]
    ; halted_reason = Some "done"
    }
  in
  Moderator.create
    ~script_id:"main"
    ~script_source_hash:"source-hash-1"
    ~current_state:(Snapshot.Record [ "count", Snapshot.Int 3 ])
    ~queued_internal_events:
      [ Snapshot.Variant ("Internal_done", [ Snapshot.String "queued" ]) ]
    ~halted:true
    ~overlay
    ()
;;

let reasoning id : Openai.Responses.Item.t =
  Openai.Responses.Item.Reasoning { summary = []; _type = "reasoning"; id; status = None }
;;

let migration_history () : Openai.Responses.Item.t list =
  let module Responses = Openai.Responses in
  [ Responses.Item.Input_message
      { role = Responses.Input_message.User
      ; content =
          [ Text { text = "input"; _type = "input_text" }
          ; Image
              { image_url = "https://example.test/image"
              ; detail = "low"
              ; _type = "input_image"
              }
          ]
      ; _type = "message"
      }
  ; Responses.Item.Output_message
      { role = Responses.Output_message.Assistant
      ; id = "output"
      ; content = [ { annotations = []; text = "output"; _type = "output_text" } ]
      ; status = "completed"
      ; phase = Some "final_answer"
      ; _type = "message"
      }
  ; Responses.Item.Reasoning
      { summary = [ { text = "reason"; _type = "summary_text" } ]
      ; _type = "reasoning"
      ; id = "reasoning"
      ; status = Some "completed"
      }
  ; Responses.Item.Function_call
      { name = "function"
      ; arguments = "{}"
      ; call_id = "shared"
      ; _type = "function_call"
      ; id = Some "function-id"
      ; status = Some "completed"
      }
  ; Responses.Item.Custom_tool_call
      { name = "custom"
      ; input = "input"
      ; call_id = "shared"
      ; _type = "custom_tool_call"
      ; id = Some "custom-id"
      }
  ; Responses.Item.Function_call_output
      { output = Responses.Tool_output.Output.Text "function output"
      ; call_id = "shared"
      ; _type = "function_call_output"
      ; id = Some "function-output-id"
      ; status = Some "completed"
      }
  ; Responses.Item.Custom_tool_call_output
      { output =
          Responses.Tool_output.Output.Content [ Input_text { text = "custom output" } ]
      ; call_id = "shared"
      ; _type = "custom_tool_call_output"
      ; id = Some "custom-output-id"
      }
  ]
;;

let ok_exn = function
  | Ok value -> value
  | Error error -> failwith error
;;

(* -------------------------------------------------------------------------- *)
(*  Session persistence – round-trip and legacy upgrade tests                  *)
(* -------------------------------------------------------------------------- *)

let%expect_test "session round-trip save/load" =
  let tmp_root =
    Filename.concat
      Filename.temp_dir_name
      ("session_rt_" ^ Int.to_string (Random.int 1_000_000))
  in
  Core_unix.mkdir_p tmp_root;
  (* Construct a fresh session value. *)
  let session =
    Session.create
      ~prompt_file:"prompt.chatmd"
      ~moderator_snapshot:(moderator_snapshot ())
      ()
  in
  Eio_main.run
  @@ fun env ->
  let snapshot_file = Filename.concat tmp_root "snapshot.bin" in
  let snapshot_path = Eio.Path.(env#fs / snapshot_file) in
  (* Persist to disk, then load back. *)
  Session.Io.File.write snapshot_path session;
  let loaded = Session.Io.File.read snapshot_path in
  let same = Sexp.equal (Session.sexp_of_t session) (Session.sexp_of_t loaded) in
  print_s [%sexp (same : bool)];
  [%expect {| true |}]
;;

let%expect_test "legacy V0 → latest upgrade" =
  let legacy : Session.Legacy.V0.t =
    { id = "legacy"
    ; prompt_file = "prompt.chatmd"
    ; history = []
    ; tasks = []
    ; kv_store = []
    ; vfs_root = "vfs"
    }
  in
  let upgraded = Session.Legacy.upgrade_v0 legacy in
  let version_ok = Int.equal upgraded.version Session.current_version
  and id_ok = String.equal upgraded.id legacy.id
  and moderator_snapshot_none = Option.is_none upgraded.moderator_state.legacy_snapshot in
  print_s [%sexp { version_ok : bool; id_ok : bool; moderator_snapshot_none : bool }];
  [%expect {| ((version_ok true) (id_ok true) (moderator_snapshot_none true)) |}]
;;

let%expect_test "legacy V1 → latest upgrade" =
  let legacy : Session.Legacy.V1.t =
    { version = 1
    ; id = "legacy-v1"
    ; prompt_file = "prompt.chatmd"
    ; history = []
    ; tasks = []
    ; kv_store = []
    ; vfs_root = "vfs"
    }
  in
  let upgraded = Session.Legacy.upgrade_v1 legacy in
  let version_ok = Int.equal upgraded.version Session.current_version
  and id_ok = String.equal upgraded.id legacy.id
  and prompt_copy_none = Option.is_none upgraded.local_prompt_copy
  and moderator_snapshot_none = Option.is_none upgraded.moderator_state.legacy_snapshot in
  print_s
    [%sexp
      { version_ok : bool
      ; id_ok : bool
      ; prompt_copy_none : bool
      ; moderator_snapshot_none : bool
      }];
  [%expect
    {|
    ((version_ok true) (id_ok true) (prompt_copy_none true)
     (moderator_snapshot_none true))
    |}]
;;

let%expect_test "legacy V2 → latest upgrade" =
  let legacy : Session.Legacy.V2.t =
    { version = 2
    ; id = "legacy-v2"
    ; prompt_file = "prompt.chatmd"
    ; local_prompt_copy = Some "prompt.chatmd"
    ; history = []
    ; tasks = []
    ; kv_store = []
    ; vfs_root = "vfs"
    }
  in
  let upgraded = Session.Legacy.upgrade_v2 legacy in
  let version_ok = Int.equal upgraded.version Session.current_version
  and id_ok = String.equal upgraded.id legacy.id
  and prompt_copy_kept =
    Option.equal String.equal upgraded.local_prompt_copy legacy.local_prompt_copy
  and moderator_snapshot_none = Option.is_none upgraded.moderator_state.legacy_snapshot in
  print_s
    [%sexp
      { version_ok : bool
      ; id_ok : bool
      ; prompt_copy_kept : bool
      ; moderator_snapshot_none : bool
      }];
  [%expect
    {|
    ((version_ok true) (id_ok true) (prompt_copy_kept true)
     (moderator_snapshot_none true))
    |}]
;;

let%expect_test "V0-V3 migrate deterministically to staged V4" =
  let history = [ reasoning "same"; reasoning "same" ] in
  let v3 : Session.Legacy.V3.t =
    { version = 3
    ; id = "session:legacy"
    ; prompt_file = "prompt.chatmd"
    ; local_prompt_copy = Some "prompt.chatmd"
    ; history
    ; tasks = []
    ; moderator_snapshot = Some (moderator_snapshot ())
    ; kv_store = [ "key", "value" ]
    ; vfs_root = "vfs"
    }
  in
  let first = Session.V4.of_v3 v3 |> ok_exn in
  let second = Session.V4.of_v3 v3 |> ok_exn in
  let ids session =
    List.map session.Session.V4.history ~f:(fun entry ->
      History_entry.id entry |> History_entry.Id.to_string)
  in
  print_s
    [%sexp
      { ids = (ids first : string list)
      ; deterministic = (List.equal String.equal (ids first) (ids second) : bool)
      ; payloads_preserved =
          (Sexp.equal
             ([%sexp_of: Openai.Responses.Item.t list] history)
             ([%sexp_of: Openai.Responses.Item.t list]
                (History_entry.items first.history))
           : bool)
      ; next_sequence = (first.next_history_sequence : int)
      ; moderator_preserved =
          (Option.is_some first.moderator_state.legacy_snapshot : bool)
      ; valid = (Result.is_ok (Session.V4.validate first) : bool)
      }];
  [%expect
    {|
    ((ids (14:session:legacy:0 14:session:legacy:1)) (deterministic true)
     (payloads_preserved true) (next_sequence 3) (moderator_preserved true)
     (valid true))
    |}]
;;

let%expect_test "every legacy migration preserves all history variants and fields" =
  let history = migration_history () in
  let base_v3 : Session.Legacy.V3.t =
    { version = 3
    ; id = "all-variants"
    ; prompt_file = "prompt.chatmd"
    ; local_prompt_copy = Some "prompt.chatmd"
    ; history
    ; tasks = [ Session.Task.create ~id:"task" ~title:"title" () ]
    ; moderator_snapshot = Some (moderator_snapshot ())
    ; kv_store = [ "key", "value" ]
    ; vfs_root = "custom-vfs"
    }
  in
  let migrations =
    [ Session.V4.of_v0
        { id = base_v3.id
        ; prompt_file = base_v3.prompt_file
        ; history
        ; tasks = base_v3.tasks
        ; kv_store = base_v3.kv_store
        ; vfs_root = base_v3.vfs_root
        }
    ; Session.V4.of_v1
        { version = 1
        ; id = base_v3.id
        ; prompt_file = base_v3.prompt_file
        ; history
        ; tasks = base_v3.tasks
        ; kv_store = base_v3.kv_store
        ; vfs_root = base_v3.vfs_root
        }
    ; Session.V4.of_v2
        { version = 2
        ; id = base_v3.id
        ; prompt_file = base_v3.prompt_file
        ; local_prompt_copy = base_v3.local_prompt_copy
        ; history
        ; tasks = base_v3.tasks
        ; kv_store = base_v3.kv_store
        ; vfs_root = base_v3.vfs_root
        }
    ; Session.V4.of_v3 base_v3
    ]
    |> List.map ~f:ok_exn
  in
  let expected_payload = [%sexp_of: Openai.Responses.Item.t list] history in
  let payloads_preserved =
    List.for_all migrations ~f:(fun session ->
      Sexp.equal
        expected_payload
        ([%sexp_of: Openai.Responses.Item.t list] (History_entry.items session.history))
      && String.equal session.vfs_root base_v3.vfs_root
      && List.equal
           (fun (key_a, value_a) (key_b, value_b) ->
              String.equal key_a key_b && String.equal value_a value_b)
           session.kv_store
           base_v3.kv_store)
  in
  let watermarks =
    List.map migrations ~f:(fun session -> session.next_history_sequence)
  in
  let deterministic_ids =
    let ids session = List.map session.Session.V4.history ~f:History_entry.id in
    match migrations with
    | first :: rest ->
      List.for_all rest ~f:(fun session ->
        List.equal History_entry.Id.equal (ids first) (ids session))
    | [] -> false
  in
  print_s
    [%sexp
      { variants = (List.length history : int)
      ; payloads_preserved : bool
      ; watermarks : int list
      ; deterministic_ids : bool
      }];
  [%expect
    {|
    ((variants 7) (payloads_preserved true) (watermarks (7 7 7 8))
     (deterministic_ids true))
    |}]
;;

let%expect_test "legacy migrations reject mismatched embedded versions" =
  let v1 : Session.Legacy.V1.t =
    { version = 99
    ; id = "wrong"
    ; prompt_file = "prompt"
    ; history = []
    ; tasks = []
    ; kv_store = []
    ; vfs_root = "vfs"
    }
  in
  let v2 : Session.Legacy.V2.t =
    { version = 99
    ; id = "wrong"
    ; prompt_file = "prompt"
    ; local_prompt_copy = None
    ; history = []
    ; tasks = []
    ; kv_store = []
    ; vfs_root = "vfs"
    }
  in
  let v3 : Session.Legacy.V3.t =
    { version = 99
    ; id = "wrong"
    ; prompt_file = "prompt"
    ; local_prompt_copy = None
    ; history = []
    ; tasks = []
    ; moderator_snapshot = None
    ; kv_store = []
    ; vfs_root = "vfs"
    }
  in
  print_s
    [%sexp
      { v1 = (Result.is_error (Session.V4.of_v1 v1) : bool)
      ; v2 = (Result.is_error (Session.V4.of_v2 v2) : bool)
      ; v3 = (Result.is_error (Session.V4.of_v3 v3) : bool)
      }];
  [%expect {| ((v1 true) (v2 true) (v3 true)) |}]
;;

let%expect_test "staged V4 binary round trip and reset preserve watermark" =
  let v3 : Session.Legacy.V3.t =
    { version = 3
    ; id = "round-trip"
    ; prompt_file = "prompt.chatmd"
    ; local_prompt_copy = None
    ; history = [ reasoning "r" ]
    ; tasks = []
    ; moderator_snapshot = Some (moderator_snapshot ())
    ; kv_store = []
    ; vfs_root = "vfs"
    }
  in
  let staged = Session.V4.of_v3 v3 |> ok_exn in
  let tmp_root =
    Filename.concat
      Filename.temp_dir_name
      ("session_v4_" ^ Int.to_string (Random.int 1_000_000))
  in
  Core_unix.mkdir_p tmp_root;
  Eio_main.run
  @@ fun env ->
  let path = Eio.Path.(env#fs / Filename.concat tmp_root "snapshot.bin") in
  Session.V4.Io.File.write path staged;
  let loaded = Session.V4.Io.File.read path in
  let reset = Session.V4.reset loaded in
  let retained = Session.V4.reset_keep_history loaded in
  print_s
    [%sexp
      { round_trip =
          (Sexp.equal (Session.V4.sexp_of_t staged) (Session.V4.sexp_of_t loaded) : bool)
      ; reset_history = (List.length reset.history : int)
      ; retained_history = (List.length retained.history : int)
      ; reset_next = (reset.next_history_sequence : int)
      ; retained_next = (retained.next_history_sequence : int)
      }];
  [%expect
    {|
    ((round_trip true) (reset_history 0) (retained_history 1) (reset_next 2)
     (retained_next 2))
    |}]
;;

let%expect_test "staged reader decodes snapshots written as V0 through V3" =
  let tmp_root =
    Filename.concat
      Filename.temp_dir_name
      ("session_legacy_files_" ^ Int.to_string (Random.int 1_000_000))
  in
  Core_unix.mkdir_p tmp_root;
  Eio_main.run
  @@ fun env ->
  let read name =
    let path = Eio.Path.(env#fs / Filename.concat tmp_root name) in
    match Session_store.read_staged_v4_file path with
    | Ok session -> session
    | Error error -> Error.raise error
  in
  let path name = Eio.Path.(env#fs / Filename.concat tmp_root name) in
  let history = migration_history () in
  let v0 : Session.Legacy.V0.t =
    { id = "v0"; prompt_file = "p"; history; tasks = []; kv_store = []; vfs_root = "vfs" }
  in
  let v1 : Session.Legacy.V1.t =
    { version = 1
    ; id = "v1"
    ; prompt_file = "p"
    ; history
    ; tasks = []
    ; kv_store = []
    ; vfs_root = "vfs"
    }
  in
  let v2 : Session.Legacy.V2.t =
    { version = 2
    ; id = "v2"
    ; prompt_file = "p"
    ; local_prompt_copy = None
    ; history
    ; tasks = []
    ; kv_store = []
    ; vfs_root = "vfs"
    }
  in
  let v3 : Session.Legacy.V3.t =
    { version = 3
    ; id = "v3"
    ; prompt_file = "p"
    ; local_prompt_copy = None
    ; history
    ; tasks = []
    ; moderator_snapshot = None
    ; kv_store = []
    ; vfs_root = "vfs"
    }
  in
  Bin_prot_utils_eio.write_bin_prot (module Session.Legacy.V0) (path "v0") v0;
  Bin_prot_utils_eio.write_bin_prot (module Session.Legacy.V1) (path "v1") v1;
  Bin_prot_utils_eio.write_bin_prot (module Session.Legacy.V2) (path "v2") v2;
  Bin_prot_utils_eio.write_bin_prot (module Session.Legacy.V3) (path "v3") v3;
  let loaded = List.map [ "v0"; "v1"; "v2"; "v3" ] ~f:read in
  let payloads_preserved =
    List.for_all loaded ~f:(fun session ->
      Sexp.equal
        ([%sexp_of: Openai.Responses.Item.t list] history)
        ([%sexp_of: Openai.Responses.Item.t list] (History_entry.items session.history)))
  in
  print_s
    [%sexp
      { loaded =
          (List.map loaded ~f:(fun session ->
             session.Session.V4.id, session.next_history_sequence)
           : (string * int) list)
      ; payloads_preserved : bool
      }];
  [%expect
    {|
    ((loaded ((v0 7) (v1 7) (v2 7) (v3 7))) (payloads_preserved true))
    |}]
;;

let%expect_test "frozen V3 codec round trips the projected production session" =
  let session =
    Session.create
      ~id:"wire-v3"
      ~prompt_file:"prompt.chatmd"
      ~local_prompt_copy:"prompt.chatmd"
      ~history:
        (let allocator =
           History_entry.Allocator.create ~namespace:"writer" ~next_sequence:0 |> ok_exn
         in
         List.map (migration_history ()) ~f:(History_entry.create ~allocator)
         |> Result.all
         |> ok_exn)
      ~tasks:[ Session.Task.create ~id:"task" ~title:"title" () ]
      ~moderator_snapshot:(moderator_snapshot ())
      ~kv_store:[ "key", "value" ]
      ~vfs_root:"vfs"
      ()
  in
  let legacy = Session.Legacy.v3_of_session session in
  let encode size write value =
    let buffer = Bigstring.create (size value) in
    let end_pos = write buffer ~pos:0 value in
    Bigstring.to_string buffer ~pos:0 ~len:end_pos
  in
  let frozen = encode Session.Legacy.V3.bin_size_t Session.Legacy.V3.bin_write_t legacy in
  let decoded = Bin_prot.Reader.of_string Session.Legacy.V3.bin_reader_t frozen in
  print_s
    [%sexp
      (Sexp.equal
         (Session.Legacy.V3.sexp_of_t legacy)
         (Session.Legacy.V3.sexp_of_t decoded)
       : bool)];
  [%expect {| true |}]
;;

let%expect_test "staged reader rejects corruption without modifying it" =
  let tmp_root =
    Filename.concat
      Filename.temp_dir_name
      ("session_corrupt_" ^ Int.to_string (Random.int 1_000_000))
  in
  let old_home = Sys.getenv "HOME" in
  Core_unix.mkdir_p tmp_root;
  Core_unix.putenv ~key:"HOME" ~data:tmp_root;
  Exn.protect
    ~f:(fun () ->
      Eio_main.run
      @@ fun env ->
      let id = "corrupt" in
      let dir = Session_store.ensure_dir ~env id in
      let snapshot = Eio.Path.(dir / "snapshot.bin") in
      let contents = "not-bin-prot" in
      Eio.Path.save ~create:(`Or_truncate 0o600) snapshot contents;
      let unreadable =
        match Session_store.read_staged_v4 ~env ~id with
        | Unreadable _ -> true
        | Missing | Loaded _ -> false
      in
      let preserved = String.equal contents (Eio.Path.load snapshot) in
      print_s [%sexp { unreadable : bool; preserved : bool }];
      [%expect {| ((unreadable true) (preserved true)) |}])
    ~finally:(fun () ->
      Core_unix.putenv ~key:"HOME" ~data:(Option.value old_home ~default:""))
;;

let%expect_test "reset clears persisted moderator snapshot" =
  let session =
    Session.create
      ~prompt_file:"prompt.chatmd"
      ~history:[]
      ~moderator_snapshot:(moderator_snapshot ())
      ()
  in
  let reset = Session.reset session in
  let reset_keep_history = Session.reset_keep_history session in
  let reset_clears = Option.is_none reset.moderator_state.legacy_snapshot in
  let reset_keep_history_clears =
    Option.is_none reset_keep_history.moderator_state.legacy_snapshot
  in
  print_s [%sexp { reset_clears : bool; reset_keep_history_clears : bool }];
  [%expect {| ((reset_clears true) (reset_keep_history_clears true)) |}]
;;
