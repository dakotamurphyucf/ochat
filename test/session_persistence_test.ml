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
             ; _type = "message"
             })
    }
  in
  let overlay =
    { Moderator.Overlay.empty with
      appended_items = [ item ]
    ; deleted_item_ids = [ "msg-1" ]
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
  and moderator_snapshot_none = Option.is_none upgraded.moderator_snapshot in
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
  and moderator_snapshot_none = Option.is_none upgraded.moderator_snapshot in
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
  and moderator_snapshot_none = Option.is_none upgraded.moderator_snapshot in
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
  let reset_clears = Option.is_none reset.moderator_snapshot in
  let reset_keep_history_clears = Option.is_none reset_keep_history.moderator_snapshot in
  print_s [%sexp { reset_clears : bool; reset_keep_history_clears : bool }];
  [%expect {| ((reset_clears true) (reset_keep_history_clears true)) |}]
;;
