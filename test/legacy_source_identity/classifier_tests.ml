open! Core
module P = Agent_protocol
module CM = Prompt.Chat_markdown
module A = Agent_store.Prompt_artifact_store
module Revision = Agent_session.Prompt_revision
module Identity = Agent_session.Legacy_moderator_checkpoint
module Registry = Chat_response.Moderator_manager.Registry

let protocol_ok = function
  | Ok value -> value
  | Error error -> raise_s [%sexp (error : P.Error.t)]
;;

let store_ok = function
  | Ok value -> value
  | Error error -> raise_s [%sexp (error : Agent_store.Store_error.t)]
;;

let source =
  {|type state = int
type event = [ `Session_start ]
let initial_state = 0
let on_event : context -> state -> event -> state task =
  fun ctx st ev -> Task.pure(st + 1)
|}
;;

let script ?project_selection id source =
  CM.
    { id
    ; language = "chatml"
    ; kind = "moderator"
    ; source
    ; project_selection
    ; source_ref = None
    }
;;

let revision env scripts =
  let root_chatmd =
    List.map scripts ~f:(fun (script : CM.script) ->
      let body =
        match script.source with
        | Inline body -> body
        | Src { source_text; _ } -> source_text
      in
      sprintf
        "<script id=%S language=\"chatml\" kind=\"moderator\">%s</script>"
        script.id
        body)
    |> String.concat ~sep:"\n"
  in
  let definition =
    Agent_session.Prompt_definition.create
      ~id:(P.Id.Prompt_definition.of_string "prd_legacy_identity" |> protocol_ok)
      ~config_name:"legacy-identity"
      ~root_file:"/captured/root.chatmd"
      ~root_invocation:Ui
      ~allowed_workspaces:[]
      ~permission_profile:"fixture"
      ~runtime_policy:None
      ~enabled:true
      ~description:None
    |> store_ok
  in
  let artifact =
    A.Artifact.create
      ~revision_id:(P.Id.Prompt_revision.of_string "prv_legacy_identity" |> protocol_ok)
      ~root_chatmd
      ~sources:[]
      ~parser_schema_version:1
      ~runtime_schema_version:1
      ~created_at:(P.Timestamp.of_string "2026-11-03T00:00:00Z" |> protocol_ok)
      ()
    |> store_ok
  in
  Revision.create
    ~definition
    ~artifact
    ~materialized_tree:(Eio.Stdenv.fs env)
    ~elements:(List.map scripts ~f:(fun script -> CM.Script script))
;;

(* A constructed prior edition: actual MD5 of the retained Single code, not a
   forged current SHA256 identity or a runtime/source authorization receipt. *)
let checkpoint ~script_id ~source_hash =
  let snapshot =
    Session.Moderator_state.Identity_snapshot.
      { script_id
      ; script_source_hash = source_hash
      ; current_state = Session.Snapshot.Int 7
      ; queued_internal_events = []
      ; halted = false
      ; revision = 3
      ; next_change_id = 0
      ; prepended_items = []
      ; appended_items = []
      ; replacements = []
      ; tombstones = []
      ; halted_reason = None
      }
  in
  match Agent_session.Moderator_checkpoint.encode snapshot with
  | `Object fields -> `Object (("future_compatible", `String "retained") :: fields)
  | _ -> assert false
;;

let historical_hash = Md5.digest_string source |> Md5.to_hex

let%expect_test "Single canonical source identity and exact historical recognition" =
  Eio_main.run (fun env ->
    let inline = script "selected" (Inline source) in
    let referenced =
      script "selected" (Src { path = "/deleted/code.chatml"; source_text = source })
    in
    let compile script =
      Registry.compile_script Registry.empty script |> Result.ok_or_failwith |> snd
    in
    let a = compile inline
    and b = compile referenced in
    let canonical = Chatmd_shell_spec.Source_ref.digest source in
    assert (String.equal (Registry.source_hash a) canonical);
    assert (String.equal (Registry.source_hash b) canonical);
    assert (String.length canonical = 64);
    let old = checkpoint ~script_id:"selected" ~source_hash:historical_hash in
    let before = Jsonaf.to_string old in
    let classify scripts checkpoint =
      Identity.classify (revision env scripts) ~checkpoint ~poll:Eio.Fiber.check
    in
    assert (
      Identity.equal (classify [ inline ] (Some old) |> protocol_ok) Rebuild_required);
    assert (
      Identity.equal (classify [ referenced ] (Some old) |> protocol_ok) Rebuild_required);
    assert (
      Identity.equal
        (classify [ script "earlier" (Inline "other"); inline ] (Some old) |> protocol_ok)
        Rebuild_required);
    assert (Identity.equal (classify [ inline ] None |> protocol_ok) No_migration);
    assert (
      Identity.equal
        (classify
           [ inline ]
           (Some (checkpoint ~script_id:"selected" ~source_hash:canonical))
         |> protocol_ok)
        No_migration);
    assert (String.equal before (Jsonaf.to_string old));
    print_endline
      "actual SHA256 covers Inline/Src; exact prior MD5 recognized without rewriting";
    List.iter
      [ []
      ; [ script "foreign" (Inline source) ]
      ; [ script "selected" (Inline (source ^ "\n")) ]
      ; [ inline; script "later" (Inline source) ]
      ; [ script
            ~project_selection:
              (Chatmd_shell_spec.Script_project.create
                 ~project:"project"
                 ~environment:None
               |> Or_error.ok_exn)
            "selected"
            (Inline source)
        ]
      ]
      ~f:(fun scripts ->
        match classify scripts (Some old) with
        | Error error -> assert (P.Error.equal_code error.code Invalid_state)
        | Ok _ -> failwith "incompatible historical source was recognized");
    let cancelled =
      try
        ignore
          (Identity.classify
             (revision env [ inline ])
             ~checkpoint:(Some old)
             ~poll:(fun () -> failwith "poll cancelled")
           : (Identity.t, P.Error.t) Result.t);
        false
      with
      | Failure message when String.equal message "poll cancelled" -> true
    in
    assert cancelled;
    print_endline
      "missing/foreign/changed/final-script/project mismatches reject; poll exception \
       preserved");
  [%expect
    {|
    actual SHA256 covers Inline/Src; exact prior MD5 recognized without rewriting
    missing/foreign/changed/final-script/project mismatches reject; poll exception preserved
    |}]
;;
