open! Core
module L = Chatml.Chatml_lang
module R = Chatml_host_runtime
module E = Chatml.Chatml_extension_surface
module M = Chat_response.Moderation

let fixture = "test/chatml_extensibility_fixtures/authoring-effects/moderator.chatml"
let ok = Result.ok_or_failwith
let require condition message = if not condition then failwith (fixture ^ ": " ^ message)

let run_surface source surface =
  let logs = Queue.create () in
  let calls = ref 0 in
  let handlers =
    { R.default_handlers with
      on_log =
        (fun _ ~level ~message ->
          Queue.enqueue logs (R.string_of_log_level level, message);
          Ok ())
    ; on_tool_call =
        (fun _ ~name ~args ->
          match String.equal name "probe" with
          | true ->
            incr calls;
            Ok (L.VVariant ("Ok", [ args ]))
          | false -> Error "fixture tool unavailable")
    }
  in
  let compiled =
    R.compile_script ~surface ~required_bindings:E.moderator_entrypoints ~source () |> ok
  in
  let session =
    R.instantiate_session
      (R.default_runtime_config ~surface ~handlers ())
      compiled
      ~entrypoints:{ initial_state_name = "initial_state"; on_event_name = "on_event" }
    |> ok
  in
  let outcomes = Queue.create () in
  let reject_commit = ref false in
  let prepare_commit ~local_effects =
    R.decode_local_effects local_effects
    |> Result.bind ~f:M.Outcome.of_runtime_effects
    |> Result.bind ~f:(fun outcome ->
      match !reject_commit with
      | true -> Error "fixture persistence rejected"
      | false -> Ok (fun () -> Queue.enqueue outcomes outcome))
  in
  let handle ?(items = [||]) phase event =
    let context =
      match Docs_chatml.context phase with
      | L.VRecord fields -> L.VRecord (Map.set fields ~key:"items" ~data:(L.VArray items))
      | _ -> assert false
    in
    R.handle_event
      ~prepare_commit
      ~limits:{ fuel = 10_000; max_tasks = 1_000 }
      session
      ~context
      ~event
  in
  let state () =
    match R.current_state session with
    | L.VInt count -> count
    | _ -> failwith "expected integer effect fixture state"
  in
  let last () = List.last_exn (Queue.to_list outcomes) in
  let fails expected result =
    match result with
    | Ok () -> failwith ("expected " ^ expected)
    | Error message -> require (String.is_substring message ~substring:expected) message
  in
  handle "session_start" (L.VVariant ("Session_start", [])) |> ok;
  require
    (Int.equal !calls 1 && Int.equal (state ()) 1)
    "successful recovery lost state or tool call";
  (match (last ()).overlay_ops with
   | [ M.Overlay.Prepend_system _; Append_item item ] ->
     require (String.equal item.id "system:Ready") "catch retained discarded notice"
   | _ -> failwith "unexpected recovered effects");
  require
    (List.equal
       (Tuple2.equal ~eq1:String.equal ~eq2:String.equal)
       (Queue.to_list logs)
       [ "debug", "begin"
       ; "error", "probe already ran"
       ; "warn", "recover"
       ; "info", "commit"
       ])
    "diagnostic logs did not survive recovery";
  let before = List.length (R.committed_local_effects session) in
  reject_commit := true;
  fails
    "fixture persistence rejected"
    (handle "session_start" (L.VVariant ("Session_start", [])));
  reject_commit := false;
  require
    (Int.equal !calls 2
     && Int.equal (state ()) 1
     && Int.equal (Queue.length outcomes) 1
     && Int.equal before (List.length (R.committed_local_effects session)))
    "failed commit changed local state/effects";
  let call name =
    Docs_chatml.record
      [ "id", L.VString "call"
      ; "name", L.VString name
      ; "args", Chatml.Chatml_value_codec.jsonaf_to_value (`Object [])
      ]
  in
  List.iter [ "approve"; "reject"; "rewrite"; "redirect" ] ~f:(fun name ->
    handle "pre_tool_call" (L.VVariant ("Pre_tool_call", [ call name ])) |> ok;
    match name, (last ()).tool_moderation with
    | "approve", Some M.Tool_moderation.Approve -> ()
    | "reject", Some (Reject "Declined by moderator") -> ()
    | "rewrite", Some (Rewrite_args args) ->
      require
        (Jsonaf.exactly_equal args (`Object [ "mode", `String "safe" ]))
        "rewrite payload"
    | "redirect", Some (Redirect ("echo", `Object [])) -> ()
    | _ -> failwith "unexpected tool decision");
  let previous = state (), Queue.length outcomes in
  fails
    "at most one tool moderation action"
    (handle "pre_tool_call" (L.VVariant ("Pre_tool_call", [ call "conflict" ])));
  require
    (Tuple2.equal
       ~eq1:Int.equal
       ~eq2:Int.equal
       previous
       (state (), Queue.length outcomes))
    "conflicting decisions committed";
  handle "turn_start" (L.VVariant ("Turn_start", [])) |> ok;
  (match (last ()).overlay_ops with
   | [ M.Overlay.Append_item _ ] -> ()
   | _ -> failwith "None did not append");
  let item id json =
    Docs_chatml.record
      [ "id", L.VString id; "value", Chatml.Chatml_value_codec.jsonaf_to_value json ]
  in
  let items =
    [| item "source" (`Object [ "type", `String "message"; "role", `String "assistant" ])
     ; item "tool" (`Object [ "type", `String "function_call_output" ])
    |]
  in
  handle ~items "turn_start" (L.VVariant ("Turn_start", [])) |> ok;
  (match (last ()).overlay_ops with
   | [ M.Overlay.Replace_item { target_id = "source"; _ } ] -> ()
   | _ -> failwith "Some did not replace");
  let internal command =
    L.VVariant
      ("Internal_event", [ Chatml.Chatml_value_codec.jsonaf_to_value (`String command) ])
  in
  handle ~items "internal_event" (internal "edit") |> ok;
  (match (last ()).overlay_ops with
   | [ M.Overlay.Replace_item { target_id = "source"; _ }
     ; Replace_item { target_id = "source"; _ }
     ; Delete_item "source"
     ; Delete_item "tool"
     ; Append_item _
     ; Append_item _
     ] -> ()
   | _ -> failwith "item/message aliases changed effect order");
  let previous = state (), Queue.length outcomes in
  fails "whole handler failed" (handle "internal_event" (internal "fail"));
  require
    (Tuple2.equal
       ~eq1:Int.equal
       ~eq2:Int.equal
       previous
       (state (), Queue.length outcomes))
    "failed handler committed";
  handle "internal_event" (internal "halt") |> ok;
  (match (last ()).overlay_ops with
   | [ M.Overlay.Halt "Review finished" ] -> ()
   | _ -> failwith "missing halt intent");
  require
    (String.equal (snd (List.last_exn (Queue.to_list logs))) "after halt construction")
    "halt stopped current task evaluation"
;;

let run env root =
  let source = Eio.Path.load Eio.Path.(Eio.Stdenv.fs env / root / fixture) in
  List.iter [ E.moderator_v1; E.delegated_moderator_v1 ] ~f:(run_surface source);
  Eio.Flow.copy_string
    "ChatML effect reference: recovery, diagnostics, commit rejection, decisions and \
     aliases on both moderator surfaces PASS (offline)\n"
    (Eio.Stdenv.stdout env)
;;
