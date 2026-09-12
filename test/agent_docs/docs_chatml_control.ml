open! Core
module L = Chatml.Chatml_lang
module R = Chatml_host_runtime
module E = Chatml.Chatml_extension_surface
module M = Chat_response.Moderation
module I = Chat_response.Moderator_invocation
module Codec = Chatml.Chatml_value_codec

let directory = "test/chatml_extensibility_fixtures/authoring-control/"
let ok = Result.ok_or_failwith
let require condition message = if not condition then failwith (directory ^ message)

let create source surface handlers =
  let compiled =
    R.compile_script ~surface ~required_bindings:E.moderator_entrypoints ~source () |> ok
  in
  let config = R.default_runtime_config ~surface ~handlers () in
  R.instantiate_session
    { config with operations = I.operations config.operations }
    compiled
    ~entrypoints:{ initial_state_name = "initial_state"; on_event_name = "on_event" }
  |> ok
;;

let decode effects =
  I.ordinary_effects effects
  |> Result.bind ~f:R.decode_local_effects
  |> Result.bind ~f:M.Outcome.of_runtime_effects
;;

let fails expected = function
  | Ok () -> failwith ("expected failure: " ^ expected)
  | Error message -> require (String.is_substring message ~substring:expected) message
;;

let runtime source surface =
  let logs = Queue.create () in
  let handlers =
    { R.default_handlers with
      on_log =
        (fun _ ~level:_ ~message ->
          Queue.enqueue logs message;
          Ok ())
    }
  in
  let session = create source surface handlers in
  let outcomes = Queue.create () in
  let rejected = ref false in
  let prepare_commit ~local_effects =
    decode local_effects
    |> Result.bind ~f:(fun outcome ->
      match !rejected with
      | true -> Error "fixture commit rejected"
      | false -> Ok (fun () -> Queue.enqueue outcomes outcome))
  in
  let handle phase event =
    R.handle_event
      ~prepare_commit
      ~limits:{ fuel = 10_000; max_tasks = 1_000 }
      session
      ~context:(Docs_chatml.context phase)
      ~event
  in
  let internal command =
    handle
      "internal_event"
      (I.internal_event (Codec.jsonaf_to_value (`String command)) |> ok)
  in
  let state () =
    match R.current_state session with
    | L.VInt count -> count
    | _ -> failwith "expected integer control state"
  in
  let events () =
    List.map (R.queued_events session) ~f:(function
      | L.VVariant ("Internal_event", [ L.VVariant ("String", [ L.VString text ]) ]) ->
        text
      | _ -> failwith "expected wrapped JSON internal event")
  in
  let snapshot () = state (), events (), R.is_halted session, Queue.length outcomes in
  let unchanged before =
    require
      (Sexp.equal
         ([%sexp_of: int * string list * bool * int] before)
         ([%sexp_of: int * string list * bool * int] (snapshot ())))
      "failed/rejected handler changed committed state"
  in
  handle "session_start" (L.VVariant ("Session_start", [])) |> ok;
  internal "recover" |> ok;
  require (List.equal String.equal (events ()) [ "ready"; "recover" ]) "catch queue";
  require (not (R.is_halted session)) "caught end_session leaked halt";
  (match (List.last_exn (Queue.to_list outcomes)).runtime_requests with
   | [ M.Runtime_request.Request_compaction ] -> ()
   | _ -> failwith "catch retained inner runtime requests");
  let before = snapshot () in
  fails "invalid in phase" (handle "turn_start" (L.VVariant ("Turn_start", [])));
  unchanged before;
  fails "whole handler failed" (internal "fail");
  unchanged before;
  rejected := true;
  fails "fixture commit rejected" (internal "continue");
  unchanged before;
  fails "fixture commit rejected" (internal "finish");
  unchanged before;
  rejected := false;
  internal "continue" |> ok;
  require
    (List.equal String.equal (events ()) [ "ready"; "recover"; "first"; "second" ])
    "committed events lost FIFO order";
  (match (List.last_exn (Queue.to_list outcomes)).runtime_requests with
   | [ M.Runtime_request.Request_turn ] -> ()
   | _ -> failwith "missing follow-up turn request");
  internal "finish" |> ok;
  require (R.is_halted session && Int.equal (state ()) 4) "end_session did not commit";
  require
    (List.equal
       String.equal
       (Queue.to_list logs)
       [ "continuation after end_session"; "continuation after end_session" ])
    "end_session interrupted continuation or rejected commit erased diagnostics";
  match (List.last_exn (Queue.to_list outcomes)).runtime_requests with
  | [ M.Runtime_request.End_session "work complete" ] -> ()
  | _ -> failwith "missing committed end-session reason"
;;

let model_process source =
  let number = Codec.value_to_jsonaf_result (Codec.jsonaf_to_value (`Number "2")) |> ok in
  let calls = Queue.create () in
  let spawned = ref 0 in
  let handlers =
    { R.default_handlers with
      on_model_call =
        (fun _ ~recipe ~payload ->
          Queue.enqueue calls (recipe, Codec.value_to_jsonaf_result payload |> ok);
          match recipe with
          | "echo" -> Ok (L.VVariant ("Ok", [ payload ]))
          | "refuse" -> Ok (L.VVariant ("Refused", [ L.VString "policy" ]))
          | "fail" -> Ok (L.VVariant ("Error", [ L.VString "upstream" ]))
          | _ -> Error "recipe not registered")
    ; on_model_spawn =
        (fun _ ~recipe ~payload ->
          Queue.enqueue calls (recipe, Codec.value_to_jsonaf_result payload |> ok);
          incr spawned;
          Ok (sprintf "fixture-job-%d" !spawned))
    ; on_process_run =
        (fun _ ~command ~args ->
          match command, args with
          | "fixture-command", L.VArray [| L.VString "a b"; L.VString ";literal" |] ->
            Ok "stdout\nstderr\n"
          | _ -> Error "unexpected process argv")
    }
  in
  let session = create source E.moderator_v1 handlers in
  R.handle_event
    ~prepare_commit:(fun ~local_effects ->
      Result.map (decode local_effects) ~f:(fun _ -> fun () -> ()))
    session
    ~context:(Docs_chatml.context "session_start")
    ~event:(L.VVariant ("Session_start", []))
  |> ok;
  require
    (List.equal
       (Tuple2.equal ~eq1:String.equal ~eq2:Jsonaf.exactly_equal)
       (Queue.to_list calls)
       [ "echo", `String "hello"
       ; "echo", `Object [ "count", number ]
       ; "refuse", `Null
       ; "fail", `Null
       ; "missing", `Null
       ; "echo", `Null
       ; "echo", `String "later"
       ])
    "model helper payload wrapping or recovery order changed";
  let actual = Codec.value_to_jsonaf_result (R.current_state session) |> ok in
  let expected =
    `Object
      [ "text", `String "hello"
      ; "data", `Object [ "count", number ]
      ; "refused", `String "refused: policy"
      ; "failed", `String "error: upstream"
      ; "missing", `String "error: recipe not registered"
      ; "jobs", `Array [ `String "fixture-job-1"; `String "fixture-job-2" ]
      ; "output", `String "stdout\nstderr\n"
      ]
  in
  require (Jsonaf.exactly_equal actual expected) (Jsonaf.to_string actual);
  fails
    "model fixture commit rejected"
    (R.handle_event
       ~prepare_commit:(fun ~local_effects:_ -> Error "model fixture commit rejected")
       session
       ~context:(Docs_chatml.context "session_start")
       ~event:(L.VVariant ("Session_start", [])));
  require
    (Int.equal !spawned 4 && Int.equal (Queue.length calls) 14)
    "rejected commit did not preserve already interpreted external callbacks";
  require
    (Jsonaf.exactly_equal
       (Codec.value_to_jsonaf_result (R.current_state session) |> ok)
       expected)
    "rejected commit changed model fixture state"
;;

let run env root =
  let load name = Eio.Path.load Eio.Path.(Eio.Stdenv.fs env / root / directory / name) in
  let source = load "runtime.chatml" in
  List.iter [ E.moderator_v1; E.delegated_moderator_v1 ] ~f:(runtime source);
  model_process (load "model-process.chatml");
  Eio.Flow.copy_string
    "ChatML control reference: transactional requests on both moderators and fake \
     model/process contracts PASS (offline)\n"
    (Eio.Stdenv.stdout env)
;;
