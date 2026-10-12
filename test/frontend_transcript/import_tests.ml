open! Core
module P = Agent_protocol
module Payload = History_entry.Payload

let with_input f =
  Eio_main.run (fun env ->
    Mirage_crypto_rng_unix.use_default ();
    let name = "ochat-transcript." ^ P.Id.Transaction.(create () |> to_string) in
    let root =
      Eio.Path.(
        Eio.Stdenv.fs env / (Sys.getenv "TMPDIR" |> Option.value ~default:"/tmp") / name)
    in
    Eio.Path.mkdir ~perm:0o700 root;
    Exn.protect
      ~finally:(fun () -> Eio.Cancel.protect (fun () -> Eio.Path.rmtree root))
      ~f:(fun () ->
        let attempts = ref 0 in
        let fixture =
          Inference_fixture.create
            ~namespace:name
            ~default_model:"offline-import"
            ~post_stream:(fun ~sw:_ ~inputs:_ ->
              Int.incr attempts;
              failwith "transcript import invoked inference")
        in
        let ctx =
          Inference_fixture.ctx
            fixture
            ~env
            ~dir:root
            ~tool_dir:root
            ~cache:(Chat_response.Cache.create ~max_size:1 ())
            ()
        in
        let file = Eio.Path.(root / "edited.chatmd") in
        let load ?(checkpoint = ignore) source =
          Eio.Path.save ~create:(`Or_truncate 0o600) file source;
          Frontend_transcript.load ctx ~file:(Eio.Path.native_exn file) ~checkpoint
        in
        f load;
        assert (Int.equal !attempts 0)))
;;

let ok result =
  result
  |> Result.map_error ~f:(fun error -> Sexp.to_string_hum (P.Error.sexp_of_t error))
  |> Result.ok_or_failwith
;;

let%test_unit "literal edited roles and opaque call pairing receive fresh history IDs" =
  with_input (fun load ->
    let source =
      {|<system>system</system><developer>developer</developer>
<user ochat-history-id="3:old:7">edited</user>
<assistant id="opaque-message" status="completed">answer</assistant>
<tool_call function_name="inspect" tool_call_id="opaque-call">RAW|
{"number":1e1,"id":"opaque"}
|RAW</tool_call>
<tool_response tool_call_id="opaque-call">result</tool_response>
<user><img src="https://example.invalid/image.png"/></user>|}
    in
    let first = load source |> ok |> P.Session_seed.Transcript.entries in
    let second = load source |> ok |> P.Session_seed.Transcript.entries in
    assert (Int.equal (List.length first) 7);
    assert (
      List.equal
        P.History.equal_role
        (List.map first ~f:(fun (entry : P.History.entry) -> entry.role))
        [ System; System; User; Assistant; Assistant; Tool; User ]);
    assert (
      List.for_all first ~f:(fun (entry : P.History.entry) ->
        P.History.Content_revision.equal
          entry.content_revision
          P.History.Content_revision.zero
        && (not entry.redacted)
        && not (String.equal (History_entry.Id.namespace entry.id) "old")));
    assert (not (History_entry.Id.equal (List.hd_exn first).id (List.hd_exn second).id));
    let payload index =
      Agent_session.History_codec.of_canonical (List.nth_exn first index)
      |> ok
      |> History_entry.payload
      |> Payload.semantic
    in
    (match Payload.Semantic.view (payload 1) with
     | Message { role = Developer; _ } -> ()
     | _ -> assert false);
    (match Payload.Semantic.view (payload 4) with
     | Call { input_bytes; _ } ->
       assert (String.is_substring input_bytes ~substring:{|"number":1e1|})
     | _ -> assert false);
    (match (Payload.Semantic.metadata (payload 4)).call_id with
     | Value id -> assert (String.equal id "opaque-call")
     | Absent | Null -> assert false);
    (match Payload.Semantic.view (payload 5) with
     | Result { relation = Bound id; _ } ->
       assert (History_entry.Id.equal id (List.nth_exn first 4).id)
     | _ -> assert false);
    match Payload.Semantic.view (payload 6) with
    | Message { content = [ Image { uri; _ } ]; _ } ->
      assert (String.equal uri "https://example.invalid/image.png")
    | _ -> assert false)
;;

let%test_unit "resource execution, declaration loss and malformed syntax fail closed" =
  with_input (fun load ->
    List.iter
      [ {|<user><doc src="missing"/></user>|}
      ; {|<user><img src="missing" local/></user>|}
      ; {|<user><agent src="missing" local/></user>|}
      ; {|<script language="chatml" src="missing"/>|}
      ; {|<import src="missing"/>|}
      ; {|<config model="unselected"/><user>ignored model</user>|}
      ; {|<ochat-unknown-item kind="future">RAW|
{}
|RAW</ochat-unknown-item>|}
      ; {|<user>wrong closing tag</assistant>|}
      ; {|<user>RAW|
unterminated|}
      ]
      ~f:(fun source -> assert (Result.is_error (load source)));
    assert (Result.is_error (load (String.make ((8 * 1024 * 1024) + 1) 'x'))))
;;

let%test_unit "callback Failure keeps original identity and throwing backtrace" =
  with_input (fun load ->
    let original = Failure "transcript-checkpoint-origin" in
    let trace = ref "" in
    let callback () =
      match raise original with
      | () -> assert false
      | exception exn ->
        let raw = Stdlib.Printexc.get_raw_backtrace () in
        trace := Stdlib.Printexc.raw_backtrace_to_string raw;
        Stdlib.Printexc.raise_with_backtrace exn raw
    in
    match load ~checkpoint:callback "<user>invalid</assistant>" with
    | _ -> assert false
    | exception exn ->
      let raw = Stdlib.Printexc.get_raw_backtrace () in
      assert (phys_equal exn original);
      assert (not (String.is_empty !trace));
      assert (
        String.is_substring
          (Stdlib.Printexc.raw_backtrace_to_string raw)
          ~substring:!trace))
;;
