open! Core
module CM = Prompt.Chat_markdown
module P = Agent_protocol

let invalid message = Error (P.Error.invalid_request message)
let max_source_bytes = 8 * 1024 * 1024
let max_entries = 100_000
let max_work_steps = 2_000_000

let load ctx ~file ~checkpoint =
  let module Invocation = struct
    exception Work_limit
  end
  in
  let remaining = ref max_work_steps in
  let checkpoint_failure = ref None in
  let poll () =
    if !remaining = 0 then raise Invocation.Work_limit;
    decr remaining;
    Eio.Fiber.check ();
    (try checkpoint () with
     | exn ->
       let trace = Backtrace.Exn.most_recent () in
       checkpoint_failure := Some (exn, trace);
       Exn.raise_with_original_backtrace exn trace);
    Eio.Fiber.check ()
  in
  let invalid_result = Result.map_error ~f:P.Error.invalid_request in
  try
    let open Result.Let_syntax in
    poll ();
    let env = Chat_response.Ctx.env ctx in
    let root = Eio.Stdenv.fs env in
    let loader = Source_loader.filesystem ~root in
    let%bind source = Source_loader.root loader ~file |> invalid_result in
    let%bind contents =
      Source_loader.read_bounded ~max_bytes:max_source_bytes loader source
      |> invalid_result
    in
    poll ();
    let limits =
      CM.Limits.create
        ~max_input_bytes:max_source_bytes
        ~max_parse_calls:1
        ~max_tokens:100_000
        ~max_depth:128
      |> Or_error.ok_exn
    in
    let%bind elements =
      try Ok (CM.parse_transcript_inputs ~dir:root ~limits ~checkpoint:poll contents) with
      | Failure message ->
        (match !checkpoint_failure with
         | Some (exn, trace) -> Exn.raise_with_original_backtrace exn trace
         | None -> invalid message)
    in
    let%bind () =
      List.fold_result elements ~init:() ~f:(fun () element ->
        poll ();
        let message =
          match element with
          | CM.Msg message
          | System message
          | Developer message
          | User message
          | Assistant message
          | Tool_call message
          | Tool_response message -> Some message
          | Reasoning _ -> None
          | Config _
          | Tool _
          | Shell_runtime _
          | Moderator_runtime _
          | Script _
          | Shell_script _
          | Extension_script _
          | Authoring_context _
          | Authoring_help _ -> assert false
        in
        match message with
        | None -> Ok ()
        | Some message ->
          let role = String.lowercase message.role in
          if
            not
              (List.mem
                 [ "system"; "developer"; "user"; "assistant"; "tool" ]
                 role
                 ~equal:String.equal)
          then invalid "transcript has an unsupported message role"
          else if
            String.equal role "assistant"
            && Option.is_none message.id
            && Option.is_some message.status
          then invalid "assistant transcript status requires its item ID"
          else if
            String.equal role "tool"
            && Option.is_none message.tool_call
            && Option.is_none message.tool_call_id
          then invalid "tool transcript result requires its opaque call ID"
          else Ok ())
    in
    let items =
      List.concat_map elements ~f:(fun element ->
        poll ();
        let items =
          Chat_response.Converter.to_items
            ~ctx
            ~run_agent:(fun ?prompt_dir:_ ?session_id:_ ~ctx:_ _ _ ->
              failwith "validated transcript unexpectedly attempted an agent invocation")
            [ element ]
        in
        poll ();
        items)
    in
    let%bind allocator =
      History_entry.Allocator.create
        ~namespace:
          ("transcript-import:"
           ^ (P.Id.Transaction.create () |> P.Id.Transaction.to_string))
        ~next_sequence:0
      |> invalid_result
    in
    let%bind entries =
      Openai.Responses_history.of_items ~checkpoint:poll ~preceding:[] ~allocator items
      |> invalid_result
    in
    let entries =
      List.map entries ~f:(fun entry ->
        poll ();
        Agent_session.History_codec.to_canonical entry)
    in
    poll ();
    P.Session_seed.Transcript.create
      entries
      ~next_history_sequence:
        (History_entry.Allocator.next_sequence allocator |> Int64.of_int)
      ~limits:Document_schema.Limits.default
      ~max_entries
      ~max_work_steps:!remaining
      ~checkpoint:poll
  with
  | Invocation.Work_limit ->
    invalid "transcript exceeds its complete conversion work bound"
;;
