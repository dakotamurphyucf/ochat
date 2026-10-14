open! Core
module Parsed = Prompt.Chat_markdown.Parsed
module Declaration = Prompt.Chat_markdown.Declaration
module Input = Chatmd_parser_input
module Error = Authoring_context_service_core.Error

exception Work_limit

type located =
  | Exact of Declaration.t option
  | Transformed

let at_authored_cursor
      ~parsed
      ~inputs
      ~authored
      ~cursor_byte
      ~max_source_bytes
      ~max_work_steps
      ~checkpoint
  =
  let source_bytes = Chatml_source.byte_length authored in
  if
    max_source_bytes <= 0
    || max_work_steps <= 0
    || max_source_bytes > Agent_protocol.Authoring.Limits.maximum_wire_integer
    || max_work_steps > Agent_protocol.Authoring.Limits.maximum_wire_integer
    || source_bytes > max_source_bytes
    || cursor_byte < 0
    || cursor_byte > source_bytes
  then Error Error.Resource_limit
  else (
    let remaining = ref max_work_steps in
    let tick () =
      if !remaining <= 0 then raise Work_limit;
      decr remaining;
      checkpoint ()
    in
    try
      let source = Chatml_source.contents authored in
      let rec admit_inputs exact = function
        | [] -> Ok exact
        | input :: rest ->
          tick ();
          if
            Input.authored_byte_length input > max_source_bytes
            || Input.effective_byte_length input > max_source_bytes
          then Error Error.Resource_limit
          else if not (Input.authored_matches input source ~checkpoint:tick)
          then Error Error.Stale_source
          else
            admit_inputs
              (Input.effective_matches input source ~checkpoint:tick && exact)
              rest
      in
      let open Result.Let_syntax in
      let%bind exact = admit_inputs true inputs in
      let rec select selected = function
        | [] -> Ok selected
        | declaration :: rest ->
          tick ();
          let range = Declaration.range declaration in
          let input = Input.Range.input range in
          let present =
            List.exists inputs ~f:(fun candidate ->
              tick ();
              phys_equal input candidate)
          in
          let contains =
            present
            && Input.Range.start_byte range <= cursor_byte
            && cursor_byte < Input.Range.end_byte range
          in
          if contains
          then (
            match selected with
            | None -> select (Some declaration) rest
            | Some _ -> Error Error.Selection_unavailable)
          else select selected rest
      in
      if exact
      then select None (Parsed.declarations parsed) |> Result.map ~f:(fun d -> Exact d)
      else Ok Transformed
    with
    | Work_limit -> Error Error.Resource_limit)
;;
