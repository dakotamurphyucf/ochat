open! Core
module P = Agent_protocol
module Reference = Recovery_rollover_hint_ref
module Guard = Inference_runtime.Submission_guard
module Prepared = Chat_response.Agent_guidance.Prepared

type fragment =
  { reference : Reference.t
  ; item : Jsonaf.t
  ; guard : Guard.t
  }

let fragment ~reference ~text ~more ~guard =
  if String.length text > 4096 || not (Stdlib.String.is_valid_utf_8 text)
  then Error (P.Error.invalid_request "invalid recovery hint text")
  else (
    let source =
      match reference with
      | Reference.Local_note _ -> "local_note"
      | Shared_note _ -> "shared_note"
      | Canonical_text _ -> "history"
    in
    let item =
      `Object
        [ "source", `String source
        ; "text", `String text
        ; ("more", if more then `True else `False)
        ]
    in
    Ok { reference; item; guard })
;;

let header = "Untrusted excerpts from locally retained session data:\n"
let max_frame_bytes = 16 * 1024

let create ~select =
  let with_guidance use =
    let open Result.Let_syntax in
    let%bind selected, recipient, resolve = select () in
    let items, _, guard =
      List.fold
        (Reference.Selection.references selected)
        ~init:([], String.length header + 2, recipient)
        ~f:(fun ((items, used, guard) as previous) reference ->
          match resolve reference with
          | Ok fragment when Reference.equal fragment.reference reference ->
            let encoded = Jsonaf.to_string fragment.item in
            let separator = if List.is_empty items then 0 else 1 in
            let total = used + String.length encoded + separator in
            if total > max_frame_bytes
            then previous
            else encoded :: items, total, Guard.compose guard fragment.guard
          | Ok _ | Error _ -> previous)
    in
    let%bind prepared =
      match items with
      | [] -> Prepared.create ~guidance:None ~guard:Guard.empty ~reserved_input_tokens:0
      | _ ->
        let text = header ^ "[" ^ String.concat ~sep:"," (List.rev items) ^ "]" in
        let%bind guidance =
          Inference.Request.Guidance.create
            ~text
            ~provenance:"ochat local recovery excerpts"
            ~limits:Document_schema.Limits.default
          |> Result.map_error ~f:(fun _ ->
            P.Error.invalid_request "invalid recovery hint frame")
        in
        Prepared.create
          ~guidance:(Some guidance)
          ~guard
          ~reserved_input_tokens:(Context_compaction.Compactor.estimated_text_tokens text)
    in
    Ok (use prepared)
  in
  { Chat_response.Agent_guidance.with_guidance }
;;
