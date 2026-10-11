open Core
module Content = Chatmd_prompt_content
module Capture = Chatmd_content_capture
module Parsed = Prompt.Chat_markdown.Parsed
module Storage = Agent_store.Prompt_program_bundle
module Limits = Content.Limits
module A = Content.Accounting

module Error = struct
  type t =
    | Content of Content.Error.t
    | Invalid of string

  let range = function
    | Content error -> Content.Error.range error
    | Invalid _ -> None
  ;;

  let sexp_of_t = function
    | Content error -> Content.Error.sexp_of_t error
    | Invalid message -> [%sexp Invalid (message : string)]
  ;;
end

type t = Content.t

let requires_captured_documents parsed ~checkpoint =
  List.exists (Parsed.document_occurrences parsed) ~f:(fun occurrence ->
    checkpoint ();
    match Prompt.Chat_markdown.Document_occurrence.selection occurrence with
    | Dynamic -> false
    | Local_capture | Builtin _ -> true)
;;

let admit storage ~programs ~limits ~checkpoint =
  Content.of_storage
    storage
    ~inputs:(Chatml_prompt_programs.parser_inputs programs)
    ~limits
    ~checkpoint
  |> Result.map_error ~f:(fun error -> Error.Content error)
;;

module Prepared = struct
  type t =
    { content : Content.t
    ; work_steps : int
    ; limits : Limits.t
    }

  let content t = t.content
  let work_steps t = t.work_steps
end

let remaining_limits limits ~work ~occurrences ~output =
  Limits.create
    ~max_occurrences:occurrences
    ~max_parser_inputs:(Limits.parser_inputs limits)
    ~max_input_bytes:(Limits.input_bytes limits)
    ~max_output_bytes:output
    ~max_work_steps:work
    ~max_nodes:(Limits.nodes limits)
    ~max_depth:(Limits.depth limits)
    ~max_payloads:(Limits.payloads limits)
    ~max_payload_bytes:(Limits.payload_bytes limits)
    ~document:(Limits.document limits)
  |> Result.map_error ~f:(fun _ -> Error.Invalid "Invalid remaining content allowance")
;;

let prepare ~parsed_contexts ~programs ~supplied ~limits ~checkpoint =
  let open Result.Let_syntax in
  let initial_work =
    Option.value_map supplied ~default:0 ~f:(fun captured ->
      Capture.Accounting.work_steps (Capture.accounting captured))
  in
  let%bind () =
    if initial_work > Limits.work_steps limits
    then Error (Error.Invalid "Completed content exceeds whole work allowance")
    else Ok ()
  in
  let remaining = ref (Limits.work_steps limits - initial_work) in
  let spend units =
    checkpoint ();
    if units < 0 || units > !remaining
    then Error (Error.Invalid "Content reconciliation work allowance")
    else (
      remaining := !remaining - units;
      Ok ())
  in
  (* Count and inspect the actual immutable contexts before copying or decoding.
     Membership/complete input equality is established by admission and actual
     materialization below; paths or display metadata are never read authority. *)
  let%bind _, _ =
    List.fold_result
      parsed_contexts
      ~init:(0, 0)
      ~f:(fun (contexts, occurrences) parsed ->
        let%bind () = spend 1 in
        if contexts >= Limits.parser_inputs limits
        then Error (Error.Invalid "Content registering context allowance")
        else (
          let%map occurrences =
            List.fold_result
              (Parsed.document_occurrences parsed)
              ~init:occurrences
              ~f:(fun count occurrence ->
                let%bind () = spend 1 in
                if count >= Limits.occurrences limits
                then Error (Error.Invalid "Content occurrence allowance")
                else (
                  match
                    ( supplied
                    , Prompt.Chat_markdown.Document_occurrence.selection occurrence )
                  with
                  | None, (Local_capture | Builtin _) ->
                    Error
                      (Error.Invalid
                         "Captured documents require completed source acquisition")
                  | None, Dynamic | Some _, (Dynamic | Local_capture | Builtin _) ->
                    Ok (count + 1)))
          in
          contexts + 1, occurrences))
  in
  let phase_limits () =
    remaining_limits
      limits
      ~work:!remaining
      ~occurrences:(Limits.occurrences limits)
      ~output:(Limits.output_bytes limits)
  in
  let%bind content =
    match supplied with
    | Some captured ->
      let%bind limits = phase_limits () in
      let%bind admitted =
        admit
          (Content.to_storage (Capture.content captured))
          ~programs
          ~limits
          ~checkpoint
      in
      let%map () = spend (A.work_steps (Content.accounting admitted)) in
      admitted
    | None ->
      let%bind limits = phase_limits () in
      let%bind captured =
        Content.capture ~parsed_contexts ~rendered:[] ~limits ~checkpoint
        |> Result.map_error ~f:(fun error -> Error.Content error)
      in
      let%bind () = spend (A.work_steps (Content.accounting captured)) in
      (* This branch has no input-affinity proof from a native capture. Reconcile
         its registering inputs with the already admitted parser/program owner. *)
      let%bind limits = phase_limits () in
      let%bind admitted =
        admit (Content.to_storage captured) ~programs ~limits ~checkpoint
      in
      let%map () = spend (A.work_steps (Content.accounting admitted)) in
      admitted
  in
  let%bind _, _ =
    List.fold_result parsed_contexts ~init:(0, 0) ~f:(fun (occurrences, output) parsed ->
      let%bind limits =
        remaining_limits
          limits
          ~work:!remaining
          ~occurrences:(Limits.occurrences limits - occurrences)
          ~output:(Limits.output_bytes limits - output)
      in
      let%bind materialized =
        Content.materialize content ~parsed ~limits ~checkpoint
        |> Result.map_error ~f:(fun error -> Error.Content error)
      in
      let receipt = Content.Materialized.accounting materialized in
      let%map () = spend (A.work_steps receipt) in
      occurrences + A.occurrences receipt, output + A.output_bytes receipt)
  in
  checkpoint ();
  Ok { Prepared.content; work_steps = Limits.work_steps limits - !remaining; limits }
;;

module Restored = struct
  type t =
    { content : Content.t
    ; materialized : Content.Materialized.t
    ; work_steps : int
    }

  let content t = t.content
  let materialized t = t.materialized
  let work_steps t = t.work_steps
end

let restore storage ~programs ~parsed ~limits ~checkpoint =
  let open Result.Let_syntax in
  let%bind content = admit storage ~programs ~limits ~checkpoint in
  let admitted_work = A.work_steps (Content.accounting content) in
  let%bind remaining =
    remaining_limits
      limits
      ~work:(Limits.work_steps limits - admitted_work)
      ~occurrences:(Limits.occurrences limits)
      ~output:(Limits.output_bytes limits)
  in
  let%bind materialized =
    Content.materialize content ~parsed ~limits:remaining ~checkpoint
    |> Result.map_error ~f:(fun error -> Error.Content error)
  in
  let work_steps =
    admitted_work + A.work_steps (Content.Materialized.accounting materialized)
  in
  Ok { Restored.content; materialized; work_steps }
;;

let replay (prepared : Prepared.t) ~parsed ~checkpoint =
  let open Result.Let_syntax in
  let%bind remaining =
    remaining_limits
      prepared.limits
      ~work:(Limits.work_steps prepared.limits - prepared.work_steps)
      ~occurrences:(Limits.occurrences prepared.limits)
      ~output:(Limits.output_bytes prepared.limits)
  in
  let%map materialized =
    Content.materialize prepared.content ~parsed ~limits:remaining ~checkpoint
    |> Result.map_error ~f:(fun error -> Error.Content error)
  in
  { Restored.content = prepared.content
  ; materialized
  ; work_steps =
      prepared.work_steps + A.work_steps (Content.Materialized.accounting materialized)
  }
;;

let fingerprint t ~checkpoint =
  let limits =
    Document_schema.Limits.create
      ~max_bytes:262144
      ~max_depth:128
      ~max_fields:100_000
      ~max_nodes:1_000_000
    |> Result.map_error ~f:(fun _ -> "Invalid fixed artifact policy")
    |> Result.ok_or_failwith
  in
  Storage.Manifest.fingerprint
    (Storage.manifest (Content.to_storage t))
    ~limits
    ~checkpoint
;;
