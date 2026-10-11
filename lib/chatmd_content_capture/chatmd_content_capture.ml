open Core
module Content = Chatmd_prompt_content
module Parsed = Prompt.Chat_markdown.Parsed
module Occurrence = Prompt.Chat_markdown.Document_occurrence
module Input = Chatmd_parser_input
module Range = Input.Range
module Member = Source.Portable_path
module Limits = Content.Limits
module A = Content.Accounting

module Error = struct
  type reason =
    | Invalid of string
    | Filesystem of Chatml_local_source.Error.t
    | Content of Content.Error.t

  type t =
    { range : Range.t option
    ; reason : reason
    }

  let invalid ?range text = { range; reason = Invalid (String.prefix text 256) }
  let content error = { range = Content.Error.range error; reason = Content error }
  let filesystem range error = { range = Some range; reason = Filesystem error }
  let range t = t.range

  let sexp_of_t t =
    let reason =
      match t.reason with
      | Invalid text -> [%sexp Invalid (text : string)]
      | Filesystem error -> [%sexp Filesystem (error : Chatml_local_source.Error.t)]
      | Content error -> [%sexp Content (error : Content.Error.t)]
    in
    let owner =
      Option.map t.range ~f:(fun range ->
        Input.path (Range.input range), Range.start_byte range, Range.end_byte range)
    in
    [%sexp (owner : (string * int * int) option), (reason : Sexp.t)]
  ;;
end

module Accounting = struct
  type t =
    { content : A.t
    ; work : int
    }

  let occurrences t = A.occurrences t.content
  let input_bytes t = A.input_bytes t.content
  let output_bytes t = A.output_bytes t.content
  let nodes t = A.nodes t.content
  let work_steps t = t.work
end

type t =
  { content : Content.t
  ; limits : Limits.t
  ; accounting : Accounting.t
  }

let content t = t.content
let accounting t = t.accounting
let limits t = t.limits

(* This owner tracks only selection/native work and rendering already performed.
   Content.capture reconciles rendering once and owns all subsequent allocations. *)
module Budget = struct
  type t =
    { limits : Limits.t
    ; mutable native_work : int
    ; mutable rendering_work : int
    ; mutable input : int
    ; mutable output : int
    ; mutable nodes : int
    }

  let create limits =
    { limits; native_work = 0; rendering_work = 0; input = 0; output = 0; nodes = 0 }
  ;;

  let remaining_work t = Limits.work_steps t.limits - t.native_work - t.rendering_work

  let work t units =
    if units < 0 || units > remaining_work t
    then Error (Error.invalid "document acquisition work limit")
    else (
      t.native_work <- t.native_work + units;
      Ok ())
  ;;

  let add_rendering t a =
    if
      A.work_steps a > remaining_work t
      || A.input_bytes a > Limits.input_bytes t.limits - t.input
      || A.output_bytes a > Limits.output_bytes t.limits - t.output
      || A.nodes a > Limits.nodes t.limits - t.nodes
    then Error (Error.invalid "document acquisition aggregate limit")
    else (
      t.rendering_work <- t.rendering_work + A.work_steps a;
      t.input <- t.input + A.input_bytes a;
      t.output <- t.output + A.output_bytes a;
      t.nodes <- t.nodes + A.nodes a;
      Ok ())
  ;;

  let render_limits t =
    Limits.create
      ~max_occurrences:(Limits.occurrences t.limits)
      ~max_parser_inputs:(Limits.parser_inputs t.limits)
      ~max_input_bytes:(Limits.input_bytes t.limits - t.input)
      ~max_output_bytes:(Limits.output_bytes t.limits - t.output)
      ~max_work_steps:(remaining_work t)
      ~max_nodes:(Limits.nodes t.limits - t.nodes)
      ~max_depth:(Limits.depth t.limits)
      ~max_payloads:(Limits.payloads t.limits)
      ~max_payload_bytes:(Limits.payload_bytes t.limits)
      ~document:(Limits.document t.limits)
    |> Result.map_error ~f:(fun _ -> Error.invalid "invalid remaining rendering limits")
  ;;

  let capture_limits t =
    (* Rendering's work/nodes and logical bytes are reconciled by capture. *)
    Limits.create
      ~max_occurrences:(Limits.occurrences t.limits)
      ~max_parser_inputs:(Limits.parser_inputs t.limits)
      ~max_input_bytes:(Limits.input_bytes t.limits)
      ~max_output_bytes:(Limits.output_bytes t.limits)
      ~max_work_steps:(Limits.work_steps t.limits - t.native_work)
      ~max_nodes:(Limits.nodes t.limits)
      ~max_depth:(Limits.depth t.limits)
      ~max_payloads:(Limits.payloads t.limits)
      ~max_payload_bytes:(Limits.payload_bytes t.limits)
      ~document:(Limits.document t.limits)
    |> Result.map_error ~f:(fun _ -> Error.invalid "invalid remaining capture limits")
  ;;
end

let portable_input budget input ~checkpoint =
  let open Result.Let_syntax in
  let%bind () = Budget.work budget (String.length (Input.path input)) in
  checkpoint ();
  Member.create (Input.path input)
  |> Result.map_error ~f:(fun _ -> Error.invalid "nonportable document declaring input")
;;

let preflight budget parsed_contexts ~checkpoint =
  let open Result.Let_syntax in
  let limits = budget.Budget.limits in
  let%bind _, _, _ =
    List.fold_result
      parsed_contexts
      ~init:(0, String.Set.empty, 0)
      ~f:(fun (contexts, inputs, occurrences) parsed ->
        checkpoint ();
        let%bind () = Budget.work budget 1 in
        if contexts >= Limits.parser_inputs limits
        then Error (Error.invalid "document registering context limit")
        else (
          let%bind _ = portable_input budget (Parsed.root_input parsed) ~checkpoint in
          let%bind _, inputs =
            List.fold_result
              (Parsed.inputs parsed)
              ~init:(0, inputs)
              ~f:(fun (count, inputs) input ->
                checkpoint ();
                let%bind () = Budget.work budget 1 in
                if count >= Limits.parser_inputs limits
                then Error (Error.invalid "document parser-input limit")
                else (
                  let%bind _ = portable_input budget input ~checkpoint in
                  (* Identity hashes scan only admitted portable paths and fixed
                     digests/lengths. Full byte equality remains Content's owner. *)
                  let%bind () =
                    Budget.work budget (String.length (Input.path input) + 256)
                  in
                  let key = Input.identity_digest input ~checkpoint in
                  let%bind () =
                    Budget.work budget (256 * (1 + Int.ceil_log2 (Set.length inputs + 1)))
                  in
                  if
                    (not (Set.mem inputs key))
                    && Set.length inputs >= Limits.parser_inputs limits
                  then Error (Error.invalid "document parser-input union limit")
                  else Ok (count + 1, Set.add inputs key)))
          in
          let%map occurrences =
            List.fold_result
              (Parsed.document_occurrences parsed)
              ~init:occurrences
              ~f:(fun count occurrence ->
                checkpoint ();
                let%bind () = Budget.work budget 1 in
                if count >= Limits.occurrences limits
                then
                  Error
                    (Error.invalid
                       ~range:(Occurrence.range occurrence)
                       "document occurrence limit")
                else Ok (count + 1))
          in
          contexts + 1, inputs, occurrences))
  in
  Ok ()
;;

let local_source owner budget occurrence ~checkpoint =
  let open Result.Let_syntax in
  let range = Occurrence.range occurrence in
  let%bind base = portable_input budget (Range.input range) ~checkpoint in
  let%bind () = Budget.work budget (String.length (Occurrence.src occurrence)) in
  let%bind member =
    Source_loader.resolve_member
      base
      ~reference:(Occurrence.src occurrence)
      ~max_reference_bytes:1024
      ~checkpoint
    |> Result.map_error ~f:(fun _ -> Error.invalid ~range "invalid local document member")
  in
  let components = List.length (String.split (Member.to_string member) ~on:'/') in
  (* A portable path has at most2048 bytes, so this arithmetic is bounded. *)
  let stage_base = (2 * components) + 6 in
  let remaining = Budget.remaining_work budget in
  if remaining < stage_base
  then Error (Error.invalid ~range "document read stage limit")
  else (
    let allowance =
      Int.min (Limits.input_bytes budget.limits - budget.input) (remaining - stage_base)
    in
    (* Admit the full stage envelope before effects. Sequential ownership means
       nothing else can spend it during Local.read; failure aborts this run. *)
    let%bind contents =
      Chatml_local_source.read owner ~member ~max_bytes:allowance ~checkpoint
      |> Result.map_error ~f:(Error.filesystem range)
    in
    let%map () = Budget.work budget (String.length contents + stage_base) in
    Content.Rendered.Source.Local { member; contents })
;;

let run owner ~parsed_contexts ~limits ~checkpoint =
  let open Result.Let_syntax in
  let budget = Budget.create limits in
  let%bind () = preflight budget parsed_contexts ~checkpoint in
  let%bind rendered =
    List.fold_result parsed_contexts ~init:[] ~f:(fun rendered parsed ->
      List.fold_result
        (Parsed.document_occurrences parsed)
        ~init:rendered
        ~f:(fun rendered occurrence ->
          checkpoint ();
          let%bind () = Budget.work budget 1 in
          match Occurrence.selection occurrence with
          | Dynamic -> Ok rendered
          | Local_capture | Builtin _ ->
            let%bind source =
              match Occurrence.selection occurrence with
              | Dynamic -> assert false
              | Local_capture -> local_source owner budget occurrence ~checkpoint
              | Builtin id ->
                let%bind id =
                  Builtin_instructions.Id.of_string id
                  |> Result.map_error ~f:(fun _ ->
                    Error.invalid
                      ~range:(Occurrence.range occurrence)
                      "invalid builtin identifier")
                in
                Builtin_instructions.find id
                |> Result.map ~f:(fun asset -> Content.Rendered.Source.Builtin asset)
                |> Result.map_error ~f:(fun _ ->
                  Error.invalid
                    ~range:(Occurrence.range occurrence)
                    "unknown builtin document")
            in
            let%bind limits = Budget.render_limits budget in
            let%bind value =
              Content.Rendered.create parsed ~occurrence ~source ~limits ~checkpoint
              |> Result.map_error ~f:Error.content
            in
            let%map () =
              Budget.add_rendering budget (Content.Rendered.accounting value)
            in
            value :: rendered))
  in
  let original_limits = limits in
  let%bind limits = Budget.capture_limits budget in
  let%bind content =
    Content.capture ~parsed_contexts ~rendered:(List.rev rendered) ~limits ~checkpoint
    |> Result.map_error ~f:Error.content
  in
  checkpoint ();
  let observed = Content.accounting content in
  Ok
    { content
    ; limits = original_limits
    ; accounting =
        { Accounting.content = observed
        ; work = budget.native_work + A.work_steps observed
        }
    }
;;
