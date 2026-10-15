open! Core
module P = Agent_protocol
module Ledger = Chatml_authoring_context
module Sources = Authoring_source_host
module Captured = Chatml_source
module Markdown = Prompt.Chat_markdown
module Selection = Authoring_script_selection
module Prepared = Chatml_prepared_program

module Error = struct
  type t =
    | Foreign_capture
    | Invalid_row
    | Row_limit
    | Retained_byte_limit
    | Work_limit
  [@@deriving sexp_of]
end

type 'preparation_error failure =
  | Row_error of Error.t
  | Preparation_error of 'preparation_error

module Limits = struct
  type t =
    { max_rows : int
    ; max_retained_source_bytes : int
    ; max_retained_canonical_input_bytes : int
    ; max_collection_work_steps : int
    }

  let create
        ~max_rows
        ~max_retained_source_bytes
        ~max_retained_canonical_input_bytes
        ~max_collection_work_steps
    =
    if
      max_rows <= 0
      || max_rows > 1024
      || max_retained_source_bytes <= 0
      || max_retained_canonical_input_bytes <= 0
      || max_collection_work_steps <= 0
      || List.exists
           [ max_retained_source_bytes
           ; max_retained_canonical_input_bytes
           ; max_collection_work_steps
           ]
           ~f:(fun n -> n > P.Authoring.Limits.maximum_wire_integer)
    then Or_error.error_string "invalid retained document selection limits"
    else
      Ok
        { max_rows
        ; max_retained_source_bytes
        ; max_retained_canonical_input_bytes
        ; max_collection_work_steps
        }
  ;;
end

module Row = struct
  type status =
    | Host_prepared of
        { source : Sources.Document_context.Script_capture.t
        ; selected : Selection.Selected.t
        }
    | Independently_selected of Selection.Selected.t
    | No_validated_build of P.Authoring.Analysis.Analyze.unavailable

  type t =
    { parsed : Markdown.Parsed.t
    ; declaration : Markdown.Declaration.t
    ; declaring_source : Captured.t
    ; metadata : Sources.Document_context.Project_capture.t option
    ; status : status
    }

  let host_prepared ~parsed ~declaration ~declaring_source ~metadata ~source ~selected =
    { parsed
    ; declaration
    ; declaring_source
    ; metadata = Some metadata
    ; status = Host_prepared { source; selected }
    }
  ;;

  let independently_selected ~parsed ~declaration ~declaring_source ~metadata ~selected =
    { parsed
    ; declaration
    ; declaring_source
    ; metadata
    ; status = Independently_selected selected
    }
  ;;

  let no_validated_build ~parsed ~declaration ~declaring_source ~metadata ~reason =
    { parsed
    ; declaration
    ; declaring_source
    ; metadata
    ; status = No_validated_build reason
    }
  ;;

  let parsed t = t.parsed
  let declaration t = t.declaration
  let declaring_source t = t.declaring_source
  let metadata t = t.metadata
  let status t = t.status
end

type t =
  { capture : Sources.Document_context.Parsed_capture.t
  ; identity : Ledger.Identity.t
  ; generation : Ledger.Generation.t
  ; source_id : Captured.Id.t
  ; source_digest : Captured.Digest.t
  ; source_bytes : int
  ; rows : Row.t list
  }

let retained_count t = List.length t.rows

let matches t ~inputs ~capture =
  let source = Ledger.Document.Inputs.source inputs in
  phys_equal t.capture capture
  && Ledger.Identity.equal t.identity (Ledger.Document.Inputs.identity inputs)
  && Ledger.Generation.equal t.generation (Ledger.Document.Inputs.generation inputs)
  && Captured.Id.equal t.source_id (Captured.id source)
  && Captured.Digest.equal t.source_digest (Captured.digest source)
  && Int.equal t.source_bytes (Captured.byte_length source)
;;

exception Exhausted

let collect ~inputs ~capture ~limits ~checkpoint ~prepare_one =
  let remaining = ref limits.Limits.max_collection_work_steps in
  let tick () =
    if !remaining <= 0 then raise Exhausted;
    decr remaining;
    checkpoint ()
  in
  let source = Ledger.Document.Inputs.source inputs in
  let captured = Sources.Document_context.Parsed_capture.source capture in
  if
    (not (Captured.Id.equal (Captured.id source) (Captured.id captured)))
    || (not (Captured.Digest.equal (Captured.digest source) (Captured.digest captured)))
    || not (Int.equal (Captured.byte_length source) (Captured.byte_length captured))
  then Error (Row_error Error.Foreign_capture)
  else (
    let receipt = Sources.Document_context.Parsed_capture.parsed capture in
    let generated = Markdown.Generated.parsed receipt in
    let parsed_units = generated.parsed_root :: List.map generated.parsed_agents ~f:snd in
    let sources = Sources.Document_context.Parsed_capture.input_sources capture in
    let declaring_source declaration =
      let input =
        Chatmd_parser_input.Range.input (Markdown.Declaration.range declaration)
      in
      let rec visit found = function
        | [] -> found
        | (candidate, source) :: rest ->
          tick ();
          if phys_equal candidate input
          then (
            match found with
            | `Missing -> visit (`Found source) rest
            | `Found _ | `Duplicate -> `Duplicate)
          else visit found rest
      in
      visit `Missing sources
    in
    try
      let rec preflight_units count reversed = function
        | [] -> Ok (List.rev reversed)
        | unit :: remaining ->
          preflight_declarations
            count
            reversed
            unit
            (Markdown.Parsed.declarations unit)
            remaining
      and preflight_declarations count reversed unit declarations remaining =
        match declarations with
        | [] -> preflight_units count reversed remaining
        | declaration :: rest ->
          tick ();
          if count >= limits.max_rows
          then Error (Row_error Error.Row_limit)
          else (
            match declaring_source declaration with
            | `Missing | `Duplicate -> Error (Row_error Error.Foreign_capture)
            | `Found source ->
              preflight_declarations
                (count + 1)
                ((unit, declaration, source) :: reversed)
                unit
                rest
                remaining)
      in
      let open Result.Let_syntax in
      let%bind preflighted = preflight_units 0 [] parsed_units in
      let used_source_bytes = ref 0 in
      let used_input_bytes = ref 0 in
      let charge used maximum bytes =
        if bytes < 0 || bytes > maximum - !used
        then Error (Row_error Error.Retained_byte_limit)
        else (
          used := !used + bytes;
          Ok ())
      in
      let charge_sources build =
        List.fold_result
          (Prepared.sources (Chatml_project_build.prepared build))
          ~init:()
          ~f:(fun () source ->
            tick ();
            charge
              used_source_bytes
              limits.max_retained_source_bytes
              (Captured.byte_length source))
      in
      let charge_selected selected =
        let build = Selection.Selected.build selected in
        let%bind () = charge_sources build in
        charge
          used_input_bytes
          limits.max_retained_canonical_input_bytes
          (Prepared.Accounting.input_bytes
             (Prepared.accounting (Chatml_project_build.prepared build)))
      in
      let validate parsed declaration declaring_source row =
        if
          (not (phys_equal (Row.parsed row) parsed))
          || (not (phys_equal (Row.declaration row) declaration))
          || not (phys_equal (Row.declaring_source row) declaring_source)
        then Error (Row_error Error.Invalid_row)
        else (
          let%bind () =
            match Row.metadata row with
            | None -> Ok ()
            | Some metadata ->
              if
                Sources.Document_context.Project_capture.matches
                  metadata
                  ~parsed:capture
                  ~declaration
              then
                charge
                  used_source_bytes
                  limits.max_retained_source_bytes
                  (Captured.byte_length
                     (Sources.Document_context.Project_capture.source metadata))
              else Error (Row_error Error.Invalid_row)
          in
          match Row.status row with
          | No_validated_build _ -> Ok ()
          | Independently_selected selected -> charge_selected selected
          | Host_prepared { source; selected } ->
            (match Row.metadata row with
             | None -> Error (Row_error Error.Invalid_row)
             | Some metadata ->
               if
                 Sources.Document_context.Script_capture.matches
                   source
                   ~parsed:capture
                   ~declaration
                   ~project:metadata
                 && phys_equal
                      (Selection.Selected.build selected)
                      (Sources.Document_context.Script_capture.build source)
                 &&
                 match
                   ( Chatml.Chatml_compiler_contract.kind
                       (Selection.Selected.contract selected)
                   , Chatml.Chatml_compiler_contract.kind
                       (Sources.Document_context.Script_capture.contract source) )
                 with
                 | Standard selected, Standard captured ->
                   Chatml.Chatml_compiler_contract.Target.equal selected captured
                 | Native, Native | Native, Standard _ | Standard _, Native -> false
               then charge_selected selected
               else Error (Row_error Error.Invalid_row)))
      in
      let%map reversed =
        List.fold_result
          preflighted
          ~init:[]
          ~f:(fun reversed (parsed, declaration, source) ->
            tick ();
            let%bind row =
              prepare_one declaration ~parsed ~declaring_source:source ~checkpoint:tick
              |> Result.map_error ~f:(fun error -> Preparation_error error)
            in
            let%map () = validate parsed declaration source row in
            row :: reversed)
      in
      { capture
      ; identity = Ledger.Document.Inputs.identity inputs
      ; generation = Ledger.Document.Inputs.generation inputs
      ; source_id = Captured.id source
      ; source_digest = Captured.digest source
      ; source_bytes = Captured.byte_length source
      ; rows = List.rev reversed
      }
    with
    | Exhausted -> Error (Row_error Error.Work_limit))
;;

let find t ~capture ~declaration ~checkpoint =
  if not (phys_equal t.capture capture)
  then Error Error.Foreign_capture
  else (
    let rec visit = function
      | [] -> Ok None
      | row :: rest ->
        checkpoint ();
        if phys_equal (Row.declaration row) declaration then Ok (Some row) else visit rest
    in
    visit t.rows)
;;
