open Core
module C = Chatml_prompt_programs
module Plan = Script_contract_plan
module CM = Prompt.Chat_markdown

type t =
  | Legacy of C.t
  | Selected of C.t

module Candidate = struct
  type t =
    | Absent
    | Unique of C.Binding.t
    | Ambiguous

  let add t binding =
    match t with
    | Absent -> Unique binding
    | Unique _ | Ambiguous -> Ambiguous
  ;;
end

module Row = struct
  type t =
    { program : Plan.Program.t
    ; mutable has_record : bool
    ; mutable exact : Candidate.t
    ; mutable remapped : Candidate.t
    }
end

exception Work_exhausted of unit ref

let diagnostic ?program code message =
  let source =
    Option.map program ~f:(fun program ->
      CM.Declaration.source_ref (Plan.Program.declaration program))
  in
  [ Chatmd_shell_spec.Diagnostic.error ?source ~code message ]
;;

let bytes_equal left right ~poll =
  poll ();
  if String.length left <> String.length right
  then false
  else (
    let offset = ref 0 in
    let same = ref true in
    while !same && !offset < String.length left do
      if !offset mod 4096 = 0 then poll ();
      same := Char.equal left.[!offset] right.[!offset];
      incr offset
    done;
    poll ();
    !same)
;;

let exact_binding original current ~poll =
  (* The authoritative equality's variable-sized field is the qualified ID.
     Paths are admitted portable members (1024 bytes), revisions fixed SHA256,
     and offsets fixed integers. Poll its byte comparison before full equality. *)
  bytes_equal (C.Binding.script_id original) (C.Binding.script_id current) ~poll
  &&
  (poll ();
   C.Binding.equal original current)
;;

let collect rows matched ~poll =
  List.iter rows ~f:(fun row ->
    poll ();
    if
      phys_equal
        (Plan.Program.declaration row.Row.program)
        (C.Declaration_match.declaration matched)
    then (
      row.has_record <- true;
      if
        Chatml_source.Digest.equal
          (Plan.Program.compiler_contract row.program)
          (C.Declaration_match.compiler_contract matched)
      then (
        let original = C.Declaration_match.original matched in
        row.remapped <- Candidate.add row.remapped original;
        if exact_binding original (Plan.Program.binding row.program) ~poll
        then row.exact <- Candidate.add row.exact original)))
;;

let choose row ~poll =
  poll ();
  let candidate =
    match row.Row.exact with
    | Unique _ | Ambiguous -> row.exact
    | Absent -> row.remapped
  in
  match candidate with
  | Candidate.Unique original ->
    Ok
      ( original
      , Plan.Program.compiler_contract row.program
      , Plan.Program.binding row.program )
  | Absent ->
    Error
      (diagnostic
         ~program:row.program
         "chatml.program_missing_role"
         "Retained script requires a complete authentic role selection")
  | Ambiguous ->
    Error
      (diagnostic
         ~program:row.program
         "chatml.program_ambiguous_role"
         "Retained script role has more than one original declaration mapping")
;;

let select plan ~custody ~authored ~limits ~max_work_steps ~checkpoint =
  let limit_error () =
    diagnostic
      "chatml.program_selection_limit"
      "Retained role selection exceeds the host allowance"
  in
  if max_work_steps <= 0
  then Error (limit_error ())
  else (
    let identity = ref () in
    let used = ref 0 in
    let poll () =
      if !used >= max_work_steps then raise (Work_exhausted identity);
      incr used;
      checkpoint ()
    in
    let map result =
      Result.map_error result ~f:(fun error ->
        poll ();
        match error with
        | C.Error.Limit_exceeded -> limit_error ()
        | Document _
        | Storage _
        | Provided _
        | Selected _
        | Invalid_binding _
        | Missing_contract _
        | Unexpected_contract _
        | Contract_mismatch _
        | Script_mismatch _
        | Duplicate_program _
        | Conflicting_source _
        | Invalid_inventory _ ->
          diagnostic
            "chatml.program_selection_invalid"
            "Retained role selection does not match this authentic parser Root")
    in
    try
      let open Result.Let_syntax in
      let parsed = Plan.parsed plan in
      let%bind () = map (C.validate_selection custody ~parsed ~limits ~checkpoint:poll) in
      let rows =
        List.map (Plan.programs plan) ~f:(fun program ->
          poll ();
          Row.{ program; has_record = false; exact = Absent; remapped = Absent })
      in
      let%bind () =
        map
          (C.fold_declaration_matches
             custody
             ~parsed
             ~limits
             ~checkpoint:poll
             ~init:()
             ~f:(fun () matched -> collect rows matched ~poll))
      in
      let recorded =
        List.exists rows ~f:(fun row ->
          poll ();
          row.Row.has_record)
      in
      poll ();
      let legacy =
        (not recorded)
        && (not (Plan.has_projects plan))
        && not (C.has_active_programs custody)
      in
      let%bind mappings =
        if legacy
        then Ok []
        else if List.is_empty rows
        then
          Error
            (diagnostic
               "chatml.program_unexpected_selection"
               "Retained active selection has no executable declaration in this role")
        else (
          let%map reversed =
            List.fold_result rows ~init:[] ~f:(fun mappings row ->
              let%map mapping = choose row ~poll in
              poll ();
              mapping :: mappings)
          in
          List.fold reversed ~init:[] ~f:(fun mappings mapping ->
            poll ();
            mapping :: mappings))
      in
      let%map selected =
        map
          (C.reroot custody ~authored ~parsed ~programs:mappings ~limits ~checkpoint:poll)
      in
      poll ();
      if legacy then Legacy selected else Selected selected
    with
    | Work_exhausted owner when phys_equal owner identity -> Error (limit_error ()))
;;
