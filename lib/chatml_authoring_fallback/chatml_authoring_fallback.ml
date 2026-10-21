open Core
module Captured = Chatml_source
module Prepared = Chatml_prepared_program
module Build = Chatml_project_build
module Contract = Chatml.Chatml_compiler_contract
module Highlight = Language_highlighting.Highlight_snapshot
module Wire = Agent_protocol.Authoring_assistance
module Limits = Fallback_model.Limits
module Error = Fallback_model.Error
module Work = Fallback_model.Work
module Site_map = Fallback_model.Site_map
module Origin = Fallback_model.Origin
module Selected_contract = Fallback_selected.Selected_contract
module Projection = Fallback_projection

let finish_result ~limits ~request ~items ~status =
  let open Result.Let_syntax in
  let%bind result =
    Wire.Assistance.Result.create ~request ~items ~status
    |> Result.map_error ~f:(fun error -> Error.Wire error)
  in
  let encoded_bytes =
    Wire.Assistance.Result.to_json result |> Jsonaf.to_string |> String.length
  in
  if encoded_bytes > limits.Limits.max_output_bytes
  then Error Error.Output_byte_limit
  else Ok result
;;

let status_unavailable reason ~request ~limits =
  finish_result
    ~limits
    ~request
    ~items:[]
    ~status:(Wire.Assistance.Status.Unavailable reason)
;;

let status_truncated reason ~request ~limits =
  finish_result
    ~limits
    ~request
    ~items:[]
    ~status:(Wire.Assistance.Status.Truncated reason)
;;

let ascii_ident_start = function
  | 'a' .. 'z' | 'A' .. 'Z' | '_' -> true
  | _ -> false
;;

let ascii_ident_continue = function
  | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '_' -> true
  | _ -> false
;;

let representable_identifier work name =
  Work.charge_string work name;
  let len = String.length name in
  len > 0
  && len <= Wire.Limits.max_label_bytes
  && ascii_ident_start name.[0]
  && String.for_all name ~f:ascii_ident_continue
;;

let syntax_templates =
  [ "let"; "fun"; "if"; "match"; "module"; "type"; "()"; "[]"; "{}"; "(| (), () |)" ]
;;

module Lexical_cursor = struct
  type t =
    { start_byte : int
    ; end_byte : int
    ; prefix : string
    ; in_code : bool
    }

  let token_contains range cursor =
    let start_byte = Highlight.Range.start_byte range in
    let end_byte = Highlight.Range.end_byte range in
    start_byte <= cursor && cursor <= end_byte
  ;;

  let inspect snapshot ~cursor ~work =
    let bytes = Highlight.source_contents snapshot in
    let active_ident = ref None in
    let in_code = ref true in
    List.iter (Highlight.spans snapshot) ~f:(fun span ->
      Work.charge work 1;
      let range = Highlight.Span.range span in
      if token_contains range cursor
      then (
        match Highlight.Span.role span with
        | Highlight.Role.Comment | String_literal ->
          if
            Highlight.Range.start_byte range < cursor
            && cursor < Highlight.Range.end_byte range
          then in_code := false
        | Identifier | Keyword ->
          active_ident
          := Some (Highlight.Range.start_byte range, Highlight.Range.end_byte range)
        | Tag_name
        | Attribute_name
        | Attribute_value
        | Raw_delimiter
        | Raw_text
        | Script_body -> ()
        | Type_variable
        | Variant_tag
        | Number_literal
        | Boolean_literal
        | Operator
        | Punctuation ->
          if
            Highlight.Range.start_byte range < cursor
            && cursor < Highlight.Range.end_byte range
          then in_code := false));
    List.iter (Highlight.issues snapshot) ~f:(fun issue ->
      Work.charge work 1;
      let range = Highlight.Issue.range issue in
      if Int.equal cursor (Highlight.Range.end_byte range)
      then (
        match Highlight.Issue.kind issue with
        | Open_comment | Open_string -> in_code := false
        | Open_raw_block | Open_tag | Open_script -> ()));
    let start_byte, end_byte =
      match !active_ident with
      | None -> cursor, cursor
      | Some range -> range
    in
    let prefix_len = cursor - start_byte in
    Work.charge work prefix_len;
    { start_byte
    ; end_byte
    ; prefix = String.sub bytes ~pos:start_byte ~len:prefix_len
    ; in_code = !in_code
    }
  ;;
end

let result_with_items ~request ~limits ~work ~range ~prefix ~selected =
  let module Suggestion = Wire.Assistance.Suggestion in
  let module Status = Wire.Assistance.Status in
  let open Result.Let_syntax in
  let catalog =
    match selected with
    | None -> Ok None
    | Some selected ->
      let prepared = Selected_contract.prepared selected in
      Wire.Assistance.Selected_catalog.create
        ~environment:(Prepared.Environment.name (Prepared.environment prepared))
        ~input_fingerprint:(Prepared.fingerprint prepared)
        ~compiler_contract:(Prepared.compiler_contract prepared)
      |> Result.map ~f:Option.some
      |> Result.map_error ~f:(fun _ -> Error.Contract_mismatch)
  in
  match catalog with
  | Error _ -> status_unavailable Status.Catalog_unavailable ~request ~limits
  | Ok catalog ->
    let items_rev = ref [] in
    let count = ref 0 in
    let reason = ref None in
    let base_bytes =
      match Wire.Assistance.Result.create ~request ~items:[] ~status:Status.Complete with
      | Error _ -> raise Work.Limit
      | Ok empty ->
        Wire.Assistance.Result.to_json empty |> Jsonaf.to_string |> String.length
    in
    let used_bytes = ref (base_bytes + 64) in
    let add ~kind ~evidence label =
      Work.charge work 1;
      if Option.is_none !reason && String.is_prefix label ~prefix
      then
        if !count >= Wire.Assistance.Request.max_items request
        then reason := Some Status.Item_limit
        else (
          match
            Suggestion.create ~kind ~label ~insert_text:label ~replace:range ~evidence
          with
          | Error _ -> reason := Some Status.Result_bytes
          | Ok item ->
            let bytes = Suggestion.to_json item |> Jsonaf.to_string |> String.length in
            if bytes + 1 > limits.Limits.max_output_bytes - !used_bytes
            then reason := Some Status.Result_bytes
            else (
              used_bytes := !used_bytes + bytes + 1;
              items_rev := item :: !items_rev;
              Int.incr count))
    in
    (try
       (match selected, catalog with
        | Some selected, Some catalog ->
          List.iter (Selected_contract.names selected) ~f:(fun name ->
            Work.charge work 1;
            let text = name.Fallback_selected.Name.text in
            if representable_identifier work text
            then
              add
                ~kind:Suggestion.Reference
                ~evidence:(Suggestion.Selected_catalog catalog)
                text)
        | None, None -> ()
        | Some _, None | None, Some _ -> assert false);
       List.iter syntax_templates ~f:(fun label ->
         Work.charge_string work label;
         add ~kind:Suggestion.Template ~evidence:Suggestion.Lexical label)
     with
     | Work.Limit -> reason := Some Status.Work_limit);
    let status =
      match !reason with
      | None -> Status.Complete
      | Some reason -> Status.Truncated reason
    in
    finish_result ~limits ~request ~items:(List.rev !items_rev) ~status
;;

let produce origin ~request ~selected ~limits ~checkpoint =
  let work = Work.create ~max_steps:limits.Limits.max_projection_work_steps ~checkpoint in
  try
    let open Result.Let_syntax in
    let%bind projection = Projection.create origin ~request ~work in
    match projection with
    | None ->
      status_unavailable Wire.Assistance.Status.Source_map_unavailable ~request ~limits
    | Some projection ->
      if
        Option.exists selected ~f:(fun selected ->
          not (Projection.selected_source_matches projection selected ~work))
      then Error Error.Stale_selected_build
      else (
        let%bind snapshot =
          Highlight.scan_chatml
            projection.compiled
            ~limits:limits.lexical
            ~checkpoint:(fun () -> Work.charge work 1)
          |> Result.map_error ~f:(fun error -> Error.Lexical_admission error)
        in
        let cursor = projection.compiled_cursor in
        let reached_cursor =
          match Highlight.status snapshot with
          | Highlight.Status.Reached_eof -> true
          | Truncated { next_byte; reason = _ } -> next_byte > cursor
        in
        if not reached_cursor
        then status_truncated Wire.Assistance.Status.Work_limit ~request ~limits
        else (
          let lexical = Lexical_cursor.inspect snapshot ~cursor ~work in
          if not lexical.in_code
          then
            finish_result
              ~limits
              ~request
              ~items:[]
              ~status:Wire.Assistance.Status.Complete
          else (
            let%bind range =
              Projection.replacement
                projection
                ~start_byte:lexical.start_byte
                ~end_byte:lexical.end_byte
                ~work
            in
            match range with
            | None ->
              status_unavailable
                Wire.Assistance.Status.Source_map_unavailable
                ~request
                ~limits
            | Some range ->
              result_with_items
                ~request
                ~limits
                ~work
                ~range
                ~prefix:lexical.prefix
                ~selected)))
  with
  | Work.Limit -> status_truncated Wire.Assistance.Status.Work_limit ~request ~limits
;;
