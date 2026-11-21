open Core
module B = Content_budget
module Input = Chatmd_parser_input
module Parsed = Prompt.Chat_markdown.Parsed
module O = Prompt.Chat_markdown.Document_occurrence
module Record = Content_record
module R = Content_rendered
module Storage = Agent_store.Prompt_program_bundle
module Originals = Agent_store.Installed_document_originals
module Projection = Prompt.Chat_markdown.Document_projection

type t =
  { inputs : Input.t list
  ; roots : Input.t list
  ; records : Record.t list
  ; payloads : Storage.Payload.t list
  ; accounting : B.Accounting.t
  }

let inputs t = t.inputs
let roots t = t.roots
let records t = t.records
let payloads t = t.payloads
let accounting t = t.accounting
let invalid reason = Error (B.Error.invalid reason)

let input_table inputs ~budget =
  let open Result.Let_syntax in
  let%bind _ =
    B.count budget inputs ~maximum:(B.Limits.parser_inputs (B.limits budget))
  in
  List.fold_result inputs ~init:String.Map.empty ~f:(fun table input ->
    let%bind key = Record.input_key input ~budget in
    let%bind () = B.map_work budget (Map.length table) in
    match Map.find table key with
    | None -> Ok (Map.set table ~key ~data:input)
    | Some previous ->
      let%bind equal = Record.input_equal previous input ~budget in
      if equal then Ok table else invalid "parser-input identity collision")
;;

let create ~inputs ~roots ~records ~budget =
  let open Result.Let_syntax in
  let limits = B.limits budget in
  let%bind table = input_table inputs ~budget in
  let%bind _ = B.count budget roots ~maximum:(B.Limits.parser_inputs limits) in
  let%bind _ = B.count budget records ~maximum:(B.Limits.occurrences limits) in
  let admit_input input =
    let%bind key = Record.input_key input ~budget in
    let%bind () = B.map_work budget (Map.length table) in
    match Map.find table key with
    | None -> invalid "document input is absent from supplied parser inventory"
    | Some actual ->
      let%bind equal = Record.input_equal input actual ~budget in
      if equal then Ok key else invalid "document parser-input collision"
  in
  let%bind root_keys =
    List.fold_result roots ~init:String.Set.empty ~f:(fun keys root ->
      if not (Input.Role.equal (Input.role root) Root)
      then invalid "non-Root registration context"
      else (
        let%bind key = admit_input root in
        let%bind () = B.map_work budget (Set.length keys) in
        if Set.mem keys key
        then invalid "duplicate content registering Root"
        else Ok (Set.add keys key)))
  in
  let%bind _, pool, bytes =
    List.fold_result
      records
      ~init:(String.Set.empty, String.Map.empty, 0)
      ~f:(fun (keys, pool, bytes) record ->
        let%bind root = admit_input (Record.root record) in
        let%bind _ = admit_input (Record.input record) in
        let%bind () = B.map_work budget (Set.length root_keys + Set.length keys) in
        if not (Set.mem root_keys root)
        then invalid "document registering Root is absent"
        else if Set.mem keys (Record.key record)
        then invalid "duplicate document occurrence"
        else (
          let keys = Set.add keys (Record.key record) in
          match Record.kind record with
          | Dynamic -> Ok (keys, pool, bytes)
          | Captured { output_digest; contents; _ } ->
            let%bind () = B.map_work budget (Map.length pool) in
            (match Map.find pool output_digest with
             | Some previous ->
               let%bind equal = B.equal_bytes budget previous contents in
               if equal
               then Ok (keys, pool, bytes)
               else invalid "rendered payload digest collision"
             | None ->
               if
                 Map.length pool >= B.Limits.payloads limits
                 || String.length contents > B.Limits.payload_bytes limits - bytes
               then invalid "content physical payload limit"
               else
                 Ok
                   ( keys
                   , Map.set pool ~key:output_digest ~data:contents
                   , bytes + String.length contents ))))
  in
  let%bind () = B.work_product budget 4 (Map.length pool) in
  let%bind payloads =
    List.fold_result (Map.to_alist pool) ~init:[] ~f:(fun acc (_, data) ->
      let%bind () = B.work budget (String.length data) in
      let%map payload =
        Storage.Payload.create data |> Result.map_error ~f:B.Error.storage
      in
      payload :: acc)
  in
  ignore (bytes : int);
  Ok
    { inputs
    ; roots
    ; records
    ; payloads = List.rev payloads
    ; accounting = B.accounting budget
    }
;;

let capture ~parsed_contexts ~rendered ~budget =
  let open Result.Let_syntax in
  let limits = B.limits budget in
  let%bind _ = B.count budget parsed_contexts ~maximum:(B.Limits.parser_inputs limits) in
  let%bind _ = B.count budget rendered ~maximum:(B.Limits.occurrences limits) in
  (* Reconcile the whole measured rendering prefix before any metadata copies.
     Input/output/occurrence counts are admitted by Record.create below exactly
     once; work/nodes reflect conversion that has already happened. *)
  let%bind () =
    List.fold_result rendered ~init:() ~f:(fun () rendered ->
      let a = R.accounting rendered in
      B.add
        budget
        (B.Accounting.create
           ~occurrences:0
           ~input_bytes:0
           ~output_bytes:0
           ~work_steps:(B.Accounting.work_steps a)
           ~nodes:(B.Accounting.nodes a)))
  in
  let%bind _ =
    List.fold_result parsed_contexts ~init:0 ~f:(fun count parsed ->
      List.fold_result (Parsed.document_occurrences parsed) ~init:count ~f:(fun count _ ->
        let%bind () = B.work budget 1 in
        if count >= B.Limits.occurrences limits
        then invalid "content aggregate occurrence limit"
        else Ok (count + 1)))
  in
  let%bind (), _ =
    List.fold_result
      rendered
      ~init:((), (0, 0))
      ~f:(fun ((), (input, output)) value ->
        let%bind () = B.work budget 1 in
        if
          R.input_bytes value > B.Limits.input_bytes limits - input
          || String.length (R.contents value) > B.Limits.output_bytes limits - output
        then invalid "rendered aggregate byte limit"
        else
          Ok ((), (input + R.input_bytes value, output + String.length (R.contents value))))
  in
  let%bind inputs, roots, records, used, originals =
    List.fold_result
      parsed_contexts
      ~init:(String.Map.empty, [], [], [], [])
      ~f:(fun (inputs, roots, records, used, originals) parsed ->
        let%bind input_count =
          B.count budget (Parsed.inputs parsed) ~maximum:(B.Limits.parser_inputs limits)
        in
        ignore (input_count : int);
        let%bind inputs =
          List.fold_result (Parsed.inputs parsed) ~init:inputs ~f:(fun inputs input ->
            let%bind key = Record.input_key input ~budget in
            let%bind () = B.map_work budget (Map.length inputs) in
            match Map.find inputs key with
            | Some actual ->
              let%bind equal = Record.input_equal actual input ~budget in
              if equal then Ok inputs else invalid "parser-input identity collision"
            | None ->
              if Map.length inputs >= B.Limits.parser_inputs limits
              then invalid "content input union limit"
              else Ok (Map.set inputs ~key ~data:input))
        in
        let%bind _ =
          B.count
            budget
            (Parsed.document_occurrences parsed)
            ~maximum:(B.Limits.occurrences limits)
        in
        let root = Parsed.root_input parsed in
        let%bind records, used, originals =
          List.fold_result
            (Parsed.document_occurrences parsed)
            ~init:(records, used, originals)
            ~f:(fun (records, used, originals) occurrence ->
              let%bind () = B.work budget 2 in
              let%bind found =
                List.fold_result rendered ~init:None ~f:(fun found value ->
                  let%bind () = B.work budget 1 in
                  if not (phys_equal occurrence (R.occurrence value))
                  then Ok found
                  else (
                    match found with
                    | None -> Ok (Some value)
                    | Some _ -> invalid "duplicate rendered occurrence"))
              in
              let%bind kind, used =
                match O.selection occurrence, found with
                | Dynamic, None -> Ok (Record.Kind.Dynamic, used)
                | Dynamic, Some _ -> invalid "rendered value for dynamic occurrence"
                | (Local_capture | Builtin _), None ->
                  invalid "missing rendered occurrence"
                | (Local_capture | Builtin _), Some value ->
                  let%bind equal = Record.input_equal root (R.root value) ~budget in
                  if not equal
                  then invalid "rendered registering Root mismatch"
                  else
                    Ok
                      ( Record.Kind.Captured
                          { provenance = R.provenance value
                          ; input_digest = R.input_digest value
                          ; input_bytes = R.input_bytes value
                          ; output_digest = R.output_digest value
                          ; contents = R.contents value
                          }
                      , value :: used )
              in
              let%bind src_digest = B.hash budget (O.src occurrence) in
              let%bind record =
                Record.create
                  ~root
                  ~range:(O.range occurrence)
                  ~ordinal:(O.ordinal occurrence)
                  ~src_digest
                  ~src_bytes:(String.length (O.src occurrence))
                  ~local:(O.local occurrence)
                  ~cleanup_html:(O.cleanup_html occurrence)
                  ~markdown:(O.markdown occurrence)
                  ~kind
                  ~budget
              in
              let%map originals =
                match Option.bind found ~f:R.original, kind with
                | ( Some raw
                  , Record.Kind.Captured
                      { provenance = Local _; input_digest; input_bytes; _ } ) ->
                  let%bind () = B.work budget (input_bytes + 256) in
                  let media_type =
                    if O.markdown occurrence || O.cleanup_html occurrence
                    then "text/html"
                    else "application/octet-stream"
                  in
                  let%bind occurrence =
                    Originals.Occurrence.of_digest (Record.key record)
                    |> Result.map_error ~f:B.Error.storage
                  in
                  let%map entry =
                    Originals.Entry.create
                      ~occurrence
                      ~sha256:input_digest
                      ~byte_length:input_bytes
                      ~media_type
                    |> Result.map_error ~f:B.Error.storage
                  in
                  (entry, raw) :: originals
                | None, _ -> Ok originals
                | Some _, (Record.Kind.Dynamic | Record.Kind.Captured _) ->
                  invalid "original document requires captured local provenance"
              in
              record :: records, used, originals)
        in
        let%bind () = B.work budget 1 in
        Ok (inputs, root :: roots, records, used, originals))
  in
  let%bind () =
    List.fold_result rendered ~init:() ~f:(fun () value ->
      let%bind found =
        List.fold_result used ~init:false ~f:(fun found actual ->
          let%map () = B.work budget 1 in
          found || phys_equal value actual)
      in
      if found then Ok () else invalid "surplus rendered value")
  in
  let%bind () = B.work_product budget 2 (Map.length inputs) in
  let%bind inventory =
    create
      ~inputs:(Map.data inputs)
      ~roots:(List.rev roots)
      ~records:(List.rev records)
      ~budget
  in
  let%map originals =
    match originals with
    | [] -> Ok None
    | _ :: _ ->
      Originals.create originals
      |> Result.map ~f:Option.some
      |> Result.map_error ~f:B.Error.storage
  in
  inventory, originals
;;

let find_occurrence records ~root ~occurrence ~budget =
  let open Result.Let_syntax in
  List.fold_result records ~init:None ~f:(fun found record ->
    let%bind equal = Record.matches record ~root occurrence ~budget in
    if not equal
    then Ok found
    else (
      match found with
      | None -> Ok (Some record)
      | Some _ -> invalid "ambiguous content occurrence"))
;;

module Original_keys = struct
  type t = (O.t * Originals.Occurrence.t) Int.Map.t

  let empty = Int.Map.empty

  let find t occurrence =
    match Map.find t (O.ordinal occurrence) with
    | Some (actual, key) when phys_equal actual occurrence -> Some key
    | _ -> None
  ;;

  let add t ~occurrence ~record ~budget =
    let open Result.Let_syntax in
    let%bind () = B.map_work budget (Map.length t) in
    let%map key =
      Originals.Occurrence.of_digest (Record.key record)
      |> Result.map_error ~f:B.Error.storage
    in
    Map.set t ~key:(O.ordinal occurrence) ~data:(occurrence, key)
  ;;
end

let original_key t ~parsed ~occurrence ~budget =
  let open Result.Let_syntax in
  let%bind present =
    List.fold_result
      (Parsed.document_occurrences parsed)
      ~init:false
      ~f:(fun found value ->
        let%map () = B.work budget 1 in
        found || phys_equal occurrence value)
  in
  if not present
  then invalid "original document occurrence belongs to a different parser result"
  else (
    let%bind record =
      find_occurrence t.records ~root:(Parsed.root_input parsed) ~occurrence ~budget
    in
    match Option.map record ~f:Record.kind, record with
    | Some (Record.Kind.Captured { provenance = Local _; _ }), Some record ->
      Originals.Occurrence.of_digest (Record.key record)
      |> Result.map_error ~f:B.Error.storage
    | _ -> invalid "installed document original requires a local captured occurrence")
;;

let materialize t ~parsed ~budget =
  let open Result.Let_syntax in
  let%bind _ =
    B.count
      budget
      (Parsed.document_occurrences parsed)
      ~maximum:(B.Limits.occurrences (B.limits budget))
  in
  let root = Parsed.root_input parsed in
  let%bind present =
    List.fold_result t.roots ~init:false ~f:(fun present actual ->
      let%map equal = Record.input_equal actual root ~budget in
      present || equal)
  in
  if not present
  then invalid "selected Root is absent from content custody"
  else (
    let%bind selected =
      List.fold_result t.records ~init:[] ~f:(fun selected record ->
        let%bind () = B.work budget 2 in
        let%map equal = Record.input_equal (Record.root record) root ~budget in
        if equal then record :: selected else selected)
    in
    let%bind pairs, matched, originals =
      List.fold_result
        (Parsed.document_occurrences parsed)
        ~init:([], String.Set.empty, Original_keys.empty)
        ~f:(fun (pairs, matched, originals) occurrence ->
          let%bind found = find_occurrence selected ~root ~occurrence ~budget in
          match found with
          | None -> invalid "actual document occurrence differs from retained custody"
          | Some record ->
            let%bind () = B.work budget 2 in
            let%bind () = B.map_work budget (Set.length matched) in
            let output_bytes =
              match Record.kind record with
              | Dynamic -> 0
              | Captured { contents; _ } -> String.length contents
            in
            let%bind () =
              B.add
                budget
                (B.Accounting.create
                   ~occurrences:1
                   ~input_bytes:0
                   ~output_bytes
                   ~work_steps:0
                   ~nodes:0)
            in
            let pairs =
              match Record.kind record with
              | Dynamic -> pairs
              | Captured { contents; _ } -> (occurrence, contents) :: pairs
            in
            let%map originals =
              match Record.kind record with
              | Captured { provenance = Local _; _ } ->
                Original_keys.add originals ~occurrence ~record ~budget
              | Dynamic | Captured _ -> Ok originals
            in
            pairs, Set.add matched (Record.key record), originals)
    in
    let%bind count =
      B.count budget selected ~maximum:(B.Limits.occurrences (B.limits budget))
    in
    if count <> Set.length matched
    then invalid "surplus retained occurrence for selected Root"
    else (
      let%bind projection =
        Prompt.Chat_markdown.materialize_documents
          parsed
          ~rendered:(List.rev pairs)
          ~max_occurrences:(B.Limits.occurrences (B.limits budget))
          ~max_output_bytes:(B.Limits.output_bytes (B.limits budget))
          ~max_work_steps:
            (B.Limits.work_steps (B.limits budget)
             - B.Accounting.work_steps (B.accounting budget))
          ~checkpoint:(fun () -> B.checkpoint budget)
        |> Result.map_error ~f:(fun _ ->
          B.Error.invalid "bounded document projection failed")
      in
      let%map () = B.work budget (Projection.work_steps projection) in
      projection, originals))
;;

let preserves t ~previous ~budget =
  let open Result.Let_syntax in
  let%bind () =
    List.fold_result previous.roots ~init:() ~f:(fun () root ->
      let%bind present =
        List.fold_result t.roots ~init:false ~f:(fun present actual ->
          let%map equal = Record.input_equal root actual ~budget in
          present || equal)
      in
      if present then Ok () else invalid "previous content Root was dropped")
  in
  List.fold_result previous.records ~init:() ~f:(fun () record ->
    let%bind present =
      List.fold_result t.records ~init:false ~f:(fun present actual ->
        let%map equal = Record.equal record actual ~budget in
        present || equal)
    in
    if present then Ok () else invalid "previous content record was dropped or changed")
;;
