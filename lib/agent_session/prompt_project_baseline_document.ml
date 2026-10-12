open Core
module Record = Prompt_project_baseline_record
module Error = Record.Error
module Limits = Record.Limits
module Work = Record.Work
module Schema = Document_schema
module Carrier = Schema.Extension_carrier
module E = Schema.Encoding
module Role = Record.Role
module Receipt = Agent_store.Prompt_source_receipt
module Id = Agent_protocol.Id

type projection =
  { generation : int
  ; entries : Record.Entry.t list
  }

type t = projection Carrier.t

let map_result values ~f =
  Result.map
    (List.fold_result values ~init:[] ~f:(fun reversed value ->
       Result.map (f value) ~f:(fun value -> value :: reversed)))
    ~f:List.rev
;;

let generation t = (Carrier.value t).generation
let entries t = (Carrier.value t).entries
let kind = "ochat.prompt-project-baselines"
let invalid field reason = Error (Schema.Error.Invalid_field { path = [ field ]; reason })

let required json name f =
  match Schema.Json.field json ~name with
  | Value value -> f value
  | Absent | Null -> invalid name "Required non-null field"
;;

let string = function
  | `String value -> Ok value
  | _ -> invalid "value" "String required"
;;

let array = function
  | `Array values -> Ok values
  | _ -> invalid "value" "Array required"
;;

let natural = function
  | `Number text ->
    (match Int.of_string_opt text with
     | Some number when number >= 0 && String.equal text (Int.to_string number) ->
       Ok number
     | None | Some _ -> invalid "generation" "Canonical nonnegative integer required")
  | _ -> invalid "generation" "Canonical nonnegative integer required"
;;

let domain =
  Result.map_error ~f:(function
    | Error.Limit_exceeded -> Schema.Error.Limit_exceeded "project baseline allowance"
    | Document error -> error
    | Invalid_association reason -> Schema.Error.Invalid_field { path = []; reason }
    | Ambiguous_association ->
      Schema.Error.Invalid_field
        { path = []; reason = "Ambiguous stable script-role association" }
    | Foreign_artifact
    | Foreign_operator_intent
    | Missing_program_bundle
    | Custody _
    | Retained _ ->
      Schema.Error.Invalid_field
        { path = []; reason = "Invalid project baseline association" })
;;

let hex_decode work limits json =
  let open Result.Let_syntax in
  let%bind text = string json in
  Work.poll work;
  if
    String.length text mod 2 <> 0
    || String.length text / 2 > Limits.max_identity_bytes limits
  then Error (Schema.Error.Limit_exceeded "identity bytes")
  else (
    let nibble = function
      | '0' .. '9' as c -> Some (Char.to_int c - Char.to_int '0')
      | 'a' .. 'f' as c -> Some (10 + Char.to_int c - Char.to_int 'a')
      | _ -> None
    in
    let valid = ref true in
    String.iteri text ~f:(fun index c ->
      if index mod 4096 = 0 then Work.poll work;
      if Option.is_none (nibble c) then valid := false);
    if not !valid
    then invalid "identity" "Lowercase hexadecimal bytes required"
    else (
      let bytes = Bytes.create (String.length text / 2) in
      for index = 0 to Bytes.length bytes - 1 do
        if index mod 2048 = 0 then Work.poll work;
        let left = Option.value_exn (nibble text.[2 * index]) in
        let right = Option.value_exn (nibble text.[(2 * index) + 1]) in
        Bytes.set bytes index (Char.of_int_exn ((left * 16) + right))
      done;
      Ok (Bytes.unsafe_to_string ~no_mutation_while_string_reachable:bytes)))
;;

let portable json =
  let open Result.Let_syntax in
  let%bind text = string json in
  if String.length text > 1024
  then invalid "path" "Portable path too long"
  else
    Source.Portable_path.create text
    |> Result.map_error ~f:(fun _ ->
      Schema.Error.Invalid_field { path = [ "path" ]; reason = "Portable path required" })
;;

let target = function
  | "one_off_v1" -> Ok Chatml.Chatml_compiler_contract.Target.One_off_v1
  | "tool_v1" -> Ok Tool_v1
  | "moderator_v1" -> Ok Moderator_v1
  | "delegated_moderator_v1" -> Ok Delegated_moderator_v1
  | _ -> invalid "target" "Unsupported standard role target"
;;

let script_kind = function
  | "moderator" -> Ok Chatmd_shell_spec.Chatmd_script_spec.Moderator
  | "shell_matcher" -> Ok Shell_matcher
  | "shell_reviewer" -> Ok Shell_reviewer
  | "shell_before_interceptor" -> Ok Shell_before_interceptor
  | "shell_after_interceptor" -> Ok Shell_after_interceptor
  | "shell_effect_analyzer" -> Ok Shell_effect_analyzer
  | "shell_audit_filter" -> Ok Shell_audit_filter
  | _ -> invalid "script_kind" "Unsupported native role kind"
;;

let decode_role work limits json =
  let open Result.Let_syntax in
  Work.poll work;
  let%bind tag = required json "kind" string in
  match tag with
  | "standard" ->
    let%map target =
      required json "target" (fun value ->
        let%bind text = string value in
        target text)
    in
    Role.Standard target
  | "legacy_moderator" ->
    let%map invocation =
      required json "invocation" (fun value ->
        let%bind value = string value in
        match value with
        | "ui" -> Ok Role.Moderator_invocation.Ui
        | "headless" -> Ok Headless
        | _ -> invalid "invocation" "Unsupported moderator invocation")
    in
    Role.Legacy_moderator invocation
  | "native_hook" ->
    let%bind kind =
      required json "script_kind" (fun value ->
        let%bind text = string value in
        script_kind text)
    in
    let%map entrypoint = required json "entrypoint_hex" (hex_decode work limits) in
    Role.Native_hook { kind; entrypoint }
  | _ -> invalid "kind" "Unsupported role kind"
;;

let decode_reference work limits json =
  let open Result.Let_syntax in
  Work.poll work;
  let%bind slot =
    required json "slot" (fun json ->
      let%bind registration_path = required json "registration_path" portable in
      let%bind declaration_path = required json "declaration_path" portable in
      let%bind declaration_role =
        required json "declaration_role" (fun value ->
          let%bind text = string value in
          match text with
          | "root" -> Ok Chatmd_parser_input.Role.Root
          | "import" -> Ok Import
          | _ -> invalid "declaration_role" "Parser role required")
      in
      let%bind script_id = required json "script_id_hex" (hex_decode work limits) in
      Record.Slot_assertion.create
        ~registration_path
        ~declaration_path
        ~declaration_role
        ~script_id
        ~limits
        ~checkpoint:(fun () -> Work.poll work)
      |> domain)
  in
  let%bind role = required json "role" (decode_role work limits) in
  let%bind program_key = required json "program_key" string in
  let%bind association_key = required json "association_key" string in
  let%bind reference =
    Record.Reference.create ~slot ~role ~program_key ~limits ~checkpoint:(fun () ->
      Work.poll work)
    |> domain
  in
  if Work.string_equal work association_key (Record.Reference.association_key reference)
  then Ok reference
  else invalid "association_key" "Stable association key mismatch"
;;

let decode_entry work limits json =
  let open Result.Let_syntax in
  Work.poll work;
  let%bind definition_id =
    required json "definition_id" (fun value ->
      Id.Prompt_definition.of_json value
      |> Result.map_error ~f:(fun _ ->
        Schema.Error.Invalid_field
          { path = [ "definition_id" ]; reason = "Definition ID required" }))
  in
  let%bind revision_id =
    required json "revision_id" (fun value ->
      Id.Prompt_revision.of_json value
      |> Result.map_error ~f:(fun _ ->
        Schema.Error.Invalid_field
          { path = [ "revision_id" ]; reason = "Revision ID required" }))
  in
  let%bind source_receipt = required json "source_receipt" Receipt.of_json in
  let%bind manifest_sha256 = required json "manifest_sha256" string in
  let%bind references = required json "references" array in
  let%bind references = map_result references ~f:(decode_reference work limits) in
  Record.Entry.of_fields
    ~definition_id
    ~source_receipt
    ~revision_id
    ~manifest_sha256
    ~references
    ~limits
    ~checkpoint:(fun () -> Work.poll work)
  |> domain
;;

let count work values maximum =
  List.fold_result values ~init:0 ~f:(fun seen _ ->
    Work.poll work;
    if seen >= maximum
    then Error (Schema.Error.Limit_exceeded "project baseline count")
    else Ok (seen + 1))
;;

let preflight_json work limits payload =
  let open Result.Let_syntax in
  let%bind rows = required payload "entries" array in
  let%bind _ = count work rows (Limits.max_definitions limits) in
  List.fold_result rows ~init:0 ~f:(fun total row ->
    let%bind references = required row "references" array in
    let%map added = count work references (Limits.max_references limits - total) in
    total + added)
;;

let validate_projection work limits value =
  let open Result.Let_syntax in
  let%bind _ = count work value.entries (Limits.max_definitions limits) in
  let%bind _ =
    List.fold_result value.entries ~init:0 ~f:(fun total entry ->
      let%map added =
        count work (Record.Entry.references entry) (Limits.max_references limits - total)
      in
      total + added)
  in
  let%bind _ =
    List.fold_result value.entries ~init:String.Set.empty ~f:(fun seen entry ->
      Work.poll work;
      let id = Id.Prompt_definition.to_string (Record.Entry.definition_id entry) in
      if Set.mem seen id
      then invalid "definition_id" "Duplicate definition baseline"
      else Ok (Set.add seen id))
  in
  if value.generation < 0
  then invalid "generation" "Nonnegative generation required"
  else
    List.fold_result value.entries ~init:() ~f:(fun () entry ->
      List.fold_result (Record.Entry.references entry) ~init:() ~f:(fun () reference ->
        Work.poll work;
        let script = Record.Slot_assertion.script_id (Record.Reference.slot reference) in
        let entrypoint =
          match Record.Reference.role reference with
          | Role.Native_hook { entrypoint; _ } -> String.length entrypoint
          | Standard _ | Legacy_moderator _ -> 0
        in
        if
          String.length script > Limits.max_identity_bytes limits
          || entrypoint > Limits.max_identity_bytes limits
        then Error (Schema.Error.Limit_exceeded "identity bytes")
        else Ok ()))
;;

let decode_projection work limits payload =
  let open Result.Let_syntax in
  let%bind _ = preflight_json work limits payload in
  let%bind generation = required payload "generation" natural in
  let%bind entries = required payload "entries" array in
  let%bind entries = map_result entries ~f:(decode_entry work limits) in
  let projection = { generation; entries } in
  let%map () = validate_projection work limits projection in
  projection
;;

let encode_role encoding role =
  match role with
  | Role.Standard target ->
    E.object_
      encoding
      [ ("kind", fun e -> E.string e "standard")
      ; ( "target"
        , fun e ->
            E.string
              e
              (match target with
               | One_off_v1 -> "one_off_v1"
               | Tool_v1 -> "tool_v1"
               | Moderator_v1 -> "moderator_v1"
               | Delegated_moderator_v1 -> "delegated_moderator_v1") )
      ]
  | Legacy_moderator invocation ->
    E.object_
      encoding
      [ ("kind", fun e -> E.string e "legacy_moderator")
      ; ( "invocation"
        , fun e ->
            E.string
              e
              (match invocation with
               | Ui -> "ui"
               | Headless -> "headless") )
      ]
  | Native_hook { kind; entrypoint } ->
    E.object_
      encoding
      [ ("kind", fun e -> E.string e "native_hook")
      ; ( "script_kind"
        , fun e -> E.string e (Chatmd_shell_spec.Chatmd_script_spec.kind_to_string kind)
        )
      ; ("entrypoint_hex", fun e -> E.hex e entrypoint)
      ]
;;

let encode_reference encoding reference =
  let slot = Record.Reference.slot reference in
  E.object_
    encoding
    [ ("association_key", fun e -> E.string e (Record.Reference.association_key reference))
    ; ( "slot"
      , fun e ->
          E.object_
            e
            [ ( "registration_path"
              , fun e ->
                  E.string
                    e
                    (Source.Portable_path.to_string
                       (Record.Slot_assertion.registration_path slot)) )
            ; ( "declaration_role"
              , fun e ->
                  E.string
                    e
                    (match Record.Slot_assertion.declaration_role slot with
                     | Root -> "root"
                     | Import -> "import") )
            ; ( "declaration_path"
              , fun e ->
                  E.string
                    e
                    (Source.Portable_path.to_string
                       (Record.Slot_assertion.declaration_path slot)) )
            ; ("script_id_hex", fun e -> E.hex e (Record.Slot_assertion.script_id slot))
            ] )
    ; ("role", fun e -> encode_role e (Record.Reference.role reference))
    ; ("program_key", fun e -> E.string e (Record.Reference.program_key reference))
    ]
;;

let encode_entry encoding entry =
  E.object_
    encoding
    [ ( "definition_id"
      , fun e ->
          E.string e (Id.Prompt_definition.to_string (Record.Entry.definition_id entry))
      )
    ; ( "source_receipt"
      , fun e -> E.raw e (Receipt.to_json (Record.Entry.source_receipt entry)) )
    ; ( "revision_id"
      , fun e ->
          E.string e (Id.Prompt_revision.to_string (Record.Entry.revision_id entry)) )
    ; ("manifest_sha256", fun e -> E.string e (Record.Entry.manifest_sha256 entry))
    ; ( "references"
      , fun e -> E.array e (Record.Entry.references entry) ~f:encode_reference )
    ]
;;

let encode_projection work limits value =
  let open Result.Let_syntax in
  let%bind envelope =
    E.render
      ~limits:(Limits.document limits)
      ~byte_limit_context:"project baseline bytes"
      ~checkpoint:(fun () -> Work.poll work)
      ~f:(fun e ->
        E.object_
          e
          [ ("format", fun e -> E.string e Schema.Document.format)
          ; ("schema_version", fun e -> E.number e 1)
          ; ("kind", fun e -> E.string e kind)
          ; ( "payload"
            , fun e ->
                E.object_
                  e
                  [ ("generation", fun e -> E.number e value.generation)
                  ; ("entries", fun e -> E.array e value.entries ~f:encode_entry)
                  ] )
          ])
  in
  required envelope "payload" (fun payload -> Ok payload)
;;

let shape =
  let open Result.Let_syntax in
  let value = Schema.Shape.value in
  let%bind slot =
    Schema.Shape.object_
      [ "registration_path", value
      ; "declaration_role", value
      ; "declaration_path", value
      ; "script_id_hex", value
      ]
  in
  let%bind standard = Schema.Shape.object_ [ "kind", value; "target", value ] in
  let%bind legacy = Schema.Shape.object_ [ "kind", value; "invocation", value ] in
  let%bind native =
    Schema.Shape.object_ [ "kind", value; "script_kind", value; "entrypoint_hex", value ]
  in
  let%bind role =
    Schema.Shape.tagged_object
      ~discriminator:"kind"
      [ "standard", standard; "legacy_moderator", legacy; "native_hook", native ]
  in
  let%bind reference =
    Schema.Shape.object_
      [ "association_key", value; "slot", slot; "role", role; "program_key", value ]
  in
  let%bind references =
    Schema.Shape.array reference ~identity_field:(Some "association_key")
  in
  let%bind entry =
    Schema.Shape.object_
      [ "definition_id", value
      ; "source_receipt", Receipt.shape
      ; "revision_id", value
      ; "manifest_sha256", value
      ; "references", references
      ]
  in
  let%bind entries = Schema.Shape.array entry ~identity_field:(Some "definition_id") in
  Schema.Shape.object_ [ "generation", value; "entries", entries ]
;;

let codec work limits =
  let open Result.Let_syntax in
  let%bind shape = shape in
  Schema.Domain_codec.create_validated
    ~limits:(Limits.document limits)
    ~kind
    ~version:1
    ~shape
    ~supported_semantics:[]
    ~validate:(validate_projection work limits)
    ~decode:(decode_projection work limits)
    ~encode:(encode_projection work limits)
;;

(* Reserve actual input visits before the existing bounded named-document
   transformation; this includes unknown fields, not merely known projections. *)
let scan_json work json =
  let scan_string bytes =
    for offset = 0 to String.length bytes / 4096 do
      ignore offset;
      Work.poll work
    done
  in
  let rec scan = function
    | `String bytes ->
      Work.poll work;
      scan_string bytes
    | `Object fields ->
      Work.poll work;
      List.iter fields ~f:(fun (key, child) ->
        scan_string key;
        scan child)
    | `Array values ->
      Work.poll work;
      List.iter values ~f:scan
    | `Number bytes ->
      Work.poll work;
      scan_string bytes
    | `Null | `True | `False -> Work.poll work
  in
  scan json
;;

let schema = Result.map_error ~f:(fun error -> Error.Document error)

let empty ~limits =
  Work.run limits ~checkpoint:ignore ~f:(fun work ->
    let open Result.Let_syntax in
    let%bind codec = codec work limits |> schema in
    let value = Carrier.of_authored_value { generation = 0; entries = [] } in
    let%map _ = Schema.Domain_codec.encode codec value |> schema in
    value)
;;

let decode ~limits ~checkpoint bytes =
  Work.run limits ~checkpoint ~f:(fun work ->
    let open Result.Let_syntax in
    if String.length bytes > Schema.Limits.max_bytes (Limits.document limits)
    then Error (Error.Document (Schema.Error.Limit_exceeded "bytes"))
    else (
      for offset = 0 to String.length bytes / 4096 do
        ignore offset;
        Work.poll work
      done;
      let%bind document =
        Schema.Document.decode ~limits:(Limits.document limits) bytes |> schema
      in
      scan_json work (Schema.Document.json document);
      let%bind codec = codec work limits |> schema in
      Schema.Domain_codec.decode codec document |> schema))
;;

let validate_template t limits =
  match Carrier.template t with
  | None -> Ok ()
  | Some document -> Schema.Document.validate document ~limits:(Limits.document limits)
;;

let encode t ~limits ~checkpoint =
  Work.run limits ~checkpoint ~f:(fun work ->
    let open Result.Let_syntax in
    let%bind () = validate_projection work limits (Carrier.value t) |> schema in
    let%bind () = validate_template t limits |> schema in
    let%bind codec = codec work limits |> schema in
    Option.iter (Carrier.template t) ~f:(fun document ->
      scan_json work (Schema.Document.json document));
    let%bind document = Schema.Domain_codec.encode codec t |> schema in
    scan_json work (Schema.Document.json document);
    Work.poll work;
    Ok (Schema.Document.to_string document))
;;

let replace_successes t ~entries:successes ~limits ~checkpoint =
  Work.run limits ~checkpoint ~f:(fun work ->
    let open Result.Let_syntax in
    let%bind () = validate_projection work limits (Carrier.value t) |> schema in
    let%bind () = validate_template t limits |> schema in
    let%bind _ = count work successes (Limits.max_definitions limits) |> schema in
    if generation t = Int.max_value
    then Error Error.Limit_exceeded
    else (
      let%bind incoming =
        List.fold_result successes ~init:String.Map.empty ~f:(fun map success ->
          Work.poll work;
          let entry = Record.Success.entry success in
          let id = Id.Prompt_definition.to_string (Record.Entry.definition_id entry) in
          if Map.mem map id
          then Error Error.Ambiguous_association
          else Ok (Map.set map ~key:id ~data:entry))
      in
      let%bind merged =
        List.fold_result (entries t) ~init:incoming ~f:(fun map entry ->
          Work.poll work;
          let id = Id.Prompt_definition.to_string (Record.Entry.definition_id entry) in
          if Map.mem map id
          then Ok map
          else if Map.length map >= Limits.max_definitions limits
          then Error Error.Limit_exceeded
          else Ok (Map.set map ~key:id ~data:entry))
      in
      let%bind _ =
        Map.fold merged ~init:(Ok 0) ~f:(fun ~key:_ ~data:entry total ->
          let%bind total = total in
          let%map added =
            count
              work
              (Record.Entry.references entry)
              (Limits.max_references limits - total)
            |> schema
          in
          total + added)
      in
      let value = { generation = generation t + 1; entries = Map.data merged } in
      let%bind () = validate_projection work limits value |> schema in
      let next = Carrier.with_value t value in
      let%bind codec = codec work limits |> schema in
      Option.iter (Carrier.template t) ~f:(fun document ->
        scan_json work (Schema.Document.json document));
      let%map _ = Schema.Domain_codec.encode codec next |> schema in
      next))
;;
