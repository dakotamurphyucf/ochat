open! Core
open Catalog_state
module J = P.Json_codec

let counter_to_json value = `String (Int64.to_string value)

let counter_of_json validate json =
  let open Result.Let_syntax in
  let%bind text = J.string json in
  match Int64.of_string_opt text with
  | Some value when String.equal (Int64.to_string value) text -> validate value
  | Some _ | None -> Error (invalid "expected a canonical decimal counter")
;;

let selection_to_json { id; revision } =
  `Object [ "id", counter_to_json id; "revision", counter_to_json revision ]
;;

let selection_of_json json =
  let open Result.Let_syntax in
  let%bind fields = J.fields json in
  let%bind id = J.required_as fields "id" (counter_of_json Note_id.of_int64) in
  let%map revision =
    J.required_as fields "revision" (counter_of_json Note_revision.of_int64)
  in
  { id; revision }
;;

let identity_to_json = function
  | Mutation_identity.Client { principal; key } ->
    `Object
      [ "kind", `String "client"
      ; "principal", P.Id.Principal.to_json principal
      ; "key", P.Idempotency_key.to_json key
      ]
  | Native invocation ->
    `Object [ "kind", `String "native"; "invocation", P.Id.Invocation.to_json invocation ]
;;

let identity_of_json json =
  let open Result.Let_syntax in
  let%bind fields = J.fields json in
  match%bind J.required_as fields "kind" J.string with
  | "client" ->
    let%bind principal = J.required_as fields "principal" P.Id.Principal.of_json in
    let%map key = J.required_as fields "key" P.Idempotency_key.of_json in
    Mutation_identity.Client { principal; key }
  | "native" ->
    let%map invocation = J.required_as fields "invocation" P.Id.Invocation.of_json in
    Mutation_identity.Native invocation
  | _ -> Error (invalid "unknown recovery note mutation identity kind")
;;

let receipt_kind_name = function
  | Receipt.Created -> "create"
  | Replaced -> "replace"
  | Appended -> "append"
  | Deleted -> "delete"
;;

let note_to_json ({ selection = { id; revision }; path; content; changed_at } : note) =
  `Object
    [ "id", counter_to_json id
    ; "revision", counter_to_json revision
    ; "path", `String (Note_path.to_string path)
    ; "content", `String content
    ; "changed_at", counter_to_json changed_at
    ]
;;

let note_of_json json =
  let open Result.Let_syntax in
  let%bind fields = J.fields json in
  let string_as validate json = Result.bind (J.string json) ~f:validate in
  let%bind selection = selection_of_json json in
  let%bind path = J.required_as fields "path" (string_as Note_path.of_string) in
  let%bind content = J.required_as fields "content" (string_as Content.of_string) in
  let%map changed_at =
    J.required_as fields "changed_at" (counter_of_json Catalog_revision.of_int64)
  in
  { selection; path; content; changed_at }
;;

let receipt_to_json { identity; request_digest; receipt } =
  `Object
    [ "catalog_revision", counter_to_json receipt.catalog_revision
    ; "identity", identity_to_json identity
    ; "request_digest", `String request_digest
    ; "kind", `String (receipt_kind_name receipt.kind)
    ; "selection", selection_to_json receipt.selection
    ]
;;

let receipt_of_json json =
  let open Result.Let_syntax in
  let%bind fields = J.fields json in
  let%bind kind =
    J.required_as
      fields
      "kind"
      (J.enum
         ~name:"note mutation"
         [ "create", Receipt.Created
         ; "replace", Replaced
         ; "append", Appended
         ; "delete", Deleted
         ])
  in
  let%bind identity = J.required_as fields "identity" identity_of_json in
  let%bind request_digest =
    J.required_as fields "request_digest" (fun json ->
      let%bind digest = J.string json in
      if
        String.length digest = 64
        && String.for_all digest ~f:(function
          | '0' .. '9' | 'a' .. 'f' -> true
          | _ -> false)
      then Ok digest
      else Error (invalid "invalid recovery note request digest"))
  in
  let%bind catalog_revision =
    J.required_as fields "catalog_revision" (counter_of_json Catalog_revision.of_int64)
  in
  let%map selection = J.required_as fields "selection" selection_of_json in
  { identity; request_digest; receipt = { kind; selection; catalog_revision } }
;;

let bounded_list max_length decode = function
  | `Array values when List.length values <= max_length ->
    Result.all (List.map values ~f:decode)
  | `Array _ -> Error (invalid "recovery note document has too many records")
  | _ -> Error (invalid "expected a recovery note document array")
;;

let state_to_json state =
  `Object
    [ "owner", P.Session_ref.to_json state.owner
    ; "revision", counter_to_json state.revision
    ; "notes", `Array (List.map state.notes ~f:note_to_json)
    ; "receipts", `Array (List.map state.receipts ~f:receipt_to_json)
    ]
;;

let state_of_json json =
  let open Result.Let_syntax in
  let%bind fields = J.fields json in
  let%bind owner = J.required_as fields "owner" P.Session_ref.of_json in
  let%bind revision =
    J.required_as fields "revision" (counter_of_json Catalog_revision.of_int64)
  in
  let%bind notes = J.required_as fields "notes" (bounded_list max_notes note_of_json) in
  let%map receipts =
    J.required_as fields "receipts" (bounded_list max_receipts receipt_of_json)
  in
  { owner; revision; notes; receipts }
;;

let valid_version_at { id; revision } changed_at =
  Int64.(id <= changed_at && revision <= succ (changed_at - id))
  && Bool.equal (Note_revision.equal revision 1L) (Int64.equal id changed_at)
;;

let validate_notes state =
  let open Result.Let_syntax in
  if List.length state.notes > max_notes || text_bytes state.notes > max_catalog_bytes
  then Error (invalid "recovery note catalog exceeds live content limits")
  else (
    let%map _, _, by_id =
      List.fold_result
        state.notes
        ~init:(String.Set.empty, Int64.Set.empty, Int64.Map.empty)
        ~f:(fun (paths, revisions, by_id) (note : note) ->
          if
            Int64.(note.changed_at > state.revision)
            || (not (valid_version_at note.selection note.changed_at))
            || Set.mem revisions note.changed_at
            || Set.mem paths (Note_path.to_string note.path)
          then Error (invalid "invalid or duplicate recovery note head")
          else (
            match Map.add by_id ~key:note.selection.id ~data:note with
            | `Duplicate -> Error (invalid "duplicate recovery note ID")
            | `Ok by_id ->
              Ok
                ( Set.add paths (Note_path.to_string note.path)
                , Set.add revisions note.changed_at
                , by_id )))
    in
    by_id)
;;

let validate_receipts state notes =
  let open Result.Let_syntax in
  let retained = List.length state.receipts in
  let expected_retained = Int64.min state.revision (Int64.of_int max_receipts) in
  if not (Int64.equal (Int64.of_int retained) expected_retained)
  then Error (invalid "invalid recovery note receipt window size")
  else (
    let before_window = Int64.(state.revision - expected_retained) in
    let%bind _, _, latest =
      List.fold_result
        (List.rev state.receipts)
        ~init:(before_window, Set.empty (module Mutation_identity), Int64.Map.empty)
        ~f:
          (fun
            (previous_revision, seen, latest) { identity; request_digest = _; receipt } ->
          let { Receipt.kind; selection; catalog_revision } = receipt in
          let valid_previous =
            match Map.find latest selection.id with
            | None ->
              Receipt.equal_kind kind Created || Int64.(selection.id <= before_window)
            | Some (previous : Receipt.t) ->
              (* A different commit always has a larger catalog revision, so a
                 valid predecessor is strictly below Int64.max_value. *)
              (not
                 (Receipt.equal_kind previous.kind Deleted
                  || Receipt.equal_kind kind Created))
              && Note_revision.equal
                   selection.revision
                   Int64.(succ previous.selection.revision)
          in
          if
            (not (Int64.equal catalog_revision (Int64.succ previous_revision)))
            || (not (valid_version_at selection catalog_revision))
            || (not
                  (Bool.equal
                     (Receipt.equal_kind kind Created)
                     (Int64.equal selection.revision 1L)))
            || (not valid_previous)
            || Set.mem seen identity
          then Error (invalid "invalid recovery note receipt history")
          else
            Ok
              ( catalog_revision
              , Set.add seen identity
              , Map.set latest ~key:selection.id ~data:receipt ))
    in
    let live_receipt_matches (note : note) (receipt : Receipt.t) =
      equal_selection note.selection receipt.selection
      && Catalog_revision.equal note.changed_at receipt.catalog_revision
      && not (Receipt.equal_kind receipt.kind Deleted)
    in
    let head_matches receipt =
      match Map.find notes receipt.Receipt.selection.id with
      | None -> Receipt.equal_kind receipt.kind Deleted
      | Some note -> live_receipt_matches note receipt
    in
    let recent_head_accounted_for (note : note) =
      Int64.(note.changed_at <= before_window)
      || Option.value_map
           (Map.find latest note.selection.id)
           ~default:false
           ~f:(live_receipt_matches note)
    in
    if
      Map.for_all latest ~f:head_matches && Map.for_all notes ~f:recent_head_accounted_for
    then Ok ()
    else Error (invalid "recovery note heads disagree with retained receipts"))
;;

let validate_state state =
  let open Result.Let_syntax in
  let%bind notes = validate_notes state in
  validate_receipts state notes
;;

let document_result result =
  Result.map_error result ~f:(fun (error : P.Error.t) ->
    D.Error.Invalid_field { path = []; reason = error.message })
;;

let codec =
  let schema_exn = function
    | Ok value -> value
    | Error error -> failwith (Sexp.to_string_hum (D.Error.sexp_of_t error))
  in
  let v = D.Shape.value in
  let obj fields = schema_exn (D.Shape.object_ fields) in
  let fields names = obj (List.map names ~f:(fun name -> name, v)) in
  let identity =
    schema_exn
      (D.Shape.tagged_object
         ~discriminator:"kind"
         [ "client", fields [ "kind"; "principal"; "key" ]
         ; "native", fields [ "kind"; "invocation" ]
         ])
  in
  let array identity_field element =
    schema_exn (D.Shape.array element ~identity_field:(Some identity_field))
  in
  let shape =
    obj
      [ "owner", fields [ "server_id"; "session_id" ]
      ; "revision", v
      ; "notes", array "id" (fields [ "id"; "revision"; "path"; "content"; "changed_at" ])
      ; ( "receipts"
        , array
            "catalog_revision"
            (obj
               [ "catalog_revision", v
               ; "identity", identity
               ; "request_digest", v
               ; "kind", v
               ; "selection", fields [ "id"; "revision" ]
               ]) )
      ]
  in
  let limits =
    schema_exn
      (D.Limits.create
         ~max_bytes:(16 * 1024 * 1024)
         ~max_depth:64
         ~max_fields:16384
         ~max_nodes:32768)
  in
  schema_exn
    (D.Domain_codec.create_validated
       ~limits
       ~kind:"ochat.recovery-note-catalog"
       ~version:1
       ~shape
       ~supported_semantics:[]
       ~validate:(fun state -> document_result (validate_state state))
       ~decode:(fun json ->
         document_result
           (let open Result.Let_syntax in
            let%bind state = state_of_json json in
            let%map () = validate_state state in
            state))
       ~encode:(fun state -> Ok (state_to_json state)))
;;

let encode t = D.Domain_codec.encode codec t
let decode document = D.Domain_codec.decode codec document
