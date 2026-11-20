open! Core
include Catalog_state

let empty ~owner =
  let open Result.Let_syntax in
  let%map owner = P.Session_ref.of_json (P.Session_ref.to_json owner) in
  D.Extension_carrier.of_authored_value
    { owner; revision = 0L; notes = []; receipts = [] }
;;

let owner t = (D.Extension_carrier.value t).owner
let revision t = (D.Extension_carrier.value t).revision

let find_note state (selection : selection) =
  match
    List.find state.notes ~f:(fun note -> Note_id.equal note.selection.id selection.id)
  with
  | None -> Error Note_not_found
  | Some note ->
    if Note_revision.equal note.selection.revision selection.revision
    then Ok note
    else Error (Stale_note_revision note.selection.revision)
;;

let read t selection =
  Result.map
    (find_note (D.Extension_carrier.value t) selection)
    ~f:(fun note -> note.content)
;;

let list t =
  let state = D.Extension_carrier.value t in
  List.sort state.notes ~compare:(fun first second ->
    Note_path.compare first.path second.path)
  |> List.map ~f:(fun { selection; path; content; changed_at = _ } ->
    { selection; path; byte_length = String.length content })
;;

let to_document = Catalog_document.encode

let of_document document ~owner:expected_owner =
  let open Result.Let_syntax in
  let%bind catalog = Catalog_document.decode document in
  if P.Session_ref.equal (owner catalog) expected_owner
  then Ok catalog
  else
    Error
      (D.Error.Invalid_field { path = [ "owner" ]; reason = "wrong recovery note owner" })
;;

(* The V1 receipt fingerprint uses a fixed domain tag and NUL-delimited fields.
   Every variable field is already validated to exclude NUL. Counters use
   canonical decimal text and content is hashed byte for byte. *)
let request_digest mutation =
  let fields =
    let edit_fields kind { expected = { id; revision }; content } =
      [ kind; Int64.to_string id; Int64.to_string revision; content ]
    in
    match mutation with
    | Create { expected_catalog_revision; path; content } ->
      [ "create"; Int64.to_string expected_catalog_revision; path; content ]
    | Replace request -> edit_fields "replace" request
    | Append request -> edit_fields "append" request
    | Delete { id; revision } ->
      [ "delete"; Int64.to_string id; Int64.to_string revision ]
  in
  String.concat ~sep:"\000" ("ochat.recovery-note-mutation.v1" :: fields)
  |> Digestif.SHA256.digest_string
  |> Digestif.SHA256.to_hex
;;

let validate_identity identity =
  let check result =
    Result.map_error (Result.map result ~f:ignore) ~f:(fun _ -> Invalid_identity)
  in
  let open Result.Let_syntax in
  match identity with
  | Mutation_identity.Client { principal; key } ->
    let%bind () = check (P.Id.Principal.of_json (P.Id.Principal.to_json principal)) in
    check (P.Idempotency_key.of_json (P.Idempotency_key.to_json key))
  | Native invocation ->
    check (P.Id.Invocation.of_json (P.Id.Invocation.to_json invocation))
;;

let replace_note state ~(old : note) ~(updated : note) =
  if
    text_bytes state.notes - String.length old.content + String.length updated.content
    > max_catalog_bytes
  then Error Catalog_byte_limit
  else
    Ok
      (List.map state.notes ~f:(fun note ->
         if Note_id.equal note.selection.id old.selection.id then updated else note))
;;

let change state mutation =
  let open Result.Let_syntax in
  let advance_catalog () =
    if Int64.equal state.revision Int64.max_value
    then Error Revision_exhausted
    else Ok (Int64.succ state.revision)
  in
  let existing selection =
    Result.map_error (find_note state selection) ~f:(fun error -> Note_precondition error)
  in
  let edit kind { expected; content } =
    let%bind old = existing expected in
    let%bind next = advance_catalog () in
    let%bind content =
      match kind with
      | `Replace -> Ok content
      | `Append ->
        let suffix_bytes = String.length content in
        if suffix_bytes = 0
        then Error Empty_append
        else if suffix_bytes > max_note_bytes - String.length old.content
        then Error Note_byte_limit
        else Ok (old.content ^ content)
    in
    (* A note revision is at most its already-admitted catalog revision. *)
    let selection = { expected with revision = Int64.succ expected.revision } in
    let%map notes =
      replace_note state ~old ~updated:{ old with selection; content; changed_at = next }
    in
    let kind =
      match kind with
      | `Replace -> Receipt.Replaced
      | `Append -> Receipt.Appended
    in
    notes, Receipt.{ kind; selection; catalog_revision = next }
  in
  match mutation with
  | Create { expected_catalog_revision; path; content } ->
    if not (Catalog_revision.equal state.revision expected_catalog_revision)
    then Error (Stale_catalog_revision state.revision)
    else if List.exists state.notes ~f:(fun note -> Note_path.equal note.path path)
    then Error Path_already_exists
    else if List.length state.notes = max_notes
    then Error Catalog_entry_limit
    else if text_bytes state.notes + String.length content > max_catalog_bytes
    then Error Catalog_byte_limit
    else (
      let%map next = advance_catalog () in
      let selection = { id = next; revision = 1L } in
      let note = { selection; path; content; changed_at = next } in
      note :: state.notes, Receipt.{ kind = Created; selection; catalog_revision = next })
  | Replace request -> edit `Replace request
  | Append request -> edit `Append request
  | Delete expected ->
    let%bind _ = existing expected in
    let%map next = advance_catalog () in
    let selection = { expected with revision = Int64.succ expected.revision } in
    let notes =
      List.filter state.notes ~f:(fun note ->
        not (Note_id.equal note.selection.id expected.id))
    in
    notes, Receipt.{ kind = Deleted; selection; catalog_revision = next }
;;

let apply t ~identity mutation =
  let open Result.Let_syntax in
  let%bind () = validate_identity identity in
  let state = D.Extension_carrier.value t in
  let digest = request_digest mutation in
  match
    List.find state.receipts ~f:(fun record ->
      Mutation_identity.equal record.identity identity)
  with
  | Some record ->
    if String.equal record.request_digest digest
    then Ok (Replayed record.receipt)
    else Error Idempotency_conflict
  | None ->
    let%bind notes, receipt = change state mutation in
    let receipts =
      List.take
        ({ identity; request_digest = digest; receipt } :: state.receipts)
        max_receipts
    in
    let catalog =
      D.Extension_carrier.with_value
        t
        { state with revision = receipt.catalog_revision; notes; receipts }
    in
    let%map _ =
      Result.map_error (to_document catalog) ~f:(fun error -> Document_error error)
    in
    Applied (catalog, receipt)
;;
