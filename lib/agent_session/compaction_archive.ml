open! Core
module D = Document_schema
module Store = Agent_store

let digest = Store.Document_record.digest

let error message =
  Agent_protocol.Error.create Persistence_error ~message ~retryable:false ()
;;

let document_error value = error (Sexp.to_string_hum (D.Error.sexp_of_t value))
let store_error value = error (Sexp.to_string_hum (Store.Store_error.sexp_of_t value))

let archive_document state ~limits =
  let open Result.Let_syntax in
  (* History capture excludes private recovery grants, notes and intent keys. *)
  let value = Session_state_document.value state in
  let state =
    match value.recovery_notes, value.recovery_note_shares, value.recovery_rollover with
    | None, None, None -> state
    | _ ->
      Session_state_document.with_value
        state
        { value with
          recovery_notes = None
        ; recovery_note_shares = None
        ; recovery_rollover = None
        }
  in
  let%bind state = Session_state_document.encode state ~limits in
  (* Admit the encoded child without replacing its captured JSON representation. *)
  let%bind _ = Session_state_document.decode ~limits state in
  D.Document.create
    ~limits
    ~kind:"session.compaction_archive"
    ~version:1
    ~payload:(`Object [ "state", D.Document.json state ])
;;

let reference_for state ~limits ~kind operation_id =
  let open Result.Let_syntax in
  let%map document =
    archive_document state ~limits |> Result.map_error ~f:document_error
  in
  let value = Session_state_document.value state in
  Session_state.Compaction_archive.
    { operation_id
    ; revision = value.counters.revision
    ; sha256 = digest (D.Document.to_string document)
    ; kind
    ; invocation_dispositions = []
    }
;;

let reference state ~limits operation_id =
  reference_for state ~limits ~kind:Compaction operation_id
;;

module Name = struct
  module Operation = Agent_protocol.Id.Operation
  module Archive = Session_state.Compaction_archive

  type t =
    { kind : Archive.kind
    ; operation_id : Operation.t
    }

  let prefix = function
    | Archive.Compaction -> "compaction-"
    | Reset -> "reset-"
    | Rebuild -> "rebuild-"
    | Upgrade -> "upgrade-"
    | Edit -> "edit-"
    | Delete -> "delete-"
    | Pending_input -> "pending-input-"
    | Rollover -> "rollover-"
  ;;

  let suffix = ".frame"

  let to_string { kind; operation_id } =
    prefix kind ^ Operation.to_string operation_id ^ suffix
  ;;

  let of_string name =
    let open Result.Let_syntax in
    let%bind kind, operation =
      List.find_map Archive.all_of_kind ~f:(fun kind ->
        String.chop_prefix name ~prefix:(prefix kind)
        |> Option.map ~f:(fun operation -> kind, operation))
      |> Result.of_option ~error:(error "unknown archive filename")
    in
    let%bind operation =
      String.chop_suffix operation ~suffix
      |> Result.of_option ~error:(error "invalid archive filename")
    in
    let%map operation_id = Operation.of_string operation in
    { kind; operation_id }
  ;;
end

let filename (reference : Session_state.Compaction_archive.t) =
  Name.to_string { kind = reference.kind; operation_id = reference.operation_id }
;;

let path handle reference =
  Filename.concat
    (Agent_store.Session_store.Handle.archive_directory handle)
    (filename reference)
;;

let decode_document ~limits document =
  let open Result.Let_syntax in
  let%bind document =
    Persistence_codec.upgrade document ~limits ~kind:"session.compaction_archive"
    |> Result.map_error ~f:document_error
  in
  let%bind () =
    Persistence_codec.validate_document
      document
      ~limits
      ~kind:"session.compaction_archive"
    |> Result.map_error ~f:document_error
  in
  let%bind fields = Agent_protocol.Json_codec.fields (D.Document.payload document) in
  let%bind json = Agent_protocol.Json_codec.required fields "state" in
  let%bind child =
    D.Document.inspect ~limits json |> Result.map_error ~f:document_error
  in
  Session_state_document.decode ~limits child |> Result.map_error ~f:document_error
;;

let decode_state handle (reference : Session_state.Compaction_archive.t) state =
  let open Result.Let_syntax in
  let state = Session_state_document.value state in
  let seen = Hash_set.create (module Agent_protocol.Id.Invocation) in
  let%bind () =
    List.fold_result reference.invocation_dispositions ~init:() ~f:(fun () disposition ->
      if Hash_set.mem seen disposition.invocation_id
      then Error (error "duplicate archived invocation disposition")
      else (
        Hash_set.add seen disposition.invocation_id;
        let%bind original =
          List.find state.invocations ~f:(fun inv ->
            Agent_protocol.Id.Invocation.compare inv.context.id disposition.invocation_id
            = 0)
          |> Result.of_option
               ~error:(error "archive disposition references an unknown invocation")
        in
        let%bind resolved =
          match disposition.interruption_reason with
          | None -> Ok original
          | Some reason -> Agent_protocol.Invocation.cancel original ~reason
        in
        match disposition.output_entry_id, disposition.publication_discarded with
        | Some id, None ->
          Agent_protocol.Invocation.publish_with_history resolved ~output_entry_id:id
          |> Result.map ~f:ignore
        | None, Some reason ->
          Agent_protocol.Invocation.discard_publication resolved ~reason
          |> Result.map ~f:ignore
        | None, None when Option.is_some disposition.interruption_reason -> Ok ()
        | _ -> Error (error "invalid archived invocation disposition")))
  in
  if
    Int64.equal state.counters.revision reference.revision
    && Agent_protocol.Id.Session.equal
         state.identity.session_id
         (Store.Session_store.Handle.session_id handle)
  then Ok state
  else Error (error "compaction archive identity mismatch")
;;

let max_frame_bytes max_payload_length =
  if max_payload_length > Int.max_value - 52
  then Int.max_value
  else max_payload_length + 52
;;

let write_document ~env ~handle ~max_payload_length reference document =
  let open Result.Let_syntax in
  let%bind limits =
    Persistence_codec.limits ~max_bytes:max_payload_length
    |> Result.map_error ~f:document_error
  in
  let%bind () =
    Persistence_codec.validate_document
      document
      ~limits
      ~kind:"session.compaction_archive"
    |> Result.map_error ~f:document_error
  in
  let%bind contents =
    Store.Document_record.encode document ~limits ~flags:0
    |> Result.map_error ~f:(fun e ->
      error (Sexp.to_string_hum (Store.Document_record.Error.sexp_of_t e)))
  in
  let%bind () =
    if
      String.equal
        reference.Session_state.Compaction_archive.sha256
        (digest (D.Document.to_string document))
    then Ok ()
    else Error (error "archive digest disagrees with captured state")
  in
  let%bind state = decode_document ~limits document in
  let%bind _ = decode_state handle reference state in
  let path = path handle reference in
  let%bind () =
    Store.Durable_file.replace ~env ~durability:Flush_file_and_directory ~path contents
    |> Result.map_error ~f:store_error
  in
  match reference.kind with
  | Rollover ->
    let%bind readback =
      Store.Durable_file.load_bounded
        ~env
        ~path
        ~max_bytes:(max_frame_bytes max_payload_length)
      |> Result.map_error ~f:store_error
    in
    if String.equal contents readback
    then Ok ()
    else Error (error "synced rollover frame differs from the verified archive")
  | Compaction | Reset | Rebuild | Upgrade | Edit | Delete | Pending_input -> Ok ()
;;

let write ~env ~handle ~max_payload_length reference state =
  let open Result.Let_syntax in
  let%bind limits =
    Persistence_codec.limits ~max_bytes:max_payload_length
    |> Result.map_error ~f:document_error
  in
  let%bind document =
    archive_document state ~limits |> Result.map_error ~f:document_error
  in
  write_document ~env ~handle ~max_payload_length reference document
;;

let decode_record ~max_payload_length ~expected_digest contents =
  let open Result.Let_syntax in
  let%bind limits =
    Persistence_codec.limits ~max_bytes:max_payload_length
    |> Result.map_error ~f:document_error
  in
  let%bind record =
    Store.Document_record.decode_file ~limits ~expected_digest contents
    |> Result.map_error ~f:(fun e ->
      error (Sexp.to_string_hum (Store.Document_record.Error.sexp_of_t e)))
  in
  let%bind () =
    if Store.Document_record.flags record = 0
    then Ok ()
    else Error (error "unknown archive frame flags")
  in
  let%map state = decode_document ~limits (Store.Document_record.document record) in
  record, state
;;

let decode_file
      ~handle
      ~max_payload_length
      (reference : Session_state.Compaction_archive.t)
      contents
  =
  let open Result.Let_syntax in
  let%bind _, state =
    decode_record ~max_payload_length ~expected_digest:(Some reference.sha256) contents
  in
  decode_state handle reference state
;;

let read ~env ~handle ~max_payload_length reference =
  let open Result.Let_syntax in
  let%bind contents =
    Store.Durable_file.load_bounded
      ~env
      ~path:(path handle reference)
      ~max_bytes:(max_frame_bytes max_payload_length)
    |> Result.map_error ~f:store_error
  in
  decode_file ~handle ~max_payload_length reference contents
;;
