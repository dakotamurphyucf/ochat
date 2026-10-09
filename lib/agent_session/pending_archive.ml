open! Core
module P = Agent_protocol
module D = Document_schema
module X = Persistence_codec
module Store = Agent_store

let error message = P.Error.create Persistence_error ~message ~retryable:false ()
let document_error value = error (Sexp.to_string_hum (D.Error.sexp_of_t value))
let store_error value = error (Sexp.to_string_hum (Store.Store_error.sexp_of_t value))

module Reference = struct
  type t =
    { session_id : P.Id.Session.t
    ; generation : int
    ; operation_id : P.Id.Operation.t
    ; pending_revision : P.Pending_input.Revision.t
    ; sha256 : string
    }
  [@@deriving equal, sexp]

  let validate t =
    if
      t.generation < 0
      || (not (Int.equal (String.length t.sha256) 64))
      || not
           (String.for_all t.sha256 ~f:(function
              | '0' .. '9' | 'a' .. 'f' -> true
              | _ -> false))
    then Error (P.Error.invalid_request "invalid pending archive reference")
    else Ok ()
  ;;

  let operation_id t = t.operation_id

  let to_jsonaf t =
    `Object
      [ "session_id", P.Id.Session.to_json t.session_id
      ; "generation", X.integer_json t.generation
      ; "operation_id", P.Id.Operation.to_json t.operation_id
      ; "pending_revision", P.Pending_input.Revision.to_json t.pending_revision
      ; "sha256", `String t.sha256
      ]
  ;;

  let of_jsonaf json =
    let open Result.Let_syntax in
    let%bind fields = P.Json_codec.fields json in
    let%bind session_id = X.required fields "session_id" P.Id.Session.of_json in
    let%bind generation =
      X.required fields "generation" (P.Json_codec.bounded_int ~min:0 ~max:Int.max_value)
    in
    let%bind operation_id = X.required fields "operation_id" P.Id.Operation.of_json in
    let%bind pending_revision =
      X.required fields "pending_revision" P.Pending_input.Revision.of_json
    in
    let%bind sha256 = X.required fields "sha256" P.Json_codec.string in
    let value = { session_id; generation; operation_id; pending_revision; sha256 } in
    let%map () = validate value in
    value
  ;;

  let unchecked_of_sexp = t_of_sexp

  let t_of_sexp sexp =
    let value = unchecked_of_sexp sexp in
    match validate value with
    | Ok () -> value
    | Error error -> Sexplib.Conv.of_sexp_error error.message sexp
  ;;

  let shape =
    X.fields_shape
      [ "session_id"; "generation"; "operation_id"; "pending_revision"; "sha256" ]
  ;;
end

type t =
  { reference : Reference.t
  ; document : D.Document.t
  }

let reference t = t.reference
let document t = t.document

let create (state : Session_state.t) ~operation_id ~pending_revision ~records ~limits =
  let open Result.Let_syntax in
  let%bind () =
    if List.is_empty records
    then Error (P.Error.invalid_request "pending expiry archive requires records")
    else Ok ()
  in
  let%bind records =
    List.map records ~f:(fun record ->
      Pending_disposition_document.to_jsonaf record ~limits
      |> Result.map_error ~f:document_error)
    |> Result.all
  in
  let payload =
    `Object
      [ "session_id", P.Id.Session.to_json state.identity.session_id
      ; "generation", X.integer_json state.identity.generation
      ; "operation_id", P.Id.Operation.to_json operation_id
      ; "pending_revision", P.Pending_input.Revision.to_json pending_revision
      ; "records", `Array records
      ]
  in
  let%map document =
    D.Document.create
      ~limits
      ~kind:"session.pending_disposition_archive"
      ~version:1
      ~payload
    |> Result.map_error ~f:document_error
  in
  { document
  ; reference =
      { Reference.session_id = state.identity.session_id
      ; generation = state.identity.generation
      ; operation_id
      ; pending_revision
      ; sha256 = Store.Document_record.digest (D.Document.to_string document)
      }
  }
;;

let filename reference =
  "pending-dispositions-"
  ^ P.Id.Operation.to_string reference.Reference.operation_id
  ^ ".frame"
;;

let write t ~env ~handle ~limits =
  let open Result.Let_syntax in
  let%bind _ =
    Store.Session_store.Handle.metadata_checked handle |> Result.map_error ~f:store_error
  in
  let%bind () =
    if
      P.Id.Session.equal
        t.reference.session_id
        (Store.Session_store.Handle.session_id handle)
    then Ok ()
    else Error (error "pending archive belongs to another session")
  in
  let%bind () =
    D.Document.validate t.document ~limits |> Result.map_error ~f:document_error
  in
  let%bind contents =
    Store.Document_record.encode t.document ~limits ~flags:0
    |> Result.map_error ~f:(fun value ->
      error (Sexp.to_string_hum (Store.Document_record.Error.sexp_of_t value)))
  in
  Store.Durable_file.replace
    ~env
    ~durability:Flush_file_and_directory
    ~path:
      (Filename.concat
         (Store.Session_store.Handle.archive_directory handle)
         (filename t.reference))
    contents
  |> Result.map_error ~f:store_error
;;
