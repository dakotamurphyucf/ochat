open! Core

let payload state = Sexp.to_string_mach (Session_state.sexp_of_t state)
let digest text = Digestif.SHA256.(digest_string text |> to_hex)

let reference_for state ~kind operation_id =
  Session_state.Compaction_archive.
    { operation_id
    ; revision = state.Session_state.counters.revision
    ; sha256 = digest (payload state)
    ; kind
    }
;;

let reference state operation_id = reference_for state ~kind:Compaction operation_id

let prefix = function
  | Session_state.Compaction_archive.Compaction -> "compaction"
  | Reset -> "reset"
  | Rebuild -> "rebuild"
  | Upgrade -> "upgrade"
;;

let path handle reference =
  Filename.concat
    (Agent_store.Session_store.Handle.archive_directory handle)
    (prefix reference.Session_state.Compaction_archive.kind
     ^ "-"
     ^ Agent_protocol.Id.Operation.to_string
         reference.Session_state.Compaction_archive.operation_id
     ^ ".frame")
;;

let error message =
  Agent_protocol.Error.create Persistence_error ~message ~retryable:false ()
;;

let store_error value =
  error (Sexp.to_string_hum (Agent_store.Store_error.sexp_of_t value))
;;

let write ~env ~handle ~max_payload_length reference state =
  let open Result.Let_syntax in
  let text = payload state in
  let%bind () =
    if String.equal reference.Session_state.Compaction_archive.sha256 (digest text)
    then Ok ()
    else Error (error "compaction archive digest disagrees with previous state")
  in
  let%bind contents =
    Agent_store.Frame.encode ~max_payload_length ~flags:0 text
    |> Result.map_error ~f:(fun value ->
      error (Sexp.to_string_hum (Agent_store.Frame.sexp_of_error value)))
  in
  Agent_store.Durable_file.replace
    ~env
    ~durability:Flush_file_and_directory
    ~path:(path handle reference)
    contents
  |> Result.map_error ~f:store_error
;;

let decode_state handle reference text =
  let open Result.Let_syntax in
  try
    let state = Session_state.t_of_sexp (Sexp.of_string text) in
    let%bind () = Session_state.validate state in
    if
      Int64.equal
        state.counters.revision
        reference.Session_state.Compaction_archive.revision
      && Agent_protocol.Id.Session.compare
           state.identity.session_id
           (Agent_store.Session_store.Handle.session_id handle)
         = 0
    then Ok state
    else Error (error "compaction archive identity mismatch")
  with
  | _ -> Error (error "invalid compaction archive state")
;;

let read ~env ~handle ~max_payload_length reference =
  let open Result.Let_syntax in
  let%bind contents =
    Agent_store.Durable_file.load ~env ~path:(path handle reference)
    |> Result.map_error ~f:store_error
  in
  let%bind decoded =
    Agent_store.Frame.decode ~max_payload_length ~contents ~offset:0
    |> Result.map_error ~f:(fun _ -> error "invalid compaction archive frame")
  in
  match decoded with
  | Complete { frame; next_offset } when next_offset = String.length contents ->
    let text = Agent_store.Frame.payload frame in
    if not (String.equal (digest text) reference.sha256)
    then Error (error "compaction archive checksum mismatch")
    else decode_state handle reference text
  | _ -> Error (error "incomplete compaction archive")
;;
