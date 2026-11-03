open! Core
include Agent_store_test_fixtures
module P = Agent_protocol
module D = Document_schema
module R = Agent_store.Delegation_record
module C = Agent_store.Delegation_document
module O = Agent_store.Delegation_captured_origin
module L = Agent_store.Delegation_store
module S = Agent_store.Session_store

let digest text = Digestif.SHA256.(digest_string text |> to_hex)
let request_sha256 = digest "same selected creation request"
let max_records = 16
let max_bytes = 1048576

let key name =
  L.Key.
    { parent_session_id = session_id
    ; parent_generation = 3
    ; principal_id = P.Id.Principal.of_string "pri_captured_parent" |> protocol_ok
    ; idempotency_key = P.Idempotency_key.of_string name |> protocol_ok
    }
;;

let admission name =
  L.Admission.
    { child_session_id = P.Id.Session.of_string ("ses_child_" ^ name) |> protocol_ok
    ; revision_id = P.Id.Prompt_revision.of_string ("prv_child_" ^ name) |> protocol_ok
    ; transaction_id = P.Id.Transaction.of_string ("txn_child_" ^ name) |> protocol_ok
    ; manifest_sha256 = digest "captured source bytes"
    ; parent_revision_id = P.Id.Prompt_revision.of_string "prv_parent" |> protocol_ok
    ; parent_stop_epoch = None
    ; authority_sha256 = digest "captured authority selection"
    ; authored_tool = None
    ; captured_recurrence = None
    ; capability_pins = []
    ; lifetime = Owned
    ; created_at = timestamp
    ; inference_target = Some (delegation_inference_target ())
    }
;;

let authored : L.Admission.authored_tool =
  { name = "worker"; source_sha256 = digest "authored tool" }
;;

let record name admission =
  R.
    { key = key name
    ; request_sha256
    ; admission
    ; stage = Reserved
    ; revocation = None
    ; artifact_collection = None
    ; preservation = None
    }
;;

let field json name =
  match D.Json.field json ~name with
  | Value value -> value
  | Null -> `Null
  | Absent -> failwith ("missing fixture field " ^ name)
;;

let set json name value =
  match json with
  | `Object fields ->
    `Object
      (List.map fields ~f:(fun (key, old) ->
         key, if String.equal key name then value else old))
  | _ -> failwith "expected fixture object"
;;

let append json name value =
  match json with
  | `Object fields -> `Object (fields @ [ name, value ])
  | _ -> failwith "expected fixture object"
;;

let remove json names =
  match json with
  | `Object fields ->
    `Object
      (List.filter fields ~f:(fun (name, _) ->
         not (List.mem names name ~equal:String.equal)))
  | _ -> failwith "expected fixture object"
;;

let inspect json = D.Document.inspect ~limits:C.limits json |> document_ok
let document record = C.of_record record |> document_ok |> C.to_document |> document_ok
let decode json = inspect json |> C.of_document |> document_ok

let change_payload document ~f =
  set (D.Document.json document) "payload" (f (D.Document.payload document)) |> inspect
;;

let origin () =
  let origin =
    O.create
      ~reservation:(`Object [ "id", `String "reserved"; "future_row", `Number "1e+00" ])
      ~definition:
        (`Object [ "name", `String "complete definition"; "future_definition", `Null ])
      ~initial_spec_sha256:(digest "initial spec without delegation locator")
    |> document_ok
  in
  let raw =
    O.to_json origin
    |> fun json -> append json "future_origin" (`Array [ `Number "2e+00"; `Null ])
  in
  D.Document.inspect ~limits:O.limits raw |> document_ok |> O.of_document |> document_ok
;;

let frame document =
  Agent_store.Document_record.encode document ~limits:C.limits ~flags:0
  |> Result.map_error ~f:Agent_store.Document_fields.record_error
  |> store_ok
;;

let file env root key =
  let name = digest (L.Key.sexp_of_t key |> Sexp.to_string_mach) ^ ".frame" in
  Eio.Path.(Eio.Stdenv.fs env / Filename.concat (Filename.concat root "delegations") name)
;;

let reserve ?(bytes = max_bytes) ledger key admission =
  L.reserve ledger ~key ~request_sha256 ~admission ~max_records ~max_bytes:bytes
;;

let row = function
  | L.New row | Replay row -> row
  | Conflict _ -> failwith "unexpected origin conflict"
;;

let create env sw root =
  S.create
    ~env
    ~sw
    ~root
    ~server_id
    ~process_start_identity:None
    ~lock_nonce:"captured-delegation"
  |> store_ok
;;

let reopen env sw root =
  S.open_existing
    ~env
    ~sw
    ~root
    ~process_start_identity:None
    ~lock_nonce:"captured-reopen"
  |> store_ok
;;

let with_store name f =
  with_temp_directory name (fun env root ->
    Eio.Switch.run (fun sw ->
      let store = create env sw root in
      Exn.protect
        ~f:(fun () -> f env sw root store)
        ~finally:(fun () -> S.close store |> store_ok)))
;;

let artifact admission =
  Agent_store.Prompt_artifact_store.Artifact.create
    ~revision_id:admission.L.Admission.revision_id
    ~root_chatmd:"<developer>captured child</developer>"
    ~sources:[]
    ~parser_schema_version:1
    ~runtime_schema_version:1
    ~created_at:timestamp
    ()
  |> store_ok
;;

let install env root admission artifact =
  let artifacts =
    Agent_store.Prompt_artifact_store.create
      ~env
      ~root:(Filename.concat root "prompt-artifacts")
    |> store_ok
  in
  Agent_store.Prompt_artifact_store.install
    artifacts
    ~transaction_id:admission.L.Admission.transaction_id
    artifact
  |> store_ok
;;

let collect ledger bytes =
  L.with_artifact_retention
    ledger
    ~max_records
    ~max_bytes:bytes
    ~max_artifact_entries:128
    ~max_artifact_bytes:max_bytes
    ~f:(fun _ -> Ok ())
  |> store_ok
;;

(* Independent bounded enumeration of the actual complete encoded carriers for
   every known remaining phase/revocation/collection choice. Production selects
   one maximal known disposition per phase; the oracle enumerates all choices. *)
let future_documents carrier =
  let current = C.value carrier in
  let stages =
    [ L.Reserved; Artifact_installed; Child_installed; Linked ]
    |> List.drop_while ~f:(fun stage -> not (L.equal_stage stage current.stage))
    |> fun stages ->
    if Option.is_some current.revocation then [ current.stage ] else stages
  in
  let reasons =
    match current.revocation with
    | Some _ as reason -> [ reason ]
    | None ->
      [ None
      ; Some L.Parent_stopped
      ; Some Parent_deleted
      ; Some Authority_changed
      ; Some Admission_failed
      ]
  in
  List.concat_map stages ~f:(fun stage ->
    List.concat_map reasons ~f:(fun revocation ->
      let collections =
        match current.artifact_collection, stage, revocation with
        | Some Prepared, _, _ -> [ Some L.Prepared ]
        | None, (Reserved | Artifact_installed), Some _ -> [ None; Some L.Prepared ]
        | None, _, _ -> [ None ]
      in
      List.map collections ~f:(fun artifact_collection ->
        C.with_disposition carrier ~stage ~revocation ~artifact_collection
        |> document_ok
        |> C.to_document
        |> document_ok)))
;;

let future_frame_bytes carrier =
  future_documents carrier
  |> List.map ~f:(fun document -> String.length (frame document))
  |> List.max_elt ~compare:Int.compare
  |> Option.value_exn
;;
