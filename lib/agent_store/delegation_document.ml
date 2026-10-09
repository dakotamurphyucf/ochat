open Core
module P = Agent_protocol
module D = Document_schema
module F = Document_fields
module R = Delegation_record
open R

type t = R.t D.Extension_carrier.t

let max_payload_length = 262144

let limits =
  F.limits ~max_bytes:max_payload_length
  |> Result.map_error ~f:(fun error -> Sexp.to_string_hum (D.Error.sexp_of_t error))
  |> Result.ok_or_failwith
;;

let text value = `String value
let nullable value ~f = F.option_json value ~f

let key_to_json (key : Key.t) =
  `Object
    [ "parent_session_id", text (P.Id.Session.to_string key.parent_session_id)
    ; "parent_generation", F.decimal_json (Int64.of_int key.parent_generation)
    ; "principal_id", text (P.Id.Principal.to_string key.principal_id)
    ; "idempotency_key", text (P.Idempotency_key.to_string key.idempotency_key)
    ]
;;

let lifetime_to_json = function
  | Admission.Owned -> `Object [ "kind", text "owned" ]
  | Invocation_owned { invocation_id } ->
    `Object
      [ "kind", text "invocation_owned"
      ; "invocation_id", text (P.Id.Invocation.to_string invocation_id)
      ]
  | Independent { authorization_sha256 } ->
    `Object
      [ "kind", text "independent"; "authorization_sha256", text authorization_sha256 ]
;;

let admission_to_json (a : Admission.t) =
  `Object
    [ "child_session_id", text (P.Id.Session.to_string a.child_session_id)
    ; "revision_id", text (P.Id.Prompt_revision.to_string a.revision_id)
    ; "transaction_id", text (P.Id.Transaction.to_string a.transaction_id)
    ; "manifest_sha256", text a.manifest_sha256
    ; "parent_revision_id", text (P.Id.Prompt_revision.to_string a.parent_revision_id)
    ; "parent_stop_epoch", nullable a.parent_stop_epoch ~f:F.decimal_json
    ; "authority_sha256", text a.authority_sha256
    ; ( "authored_tool"
      , nullable a.authored_tool ~f:(fun authored ->
          `Object
            [ "name", text authored.name; "source_sha256", text authored.source_sha256 ])
      )
    ; ( "capability_pins"
      , `Array
          (List.map a.capability_pins ~f:(fun (name, pin) ->
             `Object [ "name", text name; "pin", text pin ])) )
    ; "lifetime", lifetime_to_json a.lifetime
    ; "created_at", text (P.Timestamp.to_string a.created_at)
    ; "inference_target", nullable a.inference_target ~f:Inference.Request.Target.to_json
    ]
;;

let stage_to_string = function
  | Reserved -> "reserved"
  | Artifact_installed -> "artifact_installed"
  | Child_installed -> "child_installed"
  | Linked -> "linked"
;;

let revocation_to_string = function
  | Parent_stopped -> "parent_stopped"
  | Parent_deleted -> "parent_deleted"
  | Authority_changed -> "authority_changed"
  | Admission_failed -> "admission_failed"
;;

let record_to_json (record : R.t) =
  `Object
    [ "key", key_to_json record.key
    ; "request_sha256", text record.request_sha256
    ; "admission", admission_to_json record.admission
    ; "stage", text (stage_to_string record.stage)
    ; ( "revocation"
      , nullable record.revocation ~f:(fun value -> text (revocation_to_string value)) )
    ; ( "artifact_collection"
      , nullable record.artifact_collection ~f:(fun Prepared -> text "prepared") )
    ]
;;

let identifier decode json =
  Result.bind (F.string json) ~f:(fun value -> F.protocol (decode value))
;;

let generation json =
  Result.bind (F.decimal json) ~f:(fun value ->
    if Int64.(value > of_int Int.max_value)
    then F.invalid "parent_generation" "generation exceeds native range"
    else Ok (Int64.to_int_exn value))
;;

let key_of_json json =
  let open Result.Let_syntax in
  let%bind parent_session_id =
    F.required json "parent_session_id" (identifier P.Id.Session.of_string)
  in
  let%bind parent_generation = F.required json "parent_generation" generation in
  let%bind principal_id =
    F.required json "principal_id" (identifier P.Id.Principal.of_string)
  in
  let%map idempotency_key =
    F.required json "idempotency_key" (identifier P.Idempotency_key.of_string)
  in
  Key.{ parent_session_id; parent_generation; principal_id; idempotency_key }
;;

let lifetime_of_json json =
  let open Result.Let_syntax in
  match%bind F.required json "kind" F.string with
  | "owned" -> Ok Admission.Owned
  | "invocation_owned" ->
    let%map invocation_id =
      F.required json "invocation_id" (identifier P.Id.Invocation.of_string)
    in
    Admission.Invocation_owned { invocation_id }
  | "independent" ->
    let%map authorization_sha256 = F.required json "authorization_sha256" F.digest in
    Admission.Independent { authorization_sha256 }
  | _ -> F.invalid "lifetime" "unknown lifetime"
;;

let admission_of_json ~limits json =
  let open Result.Let_syntax in
  let%bind child_session_id =
    F.required json "child_session_id" (identifier P.Id.Session.of_string)
  in
  let%bind revision_id =
    F.required json "revision_id" (identifier P.Id.Prompt_revision.of_string)
  in
  let%bind transaction_id =
    F.required json "transaction_id" (identifier P.Id.Transaction.of_string)
  in
  let%bind manifest_sha256 = F.required json "manifest_sha256" F.digest in
  let%bind parent_revision_id =
    F.required json "parent_revision_id" (identifier P.Id.Prompt_revision.of_string)
  in
  let%bind parent_stop_epoch = F.optional json "parent_stop_epoch" F.decimal in
  let%bind authority_sha256 = F.required json "authority_sha256" F.digest in
  let%bind authored_tool =
    F.optional json "authored_tool" (fun json ->
      let%bind name = F.required json "name" F.string in
      let%map source_sha256 = F.required json "source_sha256" F.digest in
      Admission.{ name; source_sha256 })
  in
  let%bind pins = F.required json "capability_pins" F.array in
  let%bind capability_pins =
    List.map pins ~f:(fun json ->
      let%bind name = F.required json "name" F.string in
      let%map pin = F.required json "pin" F.digest in
      name, pin)
    |> Result.all
  in
  let%bind lifetime = F.required json "lifetime" lifetime_of_json in
  let%bind created_at = F.required json "created_at" (identifier P.Timestamp.of_string) in
  let%map inference_target =
    F.required json "inference_target" (fun json ->
      Inference.Request.Target.of_json json ~limits
      |> Result.map ~f:Option.some
      |> Result.map_error ~f:(fun error ->
        D.Error.Invalid_field
          { path = [ "inference_target" ]
          ; reason = Sexp.to_string_hum (Inference.Request.Error.sexp_of_t error)
          }))
  in
  Admission.
    { child_session_id
    ; revision_id
    ; transaction_id
    ; manifest_sha256
    ; parent_revision_id
    ; parent_stop_epoch
    ; authority_sha256
    ; authored_tool
    ; capability_pins
    ; lifetime
    ; created_at
    ; inference_target
    }
;;

let record_of_json ~limits json =
  let open Result.Let_syntax in
  let%bind key = F.required json "key" key_of_json in
  let%bind request_sha256 = F.required json "request_sha256" F.digest in
  let%bind admission = F.required json "admission" (admission_of_json ~limits) in
  let%bind stage =
    F.required json "stage" (fun json ->
      match%bind F.string json with
      | "reserved" -> Ok Reserved
      | "artifact_installed" -> Ok Artifact_installed
      | "child_installed" -> Ok Child_installed
      | "linked" -> Ok Linked
      | _ -> F.invalid "stage" "unknown stage")
  in
  let%bind revocation =
    F.optional json "revocation" (fun json ->
      match%bind F.string json with
      | "parent_stopped" -> Ok Parent_stopped
      | "parent_deleted" -> Ok Parent_deleted
      | "authority_changed" -> Ok Authority_changed
      | "admission_failed" -> Ok Admission_failed
      | _ -> F.invalid "revocation" "unknown revocation")
  in
  let%bind artifact_collection =
    F.optional json "artifact_collection" (fun json ->
      match%bind F.string json with
      | "prepared" -> Ok Prepared
      | _ -> F.invalid "artifact_collection" "unknown collection state")
  in
  let record =
    { key
    ; request_sha256
    ; admission
    ; stage
    ; revocation
    ; artifact_collection
    ; preservation = None
    }
  in
  let%map () =
    R.validate record ~limits
    |> Result.map_error ~f:(fun error ->
      D.Error.Invalid_field
        { path = []; reason = Sexp.to_string_hum (Store_error.sexp_of_t error) })
  in
  record
;;

let record_shape =
  let scalar = D.Shape.value in
  let key =
    F.shape
      [ "parent_session_id", scalar
      ; "parent_generation", scalar
      ; "principal_id", scalar
      ; "idempotency_key", scalar
      ]
  in
  let authored = F.shape [ "name", scalar; "source_sha256", scalar ] in
  let pins =
    D.Shape.array
      (F.shape [ "name", scalar; "pin", scalar ])
      ~identity_field:(Some "name")
    |> Result.map_error ~f:(fun error -> Sexp.to_string_hum (D.Error.sexp_of_t error))
    |> Result.ok_or_failwith
  in
  let lifetime =
    D.Shape.tagged_object
      ~discriminator:"kind"
      [ "owned", F.shape [ "kind", scalar ]
      ; "invocation_owned", F.shape [ "kind", scalar; "invocation_id", scalar ]
      ; "independent", F.shape [ "kind", scalar; "authorization_sha256", scalar ]
      ]
    |> Result.map_error ~f:(fun error -> Sexp.to_string_hum (D.Error.sexp_of_t error))
    |> Result.ok_or_failwith
  in
  let admission =
    F.shape
      [ "child_session_id", scalar
      ; "revision_id", scalar
      ; "transaction_id", scalar
      ; "manifest_sha256", scalar
      ; "parent_revision_id", scalar
      ; "parent_stop_epoch", scalar
      ; "authority_sha256", scalar
      ; "authored_tool", D.Shape.nullable authored
      ; "capability_pins", pins
      ; "lifetime", lifetime
      ; "created_at", scalar
      ; "inference_target", scalar
      ]
  in
  F.shape
    [ "key", key
    ; "request_sha256", scalar
    ; "admission", admission
    ; "stage", scalar
    ; "revocation", scalar
    ; "artifact_collection", scalar
    ]
;;

let codec () =
  D.Domain_codec.create
    ~limits
    ~kind:"delegation.intent"
    ~version:6
    ~shape:record_shape
    ~supported_semantics:[]
    ~decode:(record_of_json ~limits)
    ~encode:(fun record -> Ok (record_to_json record))
;;

let value = D.Extension_carrier.value

let of_document document =
  let open Result.Let_syntax in
  let%bind codec = codec () in
  D.Domain_codec.decode codec document
;;

let to_document t =
  let open Result.Let_syntax in
  let%bind codec = codec () in
  let%bind encoded = D.Domain_codec.encode codec t in
  match D.Extension_carrier.template t with
  | None -> Ok encoded
  | Some original ->
    (* Immutable admission evidence includes presence and lexical JSON, not just
       the decoded record. Disposition publication must never reauthor it. *)
    let%bind key = F.required (D.Document.payload original) "key" (fun json -> Ok json) in
    let%bind admission =
      F.required (D.Document.payload original) "admission" (fun json -> Ok json)
    in
    let%bind payload =
      match D.Document.payload encoded with
      | `Object fields ->
        Ok
          (`Object
              (List.map fields ~f:(fun (name, json) ->
                 ( name
                 , if String.equal name "key"
                   then key
                   else if String.equal name "admission"
                   then admission
                   else json ))))
      | _ -> F.invalid "payload" "expected delegation object"
    in
    let%bind document =
      match D.Document.json encoded with
      | `Object fields ->
        D.Document.inspect
          ~limits
          (`Object
              (List.map fields ~f:(fun (name, json) ->
                 name, if String.equal name "payload" then payload else json)))
      | _ -> F.invalid "document" "expected document object"
    in
    let%map (_ : t) = D.Domain_codec.decode codec document in
    document
;;

let validate record =
  R.validate record ~limits
  |> Result.map_error ~f:(fun error ->
    D.Error.Invalid_field
      { path = []; reason = Sexp.to_string_hum (Store_error.sexp_of_t error) })
;;

let create record =
  let open Result.Let_syntax in
  let%bind () = validate record in
  if Option.is_some record.R.preservation
  then F.invalid "preservation" "use of_record for an existing delegation document"
  else Ok (D.Extension_carrier.of_authored_value record)
;;

let to_record t =
  { (value t) with preservation = Some (D.Extension_carrier.with_value t ()) }
;;

let of_record record =
  let open Result.Let_syntax in
  let%bind () = validate record in
  match record.R.preservation with
  | None -> create record
  | Some previous ->
    let%bind () =
      match D.Extension_carrier.template previous with
      | None -> Ok ()
      | Some original ->
        let%bind previous = of_document original in
        let old = value previous in
        if
          Key.equal old.key record.key
          && String.equal old.request_sha256 record.request_sha256
          && Admission.equal old.admission record.admission
        then Ok ()
        else F.invalid "admission" "immutable delegation admission changed"
    in
    Ok (D.Extension_carrier.with_value previous record)
;;

let stored_key document =
  let open Result.Let_syntax in
  let%bind () =
    if String.equal (D.Document.kind document) "delegation.intent"
    then Ok ()
    else F.invalid "kind" "not a delegation intent"
  in
  F.required (D.Document.payload document) "key" key_of_json
;;

let admission_sha256 t =
  let original =
    match
      Option.bind (D.Extension_carrier.template t) ~f:(fun document ->
        match D.Json.field (D.Document.payload document) ~name:"admission" with
        | Value value -> Some value
        | Null | Absent -> None)
    with
    | Some value -> value
    | None -> admission_to_json (value t).admission
  in
  Digestif.SHA256.(digest_string (Jsonaf.to_string original) |> to_hex)
;;

let with_disposition t ~stage ~revocation ~artifact_collection =
  let record = { (to_record t) with stage; revocation; artifact_collection } in
  of_record record
;;
