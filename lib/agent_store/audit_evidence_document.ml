open! Core
module D = Document_schema
module F = Document_fields

module Evidence = struct
  type t =
    { previous_hash : string option
    ; event_bytes : string
    ; record_hash : string
    }
end

type t =
  { evidence : Evidence.t D.Extension_carrier.t
  ; event : Audit_event_document.t
  }

let hash previous_hash event_bytes =
  Digestif.SHA256.(
    digest_string (Option.value previous_hash ~default:"" ^ "\000" ^ event_bytes)
    |> to_hex)
;;

let decode json =
  let open Result.Let_syntax in
  let%bind previous_hash =
    F.required json "previous_hash" (function
      | `Null -> Ok None
      | value -> Result.map (F.digest value) ~f:Option.some)
  in
  let%bind event_bytes = F.required json "event_document_bytes" F.string in
  let%map record_hash = F.required json "record_hash" F.digest in
  Evidence.{ previous_hash; event_bytes; record_hash }
;;

let codec ~limits =
  D.Domain_codec.create
    ~limits
    ~kind:"store.audit_evidence"
    ~version:1
    ~shape:
      (F.shape
         [ "previous_hash", D.Shape.value
         ; "event_document_bytes", D.Shape.value
         ; "record_hash", D.Shape.value
         ])
    ~supported_semantics:[]
    ~decode
    ~encode:(fun (evidence : Evidence.t) ->
      Ok
        (`Object
            [ ( "previous_hash"
              , F.option_json evidence.previous_hash ~f:(fun hash -> `String hash) )
            ; "event_document_bytes", `String evidence.event_bytes
            ; "record_hash", `String evidence.record_hash
            ]))
;;

let event t = t.event
let event_bytes t = (D.Extension_carrier.value t.evidence).event_bytes
let record_hash t = (D.Extension_carrier.value t.evidence).record_hash

let to_document t ~limits =
  let open Result.Let_syntax in
  let%bind codec = codec ~limits in
  D.Domain_codec.encode codec t.evidence
;;

let create event ~previous_hash ~limits =
  let open Result.Let_syntax in
  let%bind () =
    match previous_hash with
    | None -> Ok ()
    | Some hash -> Result.map (F.digest (`String hash)) ~f:(fun _ -> ())
  in
  let%bind document = Audit_event_document.to_document event ~limits in
  let event_bytes = D.Document.to_string document in
  let evidence =
    D.Extension_carrier.of_authored_value
      Evidence.
        { previous_hash; event_bytes; record_hash = hash previous_hash event_bytes }
  in
  let t = { evidence; event } in
  let%map (_ : D.Document.t) = to_document t ~limits in
  t
;;

let of_document original ~previous_hash ~next_sequence ~limits =
  let open Result.Let_syntax in
  let%bind () = D.Document.validate original ~limits |> F.store in
  let%bind () =
    F.expect_versions original ~kind:"store.audit_evidence" ~versions:[ 1 ] |> F.store
  in
  let%bind stored = decode (D.Document.payload original) |> F.store in
  let%bind () =
    if not (Option.equal String.equal stored.previous_hash previous_hash)
    then Error (Store_error.Corrupt "audit hash chain is discontinuous")
    else if
      not (String.equal stored.record_hash (hash stored.previous_hash stored.event_bytes))
    then
      Error (Store_error.Corrupt "audit record hash does not match original event bytes")
    else Ok ()
  in
  let%bind document =
    F.upgrade original ~limits ~kind:"store.audit_evidence" |> F.store
  in
  let%bind codec = codec ~limits |> F.store in
  let%bind evidence = D.Domain_codec.decode codec document |> F.store in
  let converted = D.Extension_carrier.value evidence in
  let%bind () =
    if
      Option.equal String.equal stored.previous_hash converted.previous_hash
      && String.equal stored.event_bytes converted.event_bytes
      && String.equal stored.record_hash converted.record_hash
    then Ok ()
    else Error (Store_error.Corrupt "audit conversion changed immutable evidence")
  in
  let%bind event_document = D.Document.decode ~limits stored.event_bytes |> F.store in
  let%bind event = Audit_event_document.of_document event_document ~limits |> F.store in
  if not (Int64.equal (Audit_event_document.value event).sequence next_sequence)
  then Error (Store_error.Corrupt "audit sequence is discontinuous")
  else Ok { evidence; event }
;;
