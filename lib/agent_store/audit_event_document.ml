open! Core
module D = Document_schema
module F = Document_fields
module P = Agent_protocol

type t = P.Audit.t D.Extension_carrier.t

let decode json =
  let open Result.Let_syntax in
  let%bind sequence = F.required json "sequence" F.decimal in
  let%bind () =
    if Int64.(sequence > 0L)
    then Ok ()
    else F.invalid "sequence" "audit sequence must be positive"
  in
  (* This is a projection of the admitted current named document, never a
     runtime reader for historical event bytes. Protocol owns domain validation. *)
  match json with
  | `Object fields ->
    P.Audit.of_json
      (`Object
          (List.map fields ~f:(fun (name, value) ->
             ( name
             , if String.equal name "sequence"
               then `Number (Int64.to_string sequence)
               else value ))))
    |> F.protocol
  | _ -> F.invalid "event" "expected current audit object"
;;

let encode (event : P.Audit.t) =
  match P.Audit.to_json event with
  | `Object fields ->
    Ok
      (`Object
          (List.map fields ~f:(fun (name, value) ->
             ( name
             , if String.equal name "sequence"
               then F.decimal_json event.sequence
               else value ))))
  | _ -> F.invalid "event" "expected audit object"
;;

let codec ~limits =
  D.Domain_codec.create
    ~limits
    ~kind:"store.audit_event"
    ~version:1
    ~shape:
      (F.shape
         (List.map
            [ "sequence"
            ; "timestamp"
            ; "level"
            ; "name"
            ; "session_id"
            ; "principal_id"
            ; "payload"
            ; "redacted"
            ]
            ~f:(fun name -> name, D.Shape.value)))
    ~supported_semantics:[]
    ~decode
    ~encode
;;

let value = D.Extension_carrier.value

let of_document document ~limits =
  let open Result.Let_syntax in
  let%bind document = F.upgrade document ~limits ~kind:"store.audit_event" in
  let%bind codec = codec ~limits in
  D.Domain_codec.decode codec document
;;

let to_document t ~limits =
  let open Result.Let_syntax in
  let%bind codec = codec ~limits in
  D.Domain_codec.encode codec t
;;

let create event ~limits =
  let open Result.Let_syntax in
  let%bind json = encode event in
  let%bind (_ : P.Audit.t) = decode json in
  let carrier = D.Extension_carrier.of_authored_value event in
  let%map (_ : D.Document.t) = to_document carrier ~limits in
  carrier
;;
