open! Core
module D = Document_schema
module F = Document_fields

let max_bytes = 16 * 1024 * 1024
let max_encoded_bytes = 32 * 1024 * 1024

let limits =
  match F.limits ~max_bytes with
  | Ok limits -> limits
  | Error error -> raise_s [%sexp "invalid outcome limits", (error : D.Error.t)]
;;

module Reference = struct
  type t =
    { digest : string
    ; encoded_bytes : int
    }
  [@@deriving equal, compare, sexp_of]

  let digest t = t.digest
  let encoded_bytes t = t.encoded_bytes

  let to_jsonaf t =
    `Object
      [ "tag", `String "terminal"
      ; "digest", `String t.digest
      ; "encoded_bytes", `String (Int.to_string t.encoded_bytes)
      ]
  ;;

  let of_jsonaf json =
    let open Result.Let_syntax in
    let%bind () = D.Json.validate ~limits json in
    let%bind tag = F.required json "tag" F.string in
    let%bind () =
      if String.equal tag "terminal"
      then Ok ()
      else F.invalid "tag" "expected terminal outcome reference"
    in
    let%bind digest = F.required json "digest" F.digest in
    let%bind bytes = F.required json "encoded_bytes" F.decimal in
    let%map () =
      if Int64.(bytes > zero && bytes <= of_int max_encoded_bytes)
      then Ok ()
      else F.invalid "encoded_bytes" "outside outcome document byte bound"
    in
    { digest; encoded_bytes = Int64.to_int_exn bytes }
  ;;
end

type value =
  | Success of Jsonaf.t
  | Failure of Agent_protocol.Error.t

type t =
  { document : D.Document.t
  ; value : value
  ; reference : Reference.t
  ; encoded : string
  }

let decode_value json =
  let open Result.Let_syntax in
  let%bind tag = F.required json "tag" F.string in
  match tag with
  | "success" ->
    let%map value = F.required json "value" Result.return in
    Success value
  | "failure" ->
    let%map error =
      F.required json "error" (fun value ->
        Agent_protocol.Error.of_json value |> F.protocol)
    in
    Failure error
  | _ -> F.invalid "tag" "expected a terminal success or failure"
;;

let composite_limits =
  match
    D.Limits.create
      ~max_bytes:max_encoded_bytes
      ~max_depth:256
      ~max_fields:2_000_000
      ~max_nodes:4_000_000
  with
  | Ok limits -> limits
  | Error error -> raise_s [%sexp "invalid composite outcome limits", (error : D.Error.t)]
;;

let pending_component raw =
  let open Result.Let_syntax in
  let%bind tag = F.required raw "tag" F.string in
  let%bind () =
    if String.equal tag "pending"
    then Ok ()
    else F.invalid "pending_custody" "expected original Pending outcome"
  in
  D.Document.create ~limits ~kind:"store.idempotency_outcome" ~version:1 ~payload:raw
;;

let of_document document =
  let open Result.Let_syntax in
  let%bind () = F.expect document ~kind:"store.idempotency_outcome" ~version:1 in
  let%bind () =
    match D.Json.field (D.Document.json document) ~name:"pending_custody" with
    | Absent -> D.Document.validate document ~limits
    | Null -> F.invalid "pending_custody" "null custody is invalid"
    | Value raw ->
      let%bind _ = pending_component raw in
      (match D.Document.json document with
       | `Object fields ->
         let%map _ =
           D.Document.inspect
             ~limits
             (`Object
                 (List.filter fields ~f:(fun (name, _) ->
                    not (String.equal name "pending_custody"))))
         in
         ()
       | _ -> F.invalid "document" "expected an object envelope")
  in
  let%map value = decode_value (D.Document.payload document) in
  let bytes = D.Document.to_string document in
  { document
  ; value
  ; reference =
      { Reference.digest = Document_record.digest bytes
      ; encoded_bytes = String.length bytes
      }
  ; encoded = bytes
  }
;;

let create ?pending_custody json =
  let open Result.Let_syntax in
  let%bind document =
    D.Document.create ~limits ~kind:"store.idempotency_outcome" ~version:1 ~payload:json
    |> F.store
  in
  let%bind document =
    match pending_custody with
    | None -> Ok document
    | Some raw ->
      let%bind _ = pending_component raw |> F.store in
      (match D.Document.json document with
       | `Object fields ->
         D.Document.inspect
           ~limits:composite_limits
           (`Object (fields @ [ "pending_custody", raw ]))
         |> F.store
       | _ -> Error (Store_error.Corrupt "invalid authored outcome envelope"))
  in
  of_document document |> F.store
;;

let value t = t.value
let jsonaf t = D.Document.payload t.document
let document t = t.document
let reference t = t.reference

let verify reference bytes =
  let open Result.Let_syntax in
  let%bind () =
    if
      Int.equal (String.length bytes) (Reference.encoded_bytes reference)
      && String.equal (Document_record.digest bytes) (Reference.digest reference)
    then Ok ()
    else
      Error (Store_error.Corrupt "idempotency outcome reference digest or size mismatch")
  in
  let%bind document = D.Document.decode ~limits:composite_limits bytes |> F.store in
  Result.map
    (of_document document |> F.store)
    ~f:(fun t -> { t with reference; encoded = bytes })
;;

let to_string t = t.encoded
let decode reference bytes = verify reference bytes
