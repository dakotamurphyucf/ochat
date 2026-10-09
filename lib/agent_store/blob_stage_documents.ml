open! Core
module D = Document_schema
module F = Document_fields
module M = Blob_metadata_document

type t =
  { temporary : M.t
  ; durable : M.t
  ; temporary_bytes : string
  ; durable_bytes : string
  }

let temporary t = t.temporary
let durable t = t.durable
let temporary_bytes t = t.temporary_bytes
let durable_bytes t = t.durable_bytes

let equal a b =
  String.equal a.temporary_bytes b.temporary_bytes
  && String.equal a.durable_bytes b.durable_bytes
;;

let of_publications ~temporary_bytes ~durable_bytes =
  let open Result.Let_syntax in
  let%bind original_temporary = D.Document.decode ~limits:M.limits temporary_bytes in
  let%bind original_durable = D.Document.decode ~limits:M.limits durable_bytes in
  let%bind temporary = M.of_document original_temporary in
  let%bind durable = M.of_document original_durable in
  let value = M.value temporary in
  let%bind () =
    if value.durable || Option.is_none value.target_session
    then F.invalid "stage" "temporary publication needs a session and durable=false"
    else if not (Blob_metadata.equal { value with durable = true } (M.value durable))
    then F.invalid "stage" "metadata publications differ beyond durable flag"
    else Ok ()
  in
  let payload = D.Document.payload original_temporary in
  let%bind expected =
    match D.Document.json original_temporary, payload with
    | `Object envelope, `Object fields ->
      let fields =
        List.map fields ~f:(fun (key, value) ->
          key, if String.equal key "durable" then `True else value)
      in
      let envelope =
        List.map envelope ~f:(fun (key, value) ->
          key, if String.equal key "payload" then `Object fields else value)
      in
      D.Document.inspect ~limits:M.limits (`Object envelope)
    | _ -> F.invalid "stage" "metadata publication is not an object"
  in
  if
    not
      (Jsonaf.exactly_equal (D.Document.json expected) (D.Document.json original_durable))
  then F.invalid "stage" "publication extensions or presence differ beyond durable flag"
  else Ok { temporary; durable; temporary_bytes; durable_bytes }
;;

let create temporary =
  let open Result.Let_syntax in
  let%bind temporary_bytes = M.to_bytes temporary in
  let%bind durable = M.with_value temporary { (M.value temporary) with durable = true } in
  let%bind durable_bytes = M.to_bytes durable in
  of_publications ~temporary_bytes ~durable_bytes
;;
