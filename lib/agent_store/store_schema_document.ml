open! Core
module D = Document_schema
module F = Document_fields
module P = Agent_protocol

type t = { created_at : P.Timestamp.t }

let decode json =
  Result.map
    (F.required json "created_at" (fun value -> P.Timestamp.of_json value |> F.protocol))
    ~f:(fun created_at -> { created_at })
;;

let encode value = Ok (`Object [ "created_at", P.Timestamp.to_json value.created_at ])

let limits =
  F.limits ~max_bytes:32768
  |> Result.map_error ~f:(fun error -> Sexp.to_string_hum (D.Error.sexp_of_t error))
  |> Result.ok_or_failwith
;;

let codec =
  D.Domain_codec.create
    ~limits
    ~kind:"store.schema"
    ~version:1
    ~shape:(F.shape [ "created_at", D.Shape.value ])
    ~supported_semantics:[]
    ~decode
    ~encode
  |> Result.map_error ~f:(fun error -> Sexp.to_string_hum (D.Error.sexp_of_t error))
  |> Result.ok_or_failwith
;;

let of_document document =
  let open Result.Let_syntax in
  let%bind document = F.upgrade document ~limits ~kind:"store.schema" in
  D.Domain_codec.decode codec document
;;

let to_document value = D.Domain_codec.encode codec value

let stored_version document =
  let open Result.Let_syntax in
  let version = D.Document.version document in
  let%bind () = F.expect document ~kind:"store.schema" ~version in
  if version = 1
  then Result.map (of_document document) ~f:(fun _ -> version)
  else Ok version
;;
