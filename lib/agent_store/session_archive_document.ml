open! Core
module D = Document_schema
module F = Document_fields
module P = Agent_protocol

type t = P.Id.Session.t

let decode json =
  F.required json "session_id" (fun value -> P.Id.Session.of_json value |> F.protocol)
;;

let encode value = Ok (`Object [ "session_id", P.Id.Session.to_json value ])

let limits =
  F.limits ~max_bytes:32768
  |> Result.map_error ~f:(fun error -> Sexp.to_string_hum (D.Error.sexp_of_t error))
  |> Result.ok_or_failwith
;;

let codec =
  D.Domain_codec.create
    ~limits
    ~kind:"store.session_archive"
    ~version:1
    ~shape:(F.shape [ "session_id", D.Shape.value ])
    ~supported_semantics:[]
    ~decode
    ~encode
  |> Result.map_error ~f:(fun error -> Sexp.to_string_hum (D.Error.sexp_of_t error))
  |> Result.ok_or_failwith
;;

let of_document document =
  let open Result.Let_syntax in
  let%bind document = F.upgrade document ~limits ~kind:"store.session_archive" in
  D.Domain_codec.decode codec document
;;

let to_document value = D.Domain_codec.encode codec value

let stored_session_id document =
  let open Result.Let_syntax in
  let%bind () =
    F.expect_versions document ~kind:"store.session_archive" ~versions:[ 1 ]
  in
  decode (D.Document.payload document)
;;
