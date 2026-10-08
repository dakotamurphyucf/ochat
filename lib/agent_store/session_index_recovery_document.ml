open! Core
module D = Document_schema
module F = Document_fields
module P = Agent_protocol

type t = unit

let limits =
  F.limits ~max_bytes:32768
  |> Result.map_error ~f:(fun error -> Sexp.to_string_hum (D.Error.sexp_of_t error))
  |> Result.ok_or_failwith
;;

let codec =
  D.Domain_codec.create
    ~limits
    ~kind:"store.session_index_recovery"
    ~version:1
    ~shape:(F.shape [])
    ~supported_semantics:[]
    ~decode:(fun _ -> Ok ())
    ~encode:(fun () -> Ok (`Object []))
  |> Result.map_error ~f:(fun error -> Sexp.to_string_hum (D.Error.sexp_of_t error))
  |> Result.ok_or_failwith
;;

let of_document document =
  let open Result.Let_syntax in
  let%bind document = F.upgrade document ~limits ~kind:"store.session_index_recovery" in
  D.Domain_codec.decode codec document
;;

let to_document value = D.Domain_codec.encode codec value
