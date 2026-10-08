open! Core
open Core

type t =
  { conflict_domain : string
  ; prompt_id : Agent_protocol.Id.Prompt_definition.t
  }
[@@deriving compare, sexp]

module X = Persistence_codec
module J = Agent_protocol.Json_codec

let storage_to_jsonaf (t : t) =
  `Object
    [ "conflict_domain", X.text_json t.conflict_domain
    ; "prompt_id", Agent_protocol.Id.Prompt_definition.to_json t.prompt_id
    ]
;;

let storage_of_jsonaf json =
  let open Result.Let_syntax in
  let%bind fields = X.object_ json in
  let%bind conflict_domain = X.required fields "conflict_domain" J.string in
  let%bind prompt_id =
    X.required fields "prompt_id" Agent_protocol.Id.Prompt_definition.of_json
  in
  let t : t = { conflict_domain; prompt_id } in
  Ok t
;;

let storage_shape =
  X.shape_exn
    [ "conflict_domain", Document_schema.Shape.value
    ; "prompt_id", Document_schema.Shape.value
    ]
;;

let to_jsonaf = storage_to_jsonaf
let of_jsonaf = storage_of_jsonaf
let shape = storage_shape
