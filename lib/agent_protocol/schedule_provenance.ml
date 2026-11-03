open! Core

let persistence_semantics = "runtime.queue-provenance.v1"

type t =
  { source : Run_source.t
  ; origin : Inference_origin.t option
  }
[@@deriving equal]

let source t = t.source
let origin t = t.origin

let validate t =
  let open Result.Let_syntax in
  let%bind () = Run_source.validate t.source in
  Option.value_map t.origin ~default:(Ok ()) ~f:Inference_origin.validate
;;

let create ~source ~origin =
  let t = { source; origin } in
  Result.map (validate t) ~f:(fun () -> t)
;;

let to_json t =
  `Object
    [ "schema_version", `Number "1"
    ; "source", Run_source.to_json t.source
    ; "origin", Option.value_map t.origin ~default:`Null ~f:Inference_origin.to_json
    ]
;;

let of_json json =
  let open Result.Let_syntax in
  let%bind fields = Json_codec.fields json in
  let%bind _ =
    Json_codec.required_as fields "schema_version" (Json_codec.bounded_int ~min:1 ~max:1)
  in
  let%bind source = Json_codec.required_as fields "source" Run_source.of_json in
  let%bind origin =
    Json_codec.required_as fields "origin" (function
      | `Null -> Ok None
      | value -> Result.map (Inference_origin.of_json value) ~f:Option.some)
  in
  create ~source ~origin
;;

let sexp_of_t t = Jsonaf.sexp_of_t (to_json t)

let t_of_sexp sexp =
  match of_json (Jsonaf.t_of_sexp sexp) with
  | Ok t -> t
  | Error error -> Sexplib.Conv.of_sexp_error error.Protocol_error.message sexp
;;
