open! Core
module P = Agent_protocol
module D = Document_schema
module X = Persistence_codec

let invalid reason = Error (P.Error.invalid_request reason)

let limits =
  D.Limits.create ~max_bytes:4096 ~max_depth:12 ~max_fields:64 ~max_nodes:128
  |> Result.map_error ~f:(fun error -> Sexp.to_string_hum (D.Error.sexp_of_t error))
  |> Result.ok_or_failwith
;;

let bounded json =
  D.Json.validate ~limits json
  |> Result.map_error ~f:(fun error ->
    P.Error.invalid_request (Sexp.to_string_hum (D.Error.sexp_of_t error)))
;;

let counter = function
  | `String text when String.length text <= 19 -> X.nonnegative_int64 (`String text)
  | _ -> invalid "recurrence coordinate requires a nonnegative decimal int64 string"
;;

module Definition = struct
  type t =
    { controller : P.Session_ref.t
    ; recurrence_id : P.Id.Recurrence.t
    ; revision : int64
    }
  [@@deriving compare, equal, sexp_of]

  let controller t = t.controller
  let recurrence_id t = t.recurrence_id
  let revision t = t.revision

  let create ~controller ~recurrence_id ~revision =
    let open Result.Let_syntax in
    let%bind _ = P.Session_ref.of_json (P.Session_ref.to_json controller) in
    let%bind _ = P.Id.Recurrence.of_json (P.Id.Recurrence.to_json recurrence_id) in
    if Int64.(revision < 0L)
    then invalid "negative recurrence definition revision"
    else Ok { controller; recurrence_id; revision }
  ;;

  let to_json t =
    `Object
      [ "controller", P.Session_ref.to_json t.controller
      ; "recurrence_id", P.Id.Recurrence.to_json t.recurrence_id
      ; "revision", X.int64_json t.revision
      ]
  ;;

  let of_json json =
    let open Result.Let_syntax in
    let%bind () = bounded json in
    let%bind fields = X.object_ json in
    let%bind controller = X.required fields "controller" P.Session_ref.of_json in
    let%bind recurrence_id = X.required fields "recurrence_id" P.Id.Recurrence.of_json in
    let%bind revision = X.required fields "revision" counter in
    create ~controller ~recurrence_id ~revision
  ;;

  let shape =
    X.shape_exn
      [ "controller", X.fields_shape [ "server_id"; "session_id" ]
      ; "recurrence_id", D.Shape.value
      ; "revision", D.Shape.value
      ]
  ;;
end

module Coordinate (Field : sig
    val name : string
  end) =
struct
  type t =
    { definition : Definition.t
    ; number : int64
    }
  [@@deriving compare, equal, sexp_of]

  let definition t = t.definition
  let number t = t.number

  let create ~definition ~number =
    if Int64.(number < 0L)
    then invalid "negative recurrence coordinate"
    else Ok { definition; number }
  ;;

  let to_json t =
    `Object
      [ "definition", Definition.to_json t.definition; Field.name, X.int64_json t.number ]
  ;;

  let of_json json =
    let open Result.Let_syntax in
    let%bind () = bounded json in
    let%bind fields = X.object_ json in
    let%bind definition = X.required fields "definition" Definition.of_json in
    let%bind number = X.required fields Field.name counter in
    create ~definition ~number
  ;;

  let shape = X.shape_exn [ "definition", Definition.shape; Field.name, D.Shape.value ]
end

module Occurrence = struct
  include Coordinate (struct
      let name = "slot"
    end)

  let slot = number
  let create ~definition ~slot = create ~definition ~number:slot
end

module Alarm = struct
  include Coordinate (struct
      let name = "sequence"
    end)

  let sequence = number
  let create ~definition ~sequence = create ~definition ~number:sequence
end
