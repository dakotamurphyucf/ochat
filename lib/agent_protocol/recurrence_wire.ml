open! Core
module J = Json_codec
module D = Document_schema

let invalid message = Error (Protocol_error.invalid_request message)
let counter_json value = `String (Int64.to_string value)
let integer_json value = `Number (Int.to_string value)
let bool_json value = if value then `True else `False
let option_json encode value = Option.value_map value ~default:`Null ~f:encode

let nullable decode = function
  | `Null -> Ok None
  | json -> Result.map (decode json) ~f:Option.some
;;

let counter = function
  | `String encoded when String.length encoded <= 19 ->
    (match Int64.of_string_opt encoded with
     | Some value when Int64.(value >= 0L) && String.equal (Int64.to_string value) encoded
       -> Ok value
     | Some _ | None -> invalid "invalid recurrence decimal counter")
  | _ -> invalid "recurrence counters must be decimal strings"
;;

let integer = J.bounded_int ~min:0 ~max:Int.max_value

let fields json names =
  let%bind.Result fields = J.fields json in
  let%map.Result () = Extension_codec.closed fields names in
  fields
;;

let session json =
  let%bind.Result _ = fields json [ "server_id"; "session_id" ] in
  Session_ref.of_json json
;;

let source json =
  let%bind.Result _ =
    fields json [ "script_id"; "source_sha256"; "generation"; "installation_epoch" ]
  in
  Run_source.of_json json
;;

let lifecycle_json value = counter_json (Session_lifecycle.Revision.to_int64 value)
let lifecycle json = Result.bind (counter json) ~f:Session_lifecycle.Revision.of_int64

let tag json =
  let%bind.Result fields = J.fields json in
  let%map.Result kind = J.required_as fields "kind" J.string in
  kind, fields
;;

let tagged kind fields = `Object (("kind", `String kind) :: fields)

let shape_exn fields =
  match D.Shape.object_ fields with
  | Ok shape -> shape
  | Error error -> raise_s [%sexp "invalid recurrence object shape", (error : D.Error.t)]
;;

let fields_shape names = shape_exn (List.map names ~f:(fun name -> name, D.Shape.value))

let tagged_shape_exn ~discriminator cases =
  match D.Shape.tagged_object ~discriminator cases with
  | Ok shape -> shape
  | Error error -> raise_s [%sexp "invalid recurrence tagged shape", (error : D.Error.t)]
;;

let list maximum decode = function
  | `Array values when List.length values <= maximum ->
    List.map values ~f:decode |> Result.all
  | _ -> invalid "recurrence query array exceeds its bound"
;;

let array encode rows = `Array (List.map rows ~f:encode)
