open! Core
module D = Document_schema
module Error = Request.Error

type view =
  | Unresolved
  | Captured of Request.Target.t

type t =
  { view : view
  ; json : Jsonaf.t
  }

let invalid field reason = Error (Error.Invalid_field { field; reason })

let validate_json json ~limits =
  D.Json.validate ~limits json |> Result.map_error ~f:(fun error -> Error.Json error)
;;

let of_json json ~limits =
  let open Result.Let_syntax in
  let%bind () = validate_json json ~limits in
  let%bind () =
    match json with
    | `Object _ -> Ok ()
    | `Array _ | `String _ | `Number _ | `True | `False | `Null ->
      invalid "selection" "must be an object"
  in
  let%map view =
    match D.Json.field json ~name:"state" with
    | Value (`String "unresolved") ->
      (match D.Json.field json ~name:"target" with
       | Absent -> Ok Unresolved
       | Null | Value _ -> invalid "target" "unresolved selection cannot contain a target")
    | Value (`String "captured") ->
      (match D.Json.field json ~name:"target" with
       | Value target ->
         Request.Target.of_json target ~limits
         |> Result.map ~f:(fun target -> Captured target)
       | Absent | Null -> invalid "target" "captured selection requires a target")
    | Absent | Null | Value _ -> invalid "state" "must be unresolved or captured"
  in
  { view; json }
;;

let unresolved ~limits = of_json (`Object [ "state", `String "unresolved" ]) ~limits

let captured target ~limits =
  let open Result.Let_syntax in
  let%bind () = Request.Target.validate target ~limits in
  of_json
    (`Object [ "state", `String "captured"; "target", Request.Target.to_json target ])
    ~limits
;;

let view t = t.view
let to_json t = t.json
let validate t ~limits = of_json t.json ~limits |> Result.map ~f:(fun _ -> ())

let equal a b =
  match a.view, b.view with
  | Unresolved, Unresolved -> true
  | Captured a, Captured b -> Request.Target.equal a b
  | Unresolved, Captured _ | Captured _, Unresolved -> false
;;

let capture t ~target ~limits =
  let open Result.Let_syntax in
  let%bind () = validate t ~limits in
  let%bind () = Request.Target.validate target ~limits in
  match t.view with
  | Captured previous ->
    if Request.Target.equal previous target
    then Ok t
    else invalid "target" "selection is already captured with a different target"
  | Unresolved ->
    (match t.json with
     | `Object fields ->
       let fields =
         List.map fields ~f:(fun (name, value) ->
           name, if String.equal name "state" then `String "captured" else value)
       in
       of_json (`Object (fields @ [ "target", Request.Target.to_json target ])) ~limits
     | `Array _ | `String _ | `Number _ | `True | `False | `Null -> assert false)
;;
