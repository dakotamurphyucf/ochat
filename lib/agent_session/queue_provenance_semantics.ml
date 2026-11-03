open! Core
module D = Document_schema

let name = Agent_protocol.Schedule_provenance.persistence_semantics
let supported = [ name ]

let positive json field =
  match D.Json.field json ~name:field with
  | Value _ -> true
  | Absent | Null -> false
;;

let queue_requires moderator = positive moderator "queue_provenance"
let schedule_requires schedule = positive schedule "inference_provenance"

let state_requires payload =
  (match D.Json.field payload ~name:"moderator" with
   | Value moderator -> queue_requires moderator
   | Absent | Null -> false)
  ||
  match D.Json.field payload ~name:"schedules" with
  | Value (`Array schedules) -> List.exists schedules ~f:schedule_requires
  | Value _ | Absent | Null -> false
;;

let delta_requires payload =
  match D.Json.field payload ~name:"changes" with
  | Value (`Array changes) ->
    List.exists changes ~f:(fun change ->
      match D.Json.field change ~name:"kind" with
      | Value (`String "created") ->
        (match D.Json.field change ~name:"state" with
         | Value state ->
           (match D.Json.field state ~name:"payload" with
            | Value payload -> state_requires payload
            | Absent | Null -> false)
         | Absent | Null -> false)
      | Value (`String "moderator_changed") ->
        (match D.Json.field change ~name:"moderator" with
         | Value moderator -> queue_requires moderator
         | Absent | Null -> false)
      | Value (`String "schedule_changed") ->
        (match D.Json.field change ~name:"value" with
         | Value schedule -> schedule_requires schedule
         | Absent | Null -> false)
      | Value _ | Absent | Null -> false)
  | Value _ | Absent | Null -> false
;;

let check document ~required =
  if
    (not required)
    || List.mem (D.Document.required_semantics document) name ~equal:String.equal
  then Ok ()
  else Error (D.Error.Required_semantics_unknown ("missing " ^ name))
;;

let require document ~required ~limits =
  if
    (not required)
    || List.mem (D.Document.required_semantics document) name ~equal:String.equal
  then Ok document
  else (
    match D.Document.json document with
    | `Object fields ->
      let markers = D.Document.required_semantics document @ [ name ] in
      let replacement = `Array (List.map markers ~f:(fun marker -> `String marker)) in
      let fields =
        if List.Assoc.mem fields "required_semantics" ~equal:String.equal
        then
          List.map fields ~f:(fun (key, value) ->
            key, if String.equal key "required_semantics" then replacement else value)
        else fields @ [ "required_semantics", replacement ]
      in
      D.Document.inspect ~limits (`Object fields)
    | _ -> assert false)
;;
