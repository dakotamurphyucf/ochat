open! Core
module D = Document_schema

let old_name = "native_goal_resume"
let current_name = "native_goal_action"
let invalid name reason = Error (D.Error.Invalid_field { path = [ name ]; reason })

let invocation_record = function
  | `Object fields as original ->
    if List.Assoc.mem fields current_name ~equal:String.equal
    then invalid current_name "native Goal action conversion destination is present"
    else if List.count fields ~f:(fun (name, _) -> String.equal name old_name) > 1
    then invalid old_name "duplicate native Goal resume conversion source"
    else (
      match List.Assoc.find fields old_name ~equal:String.equal with
      | None -> Ok original
      | Some intent ->
        let action =
          match intent with
          | `Null -> `Null
          | `Object _ | `Array _ | `String _ | `Number _ | `True | `False ->
            `Object [ "kind", `String "resume"; "intent", intent ]
        in
        Ok
          (`Object
              (List.map fields ~f:(fun (name, value) ->
                 if String.equal name old_name then current_name, action else name, value))))
  | `Null | `Array _ | `String _ | `Number _ | `True | `False ->
    invalid "invocation" "native Goal action conversion requires an object"
;;

let reject_old_field = function
  | `Object fields ->
    if List.Assoc.mem fields old_name ~equal:String.equal
    then invalid old_name "old native Goal field requires its declared schema conversion"
    else Ok ()
  | `Null | `Array _ | `String _ | `Number _ | `True | `False ->
    invalid "invocation" "native Goal action reader requires an object"
;;
