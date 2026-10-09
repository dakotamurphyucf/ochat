open! Core
module D = Document_schema
module P = Agent_protocol
module F = Agent_store.Document_fields

module Delivery_id = struct
  include P.Id.Delivery
  include Comparator.Make (P.Id.Delivery)
end

type t =
  { run_state_absent : bool
  ; delivery_bindings : bool Map.M(Delivery_id).t
  }

let authored =
  { run_state_absent = false; delivery_bindings = Map.empty (module Delivery_id) }
;;

let run_state_is_absent t = t.run_state_absent

let delivery_binding_is_absent t id =
  Option.value (Map.find t.delivery_bindings id) ~default:false
;;

let of_document document ~limits =
  let open Result.Let_syntax in
  let%bind () = D.Document.validate document ~limits in
  let%bind () =
    if String.equal (D.Document.kind document) "session.state"
    then Ok ()
    else
      Error
        (D.Error.Wrong_kind
           { expected = "session.state"; actual = D.Document.kind document })
  in
  let payload = D.Document.payload document in
  let run_state_absent =
    match D.Json.field payload ~name:"run_state" with
    | Absent -> true
    | Null | Value _ -> false
  in
  let%bind deliveries = F.required payload "deliveries" F.array in
  let%map delivery_bindings =
    List.fold_result
      deliveries
      ~init:(Map.empty (module Delivery_id))
      ~f:(fun seen delivery ->
        let%bind id =
          F.required delivery "id" (fun json ->
            P.Id.Delivery.of_json json
            |> Result.map_error ~f:(fun error -> D.Error.Malformed error.message))
        in
        let%bind absent =
          match D.Json.field delivery ~name:"ownership" with
          | Null -> Ok false
          | Value (`Object _ as ownership) ->
            (match D.Json.field ownership ~name:"subscription_binding" with
             | Absent -> Ok true
             | Null | Value _ -> Ok false)
          | Absent | Value (`Null | `True | `False | `Number _ | `String _ | `Array _) ->
            Error
              (D.Error.Invalid_field
                 { path = [ "deliveries"; P.Id.Delivery.to_string id; "ownership" ]
                 ; reason = "expected nullable ownership object"
                 })
        in
        match Map.add seen ~key:id ~data:absent with
        | `Ok seen -> Ok seen
        | `Duplicate ->
          Error
            (D.Error.Invalid_field
               { path = [ "deliveries" ]
               ; reason = "duplicate delivery identity in original state"
               }))
  in
  { run_state_absent; delivery_bindings }
;;
