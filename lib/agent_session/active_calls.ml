open! Core

type t = (string, Agent_protocol.Event.Recoverable.t) Hashtbl.t

let create () = Hashtbl.create (module String)

let field payload name =
  match payload with
  | `Object fields -> List.Assoc.find fields name ~equal:String.equal
  | _ -> None
;;

let key event =
  match field event.Agent_protocol.Event.Recoverable.payload "call_id" with
  | Some (`String call_id) ->
    Some (Agent_protocol.Id.Operation.to_string event.operation_id ^ ":" ^ call_id)
  | _ -> None
;;

let bounded_payload = function
  | `Object fields ->
    `Object
      (List.map fields ~f:(fun (name, value) ->
         ( name
         , match value with
           | `String text when String.equal name "payload" && String.length text > 4096 ->
             `String "<summary field omitted: exceeds 4096 bytes>"
           | json -> json )))
  | json -> json
;;

let observe t event =
  Option.iter (key event) ~f:(fun key ->
    match event.Agent_protocol.Event.Recoverable.kind with
    | Tool_started when Hashtbl.length t < 1024 || Hashtbl.mem t key ->
      Hashtbl.set t ~key ~data:{ event with payload = bounded_payload event.payload }
    | Tool_finished -> Hashtbl.remove t key
    | _ -> ())
;;

let finish t (event : Agent_protocol.Event.Durable.t) =
  match event.kind with
  | Operation_completed | Operation_cancelled | Operation_failed | Operation_interrupted
    ->
    (match Agent_protocol.Operation.of_json event.payload with
     | Error _ -> ()
     | Ok operation ->
       Hashtbl.filter_inplace t ~f:(fun entry ->
         Agent_protocol.Id.Operation.compare
           entry.Agent_protocol.Event.Recoverable.operation_id
           operation.id
         <> 0))
  | _ -> ()
;;

let snapshot t =
  let calls =
    Hashtbl.data t
    |> List.sort ~compare:(fun a b ->
      match
        Agent_protocol.Id.Operation.compare
          a.Agent_protocol.Event.Recoverable.operation_id
          b.Agent_protocol.Event.Recoverable.operation_id
      with
      | 0 -> Int64.compare a.operation_sequence b.operation_sequence
      | order -> order)
  in
  let agents =
    List.filter calls ~f:(fun event ->
      match field event.payload "agent_page_kind" with
      | Some (`String _) -> true
      | _ -> false)
  in
  ( List.map calls ~f:Agent_protocol.Event.Recoverable.to_json
  , List.map agents ~f:Agent_protocol.Event.Recoverable.to_json )
;;
