open! Core
module P = Agent_protocol

module Key = struct
  type t = P.Id.Operation.t * P.Activity.Key.t [@@deriving compare, hash, sexp_of]
end

type entry =
  { operation_id : P.Id.Operation.t
  ; sequence : int64
  ; summary : P.Activity.Tool.summary
  }

type t = (Key.t, entry) Hashtbl.t

let create () = Hashtbl.create (module Key)

let summary_limits =
  match
    Document_schema.Limits.create
      ~max_bytes:4096
      ~max_depth:32
      ~max_fields:256
      ~max_nodes:512
  with
  | Ok limits -> limits
  | Error _ -> failwith "invalid active call summary limits"
;;

let observe t (event : P.Event.Recoverable.t) =
  match event.payload with
  | Transcript _ -> ()
  | Tool_activity activity ->
    let key = event.operation_id, P.Activity.Tool.key activity in
    (match activity with
     | Started descriptor ->
       if Hashtbl.length t < 1024 || Hashtbl.mem t key
       then (
         let summary =
           P.Activity.Tool.summary
             descriptor.key
             ~descriptor:(Some descriptor)
             ~channels:[]
             ~state:Running
         in
         match summary with
         | Error _ -> Hashtbl.remove t key
         | Ok summary ->
           (match
              Document_schema.Json.validate
                ~limits:summary_limits
                (P.Activity.Tool.summary_to_json summary)
            with
            | Error _ -> Hashtbl.remove t key
            | Ok () ->
              Hashtbl.set
                t
                ~key
                ~data:
                  { operation_id = event.operation_id
                  ; sequence = event.operation_sequence
                  ; summary
                  }))
     | Progress _ -> ()
     | Finished _ -> Hashtbl.remove t key)
;;

let finish t (event : P.Event.Durable.t) =
  match event.kind with
  | Operation_completed | Operation_cancelled | Operation_failed | Operation_interrupted
    ->
    (match P.Operation.of_json event.payload with
     | Error _ -> ()
     | Ok operation ->
       Hashtbl.filter_inplace t ~f:(fun entry ->
         not (P.Id.Operation.equal entry.operation_id operation.id)))
  | Session_created
  | Session_state_changed
  | Session_updated
  | Attachment_owner_changed
  | History_message_deferred
  | History_appended
  | History_replaced
  | Moderator_overlay_changed
  | Moderator_notification
  | Permission_requested
  | Permission_resolved
  | Grant_created
  | Grant_revoked
  | Operation_started
  | Job_state_changed
  | Schedule_created
  | Schedule_state_changed
  | Schedule_cancelled
  | Prompt_upgraded
  | Workspace_state_changed
  | Session_error -> ()
;;

let snapshot t =
  let calls =
    Hashtbl.data t
    |> List.sort ~compare:(fun a b ->
      match P.Id.Operation.compare a.operation_id b.operation_id with
      | 0 -> Int64.compare a.sequence b.sequence
      | order -> order)
  in
  let agents =
    List.filter calls ~f:(fun entry ->
      Option.exists entry.summary.descriptor ~f:(fun d -> Option.is_some d.classification))
  in
  let encode entry = P.Activity.Tool.summary_to_json entry.summary in
  List.map calls ~f:encode, List.map agents ~f:encode
;;
