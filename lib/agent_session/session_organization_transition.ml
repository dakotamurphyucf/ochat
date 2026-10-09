open! Core
module Values = Agent_protocol.Session_organization.Values

type t =
  { values : Values.t
  ; additions : Values.t
  ; delta : Session_delta.t
  ; changed : bool
  }

let changed t = t.changed
let values t = t.values
let additions t = t.additions
let delta t = t.delta

let create state ~expected_metadata_revision ~patch =
  let open Result.Let_syntax in
  let previous = state.Session_state.identity.organization in
  if not (Int64.equal state.identity.metadata_revision expected_metadata_revision)
  then
    Error
      (Agent_protocol.Error.create
         Conflict
         ~message:"session metadata revision does not match"
         ~retryable:false
         ())
  else (
    let%bind values = Agent_protocol.Session_organization.Patch.apply patch ~previous in
    let changed = not (Values.equal previous values) in
    if changed && Int64.equal expected_metadata_revision Int64.max_value
    then
      Error (Agent_protocol.Error.invalid_request "session metadata revision exhausted")
    else (
      let project_id =
        if
          Option.equal
            Agent_protocol.Id.Project.equal
            previous.project_id
            values.project_id
        then None
        else values.project_id
      in
      let collection_ids =
        List.filter values.collection_ids ~f:(fun id ->
          not
            (List.mem
               previous.collection_ids
               id
               ~equal:Agent_protocol.Id.Collection.equal))
      in
      let%map additions = Values.create ~project_id ~collection_ids in
      let delta =
        if changed
        then
          Session_delta.Organization_changed
            (values, Int64.succ expected_metadata_revision)
        else Batch []
      in
      { values; additions; delta; changed }))
;;
