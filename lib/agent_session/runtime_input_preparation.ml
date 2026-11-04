open! Core

let invalid message =
  Agent_protocol.Error.create Invalid_state ~message ~retryable:false ()
;;

module Prepared_input = struct
  type t =
    { request_sha256 : string
    ; content : Agent_protocol.Session.Message_content.t
    ; entry : Agent_protocol.History.entry
    }

  let create ~request ~id ~entry =
    let open Result.Let_syntax in
    let%bind content =
      match request.Run_admission_request.input with
      | User_submission content -> Ok content
      | Authored_start -> Error (invalid "prepared input requires User_submission")
    in
    let%bind request_sha256 = Run_admission_request.request_sha256 request in
    let%bind _ = History_codec.of_canonical entry in
    if
      not
        (History_entry.Id.equal id entry.Agent_protocol.History.id
         && Agent_protocol.History.equal_role entry.role User
         && Agent_protocol.History.equal_kind entry.kind Message
         && Agent_protocol.History.equal_provenance entry.provenance Canonical
         && Agent_protocol.History.Content_revision.equal
              entry.content_revision
              Agent_protocol.History.Content_revision.zero)
    then Error (invalid "prepared input differs from its reserved canonical user entry")
    else Ok { request_sha256; content; entry }
  ;;

  let validate t ~request ~id =
    let open Result.Let_syntax in
    let%bind expected = create ~request ~id ~entry:t.entry in
    if
      String.equal t.request_sha256 expected.request_sha256
      && Jsonaf.exactly_equal
           (Agent_protocol.Session.Message_content.to_json t.content)
           (Agent_protocol.Session.Message_content.to_json expected.content)
    then Ok ()
    else Error (invalid "prepared input differs from its original request")
  ;;

  let equal a b =
    String.equal a.request_sha256 b.request_sha256
    && Jsonaf.exactly_equal
         (Agent_protocol.Session.Message_content.to_json a.content)
         (Agent_protocol.Session.Message_content.to_json b.content)
    && Agent_protocol.History.equal_entry a.entry b.entry
  ;;

  let request_sha256 t = t.request_sha256
  let content t = t.content
  let entry t = t.entry
end

module Execution = struct
  type t =
    { request : Run_admission_request.t
    ; id : History_entry.Id.t
    ; checkpoint : unit -> (unit, Agent_protocol.Error.t) result
    ; refresh :
        unit
        -> (unit -> (unit, Agent_protocol.Error.t) result, Agent_protocol.Error.t) result
    ; allocate_inference : Inference_allocation.Port.t
    }
end

module Tracking = struct
  type t =
    { identity : Inference_client.Identity.t
    ; on_attempt : Inference_runtime.Attempt.t -> unit
    ; on_observation : Inference.Observation.t -> unit
    ; on_completion : Inference_client.Completion.t -> unit
    }
end
