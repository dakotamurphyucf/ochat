open Core
module Error = Protocol_error

type body =
  | Conversation of Job.t
  | Developer of Developer_job.t

type t =
  { body : body
  ; owner : Execution_owner.t
  }

module Identity = struct
  module T = struct
    type t =
      { owner : Execution_owner.t
      ; job_id : Id.Job.t
      }
    [@@deriving compare, equal, sexp_of]
  end

  include T
  include Comparator.Make (T)

  let owner t = t.owner
  let job_id t = t.job_id
end

let validate_id to_json of_json id = Extension_codec.validate_id to_json of_json id

let of_conversation (job : Job.t) =
  let open Result.Let_syntax in
  let%bind () = validate_id Id.Job.to_json Id.Job.of_json job.id in
  let%bind owner =
    Execution_owner.conversation ~session_id:job.session_id ~generation:job.generation
  in
  let%bind () =
    if job.attempt < 0
    then Error (Error.invalid_request "negative conversation job attempt")
    else Ok ()
  in
  let%bind () = Job.validate_result job in
  return { body = Conversation job; owner }
;;

let of_developer job =
  let owner = Execution_owner.Developer (Developer_job.owner job) in
  Result.map (Execution_owner.validate owner) ~f:(fun () ->
    { body = Developer job; owner })
;;

let to_conversation { body; _ } =
  match body with
  | Conversation job -> Some job
  | Developer _ -> None
;;

let to_developer { body; _ } =
  match body with
  | Conversation _ -> None
  | Developer job -> Some job
;;

let owner t = t.owner

let id { body; _ } =
  match body with
  | Conversation job -> job.id
  | Developer job -> Developer_job.id job
;;

let identity job = { Identity.owner = owner job; job_id = id job }

let kind { body; _ } =
  match body with
  | Conversation job -> job.kind
  | Developer job -> Developer_job.kind job
;;

let status { body; _ } =
  match body with
  | Conversation job -> job.status
  | Developer job -> (Developer_job.state job).status
;;

let attempt { body; _ } =
  match body with
  | Conversation job -> job.attempt
  | Developer job -> (Developer_job.state job).attempt
;;

let nested_depth { body; _ } =
  match body with
  | Conversation job ->
    (match job.launch with
     | None -> 0
     | Some launch -> launch.nested_depth)
  | Developer job -> Developer_job.Launch.nested_depth (Developer_job.launch job)
;;

module Lifecycle = struct
  type t =
    | New_queued
    | Queued_retry
    | Claimed_or_waiting
    | Terminal
  [@@deriving equal, sexp_of]

  let classify ~status ~attempt =
    match status with
    | Job.Queued -> if Int.equal attempt 0 then New_queued else Queued_retry
    | Running | Waiting_permission _ | Waiting_completion _ -> Claimed_or_waiting
    | Succeeded | Failed _ | Cancelled | Interrupted _ -> Terminal
  ;;
end

let lifecycle t = Lifecycle.classify ~status:(status t) ~attempt:(attempt t)
