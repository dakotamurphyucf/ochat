open Core
module P = Agent_protocol

module Developer_parent = struct
  type t =
    | Root_action
    | Native_invocation of P.Id.Invocation.t
    | Worker_claim of
        { job_id : P.Id.Job.t
        ; attempt : int
        }
  [@@deriving equal, sexp_of]
end

type t =
  | Conversation of P.Job.launch_owner
  | Developer of
      { owner : P.Execution_owner.developer
      ; parent : Developer_parent.t
      }

let conversation owner = Conversation owner

let developer ~owner ~parent =
  let valid_parent =
    match parent with
    | Developer_parent.Root_action | Developer_parent.Native_invocation _ -> true
    | Developer_parent.Worker_claim { attempt; _ } -> attempt >= 0
  in
  if not valid_parent
  then Error (P.Error.invalid_request "developer source launch has invalid attempt")
  else
    Result.map
      (P.Execution_owner.validate (P.Execution_owner.Developer owner))
      ~f:(fun () -> Developer { owner; parent })
;;

let conversation_owner = function
  | Conversation owner -> Some owner
  | Developer _ -> None
;;

let developer_owner = function
  | Conversation _ -> None
  | Developer { owner; _ } -> Some owner
;;

let developer_parent = function
  | Conversation _ -> None
  | Developer { parent; _ } -> Some parent
;;
