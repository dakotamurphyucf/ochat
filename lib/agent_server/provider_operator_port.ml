open! Core
module P = Agent_protocol
module DTO = P.Provider_operator

type t =
  { dispatch :
      actor:Operator_authorization.t
      -> P.Command.t
      -> (P.Method_result.t, DTO.Error.t) Result.t
  ; receipt :
      actor:Operator_authorization.t
      -> P.Command.t
      -> (P.Command_receipt.t, DTO.Error.t) Result.t
  ; close : unit -> unit
  ; mutable closed : bool
  }

let create ~dispatch ~receipt ~close = { dispatch; receipt; close; closed = false }

type factory = sw:Eio.Switch.t -> server_id:P.Id.Server.t -> (t, P.Error.t) Result.t

let protocol_error provider_error =
  let code =
    match provider_error with
    | DTO.Error.Unsupported -> P.Error.Method_not_found
    | Denied | Account_denied | Model_denied -> Permission_denied
    | Busy -> Command_queue_full
    | Store_unavailable -> Persistence_error
    | Submission_uncertain | Flow_interrupted -> Interrupted
    | Missing_profile -> Invalid_request
    | Closed -> Server_shutting_down
    | Invalid_request -> Invalid_request
    | Flow_expired | Challenge_unavailable | Network -> Invalid_state
  in
  P.Error.create
    code
    ~message:"Provider operator request could not complete."
    ~retryable:(DTO.Error.equal provider_error Busy)
    ~data:(`Object [ "provider_error", DTO.Error.to_json provider_error ])
    ()
;;

let dispatch t ~actor command =
  match t with
  | None -> Error (protocol_error Unsupported)
  | Some t when t.closed -> Error (protocol_error Closed)
  | Some _ when not (Operator_authorization.is_current actor) ->
    Error (protocol_error Denied)
  | Some t -> t.dispatch ~actor command |> Result.map_error ~f:protocol_error
;;

let receipt t ~actor command =
  match t with
  | None -> Error (protocol_error Unsupported)
  | Some t when t.closed -> Error (protocol_error Closed)
  | Some _ when not (Operator_authorization.is_current actor) ->
    Error (protocol_error Denied)
  | Some t -> t.receipt ~actor command |> Result.map_error ~f:protocol_error
;;

let close t =
  if not t.closed
  then (
    t.closed <- true;
    t.close ())
;;

let is_provider_command = function
  | P.Command.Provider_setup _
  | Provider_status _
  | Provider_login_begin _
  | Provider_login_challenge _
  | Provider_login_cancel _
  | Provider_logout _
  | Provider_select _
  | Provider_configure_environment _ -> true
  | _ -> false
;;
