open! Core
module P = Agent_protocol
module DTO = P.Provider_operator

module Close_state = struct
  type t =
    | Open
    | Closing
    | Closed
end

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
  ; close_state : Close_state.t Atomic.t
  ; close_mutex : Eio.Mutex.t
  }

let create ~dispatch ~receipt ~close =
  { dispatch
  ; receipt
  ; close
  ; close_state = Atomic.make Close_state.Open
  ; close_mutex = Eio.Mutex.create ()
  }
;;

let admission_open t =
  match Atomic.get t.close_state with
  | Open -> true
  | Closing | Closed -> false
;;

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
  | Some t when not (admission_open t) -> Error (protocol_error Closed)
  | Some _ when not (Operator_authorization.is_current actor) ->
    Error (protocol_error Denied)
  | Some t -> t.dispatch ~actor command |> Result.map_error ~f:protocol_error
;;

let receipt t ~actor command =
  match t with
  | None -> Error (protocol_error Unsupported)
  | Some t when not (admission_open t) -> Error (protocol_error Closed)
  | Some _ when not (Operator_authorization.is_current actor) ->
    Error (protocol_error Denied)
  | Some t -> t.receipt ~actor command |> Result.map_error ~f:protocol_error
;;

let close t =
  (* Atomic transition closes admission before waiting on another closing caller;
     the private coordinator alone acknowledges actual callback completion. *)
  ignore (Atomic.compare_and_set t.close_state Close_state.Open Closing : bool);
  Eio.Cancel.protect (fun () ->
    Eio.Mutex.use_ro t.close_mutex (fun () ->
      match Atomic.get t.close_state with
      | Closed -> ()
      | Open | Closing ->
        t.close ();
        Atomic.set t.close_state Closed))
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
