open! Core
module P = Agent_protocol
module DTO = P.Provider_operator
module Bridge = Inference_host.Credential_bridge
module Context = Inference_runtime.Context

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
  ; current_model_bridge : unit -> (Bridge.t, DTO.Error.t) Result.t
  ; close : unit -> unit
  ; close_state : Close_state.t Atomic.t
  ; close_mutex : Eio.Mutex.t
  }

let create ~dispatch ~receipt ~current_model_bridge ~close =
  { dispatch
  ; receipt
  ; current_model_bridge
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

let model_unavailable () =
  P.Error.create
    P.Error.Invalid_state
    ~message:"Selected model provider is unavailable."
    ~retryable:false
    ()
;;

let model_denied () =
  P.Error.create
    P.Error.Permission_denied
    ~message:"Selected model provider is unavailable."
    ~retryable:false
    ()
;;

let bridge_error = function
  | Bridge.Error.Denied | Profile Denied -> model_denied ()
  | Invalid_mapping
  | Invalid_environment
  | Invalid_credential
  | Missing_profile
  | Stale_authorization
  | Profile _
  | Lifecycle _
  | Preparation _ -> model_unavailable ()
;;

module Model_selection = struct
  type nonrec t =
    { issuer : t
    ; actor : Operator_authorization.t
    ; target : Inference.Request.Target.t
    ; bridge : Bridge.t
    ; context : Context.t
    }

  let actor t = t.actor
  let target t = t.target

  let check_current t =
    if (not (admission_open t.issuer)) || not (Operator_authorization.is_current t.actor)
    then Error (model_denied ())
    else (
      match t.issuer.current_model_bridge () with
      | Error error -> Error (protocol_error error)
      | Ok current when not (phys_equal current t.bridge) -> Error (model_unavailable ())
      | Ok _ ->
        (match Context.derive t.context ~target:t.target with
         | Error _ -> Error (model_unavailable ())
         | Ok _ ->
           if admission_open t.issuer && Operator_authorization.is_current t.actor
           then (
             match t.issuer.current_model_bridge () with
             | Ok current when phys_equal current t.bridge -> Ok ()
             | Ok _ -> Error (model_unavailable ())
             | Error error -> Error (protocol_error error))
           else Error (model_denied ())))
  ;;
end

let select_model t ~actor ~target =
  if not (admission_open t)
  then Error (protocol_error Closed)
  else if not (Operator_authorization.is_current actor)
  then Error (model_denied ())
  else
    let open Result.Let_syntax in
    let%bind bridge = t.current_model_bridge () |> Result.map_error ~f:protocol_error in
    let principal = (Operator_authorization.principal actor).id in
    let%bind context =
      Bridge.resolve bridge ~principal:(P.Id.Principal.to_string principal) target
      |> Result.map_error ~f:bridge_error
    in
    let%bind () =
      if Inference.Request.Target.equal (Context.target context) target
      then Ok ()
      else Error (model_unavailable ())
    in
    let selected : Model_selection.t = { issuer = t; actor; target; bridge; context } in
    let%map () = Model_selection.check_current selected in
    selected
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
