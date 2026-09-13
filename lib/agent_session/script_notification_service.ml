open Core
module P = Agent_protocol
module Ops = Chat_response.Notification_operations

type host =
  { create :
      P.Job.launch_owner
      -> P.Invocation.observer
      -> correlation:Ops.correlation
      -> completion:P.Completion.t
      -> wake:P.Completion.wake
      -> disclosure_pins:(string * string) list
      -> (int * P.Delivery.t, P.Error.t) result
  ; get :
      P.Job.launch_owner
      -> P.Invocation.observer
      -> P.Id.Delivery.t
      -> (P.Delivery.t, P.Error.t) result
  ; select :
      P.Job.launch_owner -> P.Invocation.observer -> int list -> (unit, P.Error.t) result
  ; abort : P.Job.launch_owner -> int -> unit
  }

type t = { host : host }

type commit_state =
  | Open
  | Prepared
  | Committed

type scope =
  { service : t
  ; owner : P.Job.launch_owner
  ; source : P.Invocation.observer
  ; jobs : Script_job_service.scope option
  ; disclosure_pins : (string * string) list
  ; active : bool Atomic.t
  ; mutable issued : (int * P.Delivery.t) list
  ; mutable commit_state : commit_state
  }

let create ~host = { host }
let message result = Result.map_error result ~f:(fun error -> error.P.Error.message)

let check scope =
  match Atomic.get scope.active, scope.commit_state with
  | true, Open -> Ok ()
  | _ -> Error "notification scope has ended"
;;

let validate_work scope = function
  | None | Some (P.Invocation.Subscription _) -> Ok ()
  | Some (Job id) ->
    (match scope.jobs with
     | Some jobs -> Script_job_service.validate_notification_access jobs id
     | None -> Error "notification job references require a scoped job service")
;;

let abort scope receipt =
  Eio.Cancel.protect (fun () -> scope.service.host.abort scope.owner receipt);
  scope.issued
  <- List.filter scope.issued ~f:(fun (other, _) -> not (Int.equal receipt other))
;;

let abort_all scope = List.iter scope.issued ~f:(fun (receipt, _) -> abort scope receipt)

let with_scope service ~owner ~source ~selected ~jobs ~error f =
  let open Result.Let_syntax in
  let%bind disclosure_pins =
    Chat_response.Background_request.capability_pins selected
    |> Result.map_error ~f:(fun failure -> error failure.P.Error.message)
  in
  let scope =
    { service
    ; owner
    ; source
    ; jobs
    ; disclosure_pins
    ; active = Atomic.make true
    ; issued = []
    ; commit_state = Open
    }
  in
  Exn.protect
    ~finally:(fun () -> Atomic.set scope.active false)
    ~f:(fun () ->
      let result =
        try
          let open Result.Let_syntax in
          let%bind value = f scope in
          match scope.commit_state with
          | Committed -> Ok value
          | Open | Prepared ->
            Error (error "notification transaction was not acknowledged")
        with
        | exn ->
          let backtrace = Stdlib.Printexc.get_raw_backtrace () in
          abort_all scope;
          Stdlib.Printexc.raise_with_backtrace exn backtrace
      in
      match result with
      | Ok _ -> result
      | Error _ ->
        abort_all scope;
        result)
;;

let moderator_transaction scope : Ops.transaction =
  let open Result.Let_syntax in
  { handlers =
      { publish =
          (fun ~correlation ~completion ~wake ->
            let%bind () = check scope in
            let%bind () = validate_work scope correlation.Ops.work in
            Eio.Cancel.protect (fun () ->
              let%map receipt, value =
                scope.service.host.create
                  scope.owner
                  scope.source
                  ~correlation
                  ~completion
                  ~wake
                  ~disclosure_pins:scope.disclosure_pins
                |> message
              in
              scope.issued <- (receipt, value) :: scope.issued;
              receipt, value))
      ; get =
          (fun id ->
            let%bind () = check scope in
            let%bind value =
              scope.service.host.get scope.owner scope.source id |> message
            in
            let%bind () = validate_work scope value.context.work in
            let%map () = check scope in
            value)
      ; rollback = abort scope
      }
  ; prepare =
      (fun receipts ->
        let%bind () = check scope in
        let%bind () =
          List.fold_result scope.issued ~init:() ~f:(fun () (receipt, value) ->
            match List.mem receipts receipt ~equal:Int.equal with
            | true -> validate_work scope value.context.work
            | false -> Ok ())
        in
        let%bind () =
          scope.service.host.select scope.owner scope.source receipts |> message
        in
        let%map () = check scope in
        scope.commit_state <- Prepared;
        fun () ->
          scope.commit_state <- Committed;
          scope.issued <- [])
  }
;;

let validate_disclosure ~current_capabilities (delivery : P.Delivery.t) =
  match delivery.disclosure_pins with
  | None ->
    Error
      (P.Error.create
         Permission_denied
         ~message:"notification has no captured disclosure ceiling"
         ~retryable:false
         ())
  | Some pins ->
    Chat_response.Background_request.rebind_capabilities
      ~pins
      ~capabilities:current_capabilities
;;
