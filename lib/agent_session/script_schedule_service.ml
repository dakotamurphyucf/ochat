open Core
module P = Agent_protocol
module S = P.Schedule
module Ops = Chat_response.Schedule_operations

type host =
  { create :
      P.Job.launch_owner
      -> P.Invocation.observer
      -> delay_ms:int
      -> payload:Jsonaf.t
      -> misfire:S.misfire
      -> (int * S.t, P.Error.t) result
  ; stage :
      P.Job.launch_owner
      -> P.Invocation.observer
      -> previous:S.t option
      -> next:S.t
      -> (int, P.Error.t) result
  ; get :
      P.Job.launch_owner
      -> P.Invocation.observer
      -> P.Id.Schedule.t
      -> (S.t, P.Error.t) result
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
  ; active : bool Atomic.t
  ; mutable issued : int list
  ; mutable dependencies : int list
  ; mutable commit_state : commit_state
  }

let create ~host = { host }
let message result = Result.map_error result ~f:(fun error -> error.P.Error.message)

let check scope =
  match Atomic.get scope.active, scope.commit_state with
  | true, Open -> Ok ()
  | _ -> Error "schedule scope has ended"
;;

let rollback scope receipt =
  Eio.Cancel.protect (fun () -> scope.service.host.abort scope.owner receipt);
  scope.issued <- List.filter scope.issued ~f:(fun other -> not (Int.equal receipt other));
  scope.dependencies
  <- List.filter scope.dependencies ~f:(fun other -> not (Int.equal receipt other))
;;

let abort_all scope = List.iter scope.issued ~f:(rollback scope)

let with_scope service ~owner ~source ~error f =
  let scope =
    { service
    ; owner
    ; source
    ; active = Atomic.make true
    ; issued = []
    ; dependencies = []
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
          | Open | Prepared -> Error (error "schedule transaction was not acknowledged")
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

let get scope id =
  let open Result.Let_syntax in
  let%bind () = check scope in
  scope.service.host.get scope.owner scope.source id |> message
;;

let stage scope ~previous ~next =
  let open Result.Let_syntax in
  let%bind () = check scope in
  Eio.Cancel.protect (fun () ->
    let%map receipt =
      scope.service.host.stage scope.owner scope.source ~previous ~next |> message
    in
    scope.issued <- receipt :: scope.issued;
    receipt)
;;

let cancel scope id =
  let open Result.Let_syntax in
  let%bind previous = get scope id in
  let next =
    match previous.status with
    | S.Scheduled | Delivering -> { previous with status = S.Cancelled }
    | Delivered | Cancelled | Failed _ -> previous
  in
  stage scope ~previous:(Some previous) ~next
;;

let receipt_set scope receipts =
  let open Result.Let_syntax in
  let seen = Hash_set.create (module Int) in
  let%map () =
    List.fold_result receipts ~init:() ~f:(fun () receipt ->
      match Hash_set.mem seen receipt, List.mem scope.issued receipt ~equal:Int.equal with
      | false, true ->
        Hash_set.add seen receipt;
        Ok ()
      | _ -> Error "schedule receipts are duplicated or foreign")
  in
  seen
;;

let retain_dependencies scope receipts =
  let open Result.Let_syntax in
  let%bind () = check scope in
  let%map _ = receipt_set scope receipts in
  scope.dependencies <- receipts
;;

let moderator_transaction scope : Ops.transaction =
  { handlers =
      { create =
          (fun ~delay_ms ~payload ~misfire ->
            let open Result.Let_syntax in
            let%bind () = check scope in
            Eio.Cancel.protect (fun () ->
              let%map receipt, schedule =
                scope.service.host.create
                  scope.owner
                  scope.source
                  ~delay_ms
                  ~payload
                  ~misfire
                |> message
              in
              scope.issued <- receipt :: scope.issued;
              receipt, schedule))
      ; get = get scope
      ; cancel = cancel scope
      ; rollback = rollback scope
      }
  ; prepare =
      (fun receipts ->
        let open Result.Let_syntax in
        let%bind () = check scope in
        let%bind explicit = receipt_set scope receipts in
        let ordered = List.rev scope.issued in
        let%bind () =
          match
            List.equal Int.equal receipts (List.filter ordered ~f:(Hash_set.mem explicit))
          with
          | true -> Ok ()
          | false -> Error "schedule receipts are out of execution order"
        in
        let%bind combined = receipt_set scope (receipts @ scope.dependencies) in
        let selected = List.filter ordered ~f:(Hash_set.mem combined) in
        let%bind () =
          scope.service.host.select scope.owner scope.source selected |> message
        in
        let%map () = check scope in
        scope.commit_state <- Prepared;
        fun () ->
          scope.commit_state <- Committed;
          scope.issued <- [];
          scope.dependencies <- [])
  }
;;
