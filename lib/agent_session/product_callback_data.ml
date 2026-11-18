open! Core
module C = Callback_cleanup
module Slot = C.Completion_snapshot.Failure_slot
module Diagnostic = Cleanup_diagnostic

module Manager = struct
  type t =
    | Unbound
    | Bound of Chat_response.Moderator_manager.t option
end

module Slot_diagnostic = struct
  type t =
    { completion : C.Completion.t
    ; kind : Slot.kind
    ; diagnostic : Diagnostic.t
    }
end

module Retained_operation = struct
  type t =
    { domain : int
    ; manager : Chat_response.Moderator_manager.t option
    ; receipt : C.Operation.t
    ; mutable slots : Slot_diagnostic.t list
    ; mutable terminal_diagnostic : Diagnostic.t option
    }
end

type t =
  { domain : int
  ; mutable manager : Manager.t
  ; mutable operations : Retained_operation.t list
  }

let conflict message = Agent_protocol.Error.create Conflict ~message ~retryable:false ()

let check_domain domain =
  if Int.equal domain (Stdlib.Domain.self () :> int)
  then Ok ()
  else Error (conflict "Product callback observations belong to another domain")
;;

let create () =
  { domain = (Stdlib.Domain.self () :> int); manager = Unbound; operations = [] }
;;

let bind_manager t ~manager =
  Result.ok_or_failwith
    (Result.map_error (check_domain t.domain) ~f:(fun error -> error.message));
  match t.manager with
  | Unbound -> t.manager <- Bound manager
  | Bound _ -> invalid_arg "Product callback Manager already bound"
;;

let slot_equal (slot : Slot_diagnostic.t) ~completion ~kind =
  phys_equal slot.completion completion && Slot.equal_kind slot.kind kind
;;

let observe_slot (operation : Retained_operation.t) completion slot =
  let kind = Slot.kind slot in
  match List.find operation.slots ~f:(fun slot -> slot_equal slot ~completion ~kind) with
  | Some actual -> actual
  | None ->
    let actual : Slot_diagnostic.t =
      { completion; kind; diagnostic = Diagnostic.create [ Slot.failure slot ] }
    in
    operation.slots <- operation.slots @ [ actual ];
    actual
;;

let observe_terminal (operation : Retained_operation.t) terminal =
  let failure =
    match terminal with
    | None | Some (Ok (Ok ())) -> None
    | Some (Ok (Error failure)) -> Some (C.Failure.Protocol failure)
    | Some (Error (exn, backtrace)) -> Some (C.Failure.Raised (exn, backtrace))
  in
  match failure, operation.terminal_diagnostic with
  | None, _ -> None
  | Some _, Some diagnostic -> Some diagnostic
  | Some failure, None ->
    let diagnostic = Diagnostic.create [ failure ] in
    operation.terminal_diagnostic <- Some diagnostic;
    Some diagnostic
;;

module Operation_snapshot = struct
  type t =
    { original : Retained_operation.t
    ; registration_closed : bool
    ; callbacks : C.Cleanup_snapshot.t option
    ; terminal : C.Operation.terminal_outcome option
    ; slots : Slot_diagnostic.t list
    ; terminal_diagnostic : Diagnostic.t option
    }

  let receipt t = t.original.receipt
  let registration_closed t = t.registration_closed
  let callbacks t = t.callbacks
  let terminal t = t.terminal
  let terminal_diagnostic t = t.terminal_diagnostic

  let completion_diagnostic t ~completion ~slot =
    List.find_map t.slots ~f:(fun actual ->
      Option.some_if (slot_equal actual ~completion ~kind:slot) actual.diagnostic)
  ;;

  let diagnostics t =
    List.map t.slots ~f:(fun actual -> actual.Slot_diagnostic.diagnostic)
    @ Option.to_list t.terminal_diagnostic
  ;;

  let observe (original : Retained_operation.t) =
    let open Result.Let_syntax in
    let%bind () = check_domain original.domain in
    let%bind observed =
      match original.manager with
      | None -> C.Operation.snapshot original.receipt
      | Some manager ->
        Callback_cleanup_data.Operation.snapshot_for_manager original.receipt ~manager
    in
    let%map terminal = C.Operation.terminal original.receipt in
    let callbacks = Option.map original.manager ~f:(fun _ -> observed) in
    let slots =
      Option.value_map callbacks ~default:[] ~f:(fun callbacks ->
        C.Cleanup_snapshot.claims callbacks
        |> List.concat_map ~f:(fun claim ->
          let completion = C.Completion_observation.completion claim in
          C.Completion_snapshot.failure_slots (C.Completion_observation.snapshot claim)
          |> List.map ~f:(observe_slot original completion)))
    in
    { original
    ; registration_closed = C.Cleanup_snapshot.registration_closed observed
    ; callbacks
    ; terminal
    ; slots
    ; terminal_diagnostic = observe_terminal original terminal
    }
  ;;

  let refresh t = observe t.original

  let positive t =
    t.registration_closed
    && Option.value_map t.callbacks ~default:true ~f:(fun callbacks ->
      (not (C.Cleanup_snapshot.pending callbacks))
      && List.is_empty (C.Cleanup_snapshot.failures callbacks))
    &&
    match t.terminal with
    | Some (Ok (Ok ())) -> true
    | None | Some (Ok (Error _) | Error _) -> false
  ;;
end

module Snapshot = struct
  type t = { operations : Operation_snapshot.t list }

  let operations t = t.operations
end

let snapshot t =
  let open Result.Let_syntax in
  let%bind () = check_domain t.domain in
  let%bind () =
    match t.manager with
    | Unbound -> Error (conflict "Product callback Manager has not been bound")
    | Bound _ -> Ok ()
  in
  let%map observed =
    List.map t.operations ~f:(fun original ->
      Result.map (Operation_snapshot.observe original) ~f:(fun snapshot ->
        original, snapshot))
    |> Result.all
  in
  let retained =
    List.filter observed ~f:(fun (_, snapshot) ->
      not (Operation_snapshot.positive snapshot))
  in
  t.operations <- List.map retained ~f:fst;
  { Snapshot.operations = List.map retained ~f:snd }
;;

let enroll t ~operation receipt =
  let open Result.Let_syntax in
  let%bind () = check_domain t.domain in
  let%bind manager =
    match t.manager with
    | Unbound -> Error (conflict "Product callback Manager has not been bound")
    | Bound manager -> Ok manager
  in
  let%bind actual = C.Operation.operation receipt in
  let%bind () =
    if
      Agent_protocol.Id.Operation.equal actual.id operation.Agent_protocol.Operation.id
      && Int.equal actual.generation operation.generation
      && Agent_protocol.Operation.equal_kind actual.kind operation.kind
    then Ok ()
    else Error (conflict "Product callback receipt differs from its original worker")
  in
  let%map _ = snapshot t in
  if
    not
      (List.exists t.operations ~f:(fun original ->
         phys_equal original.Retained_operation.receipt receipt))
  then
    t.operations
    <- t.operations
       @ [ { Retained_operation.domain = t.domain
           ; manager
           ; receipt
           ; slots = []
           ; terminal_diagnostic = None
           }
         ]
;;

module Observation = struct
  type nonrec t = t

  module Operation_snapshot = Operation_snapshot
  module Snapshot = Snapshot

  let snapshot = snapshot
end
