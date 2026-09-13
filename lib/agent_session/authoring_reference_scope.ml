open Core
module P = Agent_protocol
module R = P.Authoring_reference
module Q = Chat_response.Authoring_context

type state =
  | Closed
  | Open of
      { receipts : R.t list
      ; bytes : int
      }

type t =
  { invocation : P.Invocation.context
  ; state : state Atomic.t
  }

let key = Eio.Fiber.create_key ()
let invalid message = Error (P.Error.invalid_request message)

let capture ~invocation_id =
  match Eio.Fiber.get key with
  | Some scope when P.Id.Invocation.equal invocation_id scope.invocation.id -> Some scope
  | None | Some _ -> None
;;

let close scope =
  match Atomic.exchange scope.state Closed with
  | Closed -> []
  | Open state -> state.receipts
;;

let with_scope invocation f =
  let scope = { invocation; state = Atomic.make (Open { receipts = []; bytes = 0 }) } in
  Exn.protect
    ~finally:(fun () -> ignore (close scope : R.t list))
    ~f:(fun () -> Eio.Fiber.with_binding key scope (fun () -> f scope))
;;

let collect invocation f =
  with_scope invocation (fun scope ->
    let result = f () in
    result, close scope)
;;

let annotate scope invocation =
  let open Result.Let_syntax in
  let%bind references =
    match Atomic.get scope.state with
    | Closed -> invalid "authoring reference scope has expired"
    | Open state -> Ok state.receipts
  in
  let%bind () =
    match P.Invocation.equal_context scope.invocation invocation.P.Invocation.context with
    | true -> Ok ()
    | false -> invalid "authoring reference resolution belongs to another invocation"
  in
  let expected =
    match invocation.status with
    | Resolved (Complete value) ->
      List.find references ~f:(fun reference -> R.matches_output reference value)
    | _ -> None
  in
  match expected, invocation.authoring_reference with
  | None, None -> Ok invocation
  | None, Some _ -> invalid "resolution has no matching collected authoring reference"
  | Some reference, _ -> P.Invocation.record_authoring_reference invocation reference
;;

let record scope ~invocation_id ~capability_fingerprint response =
  let open Result.Let_syntax in
  let%bind () =
    match Atomic.get scope.state with
    | Closed -> invalid "authoring reference scope has expired"
    | Open _ -> Ok ()
  in
  let%bind () =
    match P.Id.Invocation.equal scope.invocation.id invocation_id with
    | true -> Ok ()
    | false -> invalid "authoring receipt belongs to another invocation"
  in
  match response.Q.receipt with
  | None -> Ok ()
  | Some receipt ->
    let%bind () =
      match
        String.equal receipt.capability_fingerprint capability_fingerprint
        && String.equal
             receipt.scope
             (R.scope_for
                ~session_id:scope.invocation.session_id
                ~generation:scope.invocation.generation)
        && Q.matches_response receipt response.json
      with
      | true -> Ok ()
      | false -> invalid "authoring receipt differs from its active caller or response"
    in
    let%bind receipt = Q.reference_to_protocol receipt in
    let size = String.length (Jsonaf.to_string (R.to_json receipt)) in
    let rec append () =
      match Atomic.get scope.state with
      | Closed -> invalid "authoring reference scope has expired"
      | Open state as before ->
        (match List.mem state.receipts receipt ~equal:R.equal with
         | true -> Ok ()
         | false
           when List.length state.receipts >= 32 || size > (1024 * 1024) - state.bytes ->
           invalid "authoring reference collection exceeds metadata budget"
         | false ->
           let after =
             Open { receipts = receipt :: state.receipts; bytes = state.bytes + size }
           in
           (match Atomic.compare_and_set scope.state before after with
            | true -> Ok ()
            | false -> append ()))
    in
    append ()
;;
