open Core
module P = Agent_protocol
module I = External_ingress

type t =
  { session_id : P.Id.Session.t
  ; generation : int
  ; revision : int64
  ; source : P.Invocation.observer
  ; producer : P.Id.Principal.t
  ; previous : I.t
  ; candidate : I.t
  ; receipt : I.receipt
  }

type decision =
  | Duplicate of I.receipt
  | Enqueue of t

let subscription (state : Session_state.t) registration =
  List.find state.subscriptions ~f:(fun subscription ->
    P.Id.Subscription.equal subscription.context.id registration.I.context.subscription_id)
  |> Result.of_option ~error:(P.Error.invalid_request "missing ingress subscription")
;;

let prepare
      ~state
      ~source
      ~producer
      ~registration_id
      ~namespace
      ~key
      ~payload
      ~now
      ~create_event_id
  =
  let open Result.Let_syntax in
  let%bind previous =
    List.find state.Session_state.ingress_registrations ~f:(fun value ->
      P.Id.Capability.equal value.context.id registration_id)
    |> Result.of_option
         ~error:
           (P.Error.create
              Permission_denied
              ~message:"ingress registration is not available"
              ~retryable:false
              ())
  in
  let%bind subscription = subscription state previous in
  let%map admitted =
    I.prepare
      previous
      ~session_id:state.identity.session_id
      ~generation:state.identity.generation
      ~source
      ~subscription
      ~producer
      ~namespace
      ~key
      ~payload
      ~now
      ~create_event_id
  in
  match admitted with
  | I.Duplicate receipt -> Duplicate receipt
  | Accepted (candidate, receipt) ->
    Enqueue
      { session_id = state.identity.session_id
      ; generation = state.identity.generation
      ; revision = state.counters.revision
      ; source
      ; producer
      ; previous
      ; candidate
      ; receipt
      }
;;

let frame t = I.delivery_frame t.candidate t.receipt

let revalidate ~state ~now t =
  let open Result.Let_syntax in
  let%bind () =
    match
      P.Id.Session.equal state.Session_state.identity.session_id t.session_id
      && state.identity.generation = t.generation
      && Int64.equal state.counters.revision t.revision
      && List.exists state.ingress_registrations ~f:(I.equal t.previous)
    with
    | true -> Ok ()
    | false ->
      Error
        (P.Error.create
           Conflict
           ~message:"ingress admission changed before queue save"
           ~retryable:true
           ())
  in
  let%bind subscription = subscription state t.previous in
  let%bind admitted =
    I.prepare
      t.previous
      ~session_id:t.session_id
      ~generation:t.generation
      ~source:t.source
      ~subscription
      ~producer:t.producer
      ~namespace:t.previous.context.namespace
      ~key:t.receipt.key
      ~payload:t.receipt.payload
      ~now
      ~create_event_id:(fun () -> t.receipt.id)
  in
  match admitted with
  | Accepted (registration, receipt) -> Ok (registration, receipt)
  | Duplicate _ ->
    Error (P.Error.invalid_request "new ingress proposal became a duplicate")
;;
