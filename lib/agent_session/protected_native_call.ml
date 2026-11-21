open! Core
module P = Agent_protocol
module C = Chat_response.Tool_capability

type rejection =
  | Missing
  | Invalid
  | Denied
  | Conflict
  | Limit
  | Expired_cursor
  | Unavailable

type result_selection =
  | Acknowledged
  | Rejected of rejection
  | Deliver of
      (call:History_entry.t
       -> output:History_entry.t
       -> Inference_runtime.Protected_delivery.t)

type live =
  { invocation : P.Invocation.context
  ; binding : C.binding
  ; input : Jsonaf.t
  ; selection : result_selection option
  ; allow_delivery : bool
  }

let key : live option Atomic.t Eio.Fiber.key = Eio.Fiber.create_key ()

let with_live_input ~allow_delivery ~(invocation : P.Invocation.t) ~binding ~input f =
  let cell =
    Atomic.make
      (Some
         { invocation = invocation.context
         ; binding
         ; input
         ; selection = None
         ; allow_delivery
         })
  in
  Exn.protect
    ~finally:(fun () -> Atomic.set cell None)
    ~f:(fun () ->
      Eio.Fiber.with_binding key cell (fun () ->
        let result = f () in
        let live = Atomic.exchange cell None in
        result, Option.bind live ~f:(fun live -> live.selection)))
;;

let with_input ~invocation ~binding ~input f =
  with_live_input ~allow_delivery:true ~invocation ~binding ~input f
;;

let with_ack_input ~(invocation : P.Invocation.t) ~binding ~input f =
  if
    P.Invocation.equal_origin invocation.context.origin Script
    && Option.is_some invocation.context.parent_invocation
    && Option.is_none invocation.context.call_entry_id
    && Jsonaf.exactly_equal invocation.context.input `Null
  then with_live_input ~allow_delivery:false ~invocation ~binding ~input f
  else invalid_arg "protected authored write requires its scrubbed child invocation"
;;

let current (call : P.Host_invocation.context) binding =
  Option.bind (Eio.Fiber.get key) ~f:(fun cell ->
    Option.bind (Atomic.get cell) ~f:(fun live ->
      match call.owner with
      | Conversation owner
        when P.Id.Session.equal owner.session_id live.invocation.session_id
             && Int.equal owner.generation live.invocation.generation
             && P.Id.Invocation.equal call.id live.invocation.id
             && phys_equal live.binding binding -> Some live
      | Conversation _ | Developer _ -> None))
;;

let input call ~binding = Option.map (current call binding) ~f:(fun live -> live.input)

let result call ~binding =
  Option.bind (current call binding) ~f:(fun live -> live.selection)
;;

let select (invocation : P.Invocation.t) ~binding selection =
  let unavailable () =
    Error (P.Error.invalid_request "protected invocation result unavailable")
  in
  match Eio.Fiber.get key with
  | None -> unavailable ()
  | Some cell ->
    (match Atomic.get cell with
     | Some live as prior
       when phys_equal live.binding binding
            && P.Invocation.equal_context live.invocation invocation.context
            && Option.is_none live.selection
            &&
            match selection with
            | Deliver _ -> live.allow_delivery
            | Acknowledged | Rejected _ -> true ->
       if
         Atomic.compare_and_set cell prior (Some { live with selection = Some selection })
       then Ok ()
       else unavailable ()
     | None | Some _ -> unavailable ())
;;
