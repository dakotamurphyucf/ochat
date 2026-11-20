open! Core
module P = Agent_protocol
module C = Chat_response.Tool_capability

type result_selection =
  | Acknowledged
  | Deliver of
      (call:History_entry.t
       -> output:History_entry.t
       -> Inference_runtime.Protected_delivery.t)

type live =
  { invocation : P.Invocation.context
  ; binding : C.binding
  ; input : Jsonaf.t
  ; selection : result_selection option
  }

let key : live option Atomic.t Eio.Fiber.key = Eio.Fiber.create_key ()

let with_input ~(invocation : P.Invocation.t) ~binding ~input f =
  let cell =
    Atomic.make
      (Some { invocation = invocation.context; binding; input; selection = None })
  in
  Exn.protect
    ~finally:(fun () -> Atomic.set cell None)
    ~f:(fun () ->
      Eio.Fiber.with_binding key cell (fun () ->
        let result = f () in
        let live = Atomic.exchange cell None in
        result, Option.bind live ~f:(fun live -> live.selection)))
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

let selected call ~binding =
  Option.exists (current call binding) ~f:(fun live -> Option.is_some live.selection)
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
            && Option.is_none live.selection ->
       if
         Atomic.compare_and_set cell prior (Some { live with selection = Some selection })
       then Ok ()
       else unavailable ()
     | None | Some _ -> unavailable ())
;;
