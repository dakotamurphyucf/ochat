open Core
module M = Chat_response.Moderation

type t =
  { active : bool Atomic.t
  ; observer : Agent_protocol.Invocation.observer option
  ; moderator : (bool Atomic.t * Agent_protocol.Invocation.observer) option
  ; prepare : M.Tool_call.t -> (M.Tool_moderation.t option, string) result
  }

let key = Eio.Fiber.create_key ()
let capture () = Option.join (Eio.Fiber.get key)
let with_context context f = Eio.Fiber.with_binding key context f

let with_handler ?active_moderator ~observer ~prepare f =
  let active = Atomic.make true in
  let moderator =
    match active_moderator with
    | Some owner -> Some (active, owner)
    | None ->
      Option.bind (capture ()) ~f:(fun context ->
        match Atomic.get context.active with
        | true -> context.moderator
        | false -> None)
  in
  let context = { active; observer; moderator; prepare } in
  Exn.protect
    ~finally:(fun () -> Atomic.set context.active false)
    ~f:(fun () -> with_context (Some context) f)
;;

let current () =
  match capture () with
  | Some context when Atomic.get context.active -> Ok context
  | None | Some _ -> Error "native tool moderation scope is not active"
;;

let observer t = t.observer

let active_moderator () =
  match capture () with
  | Some { active; moderator = Some (owner_active, owner); _ }
    when Atomic.get active && Atomic.get owner_active -> Some owner
  | _ -> None
;;

let prepare t call =
  match Atomic.get t.active with
  | false -> Error "native tool moderation scope has ended"
  | true -> t.prepare call
;;
