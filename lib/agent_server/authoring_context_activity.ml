open! Core
module P = Agent_protocol
module Ledger = Chatml_authoring_context
module Sources = Authoring_source_host
module Upload = Authoring_upload_host

type phase =
  | Active
  | Revoking
  | Closed

type context_owner =
  | Host_project of Sources.Context.t
  | Host_document of Sources.Document_context.t
  | Uploaded_project of Upload.Context_capture.t
  | Supplied_document of Operator_authorization.t

type context_entry =
  { id : P.Id.Authoring_context.t
  ; identity : Ledger.Identity.t
  ; original_actor : Operator_authorization.t
  ; origin : P.Authoring.Context.origin
  ; owner : context_owner
  ; mutable view : P.Authoring.Context.t
  }

type connection_entry =
  { connection : Connection_context.t
  ; lifecycle_gate : Eio.Mutex.t
  ; mutable idle : unit Eio.Promise.t option
  ; mutable idle_waker : unit Eio.Promise.u option
  ; mutable negotiation : Connection_context.Authoring_negotiation.t option
  ; mutable phase : phase
  ; mutable active : Eio.Cancel.t list
  ; mutable roots : (P.Id.Authoring_root.t * Sources.Root.t) list
  ; mutable contexts : context_entry list
  ; mutable retired_owners : context_owner list
  }

let create connection =
  { connection
  ; lifecycle_gate = Eio.Mutex.create ()
  ; idle = None
  ; idle_waker = None
  ; negotiation = None
  ; phase = Revoking
  ; active = []
  ; roots = []
  ; contexts = []
  ; retired_owners = []
  }
;;

let find_context entry id =
  List.find entry.contexts ~f:(fun context -> P.Id.Authoring_context.equal context.id id)
;;

let valid_negotiation entry =
  Option.exists entry.negotiation ~f:(fun negotiation ->
    Connection_context.current_authoring_negotiation entry.connection negotiation)
;;

let with_lifecycle entry f =
  let outcome =
    Eio.Mutex.use_rw ~protect:false entry.lifecycle_gate (fun () ->
      match f () with
      | result -> `Returned result
      | exception exn -> `Raised (exn, Stdlib.Printexc.get_raw_backtrace ()))
  in
  match outcome with
  | `Returned result -> result
  | `Raised (exn, backtrace) -> Stdlib.Printexc.raise_with_backtrace exn backtrace
;;

let active_entries : connection_entry list Eio.Fiber.key = Eio.Fiber.create_key ()

let in_active entry =
  Option.value (Eio.Fiber.get active_entries) ~default:[]
  |> List.exists ~f:(phys_equal entry)
;;

let with_active entry f =
  let stack = Option.value (Eio.Fiber.get active_entries) ~default:[] in
  Eio.Fiber.with_binding active_entries (entry :: stack) f
;;

let close_owner = function
  | Host_project owner -> Sources.Context.close_and_wait owner
  | Host_document owner -> Sources.Document_context.close_and_wait owner
  | Uploaded_project _ | Supplied_document _ -> ()
;;
