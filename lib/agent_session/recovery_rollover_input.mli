(** Immutable local input preview. Preparing a successor does not adopt any
    queue item or acknowledge a notification wake. *)
open! Core

module P = Agent_protocol

type t

val dispatch_selection
  :  Session_state.t
  -> operation:P.Id.Operation.t
  -> (P.History.entry list * P.Delivery.t list, P.Error.t) result

val capture : Session_state.t -> runtime_admission_open:bool -> (t, P.Error.t) result
val required : t -> History_entry.t list
val entries : t -> P.History.entry list
val wakes : t -> P.Delivery.t list
val wakes_authorized : t -> bool

(** At actual dispatch admission revalidation tolerates unrelated journal
    writes, but requires the same canonical input, selected FIFO and wakes. *)
val check
  :  t
  -> Session_state.t
  -> operation:P.Id.Operation.t
  -> runtime_admission_open:bool
  -> (unit, P.Error.t) result

(** Extend the witness only with the exact newly published rows from the
    selected worker's genuine installed-authoring request. *)
val record_authoring
  :  t
  -> Session_state.t
  -> History_entry.t list
  -> (t, P.Error.t) result

(** Refresh publisher-authorized wakes after the selected worker's acknowledged
    initial notification plan. The FIFO and any unrelated history must match. *)
val record_notifications
  :  t
  -> Session_state.t
  -> History_entry.t list
  -> proposal:Notification_delivery.idle
  -> (t, P.Error.t) result
