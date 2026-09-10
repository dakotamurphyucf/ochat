(** Private host queue frame for accepted external data. Structural decoding is
    not authorization: the actor must match the retained registration/receipt,
    current source/generation and prior claims before projecting script data. *)
type t = private
  { registration_id : Agent_protocol.Id.Capability.t
  ; event_id : Agent_protocol.Id.Ingress_event.t
  ; subscription_id : Agent_protocol.Id.Subscription.t
  ; epoch : int
  ; namespace : string
  ; payload : Jsonaf.t
  }

val create
  :  registration_id:Agent_protocol.Id.Capability.t
  -> event_id:Agent_protocol.Id.Ingress_event.t
  -> subscription_id:Agent_protocol.Id.Subscription.t
  -> epoch:int
  -> namespace:string
  -> payload:Jsonaf.t
  -> (t, string) result

val equal : t -> t -> bool
val capture : t -> Chatml.Chatml_lang.value
val decode : Chatml.Chatml_lang.value -> (t option, string) result

(** Project only an Internal_event containing external_data JSON. Epoch is a
    decimal string so the JSON/ChatML float bridge cannot round authority metadata.
    Helper payload remains nested under data, never a native event constructor. *)
val script_event : Chatml.Chatml_lang.value -> (Chatml.Chatml_lang.value, string) result
