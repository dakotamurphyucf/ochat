open! Core
open Authoring_context_service_core

(** Current owner/grant/selection checks live outside the short ledger gate.
    The final variants are bounded and nonyielding; the full variants may
    perform joined Store verification and must complete before publication. *)
val source_host_error : Sources.Error.t -> Error.t

val upload_host_error : Upload.Error.t -> Error.t

val check_context_owner
  :  ?ledger:Ledger.t
  -> t
  -> context_entry
  -> actor:Operator_authorization.t
  -> checkpoint:(unit -> unit)
  -> (unit, Error.t) result

val check_context_selection_now
  :  ?ledger:Ledger.t
  -> t
  -> context_entry
  -> actor:Operator_authorization.t
  -> (unit, Error.t) result

val with_current
  :  ?feature:Selected_feature.t
  -> ?final_check:(unit -> (unit, Error.t) result)
  -> ?publish:('a -> ('a, Error.t) result)
  -> t
  -> actor:Operator_authorization.t
  -> connection:Connection_context.t
  -> expected:P.Authoring.Context.Expected.t
  -> checkpoint:(unit -> unit)
  -> f:(Current.t -> checkpoint:(unit -> unit) -> ('a, Error.t) result)
  -> ('a, Error.t) result

(** [feature] chooses the negotiated sibling; absent means Core authoring.
    [final_check] adds a bounded nonyielding current host selection check
    immediately before the final gate, after the ordinary actor/source/context
    recheck. [publish] runs inside that gate only after the exact connection,
    context and expected generation are compared again. It may perform a pure
    compare-and-swap of already prepared immutable values only after comparing
    BOTH analysis cache heads, the certified analysis-index head, the
    provisional-index head and the retained document-capture head borrowed
    at request admission; any changed head rejects the result. It constructs
    no new graph, parser result or wire response. The borrowed [Current] token
    has expired by then; neither callback may use it or perform I/O, wait or
    invoke a yielding checkpoint. An error publishes nothing. *)

val view_of_context
  :  t
  -> Ledger.t
  -> id:P.Id.Authoring_context.t
  -> identity:Ledger.Identity.t
  -> origin:P.Authoring.Context.origin
  -> checkpoint:(unit -> unit)
  -> (P.Authoring.Context.t, Error.t) result

val checked_response : t -> W.Response.t -> (W.Response.t, Error.t) result
