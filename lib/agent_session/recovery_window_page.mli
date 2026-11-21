(** Bounded lineage summaries. No content, member counts or archive locations
    are disclosed. The caller must prove current transcript and session access. *)
open! Core

module P = Agent_protocol
module Window = Recovery_window_record

type summary = P.Recovery_query.Summary.t =
  { scope : P.Recovery_window.Scope.t
  ; state : Window.state
  }
[@@deriving sexp_of]

type signer

(** Create once per physical actor. Reconstructing an actor expires its cursors. *)
val create : unit -> signer

(** An omitted generation selects the current one. The selected generation
    and optional state are exact filters. An append to the active window
    preserves paging. Changed lineage, authorization origin or lifecycle
    requires a refresh. The maximum page size is 50. *)
val list
  :  signer
  -> Session_state.t
  -> actor:Operator_authorization.t
  -> generation:int option
  -> state:Window.state option
  -> P.Page.Request.t
  -> (summary P.Page.t, P.Error.t) result

(** The actor has already checked the actual foreground operation and its
    transcript authority. Its operation identity confines cursor replay. *)
val list_for_origin
  :  signer
  -> Session_state.t
  -> origin:int
  -> operation:P.Id.Operation.t option
  -> generation:int option
  -> state:Window.state option
  -> P.Page.Request.t
  -> (summary P.Page.t, P.Error.t) result
