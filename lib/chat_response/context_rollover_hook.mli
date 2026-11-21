open! Core

(** Body-free events for the optional installed moderator hook. The caller owns
    the actor's phase claim and validates the returned proposal before save. *)
module P = Agent_protocol

type pressure =
  | Unknown
  | Within_budget
  | Near_limit
  | Over_limit

type checkpoint =
  | Missing
  | Checkpointed of Jsonaf.t list
  | Archive_only

type t =
  | Before of
      { intent : P.Id.Operation.t
      ; window : P.Recovery_window.Scope.t
      ; expected_revision : int64
      ; pressure : pressure
      ; checkpoint : checkpoint
      }
  | After of
      { intent : P.Id.Operation.t
      ; previous : P.Recovery_window.Scope.t
      ; window : P.Recovery_window.Scope.t
      ; cut_revision : int64
      ; checkpoint : checkpoint
      }

type continuation =
  | Continue
  | Wait
  | Finish
[@@deriving equal]

module Decision : sig
  type event := t

  type t =
    | Veto
    | Proceed of
        { hints : Jsonaf.t list option
        ; continuation : continuation option
        }
    | Followup of continuation

  (** Checks the output variant matches the exact event phase and bounds serialized
      metadata. The owner subsequently checks and admits all reference types. *)
  val of_value
    :  ?control:Chatml.Chatml_lang.execution_control
    -> event:event
    -> Chatml.Chatml_lang.value
    -> (t, string) result
end

val to_value
  :  ?control:Chatml.Chatml_lang.execution_control
  -> t
  -> Chatml.Chatml_lang.value
