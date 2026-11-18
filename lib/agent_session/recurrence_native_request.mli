open! Core
module P = Agent_protocol

module Target : sig
  (** Selectors only. Neither accepts artifacts, private pins, credentials,
      arbitrary source carriers, caller identity or an execution permit. *)
  type t =
    | Existing_session of
        { session : P.Session_ref.t
        ; input : P.Session.Message_content.t
        }
    | Authored_child of
        { name : string
        ; mode : P.Run.Mode.t
        }
  [@@deriving sexp_of]
end

module Basis : sig
  (** All fields echoed from an actual read. Source/lifecycle are None only for
      safe Pause/Cancel. Values are CAS DATA, never permission. *)
  type t = private
    { generation : int
    ; expected_catalog_revision : int64
    ; expected_control_revision : int64 option
    ; source : P.Run_source.t option
    ; expected_lifecycle_revision : P.Session_lifecycle.Revision.t option
    ; key : P.Idempotency_key.t
    }
end

module Capture : sig
  type t = private
    { schedule : Recurrence_definition.Schedule.t
    ; target : Target.t
    ; policy : Recurrence_policy.t
    }
end

type t =
  | Create of
      { id : P.Id.Recurrence.t
      ; basis : Basis.t
      ; capture : Capture.t
      }
  | Edit of
      { id : P.Id.Recurrence.t
      ; basis : Basis.t
      ; capture : Capture.t
      }
  | Pause of
      { id : P.Id.Recurrence.t
      ; basis : Basis.t
      }
  | Resume of
      { id : P.Id.Recurrence.t
      ; basis : Basis.t
      }
  | Cancel of
      { id : P.Id.Recurrence.t
      ; basis : Basis.t
      }
  | List
  | Get of P.Id.Recurrence.t
  | Status of P.Id.Recurrence.t
  | History of P.Id.Recurrence.t

(** One strict decoder also supplies each native schema. Bound encoded bytes,
    depth, nodes and fields before recursion; reject duplicate/unknown fields,
    noncanonical decimal revisions, unsupported target/mode/policy/rule variants.
    Existing 256KiB input limit remains independent of enclosing request limit. *)
val decode
  :  effect_kind:Recurrence_native_admission.Effect.t
  -> Jsonaf.t
  -> (t, P.Error.t) result

val decode_text_exn : string -> Jsonaf.t
val parameters : Recurrence_native_admission.Effect.t -> Jsonaf.t
