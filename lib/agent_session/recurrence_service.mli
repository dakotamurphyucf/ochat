open! Core
module P = Agent_protocol
module Admission = Recurrence_native_admission
module Request = Recurrence_native_request

type t

(** Trusted session-owned composition, installed by Factory into Script_tool_calls.
    Every closure resolves the SAME invoking Actor/registered entry. Query includes
    authorized target visibility rendering + final controller basis validation. No controller
    parameter comes from JSON. Capture receives the already active native borrow;
    it must not acquire a second parent lease or force lazy realization. Both query
    and capture run the actual Script_tool_calls metadata admission check outside
    Actor calls before/after IO; apply uses only nonyielding exact admission/state
    checks inside the Actor authorizer, never the yielding host check there. *)
val create
  :  state:(unit -> (Session_state.t, P.Error.t) result)
  -> query:
       (admission:Admission.t
        -> request:Recurrence_query.request
        -> (Jsonaf.t, P.Error.t) result)
  -> capture_target:
       (admission:Admission.t
        -> target:Request.Target.t
        -> (Recurrence_target.t, P.Error.t) result)
  -> apply:
       (admission:Admission.t
        -> request:Recurrence_command.t
        -> (Recurrence_command_receipt.t, P.Error.t) result)
  -> t

(** Entire dispatch remains inside this exact admitted native callback. Strict
    request matching and check_now precede every closure. Actor remains the sole
    command publisher; ACK is returned without reinterpreting later revocation
    as an unacknowledged write. Cancellation and unexpected exceptions propagate.

    Fresh Create/Edit captures current target DATA. Exact retry first resolves
    the principal/key receipt + retained acknowledged definition, verifies original
    selector/schedule/policy and explicit CAS, reconstructs the SAME command using
    its retained target, then uses the Actor's digest/receipt check. Never silently
    refresh CAS, recapture different source under an acknowledged key, or issue a
    new execution permit from retained metadata. *)
val run : t -> admission:Admission.t -> request:Request.t -> (Jsonaf.t, P.Error.t) result
