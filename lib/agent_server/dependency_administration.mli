open! Core
module Selection = Agent_protocol.Prompt_dependency_selection

module Failure : sig
  type t =
    | Rejected of Agent_protocol.Error.t
    | Uncertain of Agent_protocol.Error.t
end

(** Trusted host port over the actual shared reload/update owner. It creates no
    source, publication or receipt owner. The host and its callbacks outlive the
    handler; the concrete owner provides closed/busy/uncertain admission. *)
type t

(** Status is a pure installed-metadata query. Update enters the SAME actual
    publication owner as reload, under existing source grants and finite limits.
    Success acknowledges the exact installed mutation; Rejected proves no commit;
    Uncertain makes no rollback or retry claim. Original callback/cancellation
    exceptions propagate, including failures after acknowledged installation.

    The borrowed checkpoint verifies the original authenticated actor and caller
    cancellation. Forward it through long preparation and immediately prewriter;
    do not call it after durable acknowledgment to relabel a committed operation
    as rejected. No actor proof or checkpoint is retained beyond the operation. *)
val create
  :  status:(unit -> (Selection.Status.t, Agent_protocol.Error.t) Result.t)
  -> update:
       (Selection.Update_request.t
        -> checkpoint:(unit -> unit)
        -> (Selection.Update_result.t, Failure.t) Result.t)
  -> t

val status : t -> (Selection.Status.t, Agent_protocol.Error.t) Result.t

val update
  :  t
  -> Selection.Update_request.t
  -> checkpoint:(unit -> unit)
  -> (Selection.Update_result.t, Failure.t) Result.t
