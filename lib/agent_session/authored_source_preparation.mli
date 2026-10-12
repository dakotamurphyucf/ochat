open Core

(** One immutable captured specialist source and its final delegated role-selected
    revision. The pair is constructed once from the genuine parent revision by
    existing Runtime_script_preparation.with_authored_child. Contains no resource
    owner, switch, Startup value, checkpoint, callback, or new authority. *)
type t

(** Pure captured custody/role preparation, before native setup. Uses the same
    borrowed Startup when supplied, original content policy, and existing parser/
    custody/role checks. [f] borrows the same context through resource construction.
    Absence starts
    one fresh context whose lifetime covers capture AND [f]; it does not end at
    token creation. The immutable token may escape, but neither callback nor
    Startup does. Expected failures return Error; original exceptions and
    cancellation preserve their transport. *)
val with_prepared
  :  startup:Chat_response.Startup_compilation.t option
  -> script_preparation:Runtime_script_preparation.t option
  -> content_limits:Chatmd_prompt_content.Limits.t option
  -> parent:Prompt_revision.t
  -> tool_name:string
  -> env:Eio_unix.Stdenv.base
  -> checkpoint:(unit -> unit)
  -> f:(Chat_response.Startup_compilation.t -> t -> ('a, Agent_protocol.Error.t) Result.t)
  -> ('a, Agent_protocol.Error.t) Result.t

val source : t -> Authored_agent_source.t
val revision : t -> Prompt_revision.t

(** Original immutable semantic preparation policy, for the real resource builder.
    This is not a source grant or native capability. *)
val script_preparation : t -> Runtime_script_preparation.t option
