(** Selected canonical-history wrapper over In_memory_stream's single tool
    engine. Captured payloads are retained and prepared by the selected adapter;
    there is no DTO replay, ambient provider selection or automatic retry. *)

open! Core

type post =
  sw:Eio.Switch.t
  -> dir:Eio.Fs.dir_ty Eio.Path.t
  -> inputs:Openai.Responses.Item.t list
  -> Openai.Responses.Response.t

(** Generation arguments are explicit overrides of the selected Context.
    Omission inherits. A supplied legacy [post] transport rejects before effects;
    tests use a selected synthetic Adapter instead. *)
val run_entries
  :  ctx:Eio_unix.Stdenv.base Ctx.t
  -> allocator:History_entry.Allocator.t
  -> ?temperature:float
  -> ?max_output_tokens:int
  -> ?tools:Openai.Responses.Request.Tool.t list
  -> ?reasoning:Openai.Responses.Request.Reasoning.t
  -> ?fork_depth:int
  -> ?history_compaction:bool
  -> ?response_dir:Eio.Fs.dir_ty Eio.Path.t
  -> ?post:post
  -> ?model:Openai.Responses.Request.model
  -> tool_tbl:(string, Ochat_function.runner) Hashtbl.t
  -> History_entry.t list
  -> History_entry.t list

module For_testing : sig
  (** [retry_request ~sleep ~f] runs [f] and retries response parsing failures
      at most five times. [sleep] receives delays of [1.], [2.], through [5.]
      seconds before the corresponding retry. *)
  val retry_request : sleep:(float -> unit) -> f:(unit -> 'a) -> 'a
end
