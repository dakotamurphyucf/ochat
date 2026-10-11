open Core

(** The only filesystem/cancellation owner. Each selected member is read once
    into an invocation-local table; source/callback/control publication is atomic
    at the result boundary. No mutable table or capability escapes. *)
val capture
  :  Chatml_project.t
  -> root:[ `Close | Eio.Fs.dir_ty ] Eio.Path.t
  -> request:Chatml_project.Selection.request
  -> limits:Chatml_local_capture_budget.Limits.t
  -> checkpoint:(unit -> unit)
  -> (Chatml_local_capture_catalog.t, Chatml_local_capture_error.t) Result.t
