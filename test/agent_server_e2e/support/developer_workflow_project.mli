open Core

(** Genuine captured, compiled Project upload for the integrated Developer
    workflow. The returned manifest has one exact immutable source closure;
    [contents] is the authored main member for this revision; callers still
    need the installed upload and rebind grants. *)
val create
  :  contents:string
  -> unit
  -> Agent_protocol.Authoring_bundle.Bundle.Manifest.t * Chatml_source.t list
