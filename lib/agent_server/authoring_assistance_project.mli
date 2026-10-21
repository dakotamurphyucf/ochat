open! Core
module Error = Authoring_context_service_core.Error

(** Decode the actual selected Project declaration retained by a genuine
    host-admitted Build, using the authoritative Chatml_project decoder. The
    declaration Source.Id comes only from Prepared.provenance and is resolved
    in that exact Prepared source inventory; no display path, basename, host
    read or catalog union chooses it. [None] is an implicit single-file Build,
    which has no Project environments or declared paths. The bounded JSON
    decoder may have one finite noninterruptible stage; callbacks before/after
    it and throughout domain conversion preserve cancellation identity. A
    missing/invalid selected declaration is an unavailable selection, never a
    fabricated empty project. *)
val selected_project
  :  Chatml_project_build.t
  -> max_project_bytes:int
  -> checkpoint:(unit -> unit)
  -> (Chatml_project.t option, Error.t) result
