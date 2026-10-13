open Core
module Roots = Chatml_local_roots
module Git = Chatml_git_host
module Build = Chatml_project_build

(** Literal stage ceilings, without mutable accounting or authority. *)
type t = private
  { roots : Roots.Limits.t
  ; local : Chatml_local_capture.Limits.t
  ; git_objects : Chatml_git_catalog.Limits.t
  ; git_native : Git.Limits.t
  ; composition : Build.Limits.t
  }

val create
  :  roots:Roots.Limits.t
  -> local:Chatml_local_capture.Limits.t
  -> git_objects:Chatml_git_catalog.Limits.t
  -> git_native:Git.Limits.t
  -> composition:Build.Limits.t
  -> t

val root_limits : t -> Roots.Limits.t
val local_limits : t -> Chatml_local_capture.Limits.t
