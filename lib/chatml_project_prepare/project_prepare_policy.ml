open Core
module Roots = Chatml_local_roots
module Git = Chatml_git_host
module Build = Chatml_project_build

(** Literal stage ceilings, without mutable accounting or authority. *)
type t =
  { roots : Roots.Limits.t
  ; local : Chatml_local_capture.Limits.t
  ; git_objects : Chatml_git_catalog.Limits.t
  ; git_native : Git.Limits.t
  ; composition : Build.Limits.t
  }

let create ~roots ~local ~git_objects ~git_native ~composition =
  { roots; local; git_objects; git_native; composition }
;;

let root_limits t = t.roots
let local_limits t = t.local
