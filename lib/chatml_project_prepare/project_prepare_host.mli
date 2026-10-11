open Core
module Roots = Chatml_local_roots
module Git = Chatml_git_host

(** Borrowed capabilities; the caller retains their lifetimes through joined
    preparation. The finite exact-route map never retains ambient lookup. *)
type t = private
  { anchors : Roots.Anchor.t list
  ; git_store : Git.Store.t option
  ; git_grants : Git.Grant.t String.Map.t
  }

val create
  :  Roots.Anchor.t list
  -> git_store:Git.Store.t option
  -> git_grants:Git.Grant.t list
  -> max_grants:int
  -> max_grant_bytes:int
  -> t Or_error.t
