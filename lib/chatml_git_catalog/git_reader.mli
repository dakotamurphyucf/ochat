open Core
open Git_catalog_model

(** Exact verified object memo owned by one live201 bracket and semantic budget.
    Multiple members/packages share this memo when the facade session supplies it;
    no value survives the session callback. *)
type t

val create : Chatml_git_host.Repository.t -> budget:Git_catalog_budget.t -> t

val object_
  :  t
  -> Chatml_git_host.Object_id.t
  -> kind:Git_object.Kind.t
  -> (Git_object.t, Error.t) Result.t

val root : t -> (Chatml_git_host.Object_id.t, Error.t) Result.t

val subtree
  :  t
  -> root:Chatml_git_host.Object_id.t
  -> components:string list
  -> path:Path.t
  -> (Chatml_git_host.Object_id.t, Error.t) Result.t

val directory
  :  t
  -> root:Chatml_git_host.Object_id.t
  -> components:string list
  -> path:Path.t
  -> (Git_tree.t, Error.t) Result.t

(** None ONLY means exact final tree-entry absence. Present entry whose object is
    missing or any invalid ancestor/mode is an error, never optional omission. *)
val blob
  :  t
  -> root:Chatml_git_host.Object_id.t
  -> path:Path.t
  -> (string option, Error.t) Result.t
