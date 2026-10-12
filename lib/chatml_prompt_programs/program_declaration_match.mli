open Core

(** Retained original source slot matched to an authentic current declaration.
    Registration context and qualified ID may differ; exact path, authored and
    effective revisions, range and extracted body must agree. No role selection,
    graph restore, compiler admission or native authority. *)
type t

val original : t -> Program_binding.t
val current : t -> Program_binding.t
val compiler_contract : t -> Chatml_source.Digest.t
val declaration : t -> Prompt.Chat_markdown.Declaration.t

(** Bound retained original count, raw-declaration integer overflow and pair-product
    overflow before scanning. Raw syntax count is not the retained-program ceiling.
    Stream each matching row to [f] without retaining an all-pairs projection or
    equating comparison-pair count with the retained-program ceiling. Poll source
    membership, hashing and every pair; caller's checkpoint owns the complete
    operation work bound. Callback exceptions propagate unchanged. *)
val fold
  :  Program_inventory.t
  -> parsed:Prompt.Chat_markdown.Parsed.t
  -> limits:Program_inventory.Limits.t
  -> checkpoint:(unit -> unit)
  -> init:'a
  -> f:('a -> t -> 'a)
  -> ('a, Program_inventory.Error.t) Result.t
