open Core

(** Genuine original-capture relationship to a declaration-selected project.
    Immutable intent only; the same Scope later supplies actual native grants. *)
type t

(** Require this exact declaration/input in the supplied original capture.
    Project filename is relative to its declaring input, independently of src.
    Resolve member spelling through the shared Source_loader helper. Outside-root
    project intent remains valid but needs an explicit additional host grant and
    never implies external entry affinity. No reads or artifact-path authority. *)
val create
  :  Prompt_source_owner.Capture.t
  -> declaration:Prompt.Chat_markdown.Declaration.t
  -> max_locator_bytes:int
  -> max_path_components:int
  -> checkpoint:(unit -> unit)
  -> t Or_error.t

val root : t -> Chatml_project_prepare.Root.t
val environment : t -> string option
val source_member : t -> Source.Portable_path.t option

(** Verify exact supplied body/digest and positive external member's captured raw
    bytes. Only SAME original-anchor containment and an actual longest-prefix policy win
    yield a root_member candidate. Borrow additional anchors without retaining
    capabilities; unrelated grants preserve the original member. Lexical policy
    comparison gives no native identity proof or new permission.
    Actual217 catalog admission then canonicalizes equality, rejects disagreement,
    or retains the honest host entry when that member is absent. No reread. *)
val entry
  :  t
  -> source:Chatml_source.t
  -> additional_anchors:Chatml_local_roots.Anchor.t list
  -> root_limits:Chatml_local_roots.Limits.t
  -> checkpoint:(unit -> unit)
  -> Chatml_project_prepare.Request.Entry.t Or_error.t
