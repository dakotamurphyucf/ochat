open Core

(** Pure selected-entry recomposition for a current parser declaration whose
    inline script body changed after an authentic host Build was selected.
    This is comparison/compilation input, not a native read, a new route grant,
    or a replacement for the host's current selected Build witness. Both
    Analyze and Prefix subsequently use [Chatmd_authoring_analysis.Script.assess]
    to bind the derived entry to the SAME genuine Parsed declaration; the host
    reselects the original Build/contract under its final publication gate. *)

module Error : sig
  type t =
    | Invalid_selected_entry
    | Body_limit
    | Lock_identity_unavailable
    | Recomposition of Chatml_project_build.Error.t
end

(** [selected] was admitted by the current original/request actor policy for
    this actual declaration. The caller already proved parser membership and
    current root/import source custody. Only the selected Build's exact
    [Prepared.entry] source may be replaced with [Declaration.script_body]; all
    other immutable captures, including its old lock and dependency closure,
    are retained. A matching body returns the selected Build unchanged. A
    changed body uses the existing bounded [Build.Effective.recompose] with the
    actual selected contract and a collision-checked new lock ID. No parser,
    filesystem access, source discovery, inference or initializer runs here.
    [max_body_bytes] is positive and checked before hashing/copy; [limits] and
    caller [checkpoint] bound the complete source walk and recomposition.
    Checkpoint/cancellation exceptions propagate with their original identity. *)
val prepare
  :  Authoring_script_selection.Selected.t
  -> declaration:Prompt.Chat_markdown.Declaration.t
  -> max_body_bytes:int
  -> limits:Chatml_project_build.Limits.t
  -> checkpoint:(unit -> unit)
  -> (Chatml_project_build.t, Error.t) Result.t
