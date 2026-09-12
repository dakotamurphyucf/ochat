open Core
module Metadata = Chatmd_shell_spec.Authoring_metadata
module Spec = Chatmd_shell_spec.Extension_spec

type error =
  { code : string
  ; message : string
  }
[@@deriving sexp]

(** Installed corpus metadata used for policy admission, without loading prose.
    The caller obtains it from the compatible installed corpus. This is not the
    corpus builder, retrieval service or token-budget validator. *)
type catalog

val catalog_fingerprint : catalog -> string

val catalog
  :  identity:string
  -> packages:Metadata.help list
  -> topics:(string * Metadata.task list) list
  -> (catalog, error) result

(** Catalog metadata with authored-package ownership for each topic's complete
    prerequisite closure. Auto/preload admission requires all these packages in
    selected authoring metadata, including for directly requested preload topics.
    Missing/duplicate ownership entries or unknown packages reject construction.
    Topics without an ownership entry are installed public references. *)
val catalog_with_ownership
  :  identity:string
  -> packages:Metadata.help list
  -> topics:(string * Metadata.task list) list
  -> topic_packages:(string * string list) list
  -> (catalog, error) result

type t

(** Pure admission plan. [ceiling] is the host-approved registry, or the parent's
    selected registry for a child. Resolution only selects its existing bindings;
    it never constructs, renames, reconnects or widens a tool implementation.
    Omission means Auto. Manual adds no context/tools. Auto/preload require an
    installed compatible catalog and both authentic helper bindings. *)
val resolve
  :  ?policy:Spec.policy
  -> ?catalog:catalog
  -> ceiling:Tool_capability.t
  -> selected_names:string list
  -> unit
  -> (t, error) result

(** Admission from the parsed ChatMD declaration, checking its version and
    retaining the policy source for inspection. An absent declaration uses Auto.
    Host setup can use [resolve] directly when it has no ChatMD source. *)
val resolve_context
  :  ?context:Spec.authoring_context
  -> ?catalog:catalog
  -> ceiling:Tool_capability.t
  -> selected_names:string list
  -> unit
  -> (t, error) result

val policy_source : t -> Chatmd_shell_spec.Source_ref.t option
val policy : t -> Spec.policy
val capabilities : t -> Tool_capability.t
val added_helpers : t -> Tool_capability.reference list
val helper_pointers : t -> (Metadata.helper * string) list
val authoring_tools : t -> (Tool_capability.reference * Metadata.help) list
val inject_primer : t -> bool
val preload_topics : t -> string list
val corpus_identity : t -> string option
val fingerprint : t -> string
