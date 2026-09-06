open Core

type source_kind =
  | Physical
  | Temporary of Workspace_definition.temporary_location
  | Current
[@@deriving compare, equal, sexp]

type canonical_identity =
  { native_path : string
  ; device : int64
  ; inode : int64
  }
[@@deriving compare, equal, sexp]

type cleanup_completion =
  { completed_at : Agent_protocol.Timestamp.t
  ; reason : string
  }
[@@deriving sexp]

type t =
  { id : Agent_protocol.Id.Workspace_instance.t
  ; definition_id : Agent_protocol.Id.Workspace_definition.t option
  ; source_kind : source_kind
  ; configured_root : string option
  ; canonical_root : canonical_identity
  ; conflict_domain : string
  ; access : Workspace_definition.access
  ; cleanup : Workspace_definition.cleanup option
  ; server_created : bool
  ; created_at : Agent_protocol.Timestamp.t
  ; cleanup_completion : cleanup_completion option
  }
[@@deriving sexp]

let with_cleanup_completion t cleanup_completion =
  { t with cleanup_completion = Some cleanup_completion }
;;
