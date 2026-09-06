(** Transport projections for configured workspace definitions. *)

type kind =
  | Physical
  | Temporary
[@@deriving compare, equal, sexp]

type temporary_location =
  | System_tmp
  | Session_dir
[@@deriving compare, equal, sexp]

type cleanup =
  | On_session_stop
  | On_session_delete
  | Retain
[@@deriving compare, equal, sexp]

type access =
  | Read_only
  | Shared_write
  | Exclusive
[@@deriving compare, equal, sexp]

type overflow =
  | Reject
  | Queue
[@@deriving compare, equal, sexp]

type availability =
  | Available
  | Unavailable of { reason : string }
[@@deriving compare, equal, sexp]

type prompt_limit =
  { prompt_id : Id.Prompt_definition.t
  ; max_root_agents : int
  ; overflow : overflow
  }
[@@deriving sexp]

type t =
  { id : Id.Workspace_definition.t
  ; name : string
  ; kind : kind
  ; temporary_location : temporary_location option
  ; cleanup : cleanup option
  ; access : access
  ; conflict_domain : string option
  ; prompt_limits : prompt_limit list
  ; availability : availability
  }
[@@deriving sexp]

(** [to_json t] encodes a workspace summary without native filesystem paths. *)
val to_json : t -> Jsonaf.t

(** [of_json json] decodes and validates a workspace summary. *)
val of_json : Jsonaf.t -> (t, Error.t) result

module List_request : sig
  type t =
    { page : Page.Request.t
    ; kind : kind option
    ; access : access option
    ; available : bool option
    }
  [@@deriving sexp]

  (** [to_json t] encodes workspace-list filters and pagination. *)
  val to_json : t -> Jsonaf.t

  (** [of_json json] decodes workspace-list filters and pagination. *)
  val of_json : Jsonaf.t -> (t, Error.t) result
end

module Get_request : sig
  type t = { workspace_id : Id.Workspace_definition.t } [@@deriving sexp]

  (** [to_json t] encodes a workspace lookup request. *)
  val to_json : t -> Jsonaf.t

  (** [of_json json] decodes a workspace lookup request. *)
  val of_json : Jsonaf.t -> (t, Error.t) result
end
