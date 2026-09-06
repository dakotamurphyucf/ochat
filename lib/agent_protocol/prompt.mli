(** Transport projections for configured ChatMD prompts. *)

type availability =
  | Available
  | Unavailable of { reason : string }
[@@deriving compare, equal, sexp]

type t =
  { id : Id.Prompt_definition.t
  ; name : string
  ; description : string option
  ; enabled : bool
  ; availability : availability
  ; current_revision : Id.Prompt_revision.t option
  ; allowed_workspaces : Id.Workspace_definition.t list
  ; permission_profile : string
  ; runtime_policy : string option
  }
[@@deriving sexp]

(** [to_json t] encodes a path-redacted prompt summary. *)
val to_json : t -> Jsonaf.t

(** [of_json json] decodes a prompt summary. *)
val of_json : Jsonaf.t -> (t, Error.t) result

module List_request : sig
  type t =
    { page : Page.Request.t
    ; enabled : bool option
    ; available : bool option
    }
  [@@deriving sexp]

  (** [to_json t] encodes prompt-list filters and pagination. *)
  val to_json : t -> Jsonaf.t

  (** [of_json json] decodes prompt-list filters and pagination. *)
  val of_json : Jsonaf.t -> (t, Error.t) result
end

module Get_request : sig
  type t = { prompt_id : Id.Prompt_definition.t } [@@deriving sexp]

  (** [to_json t] encodes a prompt lookup request. *)
  val to_json : t -> Jsonaf.t

  (** [of_json json] decodes a prompt lookup request. *)
  val of_json : Jsonaf.t -> (t, Error.t) result
end
