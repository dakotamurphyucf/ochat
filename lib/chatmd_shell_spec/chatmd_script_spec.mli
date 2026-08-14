open! Core

(** Canonical ChatML script declarations used by shell extensions. *)

type kind =
  | Moderator
  | Shell_matcher
  | Shell_reviewer
  | Shell_before_interceptor
  | Shell_after_interceptor
  | Shell_effect_analyzer
  | Shell_audit_filter
[@@deriving sexp, compare, equal, hash, bin_io, jsonaf]

type source =
  | Inline of string
  | Src of
      { path : string
      ; source_text : string
      }
[@@deriving sexp, compare, equal, hash, bin_io, jsonaf]

type limits =
  { wall_time : Duration.t
  ; fuel : int
  ; max_tasks : int
  ; max_value_bytes : Duration.bytes
  ; max_output_bytes : Duration.bytes
  ; max_array_items : int
  ; max_depth : int
  }
[@@deriving sexp, compare, equal, hash, bin_io, jsonaf]

type t =
  { id : string
  ; language : string
  ; kind : kind
  ; source : source
  ; source_ref : Source_ref.t
  ; source_sha256 : string
  ; limits : limits
  }
[@@deriving sexp, compare, equal, hash, bin_io, jsonaf]

(** [default_limits] bounds one ChatML extension invocation. *)
val default_limits : limits

(** [kind_of_string source value] parses one closed script-kind name. *)
val kind_of_string : Source_ref.t -> string -> (kind, Diagnostic.t) result

(** [kind_to_string kind] returns the ChatMD spelling of [kind]. *)
val kind_to_string : kind -> string

(** [source_text t] returns the exact bytes compiled as ChatML source. *)
val source_text : t -> string

(** [qualify t] qualifies [t.id] using its declaration namespace. *)
val qualify : t -> t
