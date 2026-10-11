open Core
module Captured = Chatml_source

(** Pure, immutable source-owned failures. No live source lookup, execution
    authority, inference state, complete inventory, or retained control callback. *)

module Site : sig
  (** Exact captured bytes plus their validated identity/revision-owned range. *)
  type t [@@deriving sexp_of]

  (** Validate only when a diagnostic or task construction site is needed;
      this is not an expression-wide annotation pass. Checkpoint exceptions
      propagate unchanged. Invalid coordinates reject rather than clamp. *)
  val create : Captured.t -> span:Source.span -> checkpoint:(unit -> unit) -> t Or_error.t

  val source : t -> Captured.t
  val location : t -> Captured.Location.t
end

module Related : sig
  type t [@@deriving sexp_of]

  (** Message retains at most16KiB. *)
  val create : Site.t -> message:string -> t

  val site : t -> Site.t
  val message : t -> string
end

(** Read-only projections preserve existing message/span consumers. [span] is
    derived from [site] when owned; an unowned span is legacy metadata and never
    grants a source association. At most8 related sites and16KiB message bytes
    are retained. Record construction goes through [create]. *)
type t = private
  { message : string
  ; span : Source.span option
  ; site : Site.t option
  ; related : Related.t list
  }
[@@deriving sexp_of]

val create : ?span:Source.span -> ?site:Site.t -> ?related:Related.t list -> string -> t

(** Fill absent ownership only. An existing dependency/callback site wins. *)
val with_fallback : t -> Site.t option -> t

(** Render at most16KiB, selecting the site's exact captured bytes. Labels,
    excerpts, indentation/carets and intermediate buffers are bounded before
    allocation. [show_source=false] preserves short single-file formatting.
    Unowned ranges remain unlocated in this renderer; [render_legacy] can
    validate them against explicitly supplied bytes. No file is invented.
    Checkpoint exceptions propagate; no checkpoint is retained. *)
val render : ?show_source:bool -> t -> label:string -> checkpoint:(unit -> unit) -> string

(** Compatibility rendering against one explicitly supplied string. Owned
    failures always keep their exact captured source; supplied text is used only
    to validate legacy unowned ranges, never to replace an owned dependency.
    Invalid/unavailable legacy ranges remain unlocated. *)
val render_legacy
  :  t
  -> source:string
  -> label:string
  -> checkpoint:(unit -> unit)
  -> string
