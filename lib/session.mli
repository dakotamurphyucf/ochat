open Core

(** Persistent chat session state.

    A value of type {!t} bundles everything the assistant needs to
    restore a chat between two executions of the binary:

    * the system/user prompt file that seeded the conversation
    * the full message exchange with OpenAI (the {!module:History})
    * an optional lightweight task-list
    * arbitrary key/value metadata
    * a virtual file-system (VFS) root used by tooling

    The module handles **schema migrations** transparently via the
    {!module:Legacy} sub-module.  Every serialised snapshot embeds its
    authoring version; upon deserialisation the value is upgraded to
    {!val:current_version} so callers always operate on the latest
    layout. *)

module History : sig
  (** Ordered list of OpenAI exchange items that form the conversation
      history.  The concrete list type is exposed for convenience but
      callers should treat the list as immutable. *)
  type t = Openai.Responses.Item.t list [@@deriving bin_io, sexp]
end

module Task : sig
  (** Task tracking for “action items” discovered during the chat. *)

  type state =
    | Pending (** Newly created – not started yet.        *)
    | In_progress (** Actively being worked on.               *)
    | Done (** Finished – kept for auditability.       *)
  [@@deriving bin_io, sexp]

  type t =
    { id : string (** 32-hex digest – stable identifier.        *)
    ; title : string (** One-line human-readable description.       *)
    ; state : state (** Current life-cycle state.                  *)
    }
  [@@deriving bin_io, sexp]

  (** [create ?id ?state ~title ()] returns a fresh {!t}.

      • [id] – defaults to an MD5 digest of [Time_ns.now ()] mixed with
        PRNG bits, giving a collision-resistant, *process-local* ID.
      • [state] – defaults to {!Pending}. *)
  val create : ?id:string -> title:string -> ?state:state -> unit -> t
end

(** Schema version emitted by the current binary.  Increment whenever
    the latest {!type:t} becomes incompatible with its previous shape. *)
val current_version : int

(** Latest on-disk representation (post-migration). *)
type t =
  { version : int (** Authoring schema version.                    *)
  ; id : string (** Globally-unique session identifier.          *)
  ; prompt_file : string (** Absolute path of the source prompt file.     *)
  ; local_prompt_copy : string option
    (** Optional prompt copy inside the session directory.    *)
  ; history : History.t
  ; tasks : Task.t list
  ; kv_store : (string * string) list
    (** Arbitrary metadata keyed by user-defined strings.       *)
  ; vfs_root : string (** Root directory for virtual files.           *)
  }
[@@deriving bin_io, sexp]

(** [create ?id ?local_prompt_copy ?history ?tasks ?kv_store ?vfs_root
    ~prompt_file ()] constructs a brand-new session value.

    All optional arguments default to the empty/neutral value except
    [id] which – when omitted – is auto-generated just like in
    {!Task.create}.

    Example – start a session for [docs/prompt.txt]:
    {[
      let open Session in
      let s = create ~prompt_file:"docs/prompt.txt" () in
      ...
    ]} *)
val create
  :  ?id:string
  -> prompt_file:string
  -> ?local_prompt_copy:string
  -> ?history:History.t
  -> ?tasks:Task.t list
  -> ?kv_store:(string * string) list
  -> ?vfs_root:string
  -> unit
  -> t

(** [reset ?prompt_file session] returns **a copy** of [session] with an
    empty {!field:history}.  Use it when the conversation should start
    over while preserving bookkeeping and VFS content.

    The prompt path is overwritten when [prompt_file] is supplied. *)
val reset : ?prompt_file:string -> t -> t

(** [reset_keep_history ?prompt_file session] behaves like {!reset} but
    keeps the current message history intact.  Only the prompt file may
    change. *)
val reset_keep_history : ?prompt_file:string -> t -> t

module Latest : sig
  (** Alias to the latest schema – useful for version-agnostic code. *)
  type nonrec t = t [@@deriving bin_io, sexp]

  val version : int
end

module Legacy : sig
  (** Previous schema versions plus upgrade paths to {!Latest}. *)

  module V0 : sig
    type t =
      { id : string
      ; prompt_file : string
      ; history : History.t
      ; tasks : Task.t list
      ; kv_store : (string * string) list
      ; vfs_root : string
      }
    [@@deriving bin_io, sexp]

    val version : int
  end

  val upgrade_v0 : V0.t -> t

  module V1 : sig
    type t =
      { version : int
      ; id : string
      ; prompt_file : string
      ; history : History.t
      ; tasks : Task.t list
      ; kv_store : (string * string) list
      ; vfs_root : string
      }
    [@@deriving bin_io, sexp]

    val version : int
  end

  val upgrade_v1 : V1.t -> t
end

module Io : sig
  (** Convenience wrappers for `Bin_prot_utils_eio` so that callers can
      persist or restore a session snapshot from within an Eio fiber. *)

  module File : sig
    (** [read path] loads a snapshot produced by {!write}.  The value is
        automatically migrated to the latest schema. *)
    val read : Bin_prot_utils_eio.path -> t

    (** [write path session] serialises [session] to [path] using
        `bin_dump` with a header.  The file is created (0600) or
        truncated atomically. *)
    val write : Bin_prot_utils_eio.path -> t -> unit
  end
end
