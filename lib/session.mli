open Core

(** Persistent chat session state.

    A value of type {!t} bundles everything the assistant needs to
    restore a chat between two executions of the binary:

    * the system/user prompt file that seeded the conversation
    * the full message exchange with OpenAI (the {!module:History})
    * an optional lightweight task-list
    * an optional persisted moderator snapshot
    * arbitrary key/value metadata
    * a virtual file-system (VFS) root used by tooling

    {!module:Legacy} defines frozen historical schemas and explicit upgrade
    functions. Use [Session_store] for migration-aware file loading;
    {!module:Io} reads only the current production schema. *)

module History : sig
  (** Ordered list of OpenAI exchange items that form the conversation
      history.  The concrete list type is exposed for convenience but
      callers should treat the list as immutable. *)
  type t = History_entry.t list [@@deriving bin_io, sexp]
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

module Snapshot = Chatml.Chatml_value_codec.Snapshot

module Moderator_snapshot : sig
  (** Persisted moderator runtime state and durable overlay data.

      For the restore and effective-history semantics that use this snapshot,
      see [docs-src/chatml-safe-point-and-effective-history.md]. *)

  module Item : sig
    type t =
      { id : string
      ; value : Snapshot.t
      }
    [@@deriving bin_io, sexp]
  end

  module Overlay : sig
    type replacement =
      { target_id : string
      ; item : Item.t
      }
    [@@deriving bin_io, sexp]

    type t =
      { prepended_system_items : Item.t list
      ; appended_items : Item.t list
      ; replacements : replacement list
      ; deleted_item_ids : string list
      ; halted_reason : string option
      }
    [@@deriving bin_io, sexp]

    val empty : t
  end

  type t =
    { script_id : string
    ; script_source_hash : string
    ; current_state : Snapshot.t
    ; queued_internal_events : Snapshot.t list
    ; halted : bool
    ; overlay : Overlay.t
    }
  [@@deriving bin_io, sexp]

  val create
    :  script_id:string
    -> script_source_hash:string
    -> ?current_state:Snapshot.t
    -> ?queued_internal_events:Snapshot.t list
    -> ?halted:bool
    -> ?overlay:Overlay.t
    -> unit
    -> t
end

module Moderator_state : sig
  module Identity_snapshot : sig
    module Inserted : sig
      type t =
        { entry_id : History_entry.Id.t
        ; change_id : int
        ; value : Snapshot.t
        ; script_label : string option
        }
      [@@deriving bin_io, sexp]
    end

    module Replacement : sig
      type t =
        { target_id : History_entry.Id.t
        ; change_id : int
        ; value : Snapshot.t
        ; script_label : string option
        }
      [@@deriving bin_io, sexp]
    end

    module Tombstone : sig
      type t =
        { target_id : History_entry.Id.t
        ; change_id : int
        }
      [@@deriving bin_io, sexp]
    end

    type t =
      { script_id : string
      ; script_source_hash : string
      ; current_state : Snapshot.t
      ; queued_internal_events : Snapshot.t list
      ; halted : bool
      ; revision : int
      ; next_change_id : int
      ; prepended_items : Inserted.t list
      ; appended_items : Inserted.t list
      ; replacements : Replacement.t list
      ; tombstones : Tombstone.t list
      ; halted_reason : string option
      }
    [@@deriving bin_io, sexp]
  end

  type t =
    { legacy_snapshot : Moderator_snapshot.t option
    ; identity_snapshot : Identity_snapshot.t option
    ; extensions : (string * Snapshot.t) list
    }
  [@@deriving bin_io, sexp]

  val of_legacy : Moderator_snapshot.t option -> t
end

(** Typed security-domain state for ChatMD shell runtimes.

    These values intentionally use only dependency-light persisted forms so
    the session schema does not depend on the shell executor implementation.
    Runtime modules translate between these records and their richer in-memory
    representations. *)
module Shell_state : sig
  module Request_kind : sig
    type t =
      | Structured
      | Script_file
      | Raw_shell
    [@@deriving bin_io, sexp]
  end

  module Approval_scope : sig
    type t =
      | Exact_session
      | Prefix_session of { prefix : string list }
      | Durable_exact
    [@@deriving bin_io, sexp]
  end

  module Reviewer : sig
    type t =
      { source : string
      ; reviewer_id : string option
      }
    [@@deriving bin_io, sexp]
  end

  module Approval_grant : sig
    type persisted =
      { grant_id : string
      ; manifest_sha256 : string
      ; runtime_id : string
      ; request_kind : Request_kind.t
      ; command_sha256 : string
      ; executable_sha256 : string
      ; argv : string list
      ; argv_prefix : string list option
      ; cwd_sha256 : string
      ; environment_sha256 : string
      ; stdin_sha256 : string option
      ; stdin_bytes : int
      ; script_sha256 : string option
      ; scope : Approval_scope.t
      ; session_id : string option
      ; user_id : string option
      ; host_id : string option
      ; created_at_ns : int64
      ; expires_at_ns : int64 option
      ; last_used_at_ns : int64 option
      ; reviewer : Reviewer.t
      ; revoked_at_ns : int64 option
      ; revocation_reason : string option
      }
    [@@deriving bin_io, sexp]
  end

  module Manifest_grant : sig
    type persisted =
      { grant_id : string
      ; manifest_sha256 : string
      ; canonical_source_root : string
      ; repository_identity : string option
      ; source_sha256 : string
      ; signer : string option
      ; issuer : string option
      ; audience : string list
      ; schema_version : int
      ; builtin_versions : (string * string) list
      ; imported_source_sha256 : (string * string) list
      ; session_id : string option
      ; user_id : string option
      ; host_id : string option
      ; created_at_ns : int64
      ; expires_at_ns : int64 option
      ; revoked_at_ns : int64 option
      ; revocation_reason : string option
      }
    [@@deriving bin_io, sexp]
  end

  module Extension_snapshot : sig
    type t =
      { extension_id : string
      ; extension_kind : string
      ; runtime_id : string
      ; manifest_sha256 : string
      ; source_sha256 : string
      ; state : Snapshot.t
      ; captured_at_ns : int64
      }
    [@@deriving bin_io, sexp]
  end

  module Interrupted_request : sig
    type t =
      { request_id : string
      ; runtime_id : string
      ; manifest_sha256 : string
      ; request_kind : Request_kind.t
      ; command_sha256 : string
      ; redacted_command : string
      ; cwd_sha256 : string
      ; effects : string list
      ; interrupted_at_ns : int64
      ; reason : string
      ; audit_sequence : int64 option
      ; retryable : bool
      }
    [@@deriving bin_io, sexp]
  end

  type t =
    { manifest_grants : Manifest_grant.persisted list
    ; approval_grants : Approval_grant.persisted list
    ; extension_snapshots : Extension_snapshot.t list
    ; last_audit_sequence : int64 option
    ; interrupted_requests : Interrupted_request.t list
    }
  [@@deriving bin_io, sexp]

  val empty : t
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
  ; next_history_sequence : int
  ; tasks : Task.t list
  ; moderator_state : Moderator_state.t
  ; shell_state : Shell_state.t
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
  -> ?next_history_sequence:int
  -> ?tasks:Task.t list
  -> ?moderator_snapshot:Moderator_snapshot.t
  -> ?moderator_state:Moderator_state.t
  -> ?shell_state:Shell_state.t
  -> ?kv_store:(string * string) list
  -> ?vfs_root:string
  -> unit
  -> t

(** [reset ?prompt_file session] returns **a copy** of [session] with an
    empty {!field:history} and no persisted moderator snapshot. Use it when
    the conversation should start
    over while preserving bookkeeping and VFS content.

    The prompt path is overwritten when [prompt_file] is supplied. *)
val reset : ?prompt_file:string -> t -> t

(** [reset_keep_history ?prompt_file session] behaves like {!reset} but
    keeps the current message history intact. The moderator snapshot is still
    cleared because a resumed prompt run must instantiate a fresh moderator
    runtime. Only the prompt file may change. *)
val reset_keep_history : ?prompt_file:string -> t -> t

module Latest : sig
  (** Alias to the latest schema – useful for version-agnostic code. *)
  type nonrec t = t [@@deriving bin_io, sexp]

  val version : int
end

module Legacy : sig
  (** Previous schema versions plus upgrade paths to {!Latest}. *)

  module Raw_history : sig
    type t = Openai.Responses.Item.t list [@@deriving bin_io, sexp]
  end

  module V0 : sig
    type t =
      { id : string
      ; prompt_file : string
      ; history : Raw_history.t
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
      ; history : Raw_history.t
      ; tasks : Task.t list
      ; kv_store : (string * string) list
      ; vfs_root : string
      }
    [@@deriving bin_io, sexp]

    val version : int
  end

  val upgrade_v1 : V1.t -> t

  module V2 : sig
    type t =
      { version : int
      ; id : string
      ; prompt_file : string
      ; local_prompt_copy : string option
      ; history : Raw_history.t
      ; tasks : Task.t list
      ; kv_store : (string * string) list
      ; vfs_root : string
      }
    [@@deriving bin_io, sexp]

    val version : int
  end

  val upgrade_v2 : V2.t -> t

  module V3 : sig
    type t =
      { version : int
      ; id : string
      ; prompt_file : string
      ; local_prompt_copy : string option
      ; history : Raw_history.t
      ; tasks : Task.t list
      ; moderator_snapshot : Moderator_snapshot.t option
      ; kv_store : (string * string) list
      ; vfs_root : string
      }
    [@@deriving bin_io, sexp]

    val version : int
  end

  val v3_of_session : t -> V3.t
  val session_of_v3 : V3.t -> t
  val v3_of_v0 : V0.t -> V3.t
  val v3_of_v1 : V1.t -> V3.t
  val v3_of_v2 : V2.t -> V3.t
end

module V4 : sig
  val version : int

  module Moderator_state : sig
    module Identity_snapshot : sig
      module Inserted : sig
        type t =
          { entry_id : History_entry.Id.t
          ; change_id : int
          ; value : Snapshot.t
          ; script_label : string option
          }
        [@@deriving bin_io, sexp]
      end

      module Replacement : sig
        type t =
          { target_id : History_entry.Id.t
          ; change_id : int
          ; value : Snapshot.t
          ; script_label : string option
          }
        [@@deriving bin_io, sexp]
      end

      module Tombstone : sig
        type t =
          { target_id : History_entry.Id.t
          ; change_id : int
          }
        [@@deriving bin_io, sexp]
      end

      type t =
        { script_id : string
        ; script_source_hash : string
        ; current_state : Snapshot.t
        ; queued_internal_events : Snapshot.t list
        ; halted : bool
        ; revision : int
        ; next_change_id : int
        ; prepended_items : Inserted.t list
        ; appended_items : Inserted.t list
        ; replacements : Replacement.t list
        ; tombstones : Tombstone.t list
        ; halted_reason : string option
        }
      [@@deriving bin_io, sexp]
    end

    type t =
      { legacy_snapshot : Moderator_snapshot.t option
      ; identity_snapshot : Identity_snapshot.t option
      ; extensions : (string * Snapshot.t) list
      }
    [@@deriving bin_io, sexp]

    val of_legacy : Moderator_snapshot.t option -> t
  end

  type t =
    { version : int
    ; id : string
    ; prompt_file : string
    ; local_prompt_copy : string option
    ; history : History_entry.t list
    ; next_history_sequence : int
    ; tasks : Task.t list
    ; moderator_state : Moderator_state.t
    ; kv_store : (string * string) list
    ; vfs_root : string
    }
  [@@deriving bin_io, sexp]

  (** [allocator t] restores the runtime allocator from [t]'s next unused
      sequence. *)
  val allocator : t -> (History_entry.Allocator.t, string) result

  val validate : t -> (unit, string) result
  val of_v0 : Legacy.V0.t -> (t, string) result
  val of_v1 : Legacy.V1.t -> (t, string) result
  val of_v2 : Legacy.V2.t -> (t, string) result
  val of_v3 : Legacy.V3.t -> (t, string) result

  (** [reset ?prompt_file t] clears history without lowering the next unused
      history sequence. *)
  val reset : ?prompt_file:string -> t -> t

  val reset_keep_history : ?prompt_file:string -> t -> t

  module Io : sig
    module File : sig
      val read : Bin_prot_utils_eio.path -> t
      val write : Bin_prot_utils_eio.path -> t -> unit
    end
  end
end

val of_v4 : V4.t -> t
val to_v4 : t -> V4.t

module V5 : sig
  val version : int

  type t =
    { version : int
    ; id : string
    ; prompt_file : string
    ; local_prompt_copy : string option
    ; history : History_entry.t list
    ; next_history_sequence : int
    ; tasks : Task.t list
    ; moderator_state : Moderator_state.t
    ; shell_state : Shell_state.t
    ; kv_store : (string * string) list
    ; vfs_root : string
    }
  [@@deriving bin_io, sexp]

  val of_v4 : V4.t -> t
  val validate : t -> (unit, string) result
  val reset : ?prompt_file:string -> t -> t
  val reset_keep_history : ?prompt_file:string -> t -> t

  module Io : sig
    module File : sig
      val read : Bin_prot_utils_eio.path -> t
      val write : Bin_prot_utils_eio.path -> t -> unit
    end
  end
end

val of_v5 : V5.t -> t
val to_v5 : t -> V5.t

module Io : sig
  (** Convenience wrappers for `Bin_prot_utils_eio` so that callers can
      persist or restore a session snapshot from within an Eio fiber. *)

  module File : sig
    (** [read path] loads a version-5 snapshot produced by {!write}.
        Use the session-store migration-aware reader for legacy snapshots. *)
    val read : Bin_prot_utils_eio.path -> t

    (** [write path session] serialises [session] to [path] using
        `bin_dump` with a header.  The file is created (0600) or
        truncated atomically. *)
    val write : Bin_prot_utils_eio.path -> t -> unit
  end
end

val allocator : t -> (History_entry.Allocator.t, string) result
val validate : t -> (unit, string) result
