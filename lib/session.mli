open Core

(** Persistent chat session state.

    A value of type {!t} bundles everything the assistant needs to
    restore a chat between two executions of the binary:

    * the system/user prompt file that seeded the conversation
    * the provider-independent message exchange (the {!module:History})
    * an optional lightweight task-list
    * an optional persisted moderator snapshot
    * arbitrary key/value metadata
    * a virtual file-system (VFS) root used by tooling

    Named-field documents are converted before constructing runtime values.
    Previous beta binary formats are deliberately unsupported. Use
    [Session_store] for atomic, locked snapshot writes. *)

module History : sig
  (** Ordered list of canonical conversation entries that form the conversation
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

module Snapshot : sig
  include
    module type of Chatml.Chatml_value_codec.Snapshot
    with type t = Chatml.Chatml_value_codec.Snapshot.t

  val validate : t -> (unit, string) Result.t
  val shape : Document_schema.Shape.t
end

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
  (** Durable overlay values are neutral history payloads, preserving captured
      provider data exactly. Script state and queued events remain ChatML values. *)
  module Identity_snapshot : sig
    module Inserted : sig
      type t =
        { entry_id : History_entry.Id.t
        ; change_id : int
        ; value : History_entry.Payload.t
        ; script_label : string option
        }
      [@@deriving bin_io, sexp]
    end

    module Replacement : sig
      type t =
        { target_id : History_entry.Id.t
        ; change_id : int
        ; value : History_entry.Payload.t
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

    (** Required nullable option fields; integer counters use decimal strings.
        Raw decoding is pure; the owning document retains unknown fields. *)
    val to_jsonaf : t -> Jsonaf.t

    val of_jsonaf : Jsonaf.t -> (t, string) Result.t
    val validate : t -> (unit, string) Result.t

    (** Insertions must be disjoint from canonical and deferred host identities.
        Replacements intentionally keep their target identity. *)
    val validate_history_ids
      :  t
      -> history_ids:History_entry.Id.t list
      -> (unit, string) Result.t

    val shape : Document_schema.Shape.t
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

  val to_jsonaf : t -> Jsonaf.t
  val of_jsonaf : Jsonaf.t -> (t, string) Result.t
  val shape : Document_schema.Shape.t
  val empty : t
end

(** Current runtime marker. Document versions are independent of changes to
    the OCaml record representation. *)
val current_version : int

(** Current runtime value. [storage] carries the immutable preservation context;
    ordinary record updates must retain it. It is not a second conversation. *)
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
  ; storage : unit Document_schema.Extension_carrier.t
  }

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

val allocator : t -> (History_entry.Allocator.t, string) result
val validate : t -> (unit, string) result

module Document : sig
  (** Complete standalone.session v1 document. Generic conversion precedes the
      validated current decoder. Unknown fields survive edits; ambiguous edits
      return Extension_conflict before any file write. *)
  val encode : t -> (Document_schema.Document.t, Document_schema.Error.t) Result.t

  val decode : Document_schema.Document.t -> (t, Document_schema.Error.t) Result.t
  val to_string : t -> (string, Document_schema.Error.t) Result.t
  val of_string : string -> (t, Document_schema.Error.t) Result.t
end

module Io : sig
  module File : sig
    (** Bounded document read. Raises on malformed or unsupported data. *)
    val read : Eio.Fs.dir_ty Eio.Path.t -> t

    (** Validates and encodes before opening the destination; direct write is
        not atomic. Prefer Session_store.save for durable application updates. *)
    val write : Eio.Fs.dir_ty Eio.Path.t -> t -> unit
  end
end
