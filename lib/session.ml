open Core

module History = struct
  type t = History_entry.t list [@@deriving bin_io, sexp]
end

module Snapshot = Chatml.Chatml_value_codec.Snapshot

(* ----------------------------------------------------------------------- *)
(*  Versioning                                                             *)
(* ----------------------------------------------------------------------- *)

(** Schema version supported by the running binary.  Increment whenever
    {!type:t} changes in a way that requires migrations. *)
let current_version = 5

module Task = struct
  type state =
    | Pending
    | In_progress
    | Done
  [@@deriving bin_io, sexp]

  type t =
    { id : string
    ; title : string
    ; state : state
    }
  [@@deriving bin_io, sexp]

  let create ?id ~title ?(state = Pending) () : t =
    let default_id () =
      let open Core in
      let data =
        let time_ns =
          Time_ns.to_int63_ns_since_epoch (Time_ns.now ()) |> Int63.to_string
        in
        time_ns ^ Int.to_string (Random.bits ())
      in
      Md5.digest_string data |> Md5.to_hex
    in
    let id = Option.value id ~default:(default_id ()) in
    { id; title; state }
  ;;

  (* Prevent -W32 until the function is exercised by external modules *)
  let _ : t = create ~title:"dummy" ()
end

module Moderator_snapshot = struct
  module Item = struct
    type t =
      { id : string
      ; value : Snapshot.t
      }
    [@@deriving bin_io, sexp]
  end

  module Overlay = struct
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

    let empty =
      { prepended_system_items = []
      ; appended_items = []
      ; replacements = []
      ; deleted_item_ids = []
      ; halted_reason = None
      }
    ;;
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

  let create
        ~script_id
        ~script_source_hash
        ?(current_state = Snapshot.Unit)
        ?(queued_internal_events = [])
        ?(halted = false)
        ?(overlay = Overlay.empty)
        ()
    =
    { script_id
    ; script_source_hash
    ; current_state
    ; queued_internal_events
    ; halted
    ; overlay
    }
  ;;
end

module Moderator_state = struct
  module Identity_snapshot = struct
    module Inserted = struct
      type t =
        { entry_id : History_entry.Id.t
        ; change_id : int
        ; value : Snapshot.t
        ; script_label : string option
        }
      [@@deriving bin_io, sexp]
    end

    module Replacement = struct
      type t =
        { target_id : History_entry.Id.t
        ; change_id : int
        ; value : Snapshot.t
        ; script_label : string option
        }
      [@@deriving bin_io, sexp]
    end

    module Tombstone = struct
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

  let of_legacy legacy_snapshot =
    { legacy_snapshot; identity_snapshot = None; extensions = [] }
  ;;
end

module Shell_state = struct
  module Request_kind = struct
    type t =
      | Structured
      | Script_file
      | Raw_shell
    [@@deriving bin_io, sexp]
  end

  module Approval_scope = struct
    type t =
      | Exact_session
      | Prefix_session of { prefix : string list }
      | Durable_exact
    [@@deriving bin_io, sexp]
  end

  module Reviewer = struct
    type t =
      { source : string
      ; reviewer_id : string option
      }
    [@@deriving bin_io, sexp]
  end

  module Approval_grant = struct
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

  module Manifest_grant = struct
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

  module Extension_snapshot = struct
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

  module Interrupted_request = struct
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

  let empty =
    { manifest_grants = []
    ; approval_grants = []
    ; extension_snapshots = []
    ; last_audit_sequence = None
    ; interrupted_requests = []
    }
  ;;
end

(* ----------------------------------------------------------------------- *)
(*  Latest schema                                                           *)
(* ----------------------------------------------------------------------- *)

(* Make the latest schema directly available at the top-level. *)
type t =
  { version : int
  ; id : string
  ; prompt_file : string
  ; local_prompt_copy : string option
  ; history : History.t
  ; next_history_sequence : int
  ; tasks : Task.t list
  ; moderator_state : Moderator_state.t
  ; shell_state : Shell_state.t
  ; kv_store : (string * string) list
  ; vfs_root : string
  }
[@@deriving bin_io, sexp]

(* Re-export under [Latest] so migration helpers can refer to the most
   recent schema while callers keep using [Session.t]. *)
module Latest = struct
  type nonrec t = t [@@deriving bin_io, sexp]

  let version = current_version
end

(* ----------------------------------------------------------------------- *)
(*  Legacy schemas and upgrade helpers                                      *)
(* ----------------------------------------------------------------------- *)

module Legacy = struct
  module Raw_history = struct
    type t = Openai.Responses.Item.t list [@@deriving bin_io, sexp]
  end

  module V0 = struct
    type t =
      { id : string
      ; prompt_file : string
      ; history : Raw_history.t
      ; tasks : Task.t list
      ; kv_store : (string * string) list
      ; vfs_root : string
      }
    [@@deriving bin_io, sexp]

    let version = 0
  end

  module V1 = struct
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

    let version = 1
  end

  module V2 = struct
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

    let version = 2
  end

  module V3 = struct
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

    let version = 3
  end

  let upgrade_v0 (v : V0.t) : Latest.t =
    let allocator =
      History_entry.Allocator.create ~namespace:v.id ~next_sequence:0
      |> Result.ok_or_failwith
    in
    let history =
      List.map v.history ~f:(History_entry.create ~allocator)
      |> Result.all
      |> Result.ok_or_failwith
    in
    { version = current_version
    ; id = v.id
    ; prompt_file = v.prompt_file
    ; local_prompt_copy = None
    ; history
    ; next_history_sequence = History_entry.Allocator.next_sequence allocator
    ; tasks = v.tasks
    ; moderator_state = Moderator_state.of_legacy None
    ; shell_state = Shell_state.empty
    ; kv_store = v.kv_store
    ; vfs_root = v.vfs_root
    }
  ;;

  let upgrade_v1 (v : V1.t) : Latest.t =
    upgrade_v0
      { id = v.id
      ; prompt_file = v.prompt_file
      ; history = v.history
      ; tasks = v.tasks
      ; kv_store = v.kv_store
      ; vfs_root = v.vfs_root
      }
  ;;

  let upgrade_v2 (v : V2.t) : Latest.t =
    { (upgrade_v0
         { id = v.id
         ; prompt_file = v.prompt_file
         ; history = v.history
         ; tasks = v.tasks
         ; kv_store = v.kv_store
         ; vfs_root = v.vfs_root
         })
      with
      local_prompt_copy = v.local_prompt_copy
    }
  ;;

  let v3_of_session (session : Latest.t) : V3.t =
    { version = session.version
    ; id = session.id
    ; prompt_file = session.prompt_file
    ; local_prompt_copy = session.local_prompt_copy
    ; history = History_entry.items session.history
    ; tasks = session.tasks
    ; moderator_snapshot = session.moderator_state.legacy_snapshot
    ; kv_store = session.kv_store
    ; vfs_root = session.vfs_root
    }
  ;;

  let session_of_v3 (v : V3.t) : Latest.t =
    let base =
      upgrade_v2
        { version = V2.version
        ; id = v.id
        ; prompt_file = v.prompt_file
        ; local_prompt_copy = v.local_prompt_copy
        ; history = v.history
        ; tasks = v.tasks
        ; kv_store = v.kv_store
        ; vfs_root = v.vfs_root
        }
    in
    { base with moderator_state = Moderator_state.of_legacy v.moderator_snapshot }
  ;;

  let v3_of_v0 v = upgrade_v0 v |> v3_of_session
  let v3_of_v1 v = upgrade_v1 v |> v3_of_session
  let v3_of_v2 v = upgrade_v2 v |> v3_of_session
end

module Production_moderator_state = Moderator_state

module V4 = struct
  let version = 4

  module Moderator_state = struct
    include Production_moderator_state
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

  let allocator t =
    History_entry.Allocator.create ~namespace:t.id ~next_sequence:t.next_history_sequence
  ;;

  let validate t =
    if t.version <> version
    then Error (sprintf "expected session schema version %d, got %d" version t.version)
    else
      Result.bind (allocator t) ~f:(fun allocator ->
        let inserted =
          match t.moderator_state.identity_snapshot with
          | None -> []
          | Some snapshot ->
            List.map
              (snapshot.prepended_items @ snapshot.appended_items)
              ~f:(fun inserted ->
                History_entry.create_with_id
                  ~id:inserted.entry_id
                  (Openai.Responses.Item.Input_message
                     { role = Openai.Responses.Input_message.System
                     ; content = []
                     ; _type = "message"
                     }))
        in
        History_entry.validate ~allocator (t.history @ inserted))
  ;;

  let legacy_item_id item generated =
    let open Openai.Responses in
    match item with
    | Item.Output_message message -> message.id, generated
    | Function_call { id = Some id; _ }
    | Custom_tool_call { id = Some id; _ }
    | Function_call_output { id = Some id; _ }
    | Custom_tool_call_output { id = Some id; _ } -> id, generated
    | Web_search_call call -> call.id, generated
    | File_search_call call -> call.id, generated
    | Reasoning reasoning -> reasoning.id, generated
    | Input_message _
    | Function_call { id = None; _ }
    | Custom_tool_call { id = None; _ }
    | Function_call_output { id = None; _ }
    | Custom_tool_call_output { id = None; _ } ->
      sprintf "host-message-%d" generated, generated + 1
  ;;

  let migrate_moderator_snapshot ~allocator ~history raw_history legacy_snapshot =
    let open Result.Let_syntax in
    match legacy_snapshot with
    | None -> Ok (Moderator_state.of_legacy None)
    | Some (snapshot : Moderator_snapshot.t) ->
      let legacy_ids, _ =
        List.fold raw_history ~init:([], 1) ~f:(fun (ids, generated) item ->
          let id, generated = legacy_item_id item generated in
          ids @ [ id ], generated)
      in
      let targets = List.zip_exn legacy_ids (List.map history ~f:History_entry.id) in
      let targets =
        List.fold targets ~init:String.Map.empty ~f:(fun targets (legacy_id, target_id) ->
          Map.add_multi targets ~key:legacy_id ~data:target_id)
      in
      let target legacy_id =
        match Map.find targets legacy_id with
        | None -> Error (sprintf "Unknown legacy moderator target %S." legacy_id)
        | Some [ target_id ] -> Ok target_id
        | Some target_ids ->
          Error
            (sprintf
               "Ambiguous legacy moderator target %S matches %d history entries."
               legacy_id
               (List.length target_ids))
      in
      let inserted_items =
        snapshot.overlay.prepended_system_items @ snapshot.overlay.appended_items
      in
      let%bind replacement_targets =
        Result.all
          (List.map snapshot.overlay.replacements ~f:(fun replacement ->
             let%map target_id = target replacement.target_id in
             replacement, target_id))
      in
      let%bind tombstone_targets =
        Result.all
          (List.map snapshot.overlay.deleted_item_ids ~f:(fun legacy_id ->
             target legacy_id))
      in
      let%bind inserted_ids =
        History_entry.Allocator.reserve allocator ~count:(List.length inserted_items)
      in
      let change_id = ref 0 in
      let next_change_id () =
        let id = !change_id in
        Int.incr change_id;
        id
      in
      let inserted ids (items : Moderator_snapshot.Item.t list) =
        List.map2_exn ids items ~f:(fun entry_id (item : Moderator_snapshot.Item.t) ->
          Moderator_state.Identity_snapshot.Inserted.
            { entry_id
            ; change_id = next_change_id ()
            ; value = item.value
            ; script_label = Some item.id
            })
      in
      let prepended_count = List.length snapshot.overlay.prepended_system_items in
      let prepended_ids, appended_ids = List.split_n inserted_ids prepended_count in
      let prepended_items =
        inserted prepended_ids snapshot.overlay.prepended_system_items
      in
      let appended_items = inserted appended_ids snapshot.overlay.appended_items in
      let replacements =
        List.map replacement_targets ~f:(fun (replacement, target_id) ->
          Moderator_state.Identity_snapshot.Replacement.
            { target_id
            ; change_id = next_change_id ()
            ; value = replacement.item.value
            ; script_label = Some replacement.item.id
            })
      in
      let tombstones =
        List.map tombstone_targets ~f:(fun target_id ->
          Moderator_state.Identity_snapshot.Tombstone.
            { target_id; change_id = next_change_id () })
      in
      let identity_snapshot =
        Moderator_state.Identity_snapshot.
          { script_id = snapshot.script_id
          ; script_source_hash = snapshot.script_source_hash
          ; current_state = snapshot.current_state
          ; queued_internal_events = snapshot.queued_internal_events
          ; halted = snapshot.halted
          ; revision =
              (if
                 List.is_empty inserted_items
                 && List.is_empty replacements
                 && List.is_empty tombstones
               then 0
               else 1)
          ; next_change_id = !change_id
          ; prepended_items
          ; appended_items
          ; replacements
          ; tombstones
          ; halted_reason = snapshot.overlay.halted_reason
          }
      in
      Ok
        Moderator_state.
          { legacy_snapshot = Some snapshot
          ; identity_snapshot = Some identity_snapshot
          ; extensions = []
          }
  ;;

  let of_v3 (v : Legacy.V3.t) =
    let open Result.Let_syntax in
    let%bind () =
      if Int.equal v.version Legacy.V3.version
      then Ok ()
      else Error (sprintf "expected session schema version 3, got %d" v.version)
    in
    let%bind allocator =
      History_entry.Allocator.create ~namespace:v.id ~next_sequence:0
    in
    let%bind history =
      List.map v.history ~f:(History_entry.create ~allocator) |> Result.all
    in
    let%bind moderator_state =
      migrate_moderator_snapshot ~allocator ~history v.history v.moderator_snapshot
    in
    Ok
      { version
      ; id = v.id
      ; prompt_file = v.prompt_file
      ; local_prompt_copy = v.local_prompt_copy
      ; history
      ; next_history_sequence = History_entry.Allocator.next_sequence allocator
      ; tasks = v.tasks
      ; moderator_state
      ; kv_store = v.kv_store
      ; vfs_root = v.vfs_root
      }
  ;;

  let of_v0 (v : Legacy.V0.t) =
    of_v3
      { version = Legacy.V3.version
      ; id = v.id
      ; prompt_file = v.prompt_file
      ; local_prompt_copy = None
      ; history = v.history
      ; tasks = v.tasks
      ; moderator_snapshot = None
      ; kv_store = v.kv_store
      ; vfs_root = v.vfs_root
      }
  ;;

  let of_v1 v =
    if Int.equal v.Legacy.V1.version Legacy.V1.version
    then
      of_v3
        { version = Legacy.V3.version
        ; id = v.id
        ; prompt_file = v.prompt_file
        ; local_prompt_copy = None
        ; history = v.history
        ; tasks = v.tasks
        ; moderator_snapshot = None
        ; kv_store = v.kv_store
        ; vfs_root = v.vfs_root
        }
    else Error (sprintf "expected session schema version 1, got %d" v.version)
  ;;

  let of_v2 v =
    if Int.equal v.Legacy.V2.version Legacy.V2.version
    then
      of_v3
        { version = Legacy.V3.version
        ; id = v.id
        ; prompt_file = v.prompt_file
        ; local_prompt_copy = v.local_prompt_copy
        ; history = v.history
        ; tasks = v.tasks
        ; moderator_snapshot = None
        ; kv_store = v.kv_store
        ; vfs_root = v.vfs_root
        }
    else Error (sprintf "expected session schema version 2, got %d" v.version)
  ;;

  let reset ?prompt_file t =
    let prompt_file = Option.value prompt_file ~default:t.prompt_file in
    { t with prompt_file; history = []; moderator_state = Moderator_state.of_legacy None }
  ;;

  let reset_keep_history ?prompt_file t =
    let prompt_file = Option.value prompt_file ~default:t.prompt_file in
    { t with prompt_file; moderator_state = Moderator_state.of_legacy None }
  ;;

  module Bin_p = struct
    type nonrec t = t [@@deriving bin_io]
  end

  module Io = Bin_prot_utils_eio.With_file_methods (Bin_p)
end

let of_v4 (session : V4.t) : t =
  { version = current_version
  ; id = session.id
  ; prompt_file = session.prompt_file
  ; local_prompt_copy = session.local_prompt_copy
  ; history = session.history
  ; next_history_sequence = session.next_history_sequence
  ; tasks = session.tasks
  ; moderator_state = session.moderator_state
  ; shell_state = Shell_state.empty
  ; kv_store = session.kv_store
  ; vfs_root = session.vfs_root
  }
;;

let to_v4 (session : t) : V4.t =
  { version = V4.version
  ; id = session.id
  ; prompt_file = session.prompt_file
  ; local_prompt_copy = session.local_prompt_copy
  ; history = session.history
  ; next_history_sequence = session.next_history_sequence
  ; tasks = session.tasks
  ; moderator_state = session.moderator_state
  ; kv_store = session.kv_store
  ; vfs_root = session.vfs_root
  }
;;

module V5 = struct
  let version = 5

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

  let of_v4 (session : V4.t) : t =
    { version
    ; id = session.id
    ; prompt_file = session.prompt_file
    ; local_prompt_copy = session.local_prompt_copy
    ; history = session.history
    ; next_history_sequence = session.next_history_sequence
    ; tasks = session.tasks
    ; moderator_state = session.moderator_state
    ; shell_state = Shell_state.empty
    ; kv_store = session.kv_store
    ; vfs_root = session.vfs_root
    }
  ;;

  let validate t =
    if not (Int.equal t.version version)
    then Error (sprintf "expected session schema version %d, got %d" version t.version)
    else
      Result.bind
        (History_entry.Allocator.create
           ~namespace:t.id
           ~next_sequence:t.next_history_sequence)
        ~f:(fun allocator -> History_entry.validate ~allocator t.history)
  ;;

  let reset ?prompt_file t =
    let prompt_file = Option.value prompt_file ~default:t.prompt_file in
    { t with
      prompt_file
    ; history = []
    ; moderator_state = Moderator_state.of_legacy None
    ; shell_state = Shell_state.empty
    }
  ;;

  let reset_keep_history ?prompt_file t =
    let prompt_file = Option.value prompt_file ~default:t.prompt_file in
    { t with
      prompt_file
    ; moderator_state = Moderator_state.of_legacy None
    ; shell_state = Shell_state.empty
    }
  ;;

  module Bin_p = struct
    type nonrec t = t [@@deriving bin_io]
  end

  module Io = Bin_prot_utils_eio.With_file_methods (Bin_p)
end

let of_v5 (session : V5.t) : t =
  { version = session.version
  ; id = session.id
  ; prompt_file = session.prompt_file
  ; local_prompt_copy = session.local_prompt_copy
  ; history = session.history
  ; next_history_sequence = session.next_history_sequence
  ; tasks = session.tasks
  ; moderator_state = session.moderator_state
  ; shell_state = session.shell_state
  ; kv_store = session.kv_store
  ; vfs_root = session.vfs_root
  }
;;

let to_v5 (session : t) : V5.t =
  { version = V5.version
  ; id = session.id
  ; prompt_file = session.prompt_file
  ; local_prompt_copy = session.local_prompt_copy
  ; history = session.history
  ; next_history_sequence = session.next_history_sequence
  ; tasks = session.tasks
  ; moderator_state = session.moderator_state
  ; shell_state = session.shell_state
  ; kv_store = session.kv_store
  ; vfs_root = session.vfs_root
  }
;;

let create
      ?id
      ~prompt_file
      ?local_prompt_copy
      ?(history = [])
      ?(next_history_sequence = 0)
      ?(tasks = [])
      ?moderator_snapshot
      ?moderator_state
      ?(shell_state = Shell_state.empty)
      ?(kv_store = [])
      ?(vfs_root = "vfs")
      ()
  : t
  =
  let default_id () =
    let data =
      let time_ns = Time_ns.to_int63_ns_since_epoch (Time_ns.now ()) |> Int63.to_string in
      time_ns ^ Int.to_string (Random.bits ())
    in
    Md5.digest_string data |> Md5.to_hex
  in
  let id = Option.value id ~default:(default_id ()) in
  { version = current_version
  ; id
  ; prompt_file
  ; local_prompt_copy
  ; history
  ; next_history_sequence
  ; tasks
  ; moderator_state =
      Option.value moderator_state ~default:(Moderator_state.of_legacy moderator_snapshot)
  ; shell_state
  ; kv_store
  ; vfs_root
  }
;;

(* ------------------------------------------------------------------------- *)
(* IO helpers                                                                *)
(* ------------------------------------------------------------------------- *)

module Bin_p = struct
  type nonrec t = t [@@deriving bin_io]
end

module Io = Bin_prot_utils_eio.With_file_methods (Bin_p)

(* Dummy reference to avoid “unused-value” compiler warnings until the
   functions gain real call-sites in subsequent milestones. *)
let _ = ignore (create ~prompt_file:"/dev/null" ())

(* ------------------------------------------------------------------------- *)
(*  Public helpers                                                            *)
(* ------------------------------------------------------------------------- *)

let reset ?prompt_file (t : t) : t =
  let prompt_file = Option.value prompt_file ~default:t.prompt_file in
  { t with
    prompt_file
  ; history = []
  ; moderator_state = Moderator_state.of_legacy None
  ; shell_state = Shell_state.empty
  }
;;

(** Same as {!reset} but preserves the existing conversation history. *)
let reset_keep_history ?prompt_file (t : t) : t =
  let prompt_file = Option.value prompt_file ~default:t.prompt_file in
  { t with
    prompt_file
  ; moderator_state = Moderator_state.of_legacy None
  ; shell_state = Shell_state.empty
  }
;;

let allocator (t : t) =
  History_entry.Allocator.create ~namespace:t.id ~next_sequence:t.next_history_sequence
;;

let validate (t : t) =
  let open Result.Let_syntax in
  let%bind allocator = allocator t in
  History_entry.validate ~allocator t.history
;;
