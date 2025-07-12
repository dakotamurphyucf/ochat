(** Thin mutable wrapper around all state currently required by the
    Chat-TUI main loop.  Later steps will turn this into an immutable record
    with explicit [patch] updates but for now we simply bundle the existing
    references so they can be passed around as a single value. *)

open Types

type t =
  { mutable history_items : Openai.Responses.Item.t list
  ; mutable messages : message list
  ; mutable input_line : string
  ; mutable auto_follow : bool
  ; msg_buffers : (string, Types.msg_buffer) Base.Hashtbl.t
  ; function_name_by_id : (string, string) Base.Hashtbl.t
  ; reasoning_idx_by_id : (string, int ref) Base.Hashtbl.t
  ; mutable fetch_sw : Eio.Switch.t option
  ; scroll_box : Notty_scroll_box.t
  ; mutable cursor_pos : int (** Current position inside [input_line] (bytes). *)
  ; mutable selection_anchor : int option
    (* --------------------------------------------------------------------------- *)
    (* Command-mode scaffolding (Phase 0)                                           *)
    (* --------------------------------------------------------------------------- *)
    (** Anchor position for active selection. *)
  ; mutable mode : editor_mode (** Current editor mode (Insert/Normal). *)
  ; mutable draft_mode : draft_mode
    (** Whether the draft buffer is plain text or raw XML. *)
  ; mutable selected_msg : int option
    (** Currently selected message (Normal mode), if any. *)
  ; mutable undo_stack : (string * int) list
    (** Undo ring – previous states (line, cursor) *)
  ; mutable redo_stack : (string * int) list
  ; mutable cmdline : string (** Current command-line buffer (":"-prefix excluded). *)
  ; mutable cmdline_cursor : int (** Cursor position inside [cmdline]. *)
  ; mutable active_fork : string option (** Currently running fork tool call-id, if any. *)
  ; mutable fork_start_index : int option (** History length when fork started. *)
  }
[@@deriving fields ~getters ~setters]

and editor_mode =
  | Insert
  | Normal
  | Cmdline

and draft_mode =
  | Plain
  | Raw_xml

(** [create ~history_items ~messages …] packs pre-existing references into a
    model record.  This is intentionally shallow – the references keep their
    identity so the surrounding code continues to work unchanged. *)
val create
  :  history_items:Openai.Responses.Item.t list
  -> messages:message list
  -> input_line:string
  -> auto_follow:bool
  -> msg_buffers:(string, Types.msg_buffer) Base.Hashtbl.t
  -> function_name_by_id:(string, string) Base.Hashtbl.t
  -> reasoning_idx_by_id:(string, int ref) Base.Hashtbl.t
  -> fetch_sw:Eio.Switch.t option
  -> scroll_box:Notty_scroll_box.t
  -> cursor_pos:int
  -> selection_anchor:int option
  -> mode:editor_mode
  -> draft_mode:draft_mode
  -> selected_msg:int option
  -> undo_stack:(string * int) list
  -> redo_stack:(string * int) list
  -> cmdline:string
  -> cmdline_cursor:int
  -> t

(** Convenience accessors – added on demand. *)
val input_line : t -> string

val cursor_pos : t -> int
val selection_anchor : t -> int option
val clear_selection : t -> unit
val set_selection_anchor : t -> int -> unit
val selection_active : t -> bool
val messages : t -> message list
val auto_follow : t -> bool

(** {1 Command-mode helpers} *)

val toggle_mode : t -> unit
val set_draft_mode : t -> draft_mode -> unit
val select_message : t -> int option -> unit

(** {1 Command-line helpers} *)

val cmdline : t -> string
val cmdline_cursor : t -> int
val set_cmdline : t -> string -> unit
val set_cmdline_cursor : t -> int -> unit

(** {1 Fork helpers} *)

val active_fork : t -> string option
val set_active_fork : t -> string option -> unit

val fork_start_index : t -> int option
val set_fork_start_index : t -> int option -> unit

(** {1 Undo / Redo helpers} *)

val push_undo : t -> unit

(** returns [true] if something was undone *)
val undo : t -> bool

(** returns [true] if something was redone *)
val redo : t -> bool

(** {1 Applying patches}

    Refactoring step 6 introduces a {e patch} based update mechanism that
    abstracts over concrete mutations to the UI state.  For the time being
    the implementation still performs inplace updates to the interior
    {!ref} values – later steps will turn [t] into an immutable record and
    rebuild a fresh value instead. *)

val apply_patch : t -> Types.patch -> t
val apply_patches : t -> Types.patch list -> t
val add_history_item : t -> Openai.Responses.Item.t -> t
