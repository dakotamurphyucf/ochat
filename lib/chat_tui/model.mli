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
  ; mutable selection_anchor : int option (** Anchor position for active selection. *)
  }
[@@deriving fields ~getters ~setters]

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

(** {1 Applying patches}

    Refactoring step 6 introduces a {e patch} based update mechanism that
    abstracts over concrete mutations to the UI state.  For the time being
    the implementation still performs inplace updates to the interior
    {!ref} values – later steps will turn [t] into an immutable record and
    rebuild a fresh value instead. *)

val apply_patch : t -> Types.patch -> t
val apply_patches : t -> Types.patch list -> t
val add_history_item : t -> Openai.Responses.Item.t -> t
