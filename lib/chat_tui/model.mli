(** Thin mutable wrapper around all state currently required by the
    Chat-TUI main loop.  Later steps will turn this into an immutable record
    with explicit [patch] updates but for now we simply bundle the existing
    references so they can be passed around as a single value. *)

open Types

type t =
  { history_items : Openai.Responses.Item.t list ref
  ; messages : message list ref
  ; input_line : string ref
  ; auto_follow : bool ref
  ; msg_buffers : (string, Types.msg_buffer) Base.Hashtbl.t
  ; function_name_by_id : (string, string) Base.Hashtbl.t
  ; reasoning_idx_by_id : (string, int ref) Base.Hashtbl.t
  ; fetch_sw : Eio.Switch.t option ref
  ; scroll_box : Notty_scroll_box.t
  ; cursor_pos : int ref (** Current position inside [input_line] (bytes). *)
  }

(** [create ~history_items ~messages …] packs pre-existing references into a
    model record.  This is intentionally shallow – the references keep their
    identity so the surrounding code continues to work unchanged. *)
val create
  :  history_items:Openai.Responses.Item.t list ref
  -> messages:message list ref
  -> input_line:string ref
  -> auto_follow:bool ref
  -> msg_buffers:(string, Types.msg_buffer) Base.Hashtbl.t
  -> function_name_by_id:(string, string) Base.Hashtbl.t
  -> reasoning_idx_by_id:(string, int ref) Base.Hashtbl.t
  -> fetch_sw:Eio.Switch.t option ref
  -> scroll_box:Notty_scroll_box.t
  -> cursor_pos:int ref
  -> t

(** Convenience accessors – added on demand. *)
val input_line : t -> string ref
val cursor_pos : t -> int ref

val messages : t -> message list ref
val auto_follow : t -> bool ref

(** {1 Applying patches}

    Refactoring step 6 introduces a {e patch} based update mechanism that
    abstracts over concrete mutations to the UI state.  For the time being
    the implementation still performs inplace updates to the interior
    {!ref} values – later steps will turn [t] into an immutable record and
    rebuild a fresh value instead. *)

val apply_patch : t -> Types.patch -> t
val apply_patches : t -> Types.patch list -> t
