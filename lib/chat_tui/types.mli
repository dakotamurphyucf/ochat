(** Shared data types used across the refactored Chat-TUI code. *)

type role = string
type message = role * string

(** Streaming-time buffer.  While we receive deltas from the OpenAI API we
    accumulate partial output in [text] and remember the target index into
    the [messages] list so the UI can update incrementally. *)
type msg_buffer =
  { text : string ref
  ; index : int
  }

(* ––– forward declarations for upcoming steps ––– *)

(* ------------------------------------------------------------------------ *)
(*  Cmd constructors introduced in refactoring step 7                       *)
(* ------------------------------------------------------------------------ *)

type cmd =
  | Persist_session of (unit -> unit)
  (** [Persist_session f] encapsulates a side-effecting persistence
          operation (normally writing the current conversation buffer back
          to disk).  The thunk [f] will be executed by [Cmd.run] in a
          suitable fibre so that IO stays outside the pure update logic.

          Deferring the effect behind a thunk means the variant can stay
          completely independent of concrete types such as [Eio.Path.t] or
          [Chat_tui.Persistence.config], preventing circular dependencies at
          the type-level. *)
  | Start_streaming of (unit -> unit)
  | Cancel_streaming of (unit -> unit)

(* ------------------------------------------------------------------------ *)
(*  Patch constructors introduced in refactoring step 6                      *)
(* ------------------------------------------------------------------------ *)

(* These constructors describe {e pure} modifications to the immutable
   [Model.t] record that will be executed by [Model.apply].  They are
   defined here – as an extension of the open variant – so that they can be
   shared between multiple compilation units without introducing circular
   dependencies.  *)

type patch =
  | Ensure_buffer of
      { id : string
      ; role : string
      }
  (** Make sure a streaming buffer ([id] → accumulated text) exists.  If
          absent a new entry is created and an empty message with the
          specified [role] is appended to the visible history. *)
  | Append_text of
      { id : string
      ; role : string
      ; text : string
      }
  (** Append [text] to the buffer identified by [id].  If the buffer does
          not exist it is first created (using [role]).  The corresponding
          entry in the visible [messages] list is updated as well. *)
  | Set_function_name of
      { id : string
      ; name : string
      }
  | Set_function_output of
      { id : string
      ; output : string
      }
  (** Remember the mapping between a streaming [item_id] and the tool / 
          function [name] that was invoked.  Used later when streaming the
          arguments so we can emit a friendly “name(…args…)” representation
          in the UI. *)
  | Update_reasoning_idx of
      { id : string
      ; idx : int
      }
  | Add_user_message of { text : string }
  (** Track the most recently seen [summary_index] for reasoning deltas so
          that the UI can insert line breaks between subsequent summaries. *)
  | Add_placeholder_message of
      { role : string
      ; text : string
      }
  (** Append a transient placeholder message (e.g. "(thinking…)" while the
            assistant has not yet started streaming).  Unlike
            [Add_user_message] this does {b not} touch [history_items] because
            the placeholder is only a UI affordance and should not be
            persisted. *)
