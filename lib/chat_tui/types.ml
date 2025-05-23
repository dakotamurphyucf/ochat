(* Shared data types used throughout the Chat-TUI implementation.  Keeping
   them in a tiny standalone module avoids circular dependencies once the
   code base is split into many small compilation units. *)

type role = string
type message = role * string

type msg_buffer =
  { text : string ref
  ; index : int
  }

(* ------------------------------------------------------------------------ *)
(*  Cmd constructors (step 7)                                               *)
(* ------------------------------------------------------------------------ *)

type cmd =
  | Persist_session of (unit -> unit)
  (** Execute a closure that kicks off the OpenAI streaming request in its own
      fibre.  The indirection through a closure allows the pure controller or
      submit handler to hand over the side–effecting job to the [{!Cmd}]
      interpreter without having to perform any IO itself. *)
  | Start_streaming of (unit -> unit)
  | Cancel_streaming of (unit -> unit)

(* ------------------------------------------------------------------------ *)
(*  Patch constructors (step 6)                                             *)
(* ------------------------------------------------------------------------ *)

type patch =
  | Ensure_buffer of
      { id : string
      ; role : string
      }
  | Append_text of
      { id : string
      ; role : string
      ; text : string
      }
  | Set_function_name of
      { id : string
      ; name : string
      }
  | Set_function_output of
      { id : string
      ; output : string
      }
  | Update_reasoning_idx of
      { id : string
      ; idx : int
      }
  (* Added in refactoring step 13 – represents the act of the user submitting
     a prompt.  The pure controller layer will emit this patch which is then
     applied by the model (adding the corresponding history item and visible
     message). *)
  | Add_user_message of { text : string }
  | Add_placeholder_message of
      { role : string
      ; text : string
      }
