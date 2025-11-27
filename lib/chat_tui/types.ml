(* Shared data types for the Ochat terminal UI.  The {!Types} module
   holds only minimal, foundational definitions and therefore has no
   dependencies on heavier libraries such as Eio or Notty.  This design
   decision prevents circular build dependencies and keeps compilation
   times down.  All authoritative documentation lives in
   [types.mli]. *)

type role = string
type message = role * string

type tool_output_kind =
  | Apply_patch
  | Read_file of { path : string option }
  | Read_directory of { path : string option }
  | Other of { name : string option }

type msg_buffer =
  { buf : Buffer.t
  ; index : int
  }

(* ------------------------------------------------------------------------ *)
(*  Cmd constructors                                                        *)
(* ------------------------------------------------------------------------ *)

type cmd =
  | Persist_session of (unit -> unit)
  | Start_streaming of (unit -> unit)
  | Cancel_streaming of (unit -> unit)

(* ------------------------------------------------------------------------ *)
(*  Patch constructors                                                      *)
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
  | Add_user_message of { text : string }
  | Add_placeholder_message of
      { role : string
      ; text : string
      }

(* ------------------------------------------------------------------------ *)
(*  Runtime settings                                                         *)
(* ------------------------------------------------------------------------ *)

type settings = { parallel_tool_calls : bool }

let default_settings () : settings = { parallel_tool_calls = true }
