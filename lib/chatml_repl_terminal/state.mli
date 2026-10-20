open! Core

(** Bounded, transport-free terminal presentation state. The portable client
    remains the owner of exact pending requests and retry handles. *)

module Limits : sig
  type t

  val create
    :  max_source_bytes:int
    -> max_source_lines:int
    -> max_history_entries:int
    -> max_history_bytes:int
    -> max_notice_bytes:int
    -> (t, Agent_protocol.Error.t) result

  val max_source_bytes : t -> int
  val max_source_lines : t -> int
  val max_notice_bytes : t -> int
end

module Editor : sig
  type t

  type motion =
    | Left
    | Right
    | Home
    | End
  [@@deriving equal, sexp_of]

  (** Inserted and pasted text is literal source, including newlines and
      leading colons. Cursor and deletion boundaries are UTF-8 grapheme
      boundaries. A rejected edit leaves the old editor unchanged. *)
  val insert : t -> limits:Limits.t -> string -> (t, Agent_protocol.Error.t) result

  val backspace : t -> t
  val delete : t -> t
  val move : t -> motion -> t
  val source : t -> string
  val cursor_byte : t -> int

  (** The final line and the preceding source are exposed separately so an
      own-line [:submit] is never sent as ChatML source. They do not mutate the
      editor or infer a server result. *)
  val final_line : t -> string

  val source_before_final_line : t -> string

  (** Removes a recognized command line while retaining the preceding source
      and its terminating newline. *)
  val without_final_line : t -> t
end

type keyed_operation =
  | Create
  | Submit
  | Run_task
  | Reset
  | Close
[@@deriving equal, sexp_of]

type operation =
  | Keyed of keyed_operation
  | Inspect_type
  | References
  | Observe
  | Cancel
[@@deriving equal, sexp_of]

type activity =
  | Ready
  | Busy of operation
  | Uncertain of keyed_operation
  | Needs_reset
  | Closed
[@@deriving equal, sexp_of]

type t

val create : limits:Limits.t -> context:string -> (t, Agent_protocol.Error.t) result
val limits : t -> Limits.t
val context : t -> string
val editor : t -> Editor.t
val activity : t -> activity
val view : t -> Agent_protocol.Developer_repl_wire.View.t option
val submission : t -> Agent_protocol.Developer_repl_wire.Submission.t option
val current_revision : t -> Agent_protocol.Developer_repl_wire.Wire_id.Revision.t option
val reference_page : t -> Agent_protocol.Developer_repl_wire.Reference_page.t option
val task : t -> Agent_protocol.Developer_repl_wire.Task_view.t option
val pending : t -> Agent_client.Developer_repl_connection.pending
val history : t -> string list
val notice : t -> string option

(** A candidate editor may have been built under a different state's bounds;
    validate bytes, lines, UTF-8 and cursor cluster boundary before adoption. *)
val with_editor : t -> Editor.t -> (t, Agent_protocol.Error.t) result

(** Bounded history navigation preserves the current unsubmitted draft and
    restores it after the newest entry. Neither action submits source. *)
val history_previous : t -> t

val history_next : t -> t

(** A successful Create or Reset replaces the view and clears references,
    submission, task and editor. Reset retains the same captured source; it
    cannot select a new project or environment. *)
val with_view : t -> Agent_protocol.Developer_repl_wire.View.t -> t

(** An accepted Submit must match the current epoch. Only its displayed
    references can be selected for an explicit Task action. *)
val with_submission
  :  t
  -> Agent_protocol.Developer_repl_wire.Submission.t
  -> (t, Agent_protocol.Error.t) result

(** Replaces only the displayed reference page for the current epoch/revision.
    The controller checks the request's captured epoch/revision again after its
    yielding read before calling this function. *)
val with_reference_page : t -> Agent_protocol.Developer_repl_wire.Reference_page.t -> t

val task_reference : t -> index:int -> (string, Agent_protocol.Error.t) result
val with_task : t -> Agent_protocol.Developer_repl_wire.Task_view.t -> t
val with_activity : t -> activity -> t
val with_pending : t -> Agent_client.Developer_repl_connection.pending -> t
val with_notice : t -> string -> t

(** Records a source in bounded local history when it satisfies this state's
    editor limits; an unrelated oversized or malformed source is omitted.
    Clears the editor unless [preserve_editor] is set for an exact retry.
    This is presentation history, never a retry journal. Abandoning portable
    client custody never proves this source did not execute. *)
val submitted : t -> source:string -> preserve_editor:bool -> t
