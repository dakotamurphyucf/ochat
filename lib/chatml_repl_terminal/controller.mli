open! Core

(** One Eio-owned terminal event loop over the closed portable REPL client.
    No compiler, provider, authority or transport method is reimplemented. *)

module Input : sig
  type t =
    | Text of string
    | Paste of string
    | Enter
    | Newline
    | Backspace
    | Delete
    | Move of State.Editor.motion
    | History_previous
    | History_next
    | Complete
    | Completion_previous
    | Dismiss
    | History_search
    | Submit
    | Cancel
    | Resize
    | Quit
  [@@deriving sexp_of]
end

module Input_queue : sig
  type t

  val create : capacity:int -> (t, Agent_protocol.Error.t) result

  (** Same-domain, nonyielding offer for Notty's event callback. Overflow
      records a terminal error and discards queued input; it never submits a
      partially accepted paste or command. *)
  val offer : t -> Input.t -> unit

  (** Records a bounded terminal input failure, including a paste exceeding
      the editor limit, without blocking or accepting a prefix. *)
  val fail : t -> message:string -> unit

  val close : t -> unit
end

module Command : sig
  type t =
    | Submit
    | Run of int
    | Type of
        { statement_index : int
        ; selector : Agent_protocol.Developer_repl_wire.Type_selector.t
        }
    | References
    | Observe
    | Cancel
    | Reset
    | Close
    | Retry
    | Abandon
    | Quit
    | Help of string option
  [@@deriving sexp_of]

  (** Recognizes only a complete final editor line. The command line is
      excluded from source sent by Submit. Unknown colon-prefixed lines remain
      source; they are never run implicitly. *)
  val parse : string -> t option
end

(** [run] claims one notification lease, starts at most one ordinary request
    worker, one exact-key control worker, and one notification reader, and owns
    all worker completion messages under its
    caller's Eio switch. Notty's event callback only calls the same-domain,
    nonyielding Input_queue.offer; it never performs network I/O or waits for
    this loop. Queue or paste overflow fails the loop before queued input is
    acted on. [present] is called only from the owning terminal fiber.

    Enter, Submit or a whole-line [:submit] sends the bounded source. An Enter
    submission with authoritative Need_more retains acknowledged bytes read-only
    and starts the unsent suffix with a continuation newline. Later requests
    send only that suffix; history and diagnostics use the exact aggregate.
    Completion and history recall/search pause until the prefix finishes or is
    discarded with Reset. Newline (Alt+Enter) inserts without submitting. Paste is
    literal source. Completion reads have a separate lane with one in-flight read
    and one coalesced latest desired site; edits remain responsive. Exact draft
    source/cursor and instance/epoch/revision guard publication. Enter never
    accepts a suggestion implicitly. Search Enter loads an unsubmitted draft;
    Escape/Cancel restores the captured draft. A displayed Task reference is only a value until explicit Run.
    Only keyed actions can become Uncertain in
    Developer_repl_connection custody and require explicit same-key
    Retry/Abandon; ordinary read errors are diagnostics. Abandon never proves
    nonexecution. One abandoned exact lookup remains available for read-only
    Observe without restoring retry custody or authorizing Cancel. A historical
    result alone does not clear unknown current-revision state. If the current
    revision cannot be authoritatively recovered,
    a fresh Reset or Create is required before another Submit. Watch hints
    trigger authoritative Observe; they are not results. While Busy, source
    edits and new keyed actions are refused, but Cancel and Quit remain
    responsive. The loop never blocks its request lane in Task Await, so exact
    Cancel can overtake a running Task. *)
val run
  :  session:Client_adapter.t
  -> create:Agent_protocol.Developer_repl_wire.Create.t
  -> initial:State.t
  -> input:Input_queue.t
  -> present:(State.t -> unit)
  -> (unit, Agent_protocol.Error.t) result
