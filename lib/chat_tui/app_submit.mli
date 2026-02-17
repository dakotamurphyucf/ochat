(** Local (synchronous) effects of a user submit, plus spawning the streaming worker.

    When the user hits enter, the UI applies immediate local updates
    (append the user message, clear the editor, show a "(thinking…)"
    placeholder, request a redraw) and then spawns the asynchronous OpenAI
    request.  This module owns those submit-specific steps.

    The helper is intentionally stateful: it mutates the supplied {!Model.t}
    and uses {!Chat_tui.App_runtime.t} to record that streaming is starting. *)

(** Captured editor state at the time of submission. *)
type request = App_runtime.submit_request

(** [capture_request ~model] snapshots the current editor buffer.

    @param model UI model supplying the current input buffer and draft mode.

    The returned value is used as an immutable submit payload so subsequent
    edits do not affect the request being processed.

    Example:
    {[
      let req = Chat_tui.App_submit.capture_request ~model in
      ignore (req : Chat_tui.App_submit.request)
    ]} *)
val capture_request : model:Model.t -> request

(** [clear_editor ~model] resets the draft buffer after a submit.

    This clears the input text, resets the cursor position to 0 and switches
    the draft mode back to {!Model.Plain}.

    The helper also invalidates and clears any in-flight type-ahead state by:
    {ul
    {- bumping {!Model.typeahead_generation} so stale completions cannot apply;}
    {- clearing {!Model.typeahead_completion}; and}
    {- closing the preview popup.}}

    @param model UI model to mutate in-place. *)
val clear_editor : model:Model.t -> unit

module Context : sig
  type t =
    { runtime : App_runtime.t
    ; streaming : App_streaming.Context.t
    }
end

(** [start ... submit_request] applies local submit effects and then spawns the
    streaming worker fibre.

    The function:
    {ul
    {- moves the draft into the transcript (as plain text or raw XML); }
    {- clears the editor and scrolls to the bottom; }
    {- injects an assistant placeholder message; }
    {- marks the runtime as [Starting_streaming]; and }
    {- forks a fibre that runs the streaming worker and reports results via the
       internal event stream.}}

    All inputs other than [submit_request] are bundled in
    {!Chat_tui.App_submit.Context.t}.

    @param submit_request Captured editor snapshot to submit.

    Example:
    {[
      let req = Chat_tui.App_submit.capture_request ~model in
      Chat_tui.App_submit.clear_editor ~model;
      Chat_tui.App_submit.start ctx req
    ]}
*)
val start : Context.t -> request -> unit
