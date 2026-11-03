open! Core

let topics =
  [ "commands"; "shortcuts"; "completion"; "types"; "tasks"; "history"; "recovery" ]
;;

let command_names =
  [ ":submit"
  ; ":run"
  ; ":type"
  ; ":refs"
  ; ":observe"
  ; ":cancel"
  ; ":reset"
  ; ":close"
  ; ":retry"
  ; ":abandon"
  ; ":quit"
  ; ":help"
  ]
;;

let paragraph ~topic =
  match Option.map topic ~f:String.strip with
  | None | Some "" ->
    "ChatML REPL: Enter submits; Alt-Enter (or reported Shift-Enter) inserts a newline. \
     :help commands | shortcuts | completion | types | tasks | history | recovery. Task \
     values run only with :run N."
  | Some "commands" ->
    ":submit submit source; :run N run displayed Task; :type N [binding M] inspect type; \
     :refs list references; :observe refresh; :cancel cancel active work; :reset fresh \
     session epoch; :close close session; :retry exact pending request; :abandon release \
     retry custody; :quit exit; :help [topic]. Indices start at 1."
  | Some "shortcuts" ->
    "Enter submit; Alt-Enter/Shift-Enter newline (terminal support varies); Tab accept \
     completion; Up/Down select completion (otherwise history); Shift-Tab previous \
     completion; Escape dismiss; Left/Right move; Home/End line boundaries; \
     Backspace/Delete erase; Ctrl-R reverse history search; Ctrl-C cancel; Ctrl-Q quit."
  | Some "completion" ->
    "Suggestions combine local commands/keywords and read-only current-session names. \
     Qualified module members are read without initialization. Unselected configured \
     modules can report unavailable. Tab inserts without submitting; Up/Down select; \
     Escape dismisses. Session names do not typecheck or certify unfinished drafts."
  | Some "types" ->
    "Successful expressions display their compiler type and value. Declarations show \
     bounded authoritative binding signatures; binding values are not invented from \
     statement previews. :type N inspects statement N; :type N binding M inspects its \
     binding M. Indices start at 1."
  | Some "tasks" ->
    "Submitting Task-valued code produces a value. :run N explicitly runs a displayed \
     Task reference. :observe refreshes its state; Ctrl-C or :cancel requests \
     cancellation. Another Submit waits for the active Task to settle."
  | Some "history" ->
    "Ctrl-R searches captured local history without running it. Type literal query text; \
     Ctrl-R cycles older matches; Backspace edits the query. Enter loads the match as an \
     unsubmitted draft; Escape or Ctrl-C restores the exact prior draft. Search is \
     available only while the session is idle and current."
  | Some "recovery" ->
    "An uncertain keyed request keeps its exact retry handle. :retry repeats that same \
     request; :observe reads its result; :abandon releases retry custody but does not \
     prove nonexecution. If the current revision is unknown, :reset is required before \
     submitting again."
  | Some _ ->
    "Unknown help topic. :help commands | shortcuts | completion | types | tasks | \
     history | recovery"
;;

(* Topic prose is ASCII and bounded. Explicit short lines remain readable in the
   notice area without treating terminal columns as source byte positions. *)
let text ~topic =
  let words = String.split (paragraph ~topic) ~on:' ' in
  let lines, current =
    List.fold words ~init:([], "") ~f:(fun (lines, current) word ->
      if String.is_empty current
      then lines, word
      else if String.length current + String.length word + 1 <= 72
      then lines, current ^ " " ^ word
      else current :: lines, word)
  in
  String.concat ~sep:"\n" (List.rev (current :: lines))
;;
