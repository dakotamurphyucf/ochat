open Core

(* Default timeout before the prompt auto-resolves.  Kept in a value so
   tests can override via [set_timeout_for_unit_test]. *)
let default_timeout_s = ref 10.0
let set_timeout_for_unit_test ~seconds = default_timeout_s := seconds [@@warning "-32"]

let prompt_archive ?(timeout_s = !default_timeout_s) ?(default = false) () : bool =
  (* Render prompt and flush so it appears immediately after releasing
     the Notty terminal (if any). *)
  Out_channel.output_string stdout "Archive conversation to ChatMarkdown file? [y/N] ";
  Out_channel.flush stdout;
  (* We leverage [select] on [stdin] to wait for user input with a
     timeout.  This keeps the implementation Unix-only but avoids extra
     dependencies.  A pure Notty-based prompt would require deeper
     integration with the event loop that is out of scope here. *)
  let fd = Core_unix.stdin in
  let read_answer () =
    let timeout =
      if Float.is_negative timeout_s || Float.equal timeout_s 0.
      then `Immediately
      else `After (Core.Time_ns.Span.of_sec timeout_s)
    in
    let ready_fds = Core_unix.select ~read:[ fd ] ~write:[] ~except:[] ~timeout () in
    match ready_fds.read with
    | [] -> None (* timed-out *)
    | _ :: _ ->
      (match In_channel.input_line In_channel.stdin with
       | None -> None
       | Some line ->
         let ans = String.lowercase (String.strip line) in
         if List.mem [ "y"; "yes" ] ans ~equal:String.equal
         then Some true
         else if List.mem [ "n"; "no" ] ans ~equal:String.equal
         then Some false
         else None)
  in
  match read_answer () with
  | Some b -> b
  | None -> default
;;
