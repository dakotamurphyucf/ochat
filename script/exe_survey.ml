open Core

(* [run_and_capture env cmd] executes [cmd] (list of arguments) using
   [Eio.Process.parse_out] and returns its stdout + stderr combined as a
   string. Any exception is converted to an [Error _]. *)
let run_and_capture env cmd : (string, string) Result.t =
  let proc_mgr = Eio.Stdenv.process_mgr env in
  try
    let out = Eio.Process.parse_out proc_mgr Eio.Buf_read.take_all cmd in
    Ok out
  with
  | exn -> Error (Exn.to_string exn)
;;

let ensure_dir path =
  if not (Stdlib.Sys.file_exists path) then Core_unix.mkdir path ~perm:0o755
;;

(* Load the list of binaries from [out/binaries.csv]. If the file is not
   present, attempt to regenerate it by invoking the [binary_list] script. *)
let load_binaries env =
  let path = "out/binaries.csv" in
  if not (Stdlib.Sys.file_exists path)
  then (
    Format.eprintf "[exe_survey] %s not found – running binary_list generator@." path;
    (* Attempt to run previously installed helper from _build/install *)
    let helper = "_build/install/default/bin/binary_list" in
    match run_and_capture env [ helper ] with
    | Ok _ -> ()
    | Error msg ->
      Format.eprintf
        "[exe_survey] Failed to generate binaries list using %s: %s@."
        helper
        msg);
  if not (Stdlib.Sys.file_exists path)
  then (
    Format.eprintf "[exe_survey] Unable to find or generate %s – aborting@." path;
    exit 1)
  else In_channel.read_lines path |> List.filter ~f:String.(Fn.non is_empty)
;;

let binary_candidates bin =
  let install_dir = "./_build/install/default/bin" in
  let compiled_dir = "./_build/default/bin" in
  let dashed_to_underscore s =
    String.map s ~f:(fun c -> if Char.equal c '-' then '_' else c)
  in
  let list =
    [ Filename.concat install_dir bin
    ; Filename.concat install_dir (bin ^ ".exe")
    ; Filename.concat compiled_dir (bin ^ ".exe")
    ; Filename.concat compiled_dir (dashed_to_underscore bin ^ ".exe")
    ]
  in
  (* special cases *)
  let list =
    match bin with
    | "gpt" -> Filename.concat compiled_dir "main.exe" :: list
    | _ -> list
  in
  List.filter list ~f:(fun path -> Stdlib.Sys.file_exists path)
;;

let capture_help env binary =
  match binary_candidates binary with
  | candidate :: _ ->
    if List.exists [ "key-dump"; "piaf_example" ] ~f:(String.equal binary)
    then Error "Does not support --help"
    else run_and_capture env [ candidate; "--help" ]
  | [] -> Error "Executable not built"
;;

let write_file ~dir ~name content =
  ensure_dir dir;
  let path = Filename.concat dir (name ^ ".txt") in
  Out_channel.write_all path ~data:content
;;

(* [sample_invocation binary] returns [Some args] for a minimal, non-networked
   example invocation of [binary].  Returns [None] if no example should be
   attempted (e.g. the command needs external resources or would perform heavy
   work). *)
let sample_invocation = function
  | "md-search" -> Some [ "--query"; "hello" ]
  | "odoc-search" -> Some [ "--query"; "List.map" ]
  | _ -> None

let capture_example env binary =
  match sample_invocation binary with
  | None -> Ok None
  | Some args ->
    (match binary_candidates binary with
     | candidate :: _ ->
       (match run_and_capture env (candidate :: args) with
        | Ok output -> Ok (Some output)
        | Error _ as e -> e)
     | [] -> Error "Executable not built")

let main env =
  let binaries = load_binaries env in
  let help_dir = "out/help" in
  let examples_dir = "out/examples" in
  ensure_dir "out";
  ensure_dir help_dir;
  ensure_dir examples_dir;
  List.iter binaries ~f:(fun bin ->
      (* Capture --help *)
      (match capture_help env bin with
       | Ok output ->
         write_file ~dir:help_dir ~name:bin output;
         Format.printf "[exe_survey] Captured help for %s@." bin
       | Error msg ->
         let fail_content = Printf.sprintf "<error> %s\n" msg in
         write_file ~dir:help_dir ~name:bin fail_content;
         Format.eprintf "[exe_survey] Error capturing help for %s: %s@." bin msg);

      (* Capture example run if configured *)
      (match capture_example env bin with
       | Ok (Some example_out) ->
         write_file ~dir:examples_dir ~name:bin example_out;
         Format.printf "[exe_survey] Captured example for %s@." bin
       | Ok None -> ()
       | Error msg ->
         let fail_content = Printf.sprintf "<error> %s\n" msg in
         write_file ~dir:examples_dir ~name:bin fail_content;
         Format.eprintf "[exe_survey] Error capturing example for %s: %s@." bin msg));
  Format.printf
    "[exe_survey] Completed capturing help for %d binaries@."
    (List.length binaries)
;;

let () = Eio_main.run main
