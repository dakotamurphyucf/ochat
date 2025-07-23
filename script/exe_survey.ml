open Core

(* [run_and_capture env cmd] executes [cmd] (list of arguments) using
   [Eio.Process.parse_out] and returns its stdout + stderr combined as a
   string. Any exception is converted to an [Error _]. *)
let run_and_capture env cmd : (string, string) Result.t =
  let proc_mgr = Eio.Stdenv.process_mgr env in
  try
    let out =
      Eio.Process.parse_out proc_mgr Eio.Buf_read.take_all cmd
    in
    Ok out
  with
  | exn -> Error (Exn.to_string exn)

let ensure_dir path =
  match Stdlib.Sys.file_exists path with
  | true -> ()
  | false -> Core_unix.mkdir path ~perm:0o755

(* Load the list of binaries from [out/binaries.csv]. If the file is not
   present, attempt to regenerate it by invoking the [binary_list] script. *)
let load_binaries env =
  let path = "out/binaries.csv" in
  if not (Stdlib.Sys.file_exists path) then (
    Format.eprintf
      "[exe_survey] %s not found – running binary_list generator@." path;
    (* Build + run the binary_list helper using dune exec *)
    (match run_and_capture env [ "dune"; "exec"; "binary_list" ] with
     | Ok _ -> ()
     | Error msg ->
       Format.eprintf "[exe_survey] Failed to generate binaries list: %s@." msg)
  );
  if not (Stdlib.Sys.file_exists path) then (
    Format.eprintf "[exe_survey] Unable to find or generate %s – aborting@." path;
    exit 1)
  else In_channel.read_lines path |> List.filter ~f:String.(Fn.non (is_empty))

let capture_help env binary =
  (* We rely on dune to locate the executable; public_name is used directly. *)
  let cmd = [ "dune"; "exec"; binary; "--"; "--help" ] in
  run_and_capture env cmd

let write_file ~dir ~name content =
  ensure_dir dir;
  let path = Filename.concat dir (name ^ ".txt") in
  Out_channel.write_all path ~data:content

let main env =
  let binaries = load_binaries env in
  let help_dir = "out/help" in
  ensure_dir "out";
  ensure_dir help_dir;
  List.iter binaries ~f:(fun bin ->
      match capture_help env bin with
      | Ok output ->
        write_file ~dir:help_dir ~name:bin output;
        Format.printf "[exe_survey] Captured help for %s@." bin
      | Error msg ->
        let fail_content = Printf.sprintf "<error> %s\n" msg in
        write_file ~dir:help_dir ~name:bin fail_content;
        Format.eprintf "[exe_survey] Error capturing help for %s: %s@." bin msg);
  Format.printf "[exe_survey] Completed capturing help for %d binaries@."
    (List.length binaries)

let () = Eio_main.run main

