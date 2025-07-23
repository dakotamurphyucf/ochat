(* readme_validation.ml
   ---------------------------------
   Automated checks for keeping the auto-generated `docs/README_skeleton.md`
   in sync with research artefacts under `out/`.

   Currently the script performs the following validations:

   1.  CLI tools table coverage – every public executable listed in
       `out/binaries.csv` must be mentioned in the markdown table under the
       "## 4  CLI Tools" heading.  A match is considered valid when the row’s
       first column contains the executable name as a substring (e.g. the row
       "gpt chat-completion" covers the `gpt` binary).

   2.  Help transcript availability – for every row in the CLI table whose
       third column looks like a relative path (e.g. "out/help/gpt.txt"), the
       script verifies that the file exists.  Rows containing an ellipsis (…)
       are treated as placeholders and ignored.

   Usage:

   ```
   dune exec readme_validation            # prints a report, exits 0
   dune exec readme_validation --strict   # exits 1 when any problem detected
   ```
*)

open Core
(* Use Stdlib.Sys to avoid Core deprecation wrt Sys module. *)
module SU = struct
  let file_exists_exn path =
    match Stdlib.Sys.file_exists path with
    | true -> true
    | false -> false

  let file_exists path = Stdlib.Sys.file_exists path
end

let skeleton_path = "docs/README_skeleton.md"
let binaries_csv  = "out/binaries.csv"

let cut_cli_tools_section (lines : string list) : string list =
  (* Extract lines from the "## 4  CLI Tools" section until the next heading. *)
  let rec drop_until_header = function
    | [] -> []
    | hd :: tl when String.is_prefix hd ~prefix:"## 4  CLI Tools" -> tl
    | _ :: tl -> drop_until_header tl
  in
  let rec take_until_next_header acc = function
    | [] -> List.rev acc
    | hd :: _ when String.is_prefix (String.strip hd) ~prefix:"## " &&
                  not (String.is_prefix (String.strip hd) ~prefix:"## 4") ->
        List.rev acc
    | hd :: tl -> take_until_next_header (hd :: acc) tl
  in
  drop_until_header lines |> take_until_next_header []

let parse_table_rows (section_lines : string list) : (string * string option) list =
  (* Return (command_cell, help_path_cell option).  Skip header/separator rows. *)
  let is_separator_row s =
    String.for_all s ~f:(function '-' | '|' | ' ' -> true | _ -> false)
  in
  section_lines
  |> List.filter ~f:(fun line -> String.is_prefix (String.strip line) ~prefix:"|")
  |> List.filter ~f:(fun l -> not (is_separator_row l))
  |> List.filter_map ~f:(fun row ->
         let cells =
           row
           |> String.substr_replace_all ~pattern:"|" ~with_:"|"
           |> String.split ~on:'|'
           |> List.map ~f:String.strip
           |> List.filter ~f:(Fn.non String.is_empty)
         in
         match cells with
         | command :: _purpose :: help :: _ -> Some (command, Some help)
         | command :: _purpose :: [] -> Some (command, None)
         | command :: _ -> Some (command, None)
         | _ -> None)

let load_binaries () : string list =
  In_channel.read_lines binaries_csv
  |> List.map ~f:String.strip
  |> List.filter ~f:(Fn.non String.is_empty)

let command_covers_binary ~command ~binary =
  String.is_substring ~substring:binary (String.lowercase command)

let () =
  let strict =
    Sys.get_argv ()
    |> Array.to_list
    |> List.exists ~f:(String.equal "--strict")
  in
  if not (SU.file_exists_exn skeleton_path) then (
    eprintf "Error: '%s' not found.\n" skeleton_path;
    exit 1);

  if not (SU.file_exists_exn binaries_csv) then (
    eprintf "Error: '%s' not found.  Have you run the executable survey?\n" binaries_csv;
    exit 1);

  let lines = In_channel.read_lines skeleton_path in
  let section = cut_cli_tools_section lines in
  let table_rows = parse_table_rows section in

  let binaries = load_binaries () in

  (* Check coverage *)
  let uncovered =
    List.filter binaries ~f:(fun bin ->
        not (List.exists table_rows ~f:(fun (cmd, _) -> command_covers_binary ~command:cmd ~binary:bin)))
  in

  (* Check help paths *)
  let missing_help_paths =
    List.filter_map table_rows ~f:(fun (cmd, help_opt) ->
        match help_opt with
        | None -> None
        | Some help when String.is_substring help ~substring:"…" -> None
        | Some help when String.is_substring help ~substring:"..." -> None
        | Some help when String.is_empty help -> None
        | Some help ->
            if SU.file_exists help then None else Some (cmd, help))
  in

  (* Report *)
  if List.is_empty uncovered && List.is_empty missing_help_paths then (
    printf "README validation: OK – all binaries documented, help transcripts present.\n";
    exit 0)
  else begin
    printf "README validation: issues detected.\n";
    if not (List.is_empty uncovered) then (
      printf "\nBinaries missing in CLI Tools table:\n";
      List.iter uncovered ~f:(fun b -> printf "  - %s\n" b));
    if not (List.is_empty missing_help_paths) then (
      printf "\nRows with missing help transcripts:\n";
      List.iter missing_help_paths ~f:(fun (cmd, help) ->
          printf "  - %s (expected at %s)\n" cmd help));
    if strict then exit 1 else exit 0
  end

