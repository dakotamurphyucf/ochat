open Core

let () =
  let bin_dune_path = "bin/dune" in
  if not (Stdlib.Sys.file_exists bin_dune_path) then (
    eprintf "Error: %s not found@." bin_dune_path;
    Stdlib.exit 1
  );
  let lines = In_channel.read_lines bin_dune_path in
  let binaries =
    List.concat_map lines ~f:(fun line ->
        let line = String.strip line in
        (* Capture "(public_name foo)" *)
        if String.is_prefix line ~prefix:"(public_name" then (
          (* Split by whitespace, expect (public_name foo) *)
          match String.split ~on:' ' line |> List.filter ~f:(Fn.non String.is_empty) with
          | _ :: name :: _ ->
            let name = String.rstrip name ~drop:(Char.equal ')') in
            [name]
          | _ -> []
        ) else if String.is_prefix line ~prefix:"(names" then (
          (* Remove prefix "(names" and trailing ')' then split *)
          let content =
            line
            |> String.chop_prefix_exn ~prefix:"(names"
            |> String.rstrip ~drop:(Char.equal ')')
            |> String.strip
          in
          String.split ~on:' ' content |> List.filter ~f:(Fn.non String.is_empty)
        ) else [] )
  in
  let binaries = List.dedup_and_sort ~compare:String.compare binaries in
  (* ensure output directory *)
  let out_dir = "out" in
(try Stdlib.Sys.mkdir out_dir 0o755 with Sys_error _ -> ());
  let csv_path = Filename.concat out_dir "binaries.csv" in
  Out_channel.with_file csv_path ~f:(fun oc ->
      List.iter binaries ~f:(fun bin -> Out_channel.output_string oc (bin ^ "\n")));
  printf "Wrote %d binaries to %s\n" (List.length binaries) csv_path

