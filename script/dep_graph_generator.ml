open Printf

let run_cmd cmd =
  let ic = Unix.open_process_in cmd in
  let buf = Buffer.create 4096 in
  (try while true do Buffer.add_channel buf ic 1024 done with End_of_file -> ());
  match Unix.close_process_in ic with
  | Unix.WEXITED 0 -> Buffer.contents buf
  | _ -> failwith ("Command failed: " ^ cmd)

let () =
  let root = Sys.getcwd () in
  let plan_dir = Filename.concat root "plan" in
  (try Unix.mkdir plan_dir 0o755 with Unix.Unix_error (Unix.EEXIST, _, _) -> ());

  (* 1. Raw sexp *)
  let sexp_data = run_cmd "dune describe workspace --lang=0.1 --format=sexp" in
  let write path data = let oc = open_out path in output_string oc data; close_out oc in
  let sexp_path = Filename.concat plan_dir "deps.sexp" in
  write sexp_path sexp_data;

  (* 2. Store JSON as simple string wrapper *)
  let json_path = Filename.concat plan_dir "deps.json" in
  write json_path (Printf.sprintf "\"%s\"" (String.escaped sexp_data));

  (* 3. Minimal Mermaid diagram listing libraries *)
  let mermaid_path = Filename.concat plan_dir "deps.mmd" in
  let oc = open_out mermaid_path in
  output_string oc "%% Auto-generated – edit via script/dep_graph_generator.ml\n";
  output_string oc "graph TD\n";
  let add_lib lib = fprintf oc "  lib_%s([\"%s\"])\n" lib lib in
  let lines = String.split_on_char '\n' sexp_data in
  List.iter (fun line ->
      if String.contains line '(' && String.contains line ')' then
        (try
           if String.sub line 0 (min 9 (String.length line)) = "(library " then
             let parts = String.split_on_char ' ' line in
             match List.filter (fun s -> String.length s > 0) parts with
             | _ :: name :: _ ->
                 let name_trim = String.trim (String.map (fun c -> if c = ')' then ' ' else c) name) in
                 add_lib name_trim
             | _ -> ()
         with _ -> ())) lines;
  close_out oc;

  (* 4. Placeholder deps-check log *)
  let log_path = Filename.concat plan_dir "deps-check.log" in
  write log_path "TODO: cross-check public names vs dune-project";

  printf "Dependency files written to %s\n" plan_dir;

