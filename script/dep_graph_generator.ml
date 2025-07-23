let run cmd =
  let ic = Unix.open_process_in cmd in
  let buf = Buffer.create 4096 in
  (try while true do Buffer.add_channel buf ic 1024 done with End_of_file -> ());
  match Unix.close_process_in ic with
  | Unix.WEXITED 0 -> Buffer.contents buf
  | _ -> failwith ("cmd failed: " ^ cmd)

let () =
  let root = Sys.getcwd () in
  let plan_dir = Filename.concat root "plan" in
  (try Unix.mkdir plan_dir 0o755 with Unix.Unix_error (Unix.EEXIST,_,_) -> ());

  let sexp_str = run "dune describe workspace --lang=0.1 --format=sexp" in
  let write path data = let oc = open_out path in output_string oc data; close_out oc in
  let sexp_path = Filename.concat plan_dir "deps.sexp" in
  write sexp_path sexp_str;

  let json_path = Filename.concat plan_dir "deps.json" in
  write json_path (Yojson.Safe.to_string (`String sexp_str));

  (* naive line scan to extract libraries *)
  let libs = ref [] in
  let expecting = ref false in
  sexp_str
  |> String.split_on_char '\n'
  |> List.iter (fun line ->
         let l = String.trim line in
         if !expecting then (
           expecting := false;
           if Str.string_match (Str.regexp "^(.*name +\\([^ )]+\\)") l 0 then (
             let name = Str.matched_group 1 l in
             libs := name :: !libs)
         )
         else if l = "(library" then expecting := true);

  let mmd_path = Filename.concat plan_dir "deps.mmd" in
  let oc = open_out mmd_path in
  output_string oc "%% Auto-generated – edit via script/dep_graph_generator.ml\n";
  output_string oc "graph TD\n";
  List.iter (fun lib -> Printf.fprintf oc "  lib_%s([\"%s\"])\n" lib lib) (List.sort_uniq String.compare !libs);
  close_out oc;

  write (Filename.concat plan_dir "deps-check.log") "TODO";
  Printf.printf "Dependency files written to %s\n" plan_dir;

