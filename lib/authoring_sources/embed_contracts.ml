open Core

(* Build-time input identity only. Implementation bodies are never embedded. *)
let rec sources = function
  | [] -> []
  | name :: path :: rest ->
    let hash =
      In_channel.read_all path |> Digestif.SHA256.digest_string |> Digestif.SHA256.to_hex
    in
    (name, hash) :: sources rest
  | _ -> failwith "expected implementation-name/source-path pairs"
;;

let () =
  let inputs =
    Array.to_list (Sys.get_argv ())
    |> List.tl_exn
    |> sources
    |> List.sort ~compare:(fun (a, _) (b, _) -> String.compare a b)
  in
  (match List.find_a_dup (List.map inputs ~f:fst) ~compare:String.compare with
   | None -> ()
   | Some name -> failwith ("duplicate implementation source: " ^ name));
  printf
    "(* Generated implementation source identities; do not edit. *)\nlet sources = [\n";
  List.iter inputs ~f:(fun (name, hash) -> printf "(%S, %S);\n" name hash);
  printf "]\n"
;;
