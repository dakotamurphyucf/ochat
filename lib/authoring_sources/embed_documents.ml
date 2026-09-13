open! Core

let rec read_pairs = function
  | [] -> []
  | name :: path :: rest -> (name, In_channel.read_all path) :: read_pairs rest
  | _ -> failwith "expected document-name/source-path pairs"
;;

let () =
  let documents =
    Sys.get_argv ()
    |> Array.to_list
    |> List.tl_exn
    |> read_pairs
    |> List.sort ~compare:(fun (a, _) (b, _) -> String.compare a b)
  in
  (match List.find_a_dup (List.map documents ~f:fst) ~compare:String.compare with
   | Some name -> failwith ("duplicate embedded document: " ^ name)
   | None -> ());
  printf "(* Generated from shared documentation; do not edit. *)\nlet documents = [\n";
  List.iter documents ~f:(fun (name, text) -> printf "(%S, %S);\n" name text);
  printf "]\n"
;;
