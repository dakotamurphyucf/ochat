open Core

(* Build-time only: emit data from the exact parser used by this installation.
   Menhir's production numbers and source locations are deliberately excluded. *)
let () =
  let filename =
    match Array.to_list (Sys.get_argv ()) with
    | [ _; filename ] -> filename
    | _ -> failwith "usage: embed_grammar CHATML.cmly"
  in
  let module G =
    MenhirSdk.Cmly_read.Read (struct
      let filename = filename
    end)
  in
  let productions =
    G.Production.fold
      (fun production result ->
         match G.Production.kind production with
         | `START -> result
         | `REGULAR ->
           let lhs = G.Nonterminal.name (G.Production.lhs production) in
           let rhs =
             G.Production.rhs production
             |> Array.to_list
             |> List.map ~f:(fun (symbol, _, _) -> G.Symbol.name symbol)
           in
           let action =
             G.Production.action production
             |> Option.value_map ~default:"" ~f:G.Action.expr
           in
           let id = lhs ^ " -> " ^ String.concat ~sep:" " rhs in
           let contract =
             Digestif.SHA256.digest_string (id ^ "\n" ^ action) |> Digestif.SHA256.to_hex
           in
           (id, contract) :: result)
      []
    |> List.sort ~compare:(fun (a, _) (b, _) -> String.compare a b)
  in
  (match List.find_a_dup (List.map productions ~f:fst) ~compare:String.compare with
   | None -> ()
   | Some id -> failwith ("duplicate grammar production: " ^ id));
  printf
    "(* Generated from the compiled ChatML grammar; do not edit. *)\n\
     let productions = [\n";
  List.iter productions ~f:(fun (id, contract) -> printf "(%S, %S);\n" id contract);
  printf "]\n"
;;
