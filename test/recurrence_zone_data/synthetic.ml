open Core

(* Structural fixture writer, not a timezone evaluator. Explicit caller-supplied
   rows make boundary/tamper tests independent of production decoder helpers. *)
let integer value width =
  String.init width ~f:(fun index ->
    let shift = (width - index - 1) * 8 in
    Int64.(shift_right_logical value shift |> fun v -> bit_and v 255L)
    |> Int64.to_int_exn
    |> Char.of_int_exn)
;;

let header ~version ~count ~types ~chars =
  "TZif"
  ^ String.of_char version
  ^ String.make 15 '\000'
  ^ String.concat
      (List.map [ 0; 0; 0; count; types; chars ] ~f:(fun n -> integer (Int64.of_int n) 4))
;;

let block ~version ~width ~types ~transitions =
  let chars = List.map types ~f:(fun (_, _, name) -> name ^ "\000") |> String.concat in
  let _, records =
    List.fold_map types ~init:0 ~f:(fun index (offset, dst, name) ->
      ( index + String.length name + 1
      , integer (Int64.of_int offset) 4
        ^ (if dst then "\001" else "\000")
        ^ String.of_char (Char.of_int_exn index) ))
  in
  header
    ~version
    ~count:(List.length transitions)
    ~types:(List.length types)
    ~chars:(String.length chars)
  ^ String.concat (List.map transitions ~f:(fun (at, _) -> integer at width))
  ^ String.concat
      (List.map transitions ~f:(fun (_, index) -> String.of_char (Char.of_int_exn index)))
  ^ String.concat records
  ^ chars
;;

let make ?(version = '3') ?(types = [ 0, false, "STD" ]) ?(transitions = []) footer =
  block ~version ~width:4 ~types ~transitions:[]
  ^ block ~version ~width:8 ~types ~transitions
  ^ "\n"
  ^ footer
  ^ "\n"
;;

let replace bytes ~at replacement =
  String.prefix bytes at
  ^ replacement
  ^ String.drop_prefix bytes (at + String.length replacement)
;;

let with_footer bytes footer =
  let final_newline = String.length bytes - 1 in
  let first_newline =
    String.rindex_from bytes (final_newline - 1) '\n' |> Option.value_exn
  in
  String.prefix bytes (first_newline + 1) ^ footer ^ "\n"
;;
