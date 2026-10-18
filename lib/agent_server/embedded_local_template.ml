open! Core

type error =
  { path : string
  ; message : string
  }

let error path message = Error { path; message }
let maximum_nodes = 65_536
let maximum_depth = 64

(* This lexical pass precedes Sexplib's recursive parser. Quoted strings and
   line comments cannot contribute structural parentheses. The local format
   rejects block and sexp comments rather than reading them recursively. *)
let finite_syntax contents =
  let depth = ref 0 in
  let nodes = ref 0 in
  let quoted = ref false in
  let escaped = ref false in
  let line_comment = ref false in
  let atom_open = ref false in
  let valid = ref true in
  String.iteri contents ~f:(fun index character ->
    if !valid
    then
      if !line_comment
      then (if Char.equal character '\n' then line_comment := false)
      else if !quoted
      then (
        if !escaped
        then escaped := false
        else if Char.equal character '\\'
        then escaped := true
        else if Char.equal character '"'
        then quoted := false)
      else if
        Char.equal character '#'
        && index + 1 < String.length contents
        && (Char.equal contents.[index + 1] '|' || Char.equal contents.[index + 1] ';')
      then valid := false
      else if Char.equal character ';'
      then (
        line_comment := true;
        atom_open := false)
      else if Char.equal character '"'
      then (
        quoted := true;
        atom_open := false;
        incr nodes)
      else if Char.equal character '('
      then (
        incr depth;
        incr nodes;
        atom_open := false)
      else if Char.equal character ')'
      then (
        decr depth;
        atom_open := false)
      else if Char.is_whitespace character
      then atom_open := false
      else if not !atom_open
      then (
        incr nodes;
        atom_open := true);
    if !depth < 0 || !depth > maximum_depth || !nodes > maximum_nodes then valid := false);
  !valid && !depth = 0 && (not !quoted) && not !escaped
;;

let list path = function
  | Sexp.List values -> Ok values
  | _ -> error path "expected a list"
;;

let record path value =
  let open Result.Let_syntax in
  let%bind values = list path value in
  let%bind fields =
    List.map values ~f:(function
      | Sexp.List [ Sexp.Atom key; value ] -> Ok (key, value)
      | _ -> error path "expected record fields")
    |> Result.all
  in
  if
    Option.is_some
      (List.find_a_dup fields ~compare:(fun (a, _) (b, _) -> String.compare a b))
  then error path "duplicate field"
  else Ok fields
;;

let required path fields name =
  match List.Assoc.find fields name ~equal:String.equal with
  | Some value -> Ok value
  | None -> error (path ^ "." ^ name) "missing required field"
;;

let replace_field path fields name ~f =
  let open Result.Let_syntax in
  let%bind original = required path fields name in
  let%map replacement = f original in
  Sexp.List
    (List.map fields ~f:(fun (key, value) ->
       Sexp.List [ Sexp.Atom key; (if String.equal key name then replacement else value) ]))
;;

let map_rows path sexp ~f =
  let open Result.Let_syntax in
  let%bind rows = list path sexp in
  let%map rows = List.map rows ~f |> Result.all in
  Sexp.List rows
;;

let operator_atom path operator_id = function
  | Sexp.Atom "Embedded_operator" ->
    Ok (Sexp.Atom (Agent_protocol.Id.Principal.to_string operator_id))
  | _ -> error path "only Embedded_operator is permitted at a principal coordinate"
;;

let principal_record path operator_id sexp =
  let open Result.Let_syntax in
  let%bind fields = record path sexp in
  let%bind kind = required path fields "authentication_kind" in
  let%bind () =
    match kind with
    | Sexp.Atom "embedded.local" -> Ok ()
    | _ -> error (path ^ ".authentication_kind") "expected embedded.local"
  in
  replace_field
    path
    fields
    "principal_id"
    ~f:(operator_atom (path ^ ".principal_id") operator_id)
;;

let bind_project operator_id sexp =
  let open Result.Let_syntax in
  let path = "authoring_project_access" in
  let%bind fields = record path sexp in
  replace_field path fields "read_grants" ~f:(fun value ->
    map_rows
      (path ^ ".read_grants")
      value
      ~f:(principal_record (path ^ ".read_grants[]") operator_id))
;;

let bind_upload operator_id sexp =
  let open Result.Let_syntax in
  let path = "authoring_upload_access" in
  let%bind fields = record path sexp in
  let bind name fields =
    replace_field path fields name ~f:(fun value ->
      map_rows
        (path ^ "." ^ name)
        value
        ~f:(principal_record (path ^ "." ^ name ^ "[]") operator_id))
  in
  let%bind grants = bind "grants" fields in
  let%bind fields = record path grants in
  bind "selections" fields
;;

let bind_developer operator_id sexp =
  let open Result.Let_syntax in
  let path = "developer" in
  let%bind fields = record path sexp in
  replace_field path fields "selections" ~f:(fun selections ->
    map_rows (path ^ ".selections") selections ~f:(fun selection ->
      let row_path = path ^ ".selections[]" in
      let%bind fields = record row_path selection in
      replace_field row_path fields "principals" ~f:(fun principals ->
        map_rows
          (row_path ^ ".principals")
          principals
          ~f:(principal_record (row_path ^ ".principals[]") operator_id))))
;;

let bind_manifest operator_id sexp =
  let open Result.Let_syntax in
  let path = "manifest_grants" in
  let%bind rows = list path sexp in
  let%bind () =
    if List.length rows <= 256 then Ok () else error path "at most 256 grants"
  in
  let%map rows =
    List.map rows ~f:(fun row ->
      let row_path = path ^ "[]" in
      let%bind fields = record row_path row in
      replace_field row_path fields "principals" ~f:(fun principals ->
        let%bind principals = list (row_path ^ ".principals") principals in
        if List.is_empty principals
        then error (row_path ^ ".principals") "manifest grant requires exact principal"
        else
          List.map principals ~f:(operator_atom (row_path ^ ".principals[]") operator_id)
          |> Result.all
          |> Result.map ~f:(fun values -> Sexp.List values)))
    |> Result.all
  in
  Sexp.List rows
;;
