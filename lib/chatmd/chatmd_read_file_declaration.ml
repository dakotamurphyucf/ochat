open! Core
module A = Chatmd_attributes
module Ast = Chatmd_ast
module D = Chatmd_shell_spec.Diagnostic
module E = Chatmd_shell_spec.Shell_element
module P = Chatmd_shell_spec.Path_expr
module Spec = Chatmd_read_file_spec

exception Parse_error of D.t

let raise_error diagnostic = raise_notrace (Parse_error diagnostic)
let fail source path code message = raise_error (D.error ~source ~path ~code message)

let attributes source path allowed values =
  match A.create ~source ~path ~allowed values with
  | Ok attributes -> attributes
  | Error diagnostic -> raise_error diagnostic
;;

let optional attributes name =
  match A.optional attributes name with
  | Ok value -> value
  | Error diagnostic -> raise_error diagnostic
;;

let required attributes name =
  match A.required attributes name with
  | Ok value -> value
  | Error diagnostic -> raise_error diagnostic
;;

let description attributes =
  Option.bind (optional attributes "description") ~f:(fun value ->
    let value = String.strip value in
    Option.some_if (not (String.is_empty value)) value)
;;

let no_children source path children =
  List.iter children ~f:(function
    | Ast.Text text when String.for_all text ~f:Char.is_whitespace -> ()
    | Ast.Text _ -> fail source path "read_file.unexpected_text" "unexpected text"
    | Ast.Element _ ->
      fail source path "read_file.unexpected_child" "read roots must be empty")
;;

let parse_path source path value =
  match P.parse ~default_base:Tool_dir value with
  | Ok value -> value
  | Error message -> fail source (path @ [ "path" ]) "read_file.invalid_path" message
;;

let parse_root source index = function
  | Ast.Element (Ast.Shell_element E.Read, raw_attributes, children) ->
    let path = [ "tool"; "read"; Int.to_string index ] in
    let attributes =
      attributes source path [ "id"; "path"; "description" ] raw_attributes
    in
    no_children source path children;
    let id = required attributes "id" |> String.strip in
    if String.is_empty id
    then fail source (path @ [ "id" ]) "read_file.invalid_root_id" "root id is empty";
    { Spec.Root.id
    ; path = parse_path source path (required attributes "path")
    ; description = description attributes
    }
  | Ast.Element _ ->
    fail source [ "tool" ] "read_file.unexpected_child" "only <read> roots are allowed"
  | Ast.Text text when String.for_all text ~f:Char.is_whitespace -> assert false
  | Ast.Text _ -> fail source [ "tool" ] "read_file.unexpected_text" "unexpected text"
;;

let roots source children =
  let roots =
    List.filter children ~f:(function
      | Ast.Text text -> not (String.for_all text ~f:Char.is_whitespace)
      | Ast.Element _ -> true)
    |> List.mapi ~f:(parse_root source)
  in
  match
    List.find_a_dup
      (List.map roots ~f:(fun root -> root.Spec.Root.id))
      ~compare:String.compare
  with
  | None -> roots
  | Some id ->
    fail source [ "tool"; "read" ] "read_file.duplicate_root" ("duplicate root id: " ^ id)
;;

let parse_exn source = function
  | Ast.Element (Ast.Tool, raw_attributes, children) ->
    let attributes =
      attributes source [ "tool" ] [ "name"; "description" ] raw_attributes
    in
    let name = required attributes "name" in
    if not (String.equal name "read_file" || String.equal name "get_contents")
    then fail source [ "tool"; "name" ] "read_file.invalid_name" "expected read_file";
    let description = description attributes in
    (match roots source children with
     | [] -> Spec.default ~source ?description ()
     | roots -> { Spec.roots; description; source })
  | Ast.Element _ -> fail source [ "tool" ] "read_file.invalid_element" "expected <tool>"
  | Ast.Text _ -> fail source [ "tool" ] "read_file.invalid_element" "expected <tool>"
;;

let parse ~source node =
  try Ok (parse_exn source node) with
  | Parse_error diagnostic -> Error [ diagnostic ]
;;

let escape value =
  String.concat_map value ~f:(function
    | '&' -> "&amp;"
    | '<' -> "&lt;"
    | '>' -> "&gt;"
    | '"' -> "&quot;"
    | '\'' -> "&apos;"
    | character -> String.of_char character)
;;

let attribute name value = sprintf " %s=\"%s\"" name (escape value)

let serialize_root (root : Spec.Root.t) =
  sprintf
    "<read%s%s%s/>"
    (attribute "id" root.id)
    (attribute "path" (P.to_string root.path))
    (Option.value_map root.description ~default:"" ~f:(attribute "description"))
;;

let serialize (specification : Spec.t) =
  let description =
    Option.value_map specification.description ~default:"" ~f:(attribute "description")
  in
  sprintf
    "<tool name=\"read_file\"%s>%s</tool>"
    description
    (String.concat (List.map specification.roots ~f:serialize_root))
;;
