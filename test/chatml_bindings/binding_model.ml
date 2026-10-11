open! Core

type value =
  | Number of int
  | Flag of bool
  | Text of string
  | Product of value list
[@@deriving sexp]

type t =
  | Bind of string * value
  | Ignore of value
  | Group of t list

let examples =
  [ Bind ("a", Number 7)
  ; Ignore (Product [ Number 7; Flag true ])
  ; Bind ("a", Product [ Number 7; Flag true ])
  ; Group [ Bind ("a", Number 7); Ignore (Flag true) ]
  ; Group [ Ignore (Number 7); Bind ("b", Text "x") ]
  ; Group [ Bind ("a", Number 7); Bind ("b", Flag true); Bind ("c", Text "x") ]
  ; Group [ Group [ Bind ("a", Number 7); Ignore (Flag true) ]; Bind ("c", Text "x") ]
  ; Group [ Ignore (Number 7); Group [ Ignore (Flag true); Ignore (Text "x") ] ]
  ]
;;

let product strings = "(|" ^ String.concat strings ~sep:", " ^ "|)"

let rec pattern = function
  | Bind (name, _) -> name
  | Ignore _ -> "_"
  | Group children -> product (List.map children ~f:pattern)
;;

let rec value = function
  | Bind (_, value) | Ignore value -> value
  | Group children -> Product (List.map children ~f:value)
;;

let rec leaves = function
  | Bind (name, value) -> [ name, value ]
  | Ignore _ -> []
  | Group children -> List.concat_map children ~f:leaves
;;

let rec source = function
  | Number n -> Int.to_string n
  | Flag b -> Bool.to_string b
  | Text text -> Printf.sprintf "%S" text
  | Product children -> product (List.map children ~f:source)
;;

let rec annotation = function
  | Number _ -> "int"
  | Flag _ -> "bool"
  | Text _ -> "string"
  | Product children ->
    "(" ^ String.concat (List.map children ~f:annotation) ~sep:" * " ^ ")"
;;

let slot = function
  | Number _ -> "int"
  | Flag _ -> "bool"
  | Text _ -> "string"
  | Product _ -> "obj"
;;
