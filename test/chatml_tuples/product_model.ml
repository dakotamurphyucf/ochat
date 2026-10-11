open! Core

type t =
  | Number of int
  | Flag of bool
  | Text of string
  | Product of t list
[@@deriving sexp]

let leaves = [ Number 0; Number 2; Flag false; Text "small" ]

let products =
  let pairs =
    List.concat_map leaves ~f:(fun first ->
      List.map leaves ~f:(fun second -> Product [ first; second ]))
  in
  let triples = List.map leaves ~f:(fun leaf -> Product [ leaf; Number 2; Flag false ]) in
  let nested =
    List.concat_map leaves ~f:(fun leaf ->
      [ Product [ Product [ leaf; Text "small" ]; Flag false ]
      ; Product [ Number 2; Product [ leaf; Flag false ] ]
      ])
  in
  pairs @ triples @ nested
;;

let rec source = function
  | Number n -> Int.to_string n
  | Flag flag -> Bool.to_string flag
  | Text text -> sprintf "%S" text
  | Product values -> "(|" ^ String.concat ~sep:", " (List.map values ~f:source) ^ "|)"
;;

let rec annotation = function
  | Number _ -> "int"
  | Flag _ -> "bool"
  | Text _ -> "string"
  | Product values -> "(" ^ String.concat ~sep:" * " (List.map values ~f:annotation) ^ ")"
;;

let rec display = function
  | Number n -> Int.to_string n
  | Flag flag -> Bool.to_string flag
  | Text text -> text
  | Product values -> "(|" ^ String.concat ~sep:", " (List.map values ~f:display) ^ "|)"
;;
