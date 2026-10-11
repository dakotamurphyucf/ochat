type t =
  | Map
  | Iter

let name = function
  | Map -> "map_array"
  | Iter -> "iter_array"
;;
