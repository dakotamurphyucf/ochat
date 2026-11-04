open! Core
module D = Document_schema

let profile limits =
  let rec depth remaining =
    if remaining <= 0 then `Null else `Array [ depth (remaining - 1) ]
  in
  `Object
    [ "bytes", `String (String.make (D.Limits.max_bytes limits) 'x')
    ; ( "fields"
      , `Object
          (List.init (D.Limits.max_fields limits) ~f:(fun index ->
             Int.to_string index, `Null)) )
    ; "nodes", `Array (List.init (D.Limits.max_nodes limits) ~f:(fun _ -> `Null))
    ; "depth", depth (D.Limits.max_depth limits)
    ]
;;
