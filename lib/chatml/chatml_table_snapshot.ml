open Core
open Chatml_lang

let to_array table =
  let invalid () = failwith "Hashtbl.to_array: expected a table of unique string keys" in
  match table with
  | VRef { contents = VArray entries } ->
    let seen = Hash_set.create (module String) in
    Array.map entries ~f:(function
      | VRecord fields when Map.length fields = 2 ->
        (match Map.find fields "key", Map.find fields "value" with
         | Some (VString key), Some value when not (Hash_set.mem seen key) ->
           Hash_set.add seen key;
           VRecord (String.Map.of_alist_exn [ "key", VString key; "value", value ])
         | _ -> invalid ())
      | _ -> invalid ())
  | _ -> invalid ()
;;
