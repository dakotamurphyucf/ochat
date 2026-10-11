open Core

type t =
  { components : string list
  ; key : string
  ; byte_length : int
  ; component_count : int
  }

type failure =
  | Invalid
  | Limit_exceeded
[@@deriving equal, sexp_of]

let components t = t.components
let key t = t.key
let byte_length t = t.byte_length
let component_count t = t.component_count

let encode components ~max_bytes ~checkpoint =
  let buffer = Buffer.create (Int.min max_bytes 1024) in
  Buffer.add_char buffer '/';
  List.iteri components ~f:(fun index component ->
    checkpoint ();
    if index > 0 then Buffer.add_char buffer '/';
    Buffer.add_string buffer component);
  Buffer.contents buffer
;;

let normalize input ~base ~max_bytes ~max_components ~checkpoint =
  let length = String.length input in
  if length = 0
  then Error Invalid
  else if length > max_bytes
  then Error Limit_exceeded
  else (
    checkpoint ();
    let absolute = Char.equal input.[0] '/' in
    let initial =
      if absolute then Some [] else Option.map base ~f:(fun t -> t.components)
    in
    match initial with
    | None -> Error Invalid
    | Some initial ->
      let initial_count = List.length initial in
      let initial_bytes =
        List.fold initial ~init:0 ~f:(fun bytes component ->
          bytes + String.length component + 1)
      in
      let rec scan offset start visited reversed count bytes =
        if offset land 4095 = 0 then checkpoint ();
        if offset < length && not (Char.equal input.[offset] '/')
        then
          if Char.equal input.[offset] '\000'
          then Error Invalid
          else if offset - start >= 1024
          then Error Invalid
          else scan (offset + 1) start visited reversed count bytes
        else (
          let size = offset - start in
          let admitted =
            if size = 0
            then Ok (visited, reversed, count, bytes)
            else if visited >= max_components
            then Error Limit_exceeded
            else (
              let component = String.sub input ~pos:start ~len:size in
              if String.equal component "."
              then Ok (visited + 1, reversed, count, bytes)
              else if String.equal component ".."
              then (
                match reversed with
                | [] -> Error Invalid
                | previous :: rest ->
                  Ok (visited + 1, rest, count - 1, bytes - String.length previous - 1))
              else if count >= max_components || size + 1 > max_bytes - bytes
              then Error Limit_exceeded
              else Ok (visited + 1, component :: reversed, count + 1, bytes + size + 1))
          in
          match admitted with
          | Error failure -> Error failure
          | Ok (visited, reversed, count, bytes) ->
            if offset < length
            then scan (offset + 1) (offset + 1) visited reversed count bytes
            else (
              let components = List.rev reversed in
              let key = encode components ~max_bytes ~checkpoint in
              if String.length key > max_bytes
              then Error Limit_exceeded
              else
                Ok
                  { components
                  ; key
                  ; byte_length = String.length key
                  ; component_count = count
                  }))
      in
      if initial_count > max_components || initial_bytes > max_bytes
      then Error Limit_exceeded
      else scan 0 0 0 (List.rev initial) initial_count initial_bytes)
;;

let suffix t ~prefix ~checkpoint =
  let rec loop target prefix =
    checkpoint ();
    match prefix, target with
    | [], rest -> Some rest
    | _ :: _, [] -> None
    | first :: tail, candidate :: rest ->
      if String.equal first candidate then loop rest tail else None
  in
  loop t.components prefix.components
;;
