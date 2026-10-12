open Core
module D = Document_schema
module Captured = Chatml_source

let invalid reason = Error (D.Error.Invalid_field { path = [ "payload" ]; reason })

let field json name =
  match D.Json.field json ~name with
  | Value value -> Ok value
  | Absent -> invalid "Missing custody field"
  | Null -> invalid "Null custody field"
;;

let string = function
  | `String value -> Ok value
  | _ -> invalid "Expected string"
;;

let int = function
  | `Number value ->
    (match Int.of_string_opt value with
     | Some number when number >= 0 && String.equal value (Int.to_string number) ->
       Ok number
     | Some _ | None -> invalid "Expected canonical nonnegative integer")
  | _ -> invalid "Expected integer"
;;

let digest json =
  Result.bind (string json) ~f:(fun value ->
    Captured.Digest.of_hex value
    |> Result.map_error ~f:(fun _ ->
      D.Error.Invalid_field { path = [ "payload" ]; reason = "Invalid SHA256" }))
;;

let hex json ~checkpoint =
  let open Result.Let_syntax in
  let%bind value = string json in
  let digit = function
    | '0' .. '9' as c -> Some (Char.to_int c - Char.to_int '0')
    | 'a' .. 'f' as c -> Some (Char.to_int c - Char.to_int 'a' + 10)
    | _ -> None
  in
  if String.length value mod 2 <> 0
  then invalid "Invalid hexadecimal byte label"
  else (
    let rec validate index =
      if index mod 4096 = 0 then checkpoint ();
      if index = String.length value
      then Ok ()
      else (
        match digit value.[index] with
        | None -> invalid "Invalid lowercase hexadecimal byte label"
        | Some _ -> validate (index + 1))
    in
    let%bind () = validate 0 in
    let output = Bytes.create (String.length value / 2) in
    for index = 0 to Bytes.length output - 1 do
      if index mod 2048 = 0 then checkpoint ();
      Bytes.set
        output
        index
        (Char.of_int_exn
           ((16 * Option.value_exn (digit value.[2 * index]))
            + Option.value_exn (digit value.[(2 * index) + 1])))
    done;
    Ok (Bytes.to_string output))
;;

let id json ~checkpoint =
  let open Result.Let_syntax in
  let%bind namespace = Result.bind (field json "namespace_hex") ~f:(hex ~checkpoint) in
  let%bind path = Result.bind (field json "path") ~f:string in
  Captured.Id.create ~package_namespace:namespace ~relative_path:path
  |> Result.map_error ~f:(fun _ ->
    D.Error.Invalid_field
      { path = [ "payload" ]; reason = "Invalid logical source identity" })
;;

let array json ~maximum ~checkpoint =
  match json with
  | `Array values ->
    let open Result.Let_syntax in
    let%map _ =
      List.fold_result values ~init:0 ~f:(fun count _ ->
        checkpoint ();
        if count >= maximum
        then Error (D.Error.Limit_exceeded "Custody array count")
        else Ok (count + 1))
    in
    values
  | _ -> invalid "Expected array"
;;
