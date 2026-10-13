open Core

module Id = struct
  type t = string [@@deriving equal, compare, sexp_of]

  let first c = Char.(c >= 'a' && c <= 'z')
  let rest c = first c || Char.is_digit c || Char.equal c '_' || Char.equal c '-'

  let of_string value =
    if String.is_empty value || String.length value > 64
    then Or_error.error_string "builtin identifier must contain1..64bytes"
    else if not (first value.[0] && String.for_all value ~f:rest)
    then Or_error.error_string "builtin identifier must match [a-z][a-z0-9_-]*"
    else Ok value
  ;;

  let to_string t = t
end

module Asset = struct
  type t =
    { id : Id.t
    ; title : string
    ; contents : string
    ; sha256 : string
    }

  let id t = t.id
  let title t = t.title
  let contents t = t.contents
  let sha256 t = t.sha256
  let byte_length t = String.length t.contents
end

module Error = struct
  type t = Unknown_id of Id.t [@@deriving equal, sexp_of]
end

module Enumeration_error = struct
  type t =
    | Invalid_limit
    | Too_many
  [@@deriving equal, sexp_of]
end

let chatml =
  let contents =
    List.Assoc.find_exn Bundled_instructions.documents "chatml" ~equal:String.equal
  in
  { Asset.id = Id.of_string "chatml" |> Or_error.ok_exn
  ; title = "ChatML starter"
  ; contents
  ; sha256 = Digestif.SHA256.(digest_string contents |> to_hex)
  }
;;

let installed = [ chatml ]

let installed_ids ~max_items ~checkpoint =
  checkpoint ();
  if max_items <= 0
  then Error Enumeration_error.Invalid_limit
  else if List.length installed > max_items
  then Error Enumeration_error.Too_many
  else
    Ok
      (List.map installed ~f:(fun asset ->
         checkpoint ();
         Asset.id asset))
;;

let find id =
  match List.find installed ~f:(fun asset -> Id.equal id (Asset.id asset)) with
  | Some asset -> Ok asset
  | None -> Error (Error.Unknown_id id)
;;
