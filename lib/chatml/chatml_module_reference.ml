open Core

type t =
  { implementation : Chatml_source.Id.t
  ; member_path : string list
  }
[@@deriving compare, equal]

let identifier name =
  (not (String.is_empty name))
  && (Char.is_alpha name.[0] || Char.equal name.[0] '_')
  && String.for_all name ~f:(fun c -> Char.is_alphanum c || Char.equal c '_')
;;

let create implementation ~member_path =
  if List.for_all member_path ~f:identifier
  then Ok { implementation; member_path }
  else Or_error.error_string "Invalid canonical module member identifier"
;;

let implementation t = t.implementation
let member_path t = t.member_path

module Wire = struct
  type t =
    { package_namespace : string
    ; relative_path : string
    ; member_path : string list
    }
  [@@deriving sexp]
end

let sexp_of_t t =
  Wire.sexp_of_t
    { package_namespace = Chatml_source.Id.package_namespace t.implementation
    ; relative_path = Chatml_source.Id.relative_path t.implementation
    ; member_path = t.member_path
    }
;;

let t_of_sexp sexp =
  let wire = Wire.t_of_sexp sexp in
  let result =
    let open Or_error.Let_syntax in
    let%bind id =
      Chatml_source.Id.create
        ~package_namespace:wire.package_namespace
        ~relative_path:wire.relative_path
    in
    create id ~member_path:wire.member_path
  in
  match result with
  | Ok t -> t
  | Error error -> of_sexp_error (Error.to_string_hum error) sexp
;;

module Binding_target = struct
  type reference = t

  type t =
    | Local_binding of string
    | Configured_member of reference
end
