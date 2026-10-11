open Core

type t =
  { environments : string list
  ; package_default : string option
  ; entry_environment : string option
  ; files : (string * string) list
  }

module Request = struct
  type t =
    | Entry of string option
    | Library of string option
  [@@deriving sexp_of]
end

module Error = struct
  type t =
    | Missing_selector
    | Unknown_environment of string
  [@@deriving equal, sexp_of]
end

let select t ~request =
  let priorities =
    match request with
    | Request.Entry explicit -> [ explicit; t.entry_environment; t.package_default ]
    | Request.Library explicit -> [ explicit; t.package_default ]
  in
  match List.find_map priorities ~f:Fn.id with
  | None -> Error Error.Missing_selector
  | Some name ->
    if List.mem t.environments name ~equal:String.equal
    then Ok name
    else Error (Error.Unknown_environment name)
;;

let scope_for t ~selected ~source =
  List.Assoc.find t.files source ~equal:String.equal |> Option.value ~default:selected
;;
