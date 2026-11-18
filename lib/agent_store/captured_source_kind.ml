open! Core
module D = Document_schema
module F = Document_fields

module Authored_tool = struct
  type t =
    { name : string
    ; source_sha256 : string
    }
  [@@deriving equal, sexp]

  let validate t =
    let open Result.Let_syntax in
    let%bind () =
      if
        (not (String.is_empty t.name))
        && String.length t.name <= 1024
        && not
             (String.exists t.name ~f:(fun c -> Char.to_int c < 32 || Char.equal c '\127'))
      then Ok ()
      else F.invalid "name" "invalid authored tool name"
    in
    let%map _ = F.digest (`String t.source_sha256) in
    ()
  ;;

  let create ~name ~source_sha256 =
    let t = { name; source_sha256 } in
    let%map.Result () = validate t in
    t
  ;;

  let limits =
    D.Limits.create ~max_bytes:262144 ~max_depth:32 ~max_fields:4096 ~max_nodes:8192
    |> Result.map_error ~f:(fun error -> Sexp.to_string_hum (D.Error.sexp_of_t error))
    |> Result.ok_or_failwith
  ;;

  let of_json json =
    let open Result.Let_syntax in
    let%bind () = D.Json.validate ~limits json in
    let%bind name = F.required json "name" F.string in
    let%bind source_sha256 = F.required json "source_sha256" F.digest in
    create ~name ~source_sha256
  ;;

  let to_json t =
    let%map.Result () = validate t in
    `Object [ "name", `String t.name; "source_sha256", `String t.source_sha256 ]
  ;;

  let shape = F.shape [ "name", D.Shape.value; "source_sha256", D.Shape.value ]
end

type t =
  | Artifact
  | Authored_tool of Authored_tool.t
[@@deriving equal, sexp_of]
