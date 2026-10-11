open Core
module Project = Chatml_project
module Lock = Project.Lock
module Path = Source.Portable_path

module Error = struct
  type t =
    | Invalid_repository_path
    | Repository_escape
  [@@deriving equal, sexp_of]
end

type t =
  { origin : Lock.Catalog.origin
  ; project : Path.t
  }

let origin t = t.origin
let project t = t.project

let portable path ~checkpoint =
  checkpoint ();
  Path.create path |> Result.map_error ~f:(fun _ -> Error.Invalid_repository_path)
;;

let repository_member ~package_root ~member ~checkpoint =
  checkpoint ();
  match package_root with
  | None -> Ok member
  | Some root ->
    let root = Path.to_string root in
    let member = Path.to_string member in
    if String.length root + 1 + String.length member > 1024
    then Error Error.Invalid_repository_path
    else portable (root ^ "/" ^ member) ~checkpoint
;;

let directory_components member ~checkpoint =
  checkpoint ();
  String.split (Path.to_string member) ~on:'/' |> List.rev |> List.tl_exn
;;

let relative_root ~declaration ~directory ~checkpoint =
  let initial = directory_components declaration ~checkpoint in
  let initial_bytes =
    List.fold initial ~init:0 ~f:(fun length component ->
      checkpoint ();
      length + String.length component + 1)
  in
  let length = String.length directory in
  let rec scan stack bytes start offset =
    if offset mod 1024 = 0 then checkpoint ();
    if offset < length && not (Char.equal directory.[offset] '/')
    then
      if offset - start >= 1024
      then Error Error.Invalid_repository_path
      else scan stack bytes start (offset + 1)
    else (
      checkpoint ();
      let component = String.sub directory ~pos:start ~len:(offset - start) in
      let open Result.Let_syntax in
      let%bind stack, bytes =
        match component, stack with
        | ("" | "."), _ -> Ok (stack, bytes)
        | "..", [] -> Error Error.Repository_escape
        | "..", head :: tail -> Ok (tail, bytes - String.length head - 1)
        | _, _ ->
          let%bind component = portable component ~checkpoint in
          let component = Path.to_string component in
          let bytes = bytes + String.length component + 1 in
          if bytes - 1 > 1024
          then Error Error.Invalid_repository_path
          else Ok (component :: stack, bytes)
      in
      if offset < length
      then scan stack bytes (offset + 1) (offset + 1)
      else (
        match stack with
        | [] -> Ok None
        | _ :: _ ->
          portable (String.concat ~sep:"/" (List.rev stack)) ~checkpoint
          |> Result.map ~f:Option.some))
  in
  scan initial initial_bytes 0 0
;;

let resolve dependency ~owner_origin ~owner_declaration ~checkpoint =
  checkpoint ();
  let project = Project.Dependency.project dependency in
  let open Result.Let_syntax in
  let%bind origin =
    match Project.Dependency.source dependency, owner_origin with
    | Project.Dependency.Git { repository; pin }, _ ->
      Ok (Lock.Catalog.Git { repository; pin; package_root = None })
    | Local _, Lock.Catalog.Local -> Ok Lock.Catalog.Local
    | Local { directory }, Git { repository; pin; package_root } ->
      if String.is_prefix directory ~prefix:"/"
      then Ok Lock.Catalog.Local
      else (
        let%bind declaration =
          portable (Chatml_source.Id.relative_path owner_declaration) ~checkpoint
        in
        let%bind declaration =
          repository_member ~package_root ~member:declaration ~checkpoint
        in
        let%map package_root = relative_root ~declaration ~directory ~checkpoint in
        Lock.Catalog.Git { repository; pin; package_root })
  in
  let%map () =
    match origin with
    | Lock.Catalog.Local -> Ok ()
    | Git { repository = _; pin = _; package_root } ->
      repository_member ~package_root ~member:project ~checkpoint
      |> Result.map ~f:(fun _ -> ())
  in
  { origin; project }
;;

let same_string left right ~checkpoint =
  let length = Int.min (String.length left) (String.length right) in
  let rec loop offset =
    if offset mod 1024 = 0 then checkpoint ();
    if offset = length
    then Int.equal (String.length left) (String.length right)
    else if Char.equal left.[offset] right.[offset]
    then loop (offset + 1)
    else false
  in
  loop 0
;;

let matches t ~(origin : Lock.Catalog.origin) ~declaration ~checkpoint =
  checkpoint ();
  let same_origin =
    match t.origin, origin with
    | Lock.Catalog.Local, Local -> true
    | Git expected, Git actual ->
      same_string expected.repository actual.repository ~checkpoint
      && Project.Git_pin.equal expected.pin actual.pin
      && Option.equal Path.equal expected.package_root actual.package_root
    | Local, Git _ | Git _, Local -> false
  in
  same_origin
  && same_string
       (Path.to_string t.project)
       (Chatml_source.Id.relative_path declaration)
       ~checkpoint
;;
