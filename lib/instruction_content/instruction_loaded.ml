open! Core
open Instruction_model

let member_valid kind member =
  let path = Source.Portable_path.to_string member in
  match kind with
  | Kind.Project_guidance -> String.equal (Filename.basename path) "AGENTS.md"
  | Skill ->
    (match String.split path ~on:'/' with
     | [ name; "SKILL.md" ] -> Result.is_ok (Skill_slug.of_string name)
     | _ -> false)
;;

let construct source ~root ~kind ~limits ~poll =
  let open Result.Let_syntax in
  let length = Capture.byte_length source in
  if length > Limits.file_bytes limits || length > Limits.total_bytes limits
  then Error Error.Limit_exceeded
  else (
    let%bind member =
      Source.Portable_path.create (Capture.Id.relative_path (Capture.id source))
      |> Result.map_error ~f:(fun _ -> Error.Invalid_identity)
    in
    if not (member_valid kind member)
    then Error Error.Invalid_identity
    else (
      let contents = Capture.contents source in
      let%map metadata, body_start_byte =
        Instruction_metadata.parse ~kind ~member contents ~poll
      in
      let observed =
        { Observed.id = { Id.root; member }
        ; kind
        ; digest = Capture.digest source
        ; byte_length = length
        ; metadata
        }
      in
      { Loaded.observed; contents; body_start_byte }))
;;

let observe source ~root ~kind ~limits ~checkpoint =
  bounded limits ~checkpoint (fun poll ->
    construct source ~root ~kind ~limits ~poll |> Result.map ~f:Loaded.observed)
;;

let load observed ~source ~limits ~checkpoint =
  bounded limits ~checkpoint (fun poll ->
    poll ();
    if
      not
        (String.equal
           (Source.Portable_path.to_string observed.Observed.id.member)
           (Capture.Id.relative_path (Capture.id source)))
    then Error Error.Invalid_identity
    else if not (Capture.Digest.equal observed.digest (Capture.digest source))
    then
      Error (Error.Stale { expected = observed.digest; observed = Capture.digest source })
    else construct source ~root:observed.id.root ~kind:observed.kind ~limits ~poll)
;;
