open! Core
module Build = Chatml_project_build
module Prepared = Chatml_prepared_program
module Captured = Chatml_source

module Error = struct
  type t =
    | Invalid_selected_entry
    | Body_limit
    | Lock_identity_unavailable
    | Recomposition of Build.Error.t
end

let fresh_lock_id_from_sources ~sources ~prior_lock ~checkpoint =
  let open Result.Let_syntax in
  let%bind seen, count =
    List.fold_result
      sources
      ~init:(Set.singleton (module Captured.Id) prior_lock, 0)
      ~f:(fun (seen, count) source ->
        checkpoint ();
        if count >= Int.max_value - 1
        then Error Error.Lock_identity_unavailable
        else Ok (Set.add seen (Captured.id source), count + 1))
  in
  let rec choose ordinal =
    checkpoint ();
    if ordinal > count + 1
    then Error Error.Lock_identity_unavailable
    else (
      let namespace = "@ochat.authoring.overlay." ^ Int.to_string ordinal in
      match
        Captured.Id.create ~package_namespace:namespace ~relative_path:"authoring.lock"
      with
      | Error _ -> Error Error.Lock_identity_unavailable
      | Ok candidate ->
        if Set.mem seen candidate then choose (ordinal + 1) else Ok candidate)
  in
  choose 0
;;

let recompose_entry build ~contract ~contents ~max_body_bytes ~limits ~checkpoint =
  let open Result.Let_syntax in
  if max_body_bytes <= 0 || String.length contents > max_body_bytes
  then Error Error.Body_limit
  else (
    let prepared = Build.prepared build in
    let entry = Prepared.entry prepared in
    let body_digest = Captured.Digest.of_contents contents ~checkpoint in
    let%bind original_entry =
      Prepared.find_source prepared entry
      |> Result.of_option ~error:Error.Invalid_selected_entry
    in
    if
      Int.equal (Captured.byte_length original_entry) (String.length contents)
      && Captured.Digest.equal (Captured.digest original_entry) body_digest
    then Ok build
    else (
      let%bind edited_entry =
        Captured.create
          entry
          ~contents
          ~expected_digest:(Some body_digest)
          ~max_bytes:(Some max_body_bytes)
          ~checkpoint
        |> Result.map_error ~f:(fun _ -> Error.Invalid_selected_entry)
      in
      let sources =
        List.map (Prepared.sources prepared) ~f:(fun source ->
          checkpoint ();
          if Captured.Id.equal (Captured.id source) entry then edited_entry else source)
      in
      let%bind lock_id =
        fresh_lock_id_from_sources
          ~sources
          ~prior_lock:(Captured.id (Build.lock_source build))
          ~checkpoint
      in
      Build.Effective.recompose build ~sources ~lock_id ~contract ~limits ~checkpoint
      |> Result.map_error ~f:(fun error -> Error.Recomposition error)))
;;
