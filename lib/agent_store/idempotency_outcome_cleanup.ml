open! Core
module Outcome = Idempotency_outcome
module F = Document_fields

let prefix = "idempotency-outcome-"
let suffix = ".json"
let max_bytes = Idempotency_outcome.max_encoded_bytes
let max_retirements = 128

type candidate =
  | Immutable of string * string
  | Temporary of string

let digest_name name =
  match
    String.chop_prefix name ~prefix
    |> Option.bind ~f:(fun name -> String.chop_suffix name ~suffix)
  with
  | None -> Error (Store_error.Corrupt "unknown idempotency outcome namespace member")
  | Some digest -> F.digest (`String digest) |> F.store
;;

let candidate name =
  if not (String.is_prefix name ~prefix)
  then Ok None
  else (
    match Durable_file.temporary_target name with
    | Some target -> Result.map (digest_name target) ~f:(fun _ -> Some (Temporary name))
    | None ->
      Result.map (digest_name name) ~f:(fun digest -> Some (Immutable (name, digest))))
;;

let collect ~directory ~reader ~retained =
  let open Result.Let_syntax in
  let retained = String.Set.of_list (List.map retained ~f:Outcome.Reference.digest) in
  let%bind names = Retention_reader.list reader ~directory:"." in
  (* Validate the complete bounded enumeration before deleting any member. *)
  let%bind candidates =
    List.fold_result names ~init:[] ~f:(fun found name ->
      let%map digest = candidate name in
      match digest with
      | None -> found
      | Some (Immutable (_, digest)) when Set.mem retained digest -> found
      | Some candidate -> candidate :: found)
  in
  let candidates = List.take candidates max_retirements in
  let%bind validated =
    List.fold_result candidates ~init:[] ~f:(fun found candidate ->
      match candidate with
      | Temporary name ->
        let%bind kind = Retention_reader.kind reader ~path:name in
        (match kind with
         | `File -> Ok (name :: found)
         | `Directory ->
           Error (Store_error.Corrupt "idempotency temporary path is not a file"))
      | Immutable (name, digest) ->
        let%bind bytes = Retention_reader.read reader ~path:name ~max_bytes in
        let%bind reference =
          Outcome.Reference.of_jsonaf
            (`Object
                [ "tag", `String "terminal"
                ; "digest", `String digest
                ; "encoded_bytes", `String (Int.to_string (String.length bytes))
                ])
          |> F.store
        in
        let%map _ = Outcome.decode reference bytes in
        name :: found)
  in
  try
    List.iter validated ~f:(fun name -> Eio.Path.unlink Eio.Path.(directory / name));
    let%map () =
      if List.is_empty validated then Ok () else Durable_file.sync_directory_in ~directory
    in
    List.length validated
  with
  | (Eio.Io _ | Core_unix.Unix_error _) as exn ->
    Error
      (Store_error.of_exn ~operation:"retire orphan idempotency outcome" ~path:"." exn)
;;
