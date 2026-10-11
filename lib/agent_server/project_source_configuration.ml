open Core

module Repository = struct
  type t =
    | Https of
        { repository : string
        ; authorization_header_file : string option
        }
    | Local of string
  [@@deriving compare, equal, sexp]
end

module Git_deployment = struct
  type t =
    { helper_directory : string
    ; ca_bundle : string option
    ; memory_bytes_per_process : int
    ; repositories : Repository.t list
    }
  [@@deriving compare, equal, sexp]
end

type t =
  { external_roots : string list
  ; git : Git_deployment.t option
  }
[@@deriving compare, equal, sexp]

let local_only = { external_roots = []; git = None }
let maximum_path_bytes = 4_096
let maximum_external_roots = 32
let maximum_git_grants = 128
let maximum_authority_bytes = 65_536
let maximum_authorization_header_bytes = 4_096

let invalid field =
  Or_error.error_s [%message "Invalid project source configuration" (field : string)]
;;

let count values ~maximum ~field =
  let rec loop remaining = function
    | [] -> Ok ()
    | _ :: rest -> if remaining = 0 then invalid field else loop (remaining - 1) rest
  in
  loop maximum values
;;

let fold_strings t ~init ~f =
  let open Or_error.Let_syntax in
  let%bind value =
    List.fold_result t.external_roots ~init ~f:(fun acc value ->
      f acc "external_roots" value)
  in
  match t.git with
  | None -> Ok value
  | Some git ->
    let%bind value = f value "git.helper_directory" git.helper_directory in
    let%bind value =
      match git.ca_bundle with
      | None -> Ok value
      | Some path -> f value "git.ca_bundle" path
    in
    List.fold_result git.repositories ~init:value ~f:(fun acc -> function
      | Repository.Local repository -> f acc "git.repositories.local" repository
      | Https { repository; authorization_header_file } ->
        let%bind acc = f acc "git.repositories.https" repository in
        (match authorization_header_file with
         | None -> Ok acc
         | Some path -> f acc "git.repositories.authorization_header_file" path))
;;

let validate_raw t =
  let open Or_error.Let_syntax in
  let%bind () =
    count t.external_roots ~maximum:maximum_external_roots ~field:"external_roots"
  in
  let%bind () =
    match t.git with
    | None -> Ok ()
    | Some git ->
      let%bind () =
        count git.repositories ~maximum:maximum_git_grants ~field:"git.repositories"
      in
      if git.memory_bytes_per_process <= 0
      then invalid "git.memory_bytes_per_process"
      else Ok ()
  in
  let%bind (_ : int) =
    fold_strings t ~init:0 ~f:(fun total field value ->
      let bytes = String.length value in
      if
        bytes = 0 || bytes > maximum_path_bytes || bytes > maximum_authority_bytes - total
      then invalid field
      else Ok (total + bytes))
  in
  fold_strings t ~init:() ~f:(fun () field value ->
    if String.exists value ~f:(Char.equal '\000') then invalid field else Ok ())
;;

let absolute field value = if Filename.is_absolute value then Ok () else invalid field

let validate t =
  let open Or_error.Let_syntax in
  let%bind () = validate_raw t in
  let%bind (_ : String.Set.t) =
    List.fold_result t.external_roots ~init:String.Set.empty ~f:(fun paths path ->
      let%bind () = absolute "external_roots" path in
      if Set.mem paths path
      then invalid "external_roots.duplicate"
      else Ok (Set.add paths path))
  in
  match t.git with
  | None -> Ok ()
  | Some git ->
    let%bind () = absolute "git.helper_directory" git.helper_directory in
    let%bind () =
      match git.ca_bundle with
      | None -> Ok ()
      | Some path -> absolute "git.ca_bundle" path
    in
    let%map (_ : String.Set.t) =
      List.fold_result
        git.repositories
        ~init:String.Set.empty
        ~f:(fun routes repository ->
          let%bind route =
            match repository with
            | Repository.Local repository ->
              let%bind () = absolute "git.repositories.local" repository in
              let%map (_ : Chatml_git_host.Grant.t) =
                Chatml_git_host.Grant.local
                  ~repository
                  ~max_input_bytes:maximum_path_bytes
                |> Result.map_error ~f:(fun _ ->
                  Error.of_string "Invalid local Git route")
              in
              repository
            | Https { repository; authorization_header_file } ->
              let%bind () =
                match authorization_header_file with
                | None -> Ok ()
                | Some path -> absolute "git.repositories.authorization_header_file" path
              in
              let%map (_ : Chatml_git_host.Grant.t) =
                Chatml_git_host.Grant.https
                  ~repository
                  ~authorization:None
                  ~max_input_bytes:maximum_path_bytes
                |> Result.map_error ~f:(fun _ ->
                  Error.of_string "Invalid HTTPS Git route")
              in
              repository
          in
          if Set.mem routes route
          then invalid "git.repositories.duplicate"
          else Ok (Set.add routes route))
    in
    ()
;;
