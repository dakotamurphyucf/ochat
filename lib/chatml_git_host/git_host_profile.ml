open Core
module Pin = Chatml_project.Git_pin

module Error = struct
  type code =
    | Unsupported_profile
    | Unsupported_provider
    | Invalid_input
    | Authority_denied
    | Limit_exceeded
    | Stale_request
    | Process_failed
    | Timed_out
    | Storage_failed
    | Cache_corrupt
    | Busy
    | Closed
  [@@deriving equal, sexp_of]

  type t = code [@@deriving sexp_of]

  let create (code : code) : t = code
  let code t = t

  let of_storage_error = function
    | Git_host_native.Code.Unsupported -> create Unsupported_provider
    | Busy -> create Busy
    | Too_large -> create Limit_exceeded
    | Missing | Exists | Denied | Corrupt | Unavailable -> create Storage_failed
  ;;
end

let bounded_absolute input ~max_bytes =
  let bytes = String.length input in
  bytes > 0
  && bytes <= max_bytes
  && Char.equal input.[0] '/'
  && not
       (String.exists input ~f:(fun c ->
          Char.equal c '\000' || Char.equal c '\n' || Char.equal c '\r'))
;;

module Deployment = struct
  type t =
    { helper_directory : string
    ; executable : string
    ; ca_bundle : string option
    }

  let executable t = t.executable
  let helper_directory t = t.helper_directory
  let ca_bundle t = t.ca_bundle
  let helper_names = [ "git"; "git-upload-pack"; "git-remote-https" ]
  let version_arguments = [ "--version"; "--build-options" ]
  let admitted_version output = String.is_prefix output ~prefix:"git version 2.56.0\n"

  let inputs ~native_helper_directory ~native_git_executable ~native_ca_bundle ~max_bytes =
    let lengths =
      [ String.length native_helper_directory; String.length native_git_executable ]
      @ Option.to_list (Option.map native_ca_bundle ~f:String.length)
    in
    let size =
      List.fold_result lengths ~init:0 ~f:(fun n bytes ->
        if n > max_bytes || bytes > max_bytes - n then Error () else Ok (n + bytes))
    in
    if
      max_bytes <= 0
      || Result.is_error size
      || (not (bounded_absolute native_helper_directory ~max_bytes))
      || (not (bounded_absolute native_git_executable ~max_bytes))
      || (not
            (String.equal
               native_git_executable
               (Filename.concat native_helper_directory "git")))
      || Option.exists native_ca_bundle ~f:(fun path ->
        not (bounded_absolute path ~max_bytes))
    then Error (Error.create Invalid_input)
    else
      Ok
        { helper_directory = native_helper_directory
        ; executable = native_git_executable
        ; ca_bundle = native_ca_bundle
        }
  ;;
end

module Grant = struct
  type route =
    | Https of string option
    | Local

  type t =
    { repository : string
    ; route : route
    }

  let repository t = t.repository

  let authorization t =
    match t.route with
    | Https value -> value
    | Local -> None
  ;;

  let input_bytes t =
    String.length t.repository
    + Option.value_map (authorization t) ~default:0 ~f:String.length
  ;;

  let is_https t =
    match t.route with
    | Https _ -> true
    | Local -> false
  ;;

  let forbidden c = Char.equal c '\000' || Char.equal c '\r' || Char.equal c '\n'

  let https ~repository ~authorization ~max_input_bytes =
    let bytes = String.length repository in
    let header_bytes = Option.value_map authorization ~default:0 ~f:String.length in
    if
      max_input_bytes <= 0
      || bytes > max_input_bytes
      || header_bytes > max_input_bytes - bytes
      || not (String.is_prefix repository ~prefix:"https://")
    then Or_error.error_string "invalid HTTPS route"
    else (
      let host_path = String.drop_prefix repository 8 in
      let authority = List.hd_exn (String.split host_path ~on:'/') in
      if
        String.is_empty authority
        || String.exists repository ~f:forbidden
        || String.exists authority ~f:(fun c ->
          Char.equal c '@' || Char.equal c '?' || Char.equal c '#')
        || String.exists repository ~f:(fun c ->
          Char.equal c '#' || Char.equal c '\\' || Char.is_whitespace c)
        || Option.exists authorization ~f:(fun header ->
          String.is_empty header
          || String.exists header ~f:forbidden
          || not (String.is_prefix header ~prefix:"Authorization: "))
      then Or_error.error_string "invalid HTTPS route or authorization header"
      else Ok { repository; route = Https authorization })
  ;;

  let local ~repository ~max_input_bytes =
    if
      max_input_bytes <= 0 || not (bounded_absolute repository ~max_bytes:max_input_bytes)
    then Or_error.error_string "invalid local repository path"
    else Ok { repository; route = Local }
  ;;
end

module Object_id = struct
  type format =
    | Sha1
    | Sha256
  [@@deriving equal, compare, sexp_of]

  type t =
    { format : format
    ; hex : string
    }
  [@@deriving equal, compare, sexp_of]

  let object_format t =
    match t.format with
    | Sha1 -> Pin.Sha1
    | Sha256 -> Pin.Sha256
  ;;

  let to_string t = t.hex

  let create ~object_format hex =
    let format, bytes =
      match object_format with
      | Pin.Sha1 -> Sha1, 40
      | Sha256 -> Sha256, 64
    in
    if
      String.length hex <> bytes
      || not
           (String.for_all hex ~f:(fun c ->
              Char.is_digit c || (Char.( >= ) c 'a' && Char.( <= ) c 'f')))
    then Or_error.error_string "invalid exact Git object ID"
    else Ok { format; hex }
  ;;

  let of_pin pin =
    { format =
        (match Pin.object_format pin with
         | Pin.Sha1 -> Sha1
         | Sha256 -> Sha256)
    ; hex = Pin.commit pin
    }
  ;;
end

type command =
  | Version
  | Init of Pin.object_format
  | Fetch of Grant.t * Pin.t
  | Raw_object of Object_id.t

let configuration deployment ~workspace =
  let config =
    [ "protocol.version=0"
    ; "fetch.fsckObjects=true"
    ; "transfer.fsckObjects=true"
    ; "fetch.unpackLimit=1"
    ; "transfer.unpackLimit=1"
    ; "pack.writeReverseIndex=false"
    ; "index.threads=1"
    ; "pack.threads=1"
    ; "fetch.uriProtocols="
    ; "promisor.acceptFromServer=none"
    ; "credential.helper="
    ; "credential.interactive=false"
    ; "core.hooksPath=" ^ Filename.concat workspace "empty"
    ; "http.cookieFile="
    ; "http.saveCookies=false"
    ; "http.followRedirects=false"
    ; "http.sslVerify=true"
    ; "protocol.allow=never"
    ; "protocol.https.allow=always"
    ; "protocol.file.allow=always"
    ]
  in
  let config =
    config
    @ Option.to_list
        (Option.map (Deployment.ca_bundle deployment) ~f:(fun path ->
           "http.sslCAInfo=" ^ path))
  in
  List.concat_map config ~f:(fun value -> [ "-c"; value ])
;;

let arguments deployment ~workspace command =
  match command with
  | Version -> Deployment.version_arguments
  | Init format ->
    configuration deployment ~workspace
    @ [ "init"
      ; "--bare"
      ; "--quiet"
      ; "--ref-format=files"
      ; "--initial-branch=chatml"
      ; "--template=" ^ Filename.concat workspace "empty"
      ; ("--object-format="
         ^
         match format with
         | Pin.Sha1 -> "sha1"
         | Sha256 -> "sha256")
      ; "."
      ]
  | Fetch (grant, pin) ->
    let config = configuration deployment ~workspace in
    if Grant.is_https grant
    then config @ [ "remote-https"; Grant.repository grant; Grant.repository grant ]
    else
      config
      @ [ "fetch-pack"
        ; "--keep"
        ; "--depth=1"
        ; "--no-progress"
        ; Grant.repository grant
        ; Pin.commit pin
        ]
  | Raw_object _ ->
    configuration deployment ~workspace
    @ [ "--no-replace-objects"; "--no-lazy-fetch"; "cat-file"; "--batch" ]
;;

let stdin = function
  | Version | Init _ -> ""
  | Fetch (grant, pin) ->
    if Grant.is_https grant
    then
      "capabilities\n\
       option depth 1\n\
       option progress false\n\
       option object-format true\n\
       fetch "
      ^ Pin.commit pin
      ^ " "
      ^ Pin.commit pin
      ^ "\n\n\n"
    else ""
  | Raw_object oid -> Object_id.to_string oid ^ "\n"
;;

let environment deployment ~workspace command =
  let private_configuration =
    match command with
    | Fetch (grant, _) ->
      (match Grant.authorization grant with
       | None -> []
       | Some header ->
         [ "GIT_CONFIG_COUNT", "1"
         ; "GIT_CONFIG_KEY_0", "http.extraHeader"
         ; "GIT_CONFIG_VALUE_0", header
         ])
    | Version | Init _ | Raw_object _ -> []
  in
  private_configuration
  @ [ "PATH", Deployment.helper_directory deployment
    ; "GIT_EXEC_PATH", Deployment.helper_directory deployment
    ; "HOME", Filename.concat workspace "home"
    ; "XDG_CONFIG_HOME", Filename.concat workspace "xdg"
    ; "TMPDIR", Filename.concat workspace "tmp"
    ; "GIT_CONFIG_NOSYSTEM", "1"
    ; "GIT_CONFIG_SYSTEM", Filename.concat workspace "config"
    ; "GIT_CONFIG_GLOBAL", Filename.concat workspace "config"
    ; "GIT_TERMINAL_PROMPT", "0"
    ; "GIT_NO_REPLACE_OBJECTS", "1"
    ; "GIT_DIR", "."
    ; "LC_ALL", "C"
    ]
;;

let cache_key ~repository ~max_repository_bytes ~poll pin =
  let bytes = String.length repository in
  if bytes = 0 || bytes > max_repository_bytes
  then Error (Error.create Limit_exceeded)
  else (
    let prefix = sprintf "chatml.git-cache.v1\000git2.56.0-protocol0\000%d\000" bytes in
    let suffix =
      "\000"
      ^ (match Pin.object_format pin with
         | Pin.Sha1 -> "sha1"
         | Sha256 -> "sha256")
      ^ "\000"
      ^ Pin.commit pin
    in
    let open Result.Let_syntax in
    let feed context text =
      let rec loop context offset =
        let%bind () = poll () in
        if offset = String.length text
        then Ok context
        else (
          let length = Int.min 4096 (String.length text - offset) in
          loop
            (Digestif.SHA256.feed_string context ~off:offset ~len:length text)
            (offset + length))
      in
      loop context 0
    in
    let%map context =
      List.fold_result [ prefix; repository; suffix ] ~init:Digestif.SHA256.empty ~f:feed
    in
    Digestif.SHA256.(to_hex (get context)))
;;

let validate_metadata ~object_format ~config ~head =
  let section = ref "" in
  let format_version = ref None in
  let bare = ref false in
  let declared_format = ref None in
  let valid =
    List.for_all (String.split_lines config) ~f:(fun line ->
      let line = String.strip line in
      if String.is_empty line
      then true
      else if String.equal line "[core]"
      then (
        section := "core";
        true)
      else if String.equal line "[extensions]"
      then (
        section := "extensions";
        true)
      else (
        match String.lsplit2 line ~on:'=' with
        | None -> false
        | Some (key, value) ->
          let key = String.lowercase (String.strip key)
          and value = String.strip value in
          (match !section, key with
           | "core", "repositoryformatversion" ->
             format_version := Some value;
             true
           | "core", "bare" ->
             bare := String.equal value "true";
             !bare
           | "core", ("filemode" | "ignorecase" | "precomposeunicode" | "symlinks") ->
             String.equal value "true" || String.equal value "false"
           | "extensions", "objectformat" ->
             declared_format := Some value;
             String.equal value "sha256"
           | _ -> false)))
  in
  valid
  && !bare
  && String.equal head "ref: refs/heads/chatml\n"
  &&
  match object_format with
  | Pin.Sha1 ->
    Option.equal String.equal !format_version (Some "0")
    && Option.is_none !declared_format
  | Sha256 ->
    Option.equal String.equal !format_version (Some "1")
    && Option.equal String.equal !declared_format (Some "sha256")
;;

let permitted_directory = function
  | []
  | [ "objects" ]
  | [ "objects"; "info" ]
  | [ "objects"; "pack" ]
  | [ "refs" ]
  | [ "refs"; "heads" ]
  | [ "refs"; "tags" ] -> true
  | _ -> false
;;

let permitted_file ~object_format = function
  | [ "config" ] | [ "HEAD" ] | [ "shallow" ] -> true
  | [ "objects"; "pack"; name ] ->
    if not (String.is_prefix name ~prefix:"pack-")
    then false
    else (
      match String.rsplit2 (String.drop_prefix name 5) ~on:'.' with
      | Some (hex, ("pack" | "idx" | "keep")) ->
        Result.is_ok (Object_id.create ~object_format hex)
      | Some (_, _) | None -> false)
  | _ -> false
;;
