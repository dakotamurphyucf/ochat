open! Core

module Error = struct
  type t =
    | Invalid_input
    | Invalid_callback
    | State_mismatch
    | Authorization_denied
    | Invalid_json
    | Response_too_large
    | Invalid_pkce
    | Invalid_grant
  [@@deriving equal, sexp_of]
end

module Presence = struct
  type 'a t =
    | Absent
    | Null
    | Value of 'a
end

let bounded value ~maximum =
  (not (String.is_empty value))
  && String.length value <= maximum
  && String.for_all value ~f:(fun c -> Char.to_int c >= 0x20 && Char.to_int c < 0x7f)
;;

module Pkce = struct
  type t =
    { verifier : string
    ; challenge : string
    }

  let verifier t = t.verifier
  let challenge t = t.challenge

  let of_verifier verifier =
    if
      String.length verifier < 43
      || String.length verifier > 128
      || not
           (String.for_all verifier ~f:(fun c ->
              Char.is_alphanum c || String.mem "-._~" c))
    then Error Error.Invalid_pkce
    else
      Ok
        { verifier
        ; challenge =
            Digestif.SHA256.(digest_string verifier |> to_raw_string)
            |> Base64.encode_exn ~pad:false ~alphabet:Base64.uri_safe_alphabet
        }
  ;;

  let of_pair ~verifier ~challenge =
    let open Result.Let_syntax in
    let%bind t = of_verifier verifier in
    if String.equal t.challenge challenge then Ok t else Error Error.Invalid_pkce
  ;;
end

let hex c =
  match c with
  | '0' .. '9' -> Some (Char.to_int c - Char.to_int '0')
  | 'a' .. 'f' -> Some (10 + Char.to_int c - Char.to_int 'a')
  | 'A' .. 'F' -> Some (10 + Char.to_int c - Char.to_int 'A')
  | _ -> None
;;

let unescape text =
  let output = Buffer.create (String.length text) in
  let rec loop i =
    if i = String.length text
    then Ok (Buffer.contents output)
    else (
      match text.[i] with
      | '%' when i + 2 < String.length text ->
        (match hex text.[i + 1], hex text.[i + 2] with
         | Some a, Some b ->
           Buffer.add_char output (Char.of_int_exn ((a * 16) + b));
           loop (i + 3)
         | _ -> Error Error.Invalid_callback)
      | '%' -> Error Error.Invalid_callback
      | '+' ->
        Buffer.add_char output ' ';
        loop (i + 1)
      | c ->
        Buffer.add_char output c;
        loop (i + 1))
  in
  loop 0
;;

module Callback = struct
  type t =
    | Code of string
    | Denied

  let parse ~expected_state ~target =
    let open Result.Let_syntax in
    if String.length target > 8192 || String.mem target '#'
    then Error Error.Invalid_callback
    else (
      match String.lsplit2 target ~on:'?' with
      | Some ("/auth/callback", query) ->
        let parts = String.split query ~on:'&' in
        if List.length parts > 16
        then Error Error.Invalid_callback
        else (
          let%bind pairs =
            List.map parts ~f:(fun part ->
              match String.lsplit2 part ~on:'=' with
              | None -> Error Error.Invalid_callback
              | Some (key, value) ->
                let%bind key = unescape key in
                let%map value = unescape value in
                key, value)
            |> Result.all
          in
          let%bind fields =
            match String.Map.of_alist pairs with
            | `Duplicate_key _ -> Error Error.Invalid_callback
            | `Ok map -> Ok map
          in
          let%bind () =
            match Map.find fields "state" with
            | Some state when String.equal state expected_state -> Ok ()
            | _ -> Error Error.State_mismatch
          in
          match Map.find fields "code", Map.find fields "error" with
          | Some code, None when bounded code ~maximum:4096 -> Ok (Code code)
          | None, Some error when bounded error ~maximum:128 -> Ok Denied
          | _ -> Error Error.Invalid_callback)
      | _ -> Error Error.Invalid_callback)
  ;;
end

let decode_object body =
  if String.length body > 262144
  then Error Error.Response_too_large
  else (
    match Jsonaf.parse body with
    | Ok (`Object fields) ->
      (match String.Map.of_alist fields with
       | `Duplicate_key _ -> Error Error.Invalid_json
       | `Ok fields -> Ok fields)
    | _ -> Error Error.Invalid_json
    | exception (Failure _ | Invalid_argument _) -> Error Error.Invalid_json)
;;

let text fields key ~maximum =
  match Map.find fields key with
  | Some (`String value) when bounded value ~maximum -> Ok value
  | _ -> Error Error.Invalid_json
;;

let presence fields key parse =
  match Map.find fields key with
  | None -> Ok Presence.Absent
  | Some `Null -> Ok Presence.Null
  | Some value -> Result.map (parse value) ~f:(fun v -> Presence.Value v)
;;

let positive_integer = function
  | `Number number ->
    (match Int64.of_string number with
     | n when Int64.(n > 0L && n <= 31536000L) -> Ok n
     | _ -> Error Error.Invalid_json
     | exception (Failure _ | Invalid_argument _) -> Error Error.Invalid_json)
  | _ -> Error Error.Invalid_json
;;

module Device = struct
  type challenge =
    { id : string
    ; user_code : string
    ; interval_seconds : int
    }

  let id t = t.id
  let user_code t = t.user_code
  let interval_seconds t = t.interval_seconds

  let decode_challenge body =
    let open Result.Let_syntax in
    let%bind fields = decode_object body in
    let%bind id = text fields "device_auth_id" ~maximum:4096 in
    let%bind user_code =
      match Map.find fields "user_code", Map.find fields "usercode" with
      | (Some (`String a), None | None, Some (`String a)) when bounded a ~maximum:128 ->
        Ok a
      | Some (`String a), Some (`String b) when String.equal a b && bounded a ~maximum:128
        -> Ok a
      | _ -> Error Error.Invalid_json
    in
    let%bind interval_seconds =
      match Map.find fields "interval" with
      | Some (`String interval) ->
        (match Int.of_string interval with
         | n when n > 0 && n <= 60 -> Ok n
         | _ -> Error Error.Invalid_json
         | exception (Failure _ | Invalid_argument _) -> Error Error.Invalid_json)
      | _ -> Error Error.Invalid_json
    in
    Ok { id; user_code; interval_seconds }
  ;;

  type grant =
    { authorization_code : string
    ; pkce : Pkce.t
    }

  let authorization_code t = t.authorization_code
  let pkce t = t.pkce

  let decode_grant body =
    let open Result.Let_syntax in
    let%bind fields = decode_object body in
    let%bind authorization_code = text fields "authorization_code" ~maximum:4096 in
    let%bind verifier = text fields "code_verifier" ~maximum:128 in
    let%bind challenge = text fields "code_challenge" ~maximum:128 in
    let%map pkce = Pkce.of_pair ~verifier ~challenge in
    { authorization_code; pkce }
  ;;
end

module Token = struct
  type t =
    { access : string
    ; refresh : string Presence.t
    ; id_token : string Presence.t
    ; scopes : string list Presence.t
    ; expires_in : int64 Presence.t
    }

  let access t = t.access
  let refresh t = t.refresh
  let id_token t = t.id_token
  let scopes t = t.scopes
  let expires_in t = t.expires_in

  let decode body =
    let open Result.Let_syntax in
    let%bind fields = decode_object body in
    let%bind access = text fields "access_token" ~maximum:131072 in
    let%bind token_type = text fields "token_type" ~maximum:16 in
    let%bind () =
      if String.Caseless.equal token_type "Bearer"
      then Ok ()
      else Error Error.Invalid_json
    in
    let parse_secret = function
      | `String value when bounded value ~maximum:131072 -> Ok value
      | _ -> Error Error.Invalid_json
    in
    let%bind refresh = presence fields "refresh_token" parse_secret in
    let%bind id_token = presence fields "id_token" parse_secret in
    let%bind scopes =
      presence fields "scope" (function
        | `String value when bounded value ~maximum:4096 ->
          let scopes = String.split value ~on:' ' in
          if
            List.for_all scopes ~f:(fun scope ->
              bounded scope ~maximum:256
              && (not (String.mem scope '"'))
              && not (String.mem scope '\\'))
            && not (List.contains_dup scopes ~compare:String.compare)
          then Ok scopes
          else Error Error.Invalid_json
        | _ -> Error Error.Invalid_json)
    in
    let%map expires_in = presence fields "expires_in" positive_integer in
    { access; refresh; id_token; scopes; expires_in }
  ;;
end

let form fields =
  List.map fields ~f:(fun (key, value) -> Uri.pct_encode key ^ "=" ^ Uri.pct_encode value)
  |> String.concat ~sep:"&"
;;
