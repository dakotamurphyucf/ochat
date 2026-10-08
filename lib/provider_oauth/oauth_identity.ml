open! Core
module P = Provider_oauth_protocol

module Error = struct
  type t =
    | Invalid_token
    | Issuer
    | Audience
    | Account
    | Subject
    | Nonce
    | Expiry
    | Scopes
  [@@deriving equal, sexp_of]
end

type t =
  { account : string
  ; subject : string
  ; scopes : string list
  ; expires_at : float
  ; nonce : string option
  ; audience : string list
  ; azp : string option
  ; auth_time : int64 option
  ; response_expires_in : int64 P.Presence.t
  }

let account t = t.account
let subject t = t.subject
let scopes t = t.scopes
let expires_at t = t.expires_at
let response_expires_in t = t.response_expires_in

let json_object = function
  | `Object fields ->
    (match String.Map.of_alist fields with
     | `Ok fields -> Ok fields
     | `Duplicate_key _ -> Error Error.Invalid_token)
  | _ -> Error Error.Invalid_token
;;

let jwt token =
  let open Result.Let_syntax in
  match String.split token ~on:'.' with
  | [ header; payload; signature ] when not (String.is_empty signature) ->
    let decode text =
      match Base64.decode ~pad:false ~alphabet:Base64.uri_safe_alphabet text with
      | Error (`Msg _) -> Error Error.Invalid_token
      | Ok data ->
        (match Jsonaf.parse data with
         | Ok json -> json_object json
         | Error _ -> Error Error.Invalid_token)
    in
    let%bind header = decode header in
    let%bind () =
      match Map.find header "alg" with
      | Some (`String ("RS256" | "ES256")) -> Ok ()
      | _ -> Error Error.Invalid_token
    in
    decode payload
  | _ -> Error Error.Invalid_token
;;

let text fields key error =
  match Map.find fields key with
  | Some (`String value)
    when (not (String.is_empty value))
         && String.length value <= 512
         && String.for_all value ~f:(fun c ->
           Char.to_int c >= 0x21 && Char.to_int c < 0x7f) -> Ok value
  | _ -> Error error
;;

let matches expected actual error =
  match expected with
  | None -> Ok ()
  | Some expected -> if String.equal expected actual then Ok () else Error error
;;

let audience_values fields =
  let values =
    match Map.find fields "aud" with
    | Some (`String v) -> Some [ v ]
    | Some (`Array values) ->
      List.map values ~f:(function
        | `String v -> Some v
        | _ -> None)
      |> Option.all
    | _ -> None
  in
  match values with
  | Some values
    when (not (List.is_empty values))
         && List.length values <= 32
         && List.for_all values ~f:(fun value ->
           (not (String.is_empty value)) && String.length value <= 512)
         && not (List.contains_dup values ~compare:String.compare) ->
    Ok (List.sort values ~compare:String.compare)
  | _ -> Error Error.Audience
;;

let audience fields expected =
  let open Result.Let_syntax in
  let%bind values = audience_values fields in
  if List.mem values expected ~equal:String.equal then Ok () else Error Error.Audience
;;

let optional_text fields key error =
  match Map.find fields key with
  | None -> Ok None
  | Some _ -> Result.map (text fields key error) ~f:Option.some
;;

let time fields key =
  match Map.find fields key with
  | Some (`Number value) ->
    (match Int64.of_string value with
     | n when Int64.(n > 0L && n <= 9007199254740991L) -> Ok (Int64.to_float n)
     | _ -> Error Error.Expiry
     | exception (Failure _ | Invalid_argument _) -> Error Error.Expiry)
  | _ -> Error Error.Expiry
;;

let account_claim fields =
  let open Result.Let_syntax in
  let%bind auth =
    match Map.find fields "https://api.openai.com/auth" with
    | Some auth -> json_object auth
    | None -> Error Error.Account
  in
  text auth "chatgpt_account_id" Error.Account
;;

let validate
      token
      ~now
      ~client
      ~expected_account
      ~expected_subject
      ~nonce
      ~requested_scopes
      ~access_scope_claim
      ~prior
  =
  let open Result.Let_syntax in
  let%bind access = jwt (P.Token.access token) in
  let check_issuer fields =
    let%bind issuer = text fields "iss" Error.Issuer in
    if String.equal issuer "https://auth.openai.com" then Ok () else Error Error.Issuer
  in
  let%bind () = check_issuer access in
  let%bind access_subject = text access "sub" Error.Subject in
  let%bind access_account = account_claim access in
  let%bind
      ( subject
      , account
      , retained_nonce
      , original_audience
      , original_azp
      , original_auth_time )
    =
    match P.Token.id_token token with
    | Null -> Error Error.Invalid_token
    | Absent ->
      (match prior with
       | None -> Error Error.Invalid_token
       | Some prior ->
         Ok
           ( prior.subject
           , prior.account
           , prior.nonce
           , prior.audience
           , prior.azp
           , prior.auth_time ))
    | Value token ->
      let%bind id = jwt token in
      let%bind () = check_issuer id in
      let%bind () = audience id client in
      let%bind original_audience = audience_values id in
      let%bind original_azp = optional_text id "azp" Error.Audience in
      let%bind original_auth_time =
        match Map.find id "auth_time" with
        | None -> Ok None
        | Some _ ->
          Result.map (time id "auth_time") ~f:(fun time -> Some (Int64.of_float time))
      in
      let%bind () =
        match prior with
        | None -> Ok ()
        | Some prior ->
          if
            (not (List.equal String.equal prior.audience original_audience))
            || (not (Option.equal String.equal prior.azp original_azp))
            ||
            match original_auth_time with
            | None -> false
            | Some actual ->
              Option.exists prior.auth_time ~f:(fun previous ->
                not (Int64.equal previous actual))
          then Error Error.Invalid_token
          else Ok ()
      in
      let%bind () =
        match Map.find id "azp" with
        | None ->
          (match Map.find id "aud" with
           | Some (`Array (_ :: _ :: _)) -> Error Error.Audience
           | _ -> Ok ())
        | Some (`String value) when String.equal value client -> Ok ()
        | _ -> Error Error.Audience
      in
      let%bind subject = text id "sub" Error.Subject in
      let%bind account = account_claim id in
      let%bind expiration = time id "exp" in
      let%bind issued = time id "iat" in
      let%bind () =
        if Float.(expiration > now && issued <= now + 60.)
        then Ok ()
        else Error Error.Expiry
      in
      let%bind retained_nonce =
        match prior with
        | None ->
          let%bind actual = optional_text id "nonce" Error.Nonce in
          let%map () =
            match nonce, actual with
            | None, _ -> Ok ()
            | Some expected, Some actual -> matches (Some expected) actual Error.Nonce
            | Some _, None -> Error Error.Nonce
          in
          actual
        | Some prior ->
          let%map () =
            match Map.find id "nonce" with
            | None -> Ok ()
            | Some (`String actual) ->
              (match prior.nonce with
               | Some expected -> matches (Some expected) actual Error.Nonce
               | None -> Error Error.Nonce)
            | _ -> Error Error.Nonce
          in
          prior.nonce
      in
      let original_audience, original_azp, original_auth_time =
        match prior with
        | None -> original_audience, original_azp, original_auth_time
        | Some prior -> prior.audience, prior.azp, prior.auth_time
      in
      Ok
        ( subject
        , account
        , retained_nonce
        , original_audience
        , original_azp
        , original_auth_time )
  in
  let%bind () = matches (Some subject) access_subject Error.Subject in
  let%bind () = matches (Some account) access_account Error.Account in
  let%bind () = matches expected_subject subject Error.Subject in
  let%bind () = matches expected_account account Error.Account in
  let%bind () =
    match prior with
    | None -> Ok ()
    | Some prior ->
      let%bind () = matches (Some prior.subject) subject Error.Subject in
      matches (Some prior.account) account Error.Account
  in
  let%bind expires_at = time access "exp" in
  let%bind () =
    if Float.is_finite now && Float.(expires_at > now) then Ok () else Error Error.Expiry
  in
  let parse_scopes value =
    let scopes = String.split value ~on:' ' in
    if
      String.length value <= 4096
      && (not (List.contains_dup scopes ~compare:String.compare))
      && List.for_all scopes ~f:(fun scope ->
        (not (String.is_empty scope))
        && String.for_all scope ~f:(fun c ->
          Char.to_int c >= 0x21
          && Char.to_int c < 0x7f
          && (not (Char.equal c '"'))
          && not (Char.equal c '\\')))
    then Ok scopes
    else Error Error.Scopes
  in
  let%bind scopes =
    match P.Token.scopes token with
    | Value scopes -> Ok scopes
    | Null -> Error Error.Scopes
    | Absent ->
      (match access_scope_claim, Map.find access "scope" with
       | true, Some (`String value) -> parse_scopes value
       | true, Some _ -> Error Error.Scopes
       | false, _ | true, None ->
         (match requested_scopes, prior with
          | Some scopes, _ -> Ok scopes
          | None, Some prior -> Ok prior.scopes
          | None, None -> Error Error.Scopes))
  in
  let required =
    match prior with
    | Some prior -> prior.scopes
    | None -> Option.value requested_scopes ~default:[ "openid" ]
  in
  let%bind () =
    if List.for_all required ~f:(fun scope -> List.mem scopes scope ~equal:String.equal)
    then Ok ()
    else Error Error.Scopes
  in
  let%bind () =
    match P.Token.expires_in token with
    | Null -> Error Error.Expiry
    | Absent -> Ok ()
    | Value seconds ->
      if Float.(expires_at <= now + Int64.to_float seconds + 60.)
      then Ok ()
      else Error Error.Expiry
  in
  Ok
    { account
    ; subject
    ; scopes
    ; expires_at
    ; nonce = retained_nonce
    ; audience = original_audience
    ; azp = original_azp
    ; auth_time = original_auth_time
    ; response_expires_in = P.Token.expires_in token
    }
;;

let continuity t =
  Jsonaf.to_string
    (`Object
        ([ "schema", `Number "1"
         ; "issuer", `String "https://auth.openai.com"
         ; "client", `String "app_EMoamEEZ73f0CkXaXp7hrann"
         ; "account", `String t.account
         ; "subject", `String t.subject
         ; "aud", `Array (List.map t.audience ~f:(fun value -> `String value))
         ]
         @ (match t.response_expires_in with
            | Absent -> []
            | Null -> [ "response_expires_in", `Null ]
            | Value seconds ->
              [ "response_expires_in", `Number (Int64.to_string seconds) ])
         @ Option.to_list (Option.map t.nonce ~f:(fun value -> "nonce", `String value))
         @ Option.to_list (Option.map t.azp ~f:(fun value -> "azp", `String value))
         @ Option.to_list
             (Option.map t.auth_time ~f:(fun value ->
                "auth_time", `Number (Int64.to_string value)))))
;;

let restore_continuity encoded ~account ~subject ~scopes ~expires_at =
  let open Result.Let_syntax in
  if String.length encoded > 8192 || not (Float.is_finite expires_at)
  then Error Error.Invalid_token
  else (
    let%bind fields =
      match Jsonaf.parse encoded with
      | Ok json -> json_object json
      | Error _ -> Error Error.Invalid_token
    in
    let allowed =
      [ "schema"
      ; "issuer"
      ; "client"
      ; "account"
      ; "subject"
      ; "aud"
      ; "nonce"
      ; "azp"
      ; "auth_time"
      ; "response_expires_in"
      ]
    in
    let%bind () =
      if
        Map.for_alli fields ~f:(fun ~key ~data:_ ->
          List.mem allowed key ~equal:String.equal)
      then Ok ()
      else Error Error.Invalid_token
    in
    let%bind () =
      match Map.find fields "schema" with
      | Some (`Number "1") -> Ok ()
      | _ -> Error Error.Invalid_token
    in
    let%bind issuer = text fields "issuer" Error.Issuer in
    let%bind client = text fields "client" Error.Audience in
    let%bind () =
      if
        String.equal issuer "https://auth.openai.com"
        && String.equal client "app_EMoamEEZ73f0CkXaXp7hrann"
      then Ok ()
      else Error Error.Invalid_token
    in
    let%bind stored_account = text fields "account" Error.Account in
    let%bind stored_subject = text fields "subject" Error.Subject in
    let%bind () = matches (Some account) stored_account Error.Account in
    let%bind () = matches (Some subject) stored_subject Error.Subject in
    let%bind audience = audience_values fields in
    let%bind () =
      if List.mem audience client ~equal:String.equal then Ok () else Error Error.Audience
    in
    let%bind nonce = optional_text fields "nonce" Error.Nonce in
    let%bind azp = optional_text fields "azp" Error.Audience in
    let%bind () = matches azp client Error.Audience in
    let%bind () =
      if List.length audience > 1 && Option.is_none azp
      then Error Error.Audience
      else Ok ()
    in
    let%bind auth_time =
      match Map.find fields "auth_time" with
      | None -> Ok None
      | Some _ ->
        Result.map (time fields "auth_time") ~f:(fun value -> Some (Int64.of_float value))
    in
    let%bind response_expires_in =
      match Map.find fields "response_expires_in" with
      | None -> Ok P.Presence.Absent
      | Some `Null -> Ok P.Presence.Null
      | Some (`Number value) ->
        (match Int64.of_string value with
         | seconds when Int64.(seconds > 0L && seconds <= 31536000L) ->
           Ok (P.Presence.Value seconds)
         | _ -> Error Error.Expiry
         | exception (Failure _ | Invalid_argument _) -> Error Error.Expiry)
      | _ -> Error Error.Expiry
    in
    Ok
      { account
      ; subject
      ; scopes
      ; expires_at
      ; nonce
      ; audience
      ; azp
      ; auth_time
      ; response_expires_in
      })
;;
