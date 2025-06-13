open! Core
module P = Piaf
module Token = Oauth2_server_types.Token
module Metadata = Oauth2_server_types.Metadata
module Storage = Oauth2_server_storage
module Client_storage = Oauth2_server_client_storage

let json_headers = P.Headers.of_list [ "content-type", "application/json" ]

let respond_json ?(status = `OK) body =
  P.Response.create ~headers:json_headers ~body:(P.Body.of_string body) status
;;

let b64url_no_pad ?(rng_len = 32) () =
  Mirage_crypto_rng.generate rng_len
  |> Base64.encode_string ~pad:false ~alphabet:Base64.uri_safe_alphabet
;;

let metadata_of_request (req : P.Request.t) (port : int) : Metadata.t =
  let scheme =
    match P.Request.scheme req with
    | `HTTP -> "http"
    | `HTTPS -> "https"
  in
  let host =
    match P.Headers.get (P.Request.headers req) "host" with
    | Some h -> h
    | None -> "localhost"
  in
  (* if true then failwith (Uri.to_string (P.Request.uri req)); *)
  (* Strip any path component – we only want scheme://host[:port] *)
  let base = Printf.sprintf "%s://%s:%d" scheme host port in
  { issuer = base
  ; authorization_endpoint = base ^ "/authorize"
  ; token_endpoint = base ^ "/token"
  ; registration_endpoint = Some (base ^ "/register")
  }
;;

let handle_metadata (req : P.Request.t) (port : int) : P.Response.t =
  metadata_of_request req port |> Metadata.jsonaf_of_t |> Jsonaf.to_string |> respond_json
;;

(* Simple parser for application/x-www-form-urlencoded strings. *)
let parse_form (body : string) : string list String.Map.t =
  body |> Uri.query_of_encoded |> String.Map.of_alist_reduce ~f:( @ )
;;

let handle_token ~env (req : P.Request.t) : P.Response.t =
  match P.Body.to_string (P.Request.body req) with
  | Error _ -> respond_json ~status:`Bad_request {|{"error":"invalid_request"}|}
  | Ok body ->
    let form = parse_form body in
    let grant_type = Map.find form "grant_type" |> Option.bind ~f:List.hd in
    let client_id = Map.find form "client_id" |> Option.bind ~f:List.hd in
    let client_secret = Map.find form "client_secret" |> Option.bind ~f:List.hd in
    (match grant_type, client_id, client_secret with
     | Some "client_credentials", Some cid, Some secret ->
       if Client_storage.validate_secret ~client_id:cid ~client_secret:(Some secret)
       then (
         try
           let tok =
             { Token.access_token = b64url_no_pad ()
             ; token_type = "Bearer"
             ; expires_in = 3600
             ; obtained_at = Eio.Time.now (Eio.Stdenv.clock env)
             }
           in
           Storage.insert tok;
           respond_json (tok |> Token.jsonaf_of_t |> Jsonaf.to_string)
         with
         | exn -> respond_json ~status:`Bad_request (Exn.to_string exn))
       else respond_json ~status:`Unauthorized {|{"error":"invalid_client"}|}
     | _ -> respond_json ~status:`Bad_request {|{"error":"unsupported_grant_type"}|})
;;

let handle_authorize _req =
  (* Not implemented – PKCE will arrive later. *)
  respond_json ~status:`Not_implemented {|{"error":"not_implemented"}|}
;;

(* ------------------------------------------------------------------ *)
(* Dynamic Client Registration                                          *)
(* ------------------------------------------------------------------ *)

let handle_register (req : P.Request.t) : P.Response.t =
  (* We only accept application/json bodies. *)
  match P.Body.to_string (P.Request.body req) with
  | Error _ -> respond_json ~status:`Bad_request {|{"error":"invalid_request"}|}
  | Ok body_str ->
    (match Or_error.try_with (fun () -> Jsonaf.of_string body_str) with
     | Error _ -> respond_json ~status:`Bad_request {|{"error":"invalid_json"}|}
     | Ok json ->
       let obj =
         match json with
         | `Object o -> o
         | _ -> []
       in
       let field_string name =
         List.Assoc.find obj name ~equal:String.equal
         |> Option.bind ~f:(function
           | `String s -> Some s
           | _ -> None)
       in
       let field_string_list name =
         List.Assoc.find obj name ~equal:String.equal
         |> Option.bind ~f:(function
           | `Array arr ->
             Some
               (List.filter_map arr ~f:(function
                  | `String s -> Some s
                  | _ -> None))
           | _ -> None)
       in
       let client_name = field_string "client_name" in
       let redirect_uris = field_string_list "redirect_uris" in
       (* Determine confidentiality.  Per RFC 7591 we look at
           [token_endpoint_auth_method].  When present and equal to "none"
           we treat the client as public (no secret).  All other values are
           considered confidential.  If the field is absent we default to
           confidential since that is safer and aligns with existing token
           endpoint expectations. *)
       (* For the current prototype we *always* treat dynamically registered
           clients as *confidential* and return a freshly generated
           [client_secret].  This avoids triggering the PKCE public-client
           flow on the CLI side, which requires human browser interaction
           and is unsuitable for automated test runs.  Once the PKCE `/authorize`
           endpoint is fully implemented we can re-enable public-client
           registration by honouring the `token_endpoint_auth_method = none`
           field. *)
       let confidential = true in
       let entry =
         Oauth2_server_client_storage.register
           ?client_name
           ?redirect_uris
           ~confidential
           ()
       in
       (* Registration already stored the new client inside
           [Oauth2_server_client_storage], so no further action is needed. *)
       respond_json
         ~status:`Created
         (entry |> Oauth2_server_types.Client.jsonaf_of_t |> Jsonaf.to_string))
;;
