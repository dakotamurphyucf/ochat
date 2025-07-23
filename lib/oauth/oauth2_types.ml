(** OAuth&nbsp;2.0 – lightweight value types.

    This module bundles a few **plain record** representations that are useful
    when implementing an OAuth&nbsp;2.0 client or server:

    • {!Metadata} – subset of the *Authorization Server Metadata* document
      defined in {{:https://www.rfc-editor.org/rfc/rfc8414}RFC&nbsp;8414}.

    • {!Client_registration} – fields returned by a *Dynamic Client
      Registration* response ({{:https://www.rfc-editor.org/rfc/rfc7591}RFC&nbsp;7591}).

    • {!Token} – access-token payload issued by the authorisation server.

    All records derive JSON serialisers via `ppx_jsonaf_conv`, so you can
    convert them to / from {!Jsonaf.t} using the automatically generated
    functions `{jsonaf_of_t, t_of_jsonaf}`.

    {1 Example}

    Decoding a token response received from the server:
    {[
      let json =
        {|{
            "access_token": "abc123",
            "token_type"  : "Bearer",
            "expires_in"  : 3600,
            "scope"       : "profile openid",
            "obtained_at" : 0.0
          }|}
      in
      let token =
        Jsonaf.of_string_exn json |> Oauth2_types.Token.t_of_jsonaf
      in
      assert (not (Oauth2_types.Token.is_expired token))
    ]}

    The helper function {!Token.is_expired} gives a 60-second safety margin so
    that callers can refresh the token *before* it actually times out.
*)

open Core
module Jsonaf = Jsonaf_ext
open Jsonaf.Export
(* Compatibility helper required by the generated code from [ppx_jsonaf_conv]. *)

module Metadata = struct
  type t =
    { authorization_endpoint : string [@key "authorization_endpoint"]
    ; token_endpoint : string [@key "token_endpoint"]
    ; registration_endpoint : string option [@key "registration_endpoint"]
    }
  [@@deriving jsonaf] [@@jsonaf.allow_extra_fields]

  (** {1 JSON shape}

      {[
        {
          "authorization_endpoint": "https://auth.server/authorize",
          "token_endpoint"       : "https://auth.server/token",
          "registration_endpoint": "https://auth.server/register"  (* optional *)
        }
      ]}

      Unknown fields are ignored thanks to {![@@jsonaf.allow_extra_fields]}. *)
end

module Client_registration = struct
  type t =
    { client_id : string [@key "client_id"]
    ; client_secret : string option [@key "client_secret"]
    }
  [@@deriving jsonaf, sexp] [@@jsonaf.allow_extra_fields]

  (** Client credentials issued by the authorisation server during *dynamic
      client registration*.

      The record mirrors the minimal fields required by RFC&nbsp;7591 §3.2.
      Applications usually persist the returned pair so that subsequent
      authorisation requests can authenticate as that client. *)
end

module Token = struct
  type t =
    { access_token : string [@key "access_token"]
    ; token_type : string [@key "token_type"]
    ; expires_in : int [@key "expires_in"]
    ; refresh_token : string option [@key "refresh_token"] [@jsonaf.option]
    ; scope : string option [@key "scope"] [@jsonaf.option]
    ; obtained_at : float [@key "obtained_at"]
    }
  [@@deriving jsonaf]

  (** [is_expired t] returns [true] if [t] will expire within the next minute.

      A small *safety window* (60&nbsp;s) is subtracted from [expires_in] so
      that callers can refresh the token a bit early and avoid race
      conditions where a request is dispatched with a token that expires
      en-route.  *)
  let is_expired t : bool =
    let now = Core_unix.gettimeofday () in
    let expiry = t.obtained_at +. Float.of_int (t.expires_in - 60) in
    Float.(now >= expiry)
  ;;
end
