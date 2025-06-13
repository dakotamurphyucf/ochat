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
end

module Client_registration = struct
  type t =
    { client_id : string [@key "client_id"]
    ; client_secret : string option [@key "client_secret"]
    }
  [@@deriving jsonaf, sexp] [@@jsonaf.allow_extra_fields]
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

  let is_expired t =
    let now = Core_unix.gettimeofday () in
    let expiry = t.obtained_at +. Float.of_int (t.expires_in - 60) in
    Float.(now >= expiry)
  ;;
end
