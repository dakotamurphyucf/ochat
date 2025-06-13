open Core
module Jsonaf = Jsonaf_ext
open Jsonaf.Export

module Token = struct
  type t =
    { access_token : string [@key "access_token"]
    ; token_type : string [@key "token_type"]
    ; expires_in : int [@key "expires_in"]
    ; obtained_at : float [@key "obtained_at"]
    }
  [@@deriving sexp, bin_io, jsonaf]
end

module Client = struct
  type t =
    { client_id : string [@key "client_id"]
    ; client_secret : string option [@key "client_secret"]
    ; client_name : string option [@key "client_name"] [@jsonaf.option]
    ; redirect_uris : string list option [@key "redirect_uris"] [@jsonaf.option]
    ; client_id_issued_at : int option [@key "client_id_issued_at"] [@jsonaf.option]
    ; client_secret_expires_at : int option
          [@key "client_secret_expires_at"] [@jsonaf.option]
    }
  [@@deriving sexp, bin_io, jsonaf]
end

module Metadata = struct
  type t =
    { issuer : string [@key "issuer"]
    ; authorization_endpoint : string [@key "authorization_endpoint"]
    ; token_endpoint : string [@key "token_endpoint"]
    ; registration_endpoint : string option [@key "registration_endpoint"]
    }
  [@@deriving sexp, bin_io, jsonaf]
end
