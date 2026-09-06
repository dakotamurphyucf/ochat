open! Core

module Request = struct
  type t =
    { tool_name : string
    ; identity_digest : string
    ; invocation_display : string
    ; effects : string list
    }
  [@@deriving sexp]
end

module Decision = struct
  type t =
    | Allow
    | Deny of string
  [@@deriving compare, equal, sexp]
end

module Error = struct
  type t =
    { code : string
    ; message : string
    }
  [@@deriving compare, equal, sexp]
end

type kind =
  | Model
  | External
[@@deriving compare, equal, sexp]

module type Reviewer = sig
  val review : Request.t -> (Decision.t, Error.t) result
end

type t =
  { id : string
  ; kind : kind
  ; revision : string
  ; review : Request.t -> (Decision.t, Error.t) result
  }

let create ~id ~kind ~revision ~review =
  if String.is_empty id
  then Error (Agent_protocol.Error.invalid_request "reviewer ID must be nonempty")
  else if String.is_empty revision
  then Error (Agent_protocol.Error.invalid_request "reviewer revision must be nonempty")
  else Ok { id; kind; revision; review }
;;

let unavailable ~id ~kind ~message =
  { id
  ; kind
  ; revision = "unavailable"
  ; review = (fun _ -> Error Error.{ code = "reviewer.unavailable"; message })
  }
;;

let review t request =
  match t.review request with
  | Ok (Decision.Deny reason) when String.is_empty reason ->
    Error
      Error.{ code = "reviewer.malformed"; message = "reviewer returned an empty denial" }
  | result -> result
  | exception (Eio.Cancel.Cancelled _ as exn) -> raise exn
  | exception exn ->
    Error
      Error.
        { code = "reviewer.exception"; message = "reviewer failed: " ^ Exn.to_string exn }
;;

let id t = t.id
let kind t = t.kind
let revision t = t.revision
