open! Core
module P = Agent_protocol

module Target = struct
  type developer =
    { host_id : P.Id.Server.t
    ; run_id : P.Id.Developer_run.t
    ; principal_id : P.Id.Principal.t
    }
  [@@deriving sexp]

  type t =
    | Unassigned
    | Session of P.Id.Session.t
    | Developer of developer
  [@@deriving sexp]

  let equal a b =
    match a, b with
    | Unassigned, Unassigned -> true
    | Session a, Session b -> P.Id.Session.equal a b
    | Developer a, Developer b ->
      P.Id.Server.equal a.host_id b.host_id
      && P.Id.Developer_run.equal a.run_id b.run_id
      && P.Id.Principal.equal a.principal_id b.principal_id
    | _ -> false
  ;;

  let session_id = function
    | Session id -> Some id
    | Unassigned | Developer _ -> None
  ;;

  let developer = function
    | Developer value -> Some value
    | Unassigned | Session _ -> None
  ;;
end

type t =
  { blob : P.Blob.Metadata.t
  ; creating_principal : P.Id.Principal.t
  ; target : Target.t
  ; allowed_use : string
  ; created_at : P.Timestamp.t
  ; expires_at : P.Timestamp.t option
  ; durable : bool
  }
[@@deriving sexp]

let blob_equal (a : P.Blob.Metadata.t) (b : P.Blob.Metadata.t) =
  P.Id.Blob.equal a.id b.id
  && P.Blob.equal_kind a.kind b.kind
  && String.equal a.media_type b.media_type
  && Int64.equal a.byte_length b.byte_length
  && String.equal a.digest b.digest
  && Option.equal String.equal a.display_name b.display_name
;;

let equal a b =
  blob_equal a.blob b.blob
  && P.Id.Principal.equal a.creating_principal b.creating_principal
  && Target.equal a.target b.target
  && String.equal a.allowed_use b.allowed_use
  && P.Timestamp.equal a.created_at b.created_at
  && Option.equal P.Timestamp.equal a.expires_at b.expires_at
  && Bool.equal a.durable b.durable
;;
