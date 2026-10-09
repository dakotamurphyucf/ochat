open! Core
module P = Agent_protocol

type t =
  { blob : P.Blob.Metadata.t
  ; creating_principal : P.Id.Principal.t
  ; target_session : P.Id.Session.t option
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
  && Option.equal P.Id.Session.equal a.target_session b.target_session
  && String.equal a.allowed_use b.allowed_use
  && P.Timestamp.equal a.created_at b.created_at
  && Option.equal P.Timestamp.equal a.expires_at b.expires_at
  && Bool.equal a.durable b.durable
;;
