open Core

(** Explicit administrative source-read grants and Standard Project target
    selections. Configured roots alone and authoring feature negotiation confer
    no read authority. Configuration root names are resolved against the real
    config source directory before admission and are never client paths. *)

module Read_grant : sig
  type t =
    { root : string
    ; principal_id : Agent_protocol.Id.Principal.t
    ; authentication_kind : string
    }
  [@@deriving compare, equal, sexp]
end

module Selection : sig
  type request =
    | Library of { environment : string option }
    | Entry of
        { name : string
        ; environment : string option
        }
  [@@deriving compare, equal, sexp]

  type t =
    { root : string
    ; project : string
    ; request : request
    ; target : Chatml.Chatml_compiler_contract.Target.t
    }
  [@@deriving compare, equal, sexp]
end

type t =
  { read_grants : Read_grant.t list
  ; selections : Selection.t list
  }
[@@deriving compare, equal, sexp]

(** Repeat admission for directly constructed records before any owner is
    opened or contract is converted. Bound counts/aggregate text bytes; require
    at least one principal grant and Project selection, distinct
    (root,principal,kind) grants and distinct (root,project,request)
    selections. Every named root must exactly match an admitted administrative
    external root, and every selected root must have an explicit read grant;
    other configured roots may remain ungranted. Project strings decode
    through [Source.Portable_path.create]; entry/environment spellings are
    admitted by [Agent_protocol.Authoring.Context.Selection.create], then
    compared using that type's [equal] after binding. Reject empty kinds and unknown
    targets. No filesystem access, source permission or Build follows from
    this pure check. *)
val validate : t -> project_sources:Project_source_configuration.t -> unit Or_error.t
