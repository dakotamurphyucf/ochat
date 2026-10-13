open Core
module Highlight = Language_highlighting.Highlight_snapshot

module Limits = struct
  type t =
    { lexical : Highlight.Limits.t
    ; max_contract_identity_bytes : int
    ; max_contract_work_steps : int
    ; max_projection_work_steps : int
    ; max_output_bytes : int
    }

  let create
        ~lexical
        ~max_contract_identity_bytes
        ~max_contract_work_steps
        ~max_projection_work_steps
        ~max_output_bytes
    =
    if
      max_contract_identity_bytes <= 0
      || max_contract_work_steps <= 0
      || max_projection_work_steps <= 0
      || max_output_bytes <= 0
      || max_output_bytes > Agent_protocol.Authoring_assistance.Limits.max_result_bytes
    then Or_error.error_string "Invalid ChatML fallback limits"
    else
      Ok
        { lexical
        ; max_contract_identity_bytes
        ; max_contract_work_steps
        ; max_projection_work_steps
        ; max_output_bytes
        }
  ;;
end

module Error = struct
  type t =
    | Stale_request_source
    | Stale_selected_build
    | Contract_mismatch
    | Invalid_projection
    | Contract_identity_limit
    | Contract_work_limit
    | Projection_work_limit
    | Output_byte_limit
    | Lexical_admission of Highlight.Error.t
    | Wire of Agent_protocol.Error.t

  let sexp_of_t = function
    | Stale_request_source -> Sexp.Atom "stale_request_source"
    | Stale_selected_build -> Sexp.Atom "stale_selected_build"
    | Contract_mismatch -> Sexp.Atom "contract_mismatch"
    | Invalid_projection -> Sexp.Atom "invalid_projection"
    | Contract_identity_limit -> Sexp.Atom "contract_identity_limit"
    | Contract_work_limit -> Sexp.Atom "contract_work_limit"
    | Projection_work_limit -> Sexp.Atom "projection_work_limit"
    | Output_byte_limit -> Sexp.Atom "output_byte_limit"
    | Lexical_admission error ->
      Sexp.List [ Sexp.Atom "lexical_admission"; Highlight.Error.sexp_of_t error ]
    | Wire _ -> Sexp.Atom "wire"
  ;;
end

module Work = struct
  exception Limit

  type t =
    { mutable remaining : int
    ; checkpoint : unit -> unit
    }

  let create ~max_steps ~checkpoint = { remaining = max_steps; checkpoint }

  let charge t units =
    if units < 0 || units > t.remaining then raise Limit;
    t.remaining <- t.remaining - units;
    t.checkpoint ()
  ;;

  let charge_string t text =
    let length = String.length text in
    charge t length
  ;;
end

module Site_map = struct
  type t =
    { compiled_point :
        int
        -> checkpoint:(unit -> unit)
        -> (Chatml_source.Location.t option, Error.t) Result.t
    ; authored_replacement :
        Chatml_source.Location.t
        -> checkpoint:(unit -> unit)
        -> (Chatml_source.Location.t option, Error.t) Result.t
    }
end

module Origin = struct
  type t =
    | Chatml of Chatml_source.t
    | Mapped of
        { compiled : Chatml_source.t
        ; authored : Chatml_source.t
        ; map : Site_map.t
        }

  let chatml source = Chatml source
  let mapped ~compiled ~authored ~map = Mapped { compiled; authored; map }
end
