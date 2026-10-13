open Core
module Captured = Chatml_source
module Prepared = Chatml_prepared_program
module Build = Chatml_project_build

module Identity = struct
  module T = struct
    type t =
      { principal : string
      ; host_incarnation : string
      ; connection_lease : string
      ; nonce : string
      }
    [@@deriving equal, compare, sexp_of]
  end

  include T
  include Comparator.Make (T)

  let create ~principal ~host_incarnation ~connection_lease ~nonce =
    if
      List.exists
        [ principal; host_incarnation; connection_lease; nonce ]
        ~f:(fun label ->
          let bytes = String.length label in
          bytes = 0 || bytes > 256)
    then Or_error.error_string "invalid authoring context identity"
    else Ok { principal; host_incarnation; connection_lease; nonce }
  ;;

  let principal t = t.principal
  let host_incarnation t = t.host_incarnation
  let connection_lease t = t.connection_lease
end

module Limits = struct
  type t =
    { max_contexts : int
    ; max_sources : int
    ; max_open_documents : int
    ; max_document_bytes : int
    ; max_effective_input_bytes : int
    ; max_context_source_bytes : int
    ; max_total_source_bytes : int
    }

  let create
        ~max_contexts
        ~max_sources
        ~max_open_documents
        ~max_document_bytes
        ~max_effective_input_bytes
        ~max_context_source_bytes
        ~max_total_source_bytes
    =
    if
      List.exists
        [ max_contexts
        ; max_sources
        ; max_open_documents
        ; max_document_bytes
        ; max_effective_input_bytes
        ; max_context_source_bytes
        ; max_total_source_bytes
        ]
        ~f:(fun value -> value <= 0)
      || max_open_documents > max_sources
      || max_context_source_bytes > max_total_source_bytes
    then Or_error.error_string "invalid authoring context limits"
    else
      Ok
        { max_contexts
        ; max_sources
        ; max_open_documents
        ; max_document_bytes
        ; max_effective_input_bytes
        ; max_context_source_bytes
        ; max_total_source_bytes
        }
  ;;
end

module Generation = struct
  type t = int [@@deriving compare, equal, sexp_of]

  let zero = 0
  let to_int t = t
  let next t = if t = Int.max_value then None else Some (t + 1)
end

module Editor_revision = struct
  type t = int [@@deriving compare, equal, sexp_of]

  let create value =
    if value <= 0 then Or_error.error_string "invalid editor revision" else Ok value
  ;;

  let to_int t = t
end

module Error = struct
  type t =
    | Unknown_context
    | Duplicate_context
    | Unknown_source of Captured.Id.t
    | Read_only_source of Captured.Id.t
    | Duplicate_editable_source of Captured.Id.t
    | Stale_generation
    | Stale_document
    | Revision_not_increasing
    | Document_not_open
    | Open_documents
    | Incompatible_environment
    | Invalid_refresh_identity
    | Unsupported_basis
    | Static_selection_unavailable
    | Invalid_requested_environment
    | Limit_exceeded
    | Generation_overflow
    | Capture of Core.Error.t
  [@@deriving sexp_of]
end

module Version = struct
  type t =
    { identity : Identity.t
    ; generation : Generation.t
    ; source : Captured.Id.t
    ; digest : Captured.Digest.t
    ; editor_revision : Editor_revision.t option
    ; overlay_open : bool
    }
  [@@deriving equal, sexp_of]

  let identity t = t.identity
  let generation t = t.generation
  let source t = t.source
  let digest t = t.digest
  let editor_revision t = t.editor_revision
  let overlay_open t = t.overlay_open
end

module View = struct
  type t =
    { identity : Identity.t
    ; generation : Generation.t
    ; base_graph : Captured.Digest.t
    ; environment : string
    ; compiler_contract : Captured.Digest.t
    ; source_count : int
    ; editable_sources : Captured.Id.t list
    ; open_document_count : int
    ; charged_source_bytes : int
    }

  let identity t = t.identity
  let generation t = t.generation
  let base_graph t = t.base_graph
  let environment t = t.environment
  let compiler_contract t = t.compiler_contract
  let source_count t = t.source_count
  let editable_sources t = t.editable_sources
  let open_document_count t = t.open_document_count
  let charged_source_bytes t = t.charged_source_bytes
end

module Basis = struct
  type t =
    | Project
    | Document
  [@@deriving equal, sexp_of]
end

module Document = struct
  module View = struct
    type t =
      { identity : Identity.t
      ; generation : Generation.t
      ; source : Captured.t
      ; requested_environment : string option
      ; selected_environment : string option
      ; compiler_contract : Captured.Digest.t option
      ; static_graph : Captured.Digest.t option
      ; open_document_count : int
      ; charged_source_bytes : int
      }

    let identity t = t.identity
    let generation t = t.generation
    let source t = t.source
    let requested_environment t = t.requested_environment
    let selected_environment t = t.selected_environment
    let compiler_contract t = t.compiler_contract
    let static_graph t = t.static_graph
    let open_document_count t = t.open_document_count
    let charged_source_bytes t = t.charged_source_bytes
  end

  module Inputs = struct
    type t =
      { identity : Identity.t
      ; generation : Generation.t
      ; source : Captured.t
      ; base : Captured.t
      ; static_build : Build.t option
      ; effective_input_bytes : int
      }

    let identity t = t.identity
    let generation t = t.generation
    let source t = t.source
    let base t = t.base
    let static_build t = t.static_build
    let effective_input_bytes t = t.effective_input_bytes
  end
end

module Inputs = struct
  type t =
    { identity : Identity.t
    ; generation : Generation.t
    ; base : Build.t
    ; sources : Captured.t list
    ; changed_sources : Captured.Id.t list
    ; effective_input_bytes : int
    }

  let identity t = t.identity
  let generation t = t.generation
  let base t = t.base
  let sources t = t.sources
  let changed_sources t = t.changed_sources
  let effective_input_bytes t = t.effective_input_bytes
end

module Analysis_key = struct
  type t =
    { identity : Identity.t
    ; generation : Generation.t
    ; validated_graph : Captured.Digest.t
    }
  [@@deriving equal, sexp_of]

  let identity t = t.identity
  let generation t = t.generation
  let validated_graph t = t.validated_graph
end

module Refresh = struct
  type policy =
    | Reject_open_documents
    | Discard_open_documents
  [@@deriving equal, sexp_of]
end
