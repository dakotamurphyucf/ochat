open! Core
module Sources = Authoring_source_host
module Upload = Agent_store.Authoring_upload_store
module Supplied = Authoring_context_service_core.Supplied_document_policy

(** A finite recording-only origin budget supplied by the installed trace
    configuration. [None] at capture construction disables historical-origin
    issuance entirely. This budget grants neither source access nor trace read. *)
module Recording : sig
  type t

  val create : max_origin_bytes:int -> t Or_error.t
  val max_origin_bytes : t -> int
end

(** Private, comparison-only source origin minted at the authentic selected
    capture branch. It is never decoded from a route, source namespace, editor
    revision, receipt or client field. It retains neither source text, an old
    editor context, actor token, Root, parser nor rebind callback. *)
module Origin : sig
  type t = private
    | Roots of Sources.Historical_roots.t
    | Published_upload of Upload.Published_pin.t
    | Supplied_document of Supplied.Identity.t

  (** These constructors accept only physical owner-issued values. The
      capture facade calls one only after matching the actual selected owner
      branch. The uploaded pin includes its bounded Store receipt; no upload
      Context_capture is retained. *)
  val roots : Sources.Historical_roots.t -> t

  val published_upload : Upload.Published_pin.t -> t
  val supplied_document : Supplied.Identity.t -> t

  (** Checked conservative retained-byte charge, including every bounded
      origin identity and the published pin's receipt. None means it cannot
      fit the caller's finite provenance cap; tracing is omitted, while
      source preparation and execution admission remain unchanged. *)
  val charge : t -> max_bytes:int -> int option
end
