open! Core
module Sources = Authoring_source_host
module Upload = Agent_store.Authoring_upload_store
module Supplied = Authoring_context_service_core.Supplied_document_policy

module Recording = struct
  type t = int

  let create ~max_origin_bytes =
    if max_origin_bytes <= 0
    then Or_error.error_string "historical origin budget must be positive"
    else Ok max_origin_bytes
  ;;

  let max_origin_bytes t = t
end

module Origin = struct
  type t =
    | Roots of Sources.Historical_roots.t
    | Published_upload of Upload.Published_pin.t
    | Supplied_document of Supplied.Identity.t

  let roots roots = Roots roots
  let published_upload pin = Published_upload pin
  let supplied_document identity = Supplied_document identity

  let charge t ~max_bytes =
    match t with
    | Roots roots -> Sources.Historical_roots.charge roots ~max_bytes
    | Published_upload pin -> Upload.Published_pin.retained_charge pin ~max_bytes
    | Supplied_document _ -> if max_bytes < 64 then None else Some 64
  ;;
end
