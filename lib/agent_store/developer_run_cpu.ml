open! Core
module D = Document_schema
module Doc = Developer_run_document

type t = { domain_mgr : Eio.Domain_manager.ty Eio.Resource.t }

let create ~domain_mgr = { domain_mgr }

(* Small documents avoid a domain handoff. The threshold is a performance
   choice only: both paths use precisely the same canonical decoder. *)
let direct_decode_bytes = 64 * 1024

let decode_document t bytes ~limits =
  let decode bytes =
    D.Document.decode ~limits bytes
    |> Result.map_error ~f:(fun error -> Store_error.Document error)
  in
  if
    String.length bytes <= direct_decode_bytes
    || String.length bytes > D.Limits.max_bytes limits
  then decode bytes
  else (
    let bytes = String.sub bytes ~pos:0 ~len:(String.length bytes) in
    Eio.Domain_manager.run t.domain_mgr (fun () -> decode bytes))
;;

let decode_with_document
      t
      bytes
      ~poll
      ~limits
      ~max_epochs
      ~max_jobs
      ~max_invocations
      ~max_job_payload_bytes
      ~max_inference_attempts
      ~max_inference_bytes
      ~commands
  =
  let open Result.Let_syntax in
  let%bind document = decode_document t bytes ~limits in
  let%map value =
    Doc.of_decoded_document
      document
      ~poll
      ~limits
      ~max_epochs
      ~max_jobs
      ~max_invocations
      ~max_job_payload_bytes
      ~max_inference_attempts
      ~max_inference_bytes
      ~commands
  in
  value, document
;;

let decode
      t
      bytes
      ~poll
      ~limits
      ~max_epochs
      ~max_jobs
      ~max_invocations
      ~max_job_payload_bytes
      ~max_inference_attempts
      ~max_inference_bytes
      ~commands
  =
  decode_with_document
    t
    bytes
    ~poll
    ~limits
    ~max_epochs
    ~max_jobs
    ~max_invocations
    ~max_job_payload_bytes
    ~max_inference_attempts
    ~max_inference_bytes
    ~commands
  |> Result.map ~f:fst
;;

let encode
      t
      document
      ~poll
      ~limits
      ~max_epochs
      ~max_jobs
      ~max_invocations
      ~max_job_payload_bytes
      ~max_inference_attempts
      ~max_inference_bytes
      ~commands
  =
  let open Result.Let_syntax in
  let%bind input =
    Doc.prepare_encode
      document
      ~poll
      ~limits
      ~max_epochs
      ~max_jobs
      ~max_invocations
      ~max_job_payload_bytes
      ~max_inference_attempts
      ~max_inference_bytes
      ~commands
  in
  Eio.Domain_manager.run t.domain_mgr (fun () -> Doc.finish_encode input)
;;
