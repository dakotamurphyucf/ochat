open Core

let cases =
  [ ( "http.audit-attribution-pagination-tamper-restart"
    , Support.Data_integrity_http.test_audit )
  ; "http.blob-bounds-digest-foreign-ownership", Support.Data_integrity_http.test_blobs
  ; "http.export-atomic-success", Support.Data_integrity_http.test_export
  ; "http-peer.export-atomic-failures", Support.Data_integrity_download.run
  ; ( "http-peer.export-atomic-cancellation"
    , Support.Data_integrity_download.run_cancellation )
  ; "maintenance-fixture.retention", Support.Data_integrity_retention.run
  ; "daemon-timer.retention-active-protection", Data_integrity_timer.run
  ]
;;

let select = function
  | None -> cases
  | Some name ->
    (match List.Assoc.find cases name ~equal:String.equal with
     | Some test -> [ name, test ]
     | None -> raise_s [%sexp "unknown data integrity case", (name : string)])
;;

let run env ~case =
  Support.Temporary_environment.with_ ~scenario:"data-integrity" ~env (fun environment ->
    let selected = select case in
    List.iter selected ~f:(fun (_name, test) -> test env environment);
    let sexp =
      [%sexp
        { scenario = ("data-integrity" : string)
        ; selected_case = (case : string option)
        ; passed_cases = (List.map selected ~f:fst : string list)
        }]
    in
    Eio.Flow.copy_string (Sexp.to_string_hum sexp ^ "\n") (Eio.Stdenv.stdout env))
;;
