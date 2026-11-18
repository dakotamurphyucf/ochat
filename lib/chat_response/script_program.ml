open Core

type t =
  | Single of Chatml_host_runtime.compiled_script
  | Prepared of Chatml_linked_program.t

let source_revision_of_prepared prepared ~single_file_revision =
  match prepared with
  | None -> single_file_revision
  | Some prepared ->
    Chatml_prepared_program.fingerprint prepared |> Chatml_source.Digest.to_hex
;;

let source_revision t ~single_file_revision =
  source_revision_of_prepared
    (match t with
     | Single _ -> None
     | Prepared program -> Some (Chatml_linked_program.prepared program))
    ~single_file_revision
;;

let run_in_scope
      t
      ?scoped_async_operation
      ?prepare_result
      ~control
      ~config
      ~entrypoint
      ~arguments
      ()
  =
  match t with
  | Single program ->
    Chatml_execution.run_in_scope
      ?prepare_result
      ?scoped_async_operation
      ~control
      ~config
      ~program
      ~entrypoint
      ~arguments
      ()
  | Prepared program ->
    Chatml_execution.run_prepared_in_scope
      program
      ?scoped_async_operation
      ?prepare_result
      ~control
      ~config
      ~entrypoint
      ~arguments
      ()
;;
