open Core

type t =
  | Single of Chatml_host_runtime.compiled_script
  | Prepared of Chatml_linked_program.t

let source_revision t ~single_file_revision =
  match t with
  | Single _ -> single_file_revision
  | Prepared program ->
    Chatml_linked_program.prepared program
    |> Chatml_prepared_program.fingerprint
    |> Chatml_source.Digest.to_hex
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
