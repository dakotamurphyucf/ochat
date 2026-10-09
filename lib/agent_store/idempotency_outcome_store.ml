open! Core
module Outcome = Idempotency_outcome

let max_bytes = Outcome.max_encoded_bytes

let basename reference =
  "idempotency-outcome-" ^ Outcome.Reference.digest reference ^ ".json"
;;

let load reference ~directory =
  let open Result.Let_syntax in
  let%bind bytes =
    Durable_file.load_bounded_in ~directory ~basename:(basename reference) ~max_bytes
  in
  Outcome.decode reference bytes
;;

let publish outcome ~directory =
  let reference = Outcome.reference outcome in
  let name = basename reference in
  try
    match Eio.Path.kind ~follow:false Eio.Path.(directory / name) with
    | `Not_found ->
      Result.map
        (Durable_file.replace_in
           ~directory
           ~durability:Flush_file_and_directory
           ~basename:name
           (Outcome.to_string outcome))
        ~f:(fun () -> reference)
    | `Regular_file ->
      let open Result.Let_syntax in
      let%bind existing = load reference ~directory in
      let%map () =
        Durable_file.replace_in
          ~directory
          ~durability:Flush_file_and_directory
          ~basename:name
          (Outcome.to_string existing)
      in
      reference
    | `Directory
    | `Symbolic_link
    | `Fifo
    | `Socket
    | `Character_special
    | `Block_device
    | `Unknown ->
      Error (Store_error.Corrupt "idempotency outcome path is not a regular owned file")
  with
  | (Eio.Io _ | Core_unix.Unix_error _) as exn ->
    Error (Store_error.of_exn ~operation:"publish idempotency outcome" ~path:name exn)
;;

let read_retained reference ~reader =
  let open Result.Let_syntax in
  let%bind bytes = Retention_reader.read reader ~path:(basename reference) ~max_bytes in
  Outcome.decode reference bytes
;;
