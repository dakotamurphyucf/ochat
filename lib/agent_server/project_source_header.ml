open Core

type error =
  | Filesystem
  | Too_large
[@@deriving equal, sexp_of]

let read fs ~path ~max_bytes ~checkpoint =
  if
    max_bytes < 0
    || max_bytes >= Project_source_configuration.maximum_authorization_header_bytes + 1
  then Error Too_large
  else
    let module Callback = struct
      exception Raised of exn * Stdlib.Printexc.raw_backtrace
    end
    in
    let protected_checkpoint () =
      try checkpoint () with
      | exn -> raise (Callback.Raised (exn, Stdlib.Printexc.get_raw_backtrace ()))
    in
    let poll () =
      Eio.Fiber.check ();
      protected_checkpoint ();
      Eio.Fiber.check ()
    in
    let buffer = Cstruct.create (max_bytes + 1) in
    let read () =
      Eio.Path.with_open_in
        Eio.Path.(fs / path)
        (fun flow ->
           let rec loop length =
             poll ();
             if length > max_bytes
             then Error Too_large
             else (
               match Eio.Flow.single_read flow (Cstruct.shift buffer length) with
               | read -> loop (length + read)
               | exception End_of_file ->
                 poll ();
                 Ok (Cstruct.to_string ~len:length buffer))
           in
           loop 0)
    in
    try read () with
    | Callback.Raised (exn, trace) -> Exn.raise_with_original_backtrace exn trace
    | Eio.Io _ -> Error Filesystem
;;
