open Core

type t =
  | Locked of string option
  | Missing of string
  | Schema_too_new of int
  | Migration_required of int
  | Corrupt of string
  | Io of
      { operation : string
      ; path : string
      ; message : string
      }
[@@deriving sexp]

let of_exn ~operation ~path exn = Io { operation; path; message = Exn.to_string exn }

let to_protocol_error = function
  | Locked owner ->
    Agent_protocol.Error.create
      Store_locked
      ~message:
        (Option.value_map owner ~default:"store is locked" ~f:(fun x ->
           "store is locked by " ^ x))
      ~retryable:true
      ()
  | Missing path ->
    Agent_protocol.Error.create
      Persistence_error
      ~message:("store path is missing: " ^ path)
      ~retryable:false
      ()
  | Schema_too_new version ->
    Agent_protocol.Error.create
      Store_schema_too_new
      ~message:(sprintf "store schema %d is newer than this server" version)
      ~retryable:false
      ()
  | Migration_required version ->
    Agent_protocol.Error.create
      Migration_required
      ~message:(sprintf "store schema %d requires migration" version)
      ~retryable:false
      ()
  | Corrupt message ->
    Agent_protocol.Error.create Journal_corrupt ~message ~retryable:false ()
  | Io { operation; path; message } ->
    Agent_protocol.Error.create
      Persistence_error
      ~message:(sprintf "%s failed for %s: %s" operation path message)
      ~retryable:true
      ()
;;
