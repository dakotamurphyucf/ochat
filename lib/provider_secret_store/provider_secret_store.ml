open! Core

module Error = struct
  type operation =
    | Open
    | Create
    | Read
    | Delete
    | Close
  [@@deriving sexp_of]

  type code =
    | Missing
    | Exists
    | Denied
    | Unavailable
    | Unsupported_context
    | Corrupt
    | Too_large
    | Invalid_identifier
    | Busy
    | Closed
  [@@deriving equal, sexp_of]

  type t =
    { operation : operation
    ; code : code
    ; publication : Private_storage.Error.publication option
    }
  [@@deriving sexp_of]

  let code t = t.code
  let operation t = t.operation
  let publication t = t.publication
  let make ?publication operation code = { operation; code; publication }

  let of_storage operation error =
    let code =
      match Private_storage.Error.code error with
      | Missing -> Missing
      | Exists -> Exists
      | Denied -> Denied
      | Unavailable -> Unavailable
      | Unsupported_filesystem -> Unsupported_context
      | Corrupt -> Corrupt
      | Too_large -> Too_large
      | Invalid_name -> Invalid_identifier
      | Busy -> Busy
      | Closed -> Closed
    in
    make operation code ?publication:(Private_storage.Error.publication error)
  ;;
end

let identifier value =
  if
    String.length value = 0
    || String.length value > 48
    || not
         (String.for_all value ~f:(function
            | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '-' | '_' -> true
            | _ -> false))
  then Error (Error.make Open Invalid_identifier)
  else Ok value
;;

module Namespace = struct
  type t = string

  let create = identifier
end

module Revision = struct
  type t = string

  let create = identifier
  let to_string t = t
end

module Secret = struct
  type t = string

  let maximum_bytes = 256 * 1024

  let of_bytes bytes =
    let length = Bytes.length bytes in
    if length = 0 || length > maximum_bytes
    then Error (Error.make Read Too_large)
    else Ok (Bytes.to_string bytes)
  ;;

  let with_string t ~f = f t
  let length = String.length
end

type t =
  { directory : Private_storage.Directory.t
  ; namespace : Namespace.t
  ; mutex : Eio.Mutex.t
  ; mutable closed : bool
  }

let close t =
  Eio.Cancel.protect (fun () ->
    Eio.Mutex.use_rw ~protect:true t.mutex (fun () -> t.closed <- true))
;;

let open_private_files ~sw ~directory ~namespace =
  let t = { directory; namespace; mutex = Eio.Mutex.create (); closed = false } in
  Eio.Switch.on_release sw (fun () -> close t);
  Ok t
;;

let with_revision t revision operation ~mutation f =
  (* Only close mutates mutex-owned lifetime state. Backend operations may
     cancel without invalidating that state or poisoning later cleanup. *)
  Eio.Mutex.use_ro t.mutex (fun () ->
    if t.closed
    then
      Error
        (Error.make
           operation
           Closed
           ?publication:
             (if mutation then Some Private_storage.Error.Not_published else None))
    else (
      (* Length-prefixed namespace prevents separator collisions. *)
      let name = sprintf "s%d-%s-%s" (String.length t.namespace) t.namespace revision in
      match Private_storage.Name.create name with
      | Error error -> Error (Error.of_storage operation error)
      | Ok name -> Result.map_error (f name) ~f:(Error.of_storage operation)))
;;

let create t ~revision secret =
  with_revision t revision Create ~mutation:true (fun name ->
    Private_storage.Directory.create_immutable t.directory name (Bytes.of_string secret))
;;

let read t ~revision =
  Result.bind
    (with_revision t revision Read ~mutation:false (fun name ->
       Private_storage.Directory.read_bounded
         t.directory
         name
         ~max_bytes:Secret.maximum_bytes))
    ~f:(fun bytes ->
      Fun.protect
        ~finally:(fun () -> Bytes.fill bytes ~pos:0 ~len:(Bytes.length bytes) '\000')
        (fun () -> Secret.of_bytes bytes))
;;

let delete t ~revision =
  with_revision t revision Delete ~mutation:true (fun name ->
    Private_storage.Directory.delete t.directory name)
;;
