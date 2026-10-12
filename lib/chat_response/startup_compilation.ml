open Core
module Compiler = Chatml_compilation
module Prepared = Chatml_prepared_program
module Digest = Chatml_source.Digest

type t =
  { clock : Eio.Time.Mono.ty Eio.Resource.t
  ; started : Mtime.t
  ; limits : Compiler.limits
  ; callback : unit -> unit
  ; texts : string list String.Table.t
  ; graphs : int String.Table.t
  ; text_limit : int
  ; domain_id : int
  ; mutable text_generation : int
  ; mutable text_remaining : int
  ; mutable graph_remaining : int
  ; mutable closed : bool
  ; mutable failure : Compiler.error option
  }

exception Rejected of Compiler.error

let error code message = Compiler.{ code; message; diagnostic = None }
let expired () = error "chatml.compile_timeout" "startup compilation budget exceeded"

let available t =
  if not (Int.equal t.domain_id (Stdlib.Domain.self () :> int))
  then
    Error
      (error
         "chatml.invalid_context"
         "startup compiler context belongs to another domain")
  else if t.closed
  then Error (error "chatml.invalid_context" "startup compiler context has closed")
  else (
    match t.failure with
    | Some failure -> Error failure
    | None -> Ok ())
;;

let remaining t =
  let elapsed = Mtime.span t.started (Eio.Time.Mono.now t.clock) in
  t.limits.wall_seconds -. (Mtime.Span.to_float_ns elapsed /. 1e9)
;;

let checkpoint t =
  (match available t with
   | Error failure -> raise (Rejected failure)
   | Ok () -> ());
  Eio.Fiber.check ();
  t.callback ();
  Eio.Fiber.check ();
  (match available t with
   | Error failure -> raise (Rejected failure)
   | Ok () -> ());
  if Float.(remaining t <= 0.) then raise (Rejected (expired ()))
;;

let remaining_limits t =
  try
    let open Result.Let_syntax in
    let%bind () = available t in
    checkpoint t;
    let wall_seconds = remaining t in
    if Float.(wall_seconds <= 0.)
    then Error (expired ())
    else Ok { t.limits with wall_seconds }
  with
  | Rejected failure -> Error failure
;;

let reject t failure =
  t.failure <- Some failure;
  Error failure
;;

let equal_text t left right =
  if String.length left <> String.length right
  then false
  else (
    let rec loop offset =
      checkpoint t;
      if offset = String.length left
      then true
      else (
        let stop = Int.min (String.length left) (offset + 4096) in
        let rec compare index =
          if index = stop
          then loop stop
          else if Char.equal left.[index] right.[index]
          then compare (index + 1)
          else false
        in
        compare offset)
    in
    loop 0)
;;

let admit_text t text =
  try
    let open Result.Let_syntax in
    let%bind () = available t in
    checkpoint t;
    (* Even a duplicate must fit the original whole allowance before hashing. The
     remaining allowance is tested only after exact duplicate detection. *)
    if String.length text > t.text_limit
    then
      reject t (error "chatml.definition_limit" "startup distinct text budget exceeded")
    else (
      let digest =
        Digest.of_contents text ~checkpoint:(fun () -> checkpoint t) |> Digest.to_hex
      in
      let rec admit () =
        checkpoint t;
        let generation = t.text_generation in
        let bucket = Hashtbl.find t.texts digest |> Option.value ~default:[] in
        let duplicate = List.exists bucket ~f:(equal_text t text) in
        (* Exact comparisons can yield. Retry after intervening admission, then
           check capacity and commit without yielding, so shared borrowers cannot
           overwrite a memo bucket or double-charge an identical text. *)
        if generation <> t.text_generation
        then admit ()
        else if duplicate
        then Ok ()
        else if String.length text > t.text_remaining
        then
          reject
            t
            (error "chatml.definition_limit" "startup distinct text budget exceeded")
        else (
          t.text_remaining <- t.text_remaining - String.length text;
          t.text_generation <- t.text_generation + 1;
          Hashtbl.set t.texts ~key:digest ~data:(text :: bucket);
          Ok ())
      in
      admit ())
  with
  | Rejected failure -> Error failure
;;

let admit_prepared t prepared =
  try
    let open Result.Let_syntax in
    let%bind () = available t in
    checkpoint t;
    let key = Prepared.fingerprint prepared |> Digest.to_hex in
    let bytes = Prepared.Accounting.input_bytes (Prepared.accounting prepared) in
    match Hashtbl.find t.graphs key with
    | Some admitted ->
      if Int.equal admitted bytes
      then Ok ()
      else reject t (error "chatml.invalid_input" "conflicting prepared input accounting")
    | None ->
      if bytes > t.graph_remaining
      then
        reject
          t
          (error "chatml.definition_limit" "startup canonical graph budget exceeded")
      else (
        t.graph_remaining <- t.graph_remaining - bytes;
        Hashtbl.set t.graphs ~key ~data:bytes;
        Ok ())
  with
  | Rejected failure -> Error failure
;;

let within t ~map_error ~f =
  try
    match remaining_limits t with
    | Error failure -> Error (map_error failure)
    | Ok limits ->
      (match
         Eio.Time.Timeout.run
           (Eio.Time.Timeout.seconds t.clock limits.wall_seconds)
           (fun () ->
              let result = f () in
              checkpoint t;
              Ok result)
       with
       | Ok result -> result
       | Error `Timeout ->
         let failure = expired () in
         t.failure <- Some failure;
         Error (map_error failure))
  with
  | Rejected failure -> Error (map_error failure)
;;

let with_context
      ?startup
      ?(max_distinct_text_bytes = 8 * 1024 * 1024)
      ?(max_distinct_graph_input_bytes = 8 * 1024 * 1024)
      ~(limits : Compiler.limits)
      ~env
      ~checkpoint:callback
      ~map_error
      ~f
      ()
  =
  match startup with
  | Some t ->
    (match available t with
     | Error failure -> Error (map_error failure)
     | Ok () ->
       if
         (not (Int.equal limits.max_source_bytes t.limits.max_source_bytes))
         || not (Float.equal limits.wall_seconds t.limits.wall_seconds)
       then
         Error
           (map_error
              (error "chatml.invalid_context" "incompatible borrowed startup policy"))
       else f t)
  | None ->
    let open Result.Let_syntax in
    let%bind () = Compiler.validate_limits limits |> Result.map_error ~f:map_error in
    let%bind () =
      if max_distinct_text_bytes < 0 || max_distinct_graph_input_bytes < 0
      then
        Error
          (map_error (error "chatml.invalid_limits" "negative startup byte allowance"))
      else Ok ()
    in
    Chatml.Chatml_checkpoint.protect callback ~f:(fun callback ->
      let clock = Eio.Stdenv.mono_clock env in
      let t =
        { clock
        ; started = Eio.Time.Mono.now clock
        ; limits
        ; callback
        ; texts = String.Table.create ()
        ; graphs = String.Table.create ()
        ; text_limit = max_distinct_text_bytes
        ; domain_id = (Stdlib.Domain.self () :> int)
        ; text_generation = 0
        ; text_remaining = max_distinct_text_bytes
        ; graph_remaining = max_distinct_graph_input_bytes
        ; closed = false
        ; failure = None
        }
      in
      Exn.protect
        ~finally:(fun () ->
          t.closed <- true;
          Hashtbl.clear t.texts;
          Hashtbl.clear t.graphs)
        ~f:(fun () ->
          try f t with
          | Rejected failure -> Error (map_error failure)))
;;
