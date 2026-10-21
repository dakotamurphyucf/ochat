open Core
module Compiler = Chatml_compilation
module Prepared = Chatml_prepared_program
module Digest = Chatml_source.Digest

type accounting =
  { clock : Eio.Time.Mono.ty Eio.Resource.t
  ; limits : Compiler.limits
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

type deadline =
  { started : Mtime.t
  ; wall_seconds : float
  ; parent : deadline option
  }

type invocation = { mutable failure : Compiler.error option }

type t =
  { accounting : accounting
  ; invocation : invocation
  ; deadline : deadline
  ; callback : unit -> unit
  ; parent : t option
  ; mutable closed : bool
  }

module Ledger = struct
  type nonrec t = { owner : t }
end

exception Rejected of Compiler.error

let error code message = Compiler.{ code; message; diagnostic = None }
let expired () = error "chatml.compile_timeout" "startup compilation budget exceeded"

let rec lifetime_available t =
  if not (Int.equal t.accounting.domain_id (Stdlib.Domain.self () :> int))
  then
    Error
      (error
         "chatml.invalid_context"
         "startup compiler context belongs to another domain")
  else if t.closed || t.accounting.closed
  then Error (error "chatml.invalid_context" "startup compiler context has closed")
  else (
    match t.parent with
    | None -> Ok ()
    | Some parent -> lifetime_available parent)
;;

let accounting_available t =
  let open Result.Let_syntax in
  let%bind () = lifetime_available t in
  match t.accounting.failure with
  | Some failure -> Error failure
  | None -> Ok ()
;;

let available t =
  let open Result.Let_syntax in
  let%bind () = accounting_available t in
  match t.invocation.failure with
  | Some failure -> Error failure
  | None -> Ok ()
;;

let ledger t = Result.map (accounting_available t) ~f:(fun () -> Ledger.{ owner = t })

let expire t =
  let failure = expired () in
  t.invocation.failure <- Some failure;
  failure
;;

let remaining t =
  let now = Eio.Time.Mono.now t.accounting.clock in
  let rec loop deadline allowance =
    let elapsed = Mtime.span deadline.started now |> Mtime.Span.to_float_ns in
    let allowance = Float.min allowance (deadline.wall_seconds -. (elapsed /. 1e9)) in
    match deadline.parent with
    | None -> allowance
    | Some parent -> loop parent allowance
  in
  loop t.deadline Float.infinity
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
  if Float.(remaining t <= 0.) then raise (Rejected (expire t))
;;

let remaining_limits t =
  try
    let open Result.Let_syntax in
    let%bind () = available t in
    checkpoint t;
    let wall_seconds = remaining t in
    if Float.(wall_seconds <= 0.)
    then Error (expire t)
    else Ok { t.accounting.limits with wall_seconds }
  with
  | Rejected failure -> Error failure
;;

let reject t failure =
  t.accounting.failure <- Some failure;
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
    if String.length text > t.accounting.text_limit
    then
      reject t (error "chatml.definition_limit" "startup distinct text budget exceeded")
    else (
      let digest =
        Digest.of_contents text ~checkpoint:(fun () -> checkpoint t) |> Digest.to_hex
      in
      let rec admit () =
        checkpoint t;
        let generation = t.accounting.text_generation in
        let bucket = Hashtbl.find t.accounting.texts digest |> Option.value ~default:[] in
        let duplicate = List.exists bucket ~f:(equal_text t text) in
        (* Exact comparisons can yield. Retry after intervening admission, then
           check capacity and commit without yielding, so shared borrowers cannot
           overwrite a memo bucket or double-charge an identical text. *)
        if generation <> t.accounting.text_generation
        then admit ()
        else if duplicate
        then Ok ()
        else if String.length text > t.accounting.text_remaining
        then
          reject
            t
            (error "chatml.definition_limit" "startup distinct text budget exceeded")
        else (
          t.accounting.text_remaining <- t.accounting.text_remaining - String.length text;
          t.accounting.text_generation <- t.accounting.text_generation + 1;
          Hashtbl.set t.accounting.texts ~key:digest ~data:(text :: bucket);
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
    match Hashtbl.find t.accounting.graphs key with
    | Some admitted ->
      if Int.equal admitted bytes
      then Ok ()
      else reject t (error "chatml.invalid_input" "conflicting prepared input accounting")
    | None ->
      if bytes > t.accounting.graph_remaining
      then
        reject
          t
          (error "chatml.definition_limit" "startup canonical graph budget exceeded")
      else (
        t.accounting.graph_remaining <- t.accounting.graph_remaining - bytes;
        Hashtbl.set t.accounting.graphs ~key ~data:bytes;
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
           (Eio.Time.Timeout.seconds t.accounting.clock limits.wall_seconds)
           (fun () ->
              let result = f () in
              checkpoint t;
              Ok result)
       with
       | Ok result -> result
       | Error `Timeout ->
         let failure = expire t in
         Error (map_error failure))
  with
  | Rejected failure -> Error (map_error failure)
;;

let compatible (accounting : accounting) (limits : Compiler.limits) =
  Int.equal limits.max_source_bytes accounting.limits.max_source_bytes
  && Float.equal limits.wall_seconds accounting.limits.wall_seconds
;;

let validate_allowances ~max_distinct_text_bytes ~max_distinct_graph_input_bytes =
  if max_distinct_text_bytes < 0 || max_distinct_graph_input_bytes < 0
  then Error (error "chatml.invalid_limits" "negative startup byte allowance")
  else Ok ()
;;

let create_accounting
      ~clock
      ~limits
      ~max_distinct_text_bytes
      ~max_distinct_graph_input_bytes
  =
  { clock
  ; limits
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
;;

let close_accounting (accounting : accounting) =
  accounting.closed <- true;
  Hashtbl.clear accounting.texts;
  Hashtbl.clear accounting.graphs
;;

let with_view
      ~accounting
      ~invocation
      ~deadline
      ~parent
      ~parent_checkpoint
      ~checkpoint:callback
      ~map_error
      ~f
  =
  Chatml.Chatml_checkpoint.protect callback ~f:(fun callback ->
    let rec t =
      { accounting
      ; invocation
      ; deadline
      ; callback =
          (fun () ->
            Option.iter parent_checkpoint ~f:(fun checkpoint -> checkpoint ());
            (match available t with
             | Error failure -> raise (Rejected failure)
             | Ok () -> ());
            callback ())
      ; parent
      ; closed = false
      }
    in
    Exn.protect
      ~finally:(fun () -> t.closed <- true)
      ~f:(fun () ->
        try f t with
        | Rejected failure -> Error (map_error failure)))
;;

let with_invocation (ledger : Ledger.t) ~limits ~checkpoint ~map_error ~f =
  let open Result.Let_syntax in
  let%bind () = accounting_available ledger.owner |> Result.map_error ~f:map_error in
  let accounting = ledger.owner.accounting in
  if not (compatible accounting limits)
  then
    Error
      (map_error (error "chatml.invalid_context" "incompatible borrowed startup policy"))
  else (
    let invocation = { failure = None } in
    let deadline =
      { started = Eio.Time.Mono.now accounting.clock
      ; wall_seconds = limits.wall_seconds
      ; parent = None
      }
    in
    with_view
      ~accounting
      ~invocation
      ~deadline
      ~parent:(Some ledger.owner)
      ~parent_checkpoint:None
      ~checkpoint
      ~map_error
      ~f)
;;

let with_source_accounting
      t
      ?limits
      ?(max_distinct_text_bytes = 8 * 1024 * 1024)
      ?(max_distinct_graph_input_bytes = 8 * 1024 * 1024)
      ~checkpoint:callback
      ~map_error
      ~f
      ()
  =
  let open Result.Let_syntax in
  let%bind () = available t |> Result.map_error ~f:map_error in
  let limits = Option.value limits ~default:t.accounting.limits in
  let%bind () = Compiler.validate_limits limits |> Result.map_error ~f:map_error in
  let%bind () =
    validate_allowances ~max_distinct_text_bytes ~max_distinct_graph_input_bytes
    |> Result.map_error ~f:map_error
  in
  let accounting =
    create_accounting
      ~clock:t.accounting.clock
      ~limits
      ~max_distinct_text_bytes
      ~max_distinct_graph_input_bytes
  in
  let deadline =
    { started = Eio.Time.Mono.now accounting.clock
    ; wall_seconds = limits.wall_seconds
    ; parent = Some t.deadline
    }
  in
  Exn.protect
    ~finally:(fun () -> close_accounting accounting)
    ~f:(fun () ->
      with_view
        ~accounting
        ~invocation:t.invocation
        ~deadline
        ~parent:(Some t)
        ~parent_checkpoint:(Some (fun () -> checkpoint t))
        ~checkpoint:callback
        ~map_error
        ~f)
;;

let with_context
      ?startup
      ?(max_distinct_text_bytes = 8 * 1024 * 1024)
      ?(max_distinct_graph_input_bytes = 8 * 1024 * 1024)
      ~(limits : Compiler.limits)
      ~env
      ~checkpoint
      ~map_error
      ~f
      ()
  =
  match startup with
  | Some t ->
    (match available t with
     | Error failure -> Error (map_error failure)
     | Ok () ->
       if not (compatible t.accounting limits)
       then
         Error
           (map_error
              (error "chatml.invalid_context" "incompatible borrowed startup policy"))
       else f t)
  | None ->
    let open Result.Let_syntax in
    let%bind () = Compiler.validate_limits limits |> Result.map_error ~f:map_error in
    let%bind () =
      validate_allowances ~max_distinct_text_bytes ~max_distinct_graph_input_bytes
      |> Result.map_error ~f:map_error
    in
    let clock = Eio.Stdenv.mono_clock env in
    let accounting =
      create_accounting
        ~clock
        ~limits
        ~max_distinct_text_bytes
        ~max_distinct_graph_input_bytes
    in
    let invocation = { failure = None } in
    let deadline =
      { started = Eio.Time.Mono.now clock
      ; wall_seconds = limits.wall_seconds
      ; parent = None
      }
    in
    Exn.protect
      ~finally:(fun () -> close_accounting accounting)
      ~f:(fun () ->
        with_view
          ~accounting
          ~invocation
          ~deadline
          ~parent:None
          ~parent_checkpoint:None
          ~checkpoint
          ~map_error
          ~f)
;;
