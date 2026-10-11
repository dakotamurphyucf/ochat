open Core
module Range = Chatmd_parser_input.Range

module Limits = struct
  type t =
    { occurrences : int
    ; parser_inputs : int
    ; input : int
    ; output : int
    ; work : int
    ; nodes : int
    ; depth : int
    ; payloads : int
    ; payload_bytes : int
    ; document : Document_schema.Limits.t
    }

  let create
        ~max_occurrences
        ~max_parser_inputs
        ~max_input_bytes
        ~max_output_bytes
        ~max_work_steps
        ~max_nodes
        ~max_depth
        ~max_payloads
        ~max_payload_bytes
        ~document
    =
    if
      List.exists
        [ max_occurrences
        ; max_parser_inputs
        ; max_input_bytes
        ; max_output_bytes
        ; max_work_steps
        ; max_nodes
        ; max_depth
        ; max_payloads
        ; max_payload_bytes
        ]
        ~f:(fun n -> n < 0)
    then Or_error.error_string "content limits must be nonnegative"
    else
      Ok
        { occurrences = max_occurrences
        ; parser_inputs = max_parser_inputs
        ; input = max_input_bytes
        ; output = max_output_bytes
        ; work = max_work_steps
        ; nodes = max_nodes
        ; depth = max_depth
        ; payloads = max_payloads
        ; payload_bytes = max_payload_bytes
        ; document
        }
  ;;

  let occurrences t = t.occurrences
  let parser_inputs t = t.parser_inputs
  let input_bytes t = t.input
  let output_bytes t = t.output
  let work_steps t = t.work
  let nodes t = t.nodes
  let depth t = t.depth
  let payloads t = t.payloads
  let payload_bytes t = t.payload_bytes
  let document t = t.document
end

module Error = struct
  type reason =
    | Invalid of string
    | Document of Document_schema.Error.t
    | Storage of Agent_store.Store_error.t
    | Conversion of Webpage_markdown.Bounded_conversion.Error.t

  type t =
    { range : Range.t option
    ; reason : reason
    }

  let invalid ?range reason = { range; reason = Invalid (String.prefix reason 256) }
  let document error = { range = None; reason = Document error }
  let storage error = { range = None; reason = Storage error }
  let conversion range error = { range = Some range; reason = Conversion error }
  let range t = t.range

  let sexp_of_t t =
    let reason =
      match t.reason with
      | Invalid text -> [%sexp Invalid (text : string)]
      | Document error -> [%sexp Document (error : Document_schema.Error.t)]
      | Storage error -> [%sexp Storage (error : Agent_store.Store_error.t)]
      | Conversion error ->
        [%sexp Conversion (error : Webpage_markdown.Bounded_conversion.Error.t)]
    in
    let owner =
      Option.map t.range ~f:(fun range ->
        ( Chatmd_parser_input.path (Range.input range)
        , Range.start_byte range
        , Range.end_byte range ))
    in
    [%sexp (owner : (string * int * int) option), (reason : Sexp.t)]
  ;;
end

module Accounting = struct
  type t =
    { occurrences : int
    ; input : int
    ; output : int
    ; work : int
    ; nodes : int
    }

  let create ~occurrences ~input_bytes ~output_bytes ~work_steps ~nodes =
    { occurrences; input = input_bytes; output = output_bytes; work = work_steps; nodes }
  ;;

  let zero = create ~occurrences:0 ~input_bytes:0 ~output_bytes:0 ~work_steps:0 ~nodes:0
  let occurrences t = t.occurrences
  let input_bytes t = t.input
  let output_bytes t = t.output
  let work_steps t = t.work
  let nodes t = t.nodes
end

type t =
  { limits : Limits.t
  ; checkpoint : unit -> unit
  ; mutable used : Accounting.t
  }

let create limits ~checkpoint = { limits; checkpoint; used = Accounting.zero }
let limits t = t.limits
let checkpoint t = t.checkpoint ()
let accounting t = t.used

let add t a =
  checkpoint t;
  let u = t.used
  and l = t.limits in
  let fits amount used maximum = amount >= 0 && amount <= maximum - used in
  if
    not
      (fits a.Accounting.occurrences u.occurrences l.occurrences
       && fits a.input u.input l.input
       && fits a.output u.output l.output
       && fits a.work u.work l.work
       && fits a.nodes u.nodes l.nodes)
  then Error (Error.invalid "content aggregate limit")
  else (
    t.used
    <- Accounting.
         { occurrences = u.occurrences + a.occurrences
         ; input = u.input + a.input
         ; output = u.output + a.output
         ; work = u.work + a.work
         ; nodes = u.nodes + a.nodes
         };
    Ok ())
;;

let work t units =
  add
    t
    (Accounting.create
       ~occurrences:0
       ~input_bytes:0
       ~output_bytes:0
       ~work_steps:units
       ~nodes:0)
;;

let work_product t units count =
  if
    units < 0
    || count < 0
    || (units > 0
        && count > (Limits.work_steps t.limits - Accounting.work_steps t.used) / units)
  then Error (Error.invalid "content work limit")
  else work t (units * count)
;;

let map_work t count =
  let rec height n total = if n = 0 then total else height (n / 2) (total + 1) in
  work t (256 * (height count 0 + 1))
;;

let count t values ~maximum =
  List.fold_result values ~init:0 ~f:(fun count _ ->
    let open Result.Let_syntax in
    let%bind () = work t 1 in
    if count >= maximum
    then Error (Error.invalid "content inventory count limit")
    else Ok (count + 1))
;;

let hash t contents =
  let open Result.Let_syntax in
  let%bind () = work t (String.length contents) in
  Ok
    (Chatml_source.Digest.of_contents contents ~checkpoint:t.checkpoint
     |> Chatml_source.Digest.to_hex)
;;

let equal_bytes t left right =
  let open Result.Let_syntax in
  if String.length left <> String.length right
  then Ok false
  else (
    let%bind () = work t (String.length left) in
    let offset = ref 0
    and equal = ref true in
    while !equal && !offset < String.length left do
      if !offset mod 4096 = 0 then checkpoint t;
      equal := Char.equal left.[!offset] right.[!offset];
      incr offset
    done;
    checkpoint t;
    Ok !equal)
;;
