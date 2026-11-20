open! Core
module P = Agent_protocol
module J = P.Json_codec

type pair =
  { call : P.History.Id.t
  ; output : P.History.Id.t
  }
[@@deriving equal, sexp_of]

type t =
  { operation : P.Id.Operation.t
  ; ordinal : int64
  ; source_revision : int64
  ; pairs : pair list
  }
[@@deriving equal, sexp_of]

let max_pairs = Recovery_window_record.max_members / 2
let invalid () = Error (P.Error.invalid_request "invalid settled rollover batch")

let create ~operation ~ordinal ~source_revision pairs =
  if
    Int64.(ordinal <= 0L || source_revision < 0L)
    || List.is_empty pairs
    || Stdlib.List.compare_length_with pairs max_pairs > 0
    || List.contains_dup
         (List.concat_map pairs ~f:(fun pair -> [ pair.call; pair.output ]))
         ~compare:P.History.Id.compare
  then invalid ()
  else Ok { operation; ordinal; source_revision; pairs }
;;

let pair_to_json pair =
  `Object
    [ "call", P.History.Id.to_json pair.call; "output", P.History.Id.to_json pair.output ]
;;

let pair_of_json json =
  let open Result.Let_syntax in
  let%bind fields = J.fields json in
  let%bind call = J.required_as fields "call" P.History.Id.of_json in
  let%map output = J.required_as fields "output" P.History.Id.of_json in
  { call; output }
;;

let to_json t =
  `Object
    [ "operation", P.Id.Operation.to_json t.operation
    ; "ordinal", `Number (Int64.to_string t.ordinal)
    ; "source_revision", `Number (Int64.to_string t.source_revision)
    ; "pairs", `Array (List.map t.pairs ~f:pair_to_json)
    ]
;;

let pairs_of_json = function
  | `Array values when Stdlib.List.compare_length_with values max_pairs <= 0 ->
    List.map values ~f:pair_of_json |> Result.all
  | _ -> invalid ()
;;

let of_json json =
  let open Result.Let_syntax in
  let%bind fields = J.fields json in
  let number = J.bounded_int64 ~min:0L ~max:Int64.max_value in
  let%bind operation = J.required_as fields "operation" P.Id.Operation.of_json in
  let%bind ordinal = J.required_as fields "ordinal" number in
  let%bind source_revision = J.required_as fields "source_revision" number in
  let%bind pairs = J.required_as fields "pairs" pairs_of_json in
  create ~operation ~ordinal ~source_revision pairs
;;
