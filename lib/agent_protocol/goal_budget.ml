open! Core
module J = Json_codec

module Dimension = struct
  type t =
    | Admitted_turns
    | Admitted_attempts
    | Observed_tokens
  [@@deriving equal, sexp_of]
end

module Token_stop = struct
  type t =
    { component : Inference.Observation.Usage.Component.t
    ; ceiling : int64
    }
  [@@deriving equal, sexp_of]

  let create ~component ~ceiling =
    if Int64.(ceiling < 0L)
    then Error (Protocol_error.invalid_request "negative observed-token ceiling")
    else Ok { component; ceiling }
  ;;
end

type t =
  { admitted_turns : int64 option
  ; admitted_attempts : int64 option
  ; observed_tokens : Token_stop.t option
  ; deadline : Timestamp.t option
  }
[@@deriving equal, sexp_of]

let create ~admitted_turns ~admitted_attempts ~observed_tokens ~deadline =
  if
    List.exists
      [ admitted_turns; admitted_attempts ]
      ~f:(Option.exists ~f:(fun x -> Int64.(x < 0L)))
  then Error (Protocol_error.invalid_request "negative goal admission ceiling")
  else Ok { admitted_turns; admitted_attempts; observed_tokens; deadline }
;;

let unlimited =
  { admitted_turns = None
  ; admitted_attempts = None
  ; observed_tokens = None
  ; deadline = None
  }
;;

let admitted_turns t = t.admitted_turns
let admitted_attempts t = t.admitted_attempts
let observed_tokens t = t.observed_tokens
let deadline t = t.deadline

let component_to_string = function
  | Inference.Observation.Usage.Component.Input -> "input"
  | Output -> "output"
  | Reported_total -> "reported_total"
  | Cached_input -> "cached_input"
  | Cache_write_input -> "cache_write_input"
  | Reasoning_output -> "reasoning_output"
;;

let component_of_json =
  J.enum
    ~name:"goal token component"
    [ "input", Inference.Observation.Usage.Component.Input
    ; "output", Output
    ; "reported_total", Reported_total
    ; "cached_input", Cached_input
    ; "cache_write_input", Cache_write_input
    ; "reasoning_output", Reasoning_output
    ]
;;

let optional encode = function
  | None -> `Null
  | Some value -> encode value
;;

let nullable decode = function
  | `Null -> Ok None
  | json -> Result.map (decode json) ~f:Option.some
;;

let natural = function
  | `String text ->
    (match Int64.of_string_opt text with
     | Some value when Int64.(value >= 0L) && String.equal text (Int64.to_string value) ->
       Ok value
     | None | Some _ ->
       Error
         (Protocol_error.invalid_request "canonical nonnegative decimal string required"))
  | _ -> Error (Protocol_error.invalid_request "goal count must be a decimal string")
;;

let to_json t =
  let count value = `String (Int64.to_string value) in
  `Object
    [ "admitted_turns", optional count t.admitted_turns
    ; "admitted_attempts", optional count t.admitted_attempts
    ; ( "observed_tokens"
      , optional
          (fun value ->
             `Object
               [ "component", `String (component_to_string value.Token_stop.component)
               ; "ceiling", count value.ceiling
               ])
          t.observed_tokens )
    ; "deadline", optional Timestamp.to_json t.deadline
    ]
;;

let of_json json =
  let open Result.Let_syntax in
  let%bind () =
    Extension_codec.validate_json
      ~max_bytes:Run_limits.max_document_bytes
      ~max_depth:Run_limits.max_depth
      json
  in
  let%bind f = J.fields json in
  let%bind admitted_turns = J.required_as f "admitted_turns" (nullable natural) in
  let%bind admitted_attempts = J.required_as f "admitted_attempts" (nullable natural) in
  let%bind observed_tokens =
    J.required_as
      f
      "observed_tokens"
      (nullable (fun json ->
         let%bind fields = J.fields json in
         let%bind component = J.required_as fields "component" component_of_json in
         let%bind ceiling = J.required_as fields "ceiling" natural in
         Token_stop.create ~component ~ceiling))
  in
  let%bind deadline = J.required_as f "deadline" (nullable Timestamp.of_json) in
  create ~admitted_turns ~admitted_attempts ~observed_tokens ~deadline
;;
