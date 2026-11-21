open! Core
module P = Agent_protocol
module Scope = P.Recovery_window.Scope
module L = Chatml.Chatml_lang
module Value = Chatml.Chatml_value_codec

type pressure =
  | Unknown
  | Within_budget
  | Near_limit
  | Over_limit

type checkpoint =
  | Missing
  | Checkpointed of Jsonaf.t list
  | Archive_only

type t =
  | Before of
      { intent : P.Id.Operation.t
      ; window : Scope.t
      ; expected_revision : int64
      ; pressure : pressure
      ; checkpoint : checkpoint
      }
  | After of
      { intent : P.Id.Operation.t
      ; previous : Scope.t
      ; window : Scope.t
      ; cut_revision : int64
      ; checkpoint : checkpoint
      }

type continuation =
  | Continue
  | Wait
  | Finish
[@@deriving equal]

let plain name = L.VVariant (name, [])
let text value = L.VString value
let record fields = L.VRecord (String.Map.of_alist_exn fields)

let window scope =
  let session = Scope.session scope in
  record
    [ "server_id", text (P.Id.Server.to_string (P.Session_ref.server_id session))
    ; "session_id", text (P.Id.Session.to_string (P.Session_ref.session_id session))
    ; "generation", L.VInt (Scope.generation scope)
    ; "id", text (P.Id.Context_window.to_string (Scope.window scope))
    ]
;;

let checkpoint ?control = function
  | Missing -> plain "Missing"
  | Archive_only -> plain "Archive_only"
  | Checkpointed notes ->
    let notes = List.map notes ~f:(Value.import_json ?control) |> Array.of_list in
    L.VVariant ("Checkpointed", [ VArray notes ])
;;

let pressure = function
  | Unknown -> plain "Unknown"
  | Within_budget -> plain "Within_budget"
  | Near_limit -> plain "Near_limit"
  | Over_limit -> plain "Over_limit"
;;

let to_value ?control = function
  | Before basis ->
    L.VVariant
      ( "Before_context_rollover"
      , [ record
            [ "intent_id", text (P.Id.Operation.to_string basis.intent)
            ; "window", window basis.window
            ; "expected_revision", text (Int64.to_string basis.expected_revision)
            ; "pressure", pressure basis.pressure
            ; "checkpoint", checkpoint ?control basis.checkpoint
            ]
        ] )
  | After committed ->
    L.VVariant
      ( "After_context_rollover"
      , [ record
            [ "intent_id", text (P.Id.Operation.to_string committed.intent)
            ; "window", window committed.window
            ; "previous", window committed.previous
            ; "cut_revision", text (Int64.to_string committed.cut_revision)
            ; "checkpoint", checkpoint ?control committed.checkpoint
            ]
        ] )
;;

module Decision = struct
  type event = t

  type t =
    | Veto
    | Proceed of
        { hints : Jsonaf.t list option
        ; continuation : continuation option
        }
    | Followup of continuation

  let invalid = "invalid installed rollover hook decision"

  let continuation = function
    | L.VVariant ("Continue", []) -> Ok Continue
    | VVariant ("Wait", []) -> Ok Wait
    | VVariant ("Finish", []) -> Ok Finish
    | _ -> Error invalid
  ;;

  let optional decode = function
    | L.VVariant ("None", []) -> Ok None
    | VVariant ("Some", [ value ]) -> Result.map (decode value) ~f:Option.some
    | _ -> Error invalid
  ;;

  let references ?control = function
    | L.VArray values when Array.length values <= 8 ->
      let open Result.Let_syntax in
      let%bind references =
        Array.to_list values |> List.map ~f:(Value.export_json ?control) |> Result.all
      in
      P.Json_codec.validate_limits ~max_depth:9 ~max_bytes:16_384 (`Array references)
      |> Result.map ~f:(fun () -> references)
      |> Result.map_error ~f:(fun error -> error.P.Error.message)
    | _ -> Error invalid
  ;;

  let of_value ?control ~event value =
    let open Result.Let_syntax in
    match event, value with
    | Before _, L.VVariant ("Before", [ VVariant ("Veto", []) ]) -> Ok Veto
    | Before _, VVariant ("Before", [ VVariant ("Proceed", [ VRecord fields ]) ])
      when Int.equal (Map.length fields) 2 ->
      let%bind hints = Value.expect_record_field "rollover proposal" fields "hints" in
      let%bind selected =
        Value.expect_record_field "rollover proposal" fields "continuation"
      in
      let%bind hints = optional (references ?control) hints in
      let%map continuation = optional continuation selected in
      Proceed { hints; continuation }
    | After _, VVariant ("After", [ value ]) ->
      Result.map (continuation value) ~f:(fun selected -> Followup selected)
    | (Before _ | After _), _ -> Error invalid
  ;;
end
