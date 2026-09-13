open Core
module P = Agent_protocol
module V = Chatml.Chatml_value_codec
module L = Chatml.Chatml_lang

module Jsonaf = struct
  include Jsonaf

  let equal = exactly_equal
end

type t =
  { registration_id : P.Id.Capability.t
  ; event_id : P.Id.Ingress_event.t
  ; subscription_id : P.Id.Subscription.t
  ; epoch : int
  ; namespace : string
  ; payload : Jsonaf.t
  }
[@@deriving equal]

let tag = "__Ochat_ingress_delivery_v1"

let json t =
  `Object
    [ "registration_id", P.Id.Capability.to_json t.registration_id
    ; "event_id", P.Id.Ingress_event.to_json t.event_id
    ; "subscription_id", P.Id.Subscription.to_json t.subscription_id
    ; "epoch", `String (Int.to_string t.epoch)
    ; "namespace", `String t.namespace
    ; "data", t.payload
    ]
;;

let of_json value =
  let open Result.Let_syntax in
  let%bind () =
    P.Json_codec.validate_limits ~max_depth:132 ~max_bytes:(2 * 1024 * 1024) value
  in
  let%bind fields = P.Json_codec.fields value in
  let%bind () =
    match List.length (P.Json_codec.to_alist fields) = 6 with
    | true -> Ok ()
    | false -> Error (P.Error.invalid_request "invalid ingress frame fields")
  in
  let get key decode = P.Json_codec.required_as fields key decode in
  let%bind registration_id = get "registration_id" P.Id.Capability.of_json in
  let%bind event_id = get "event_id" P.Id.Ingress_event.of_json in
  let%bind subscription_id = get "subscription_id" P.Id.Subscription.of_json in
  let%bind epoch = get "epoch" P.Json_codec.string in
  let%bind epoch =
    Result.try_with (fun () -> Int.of_string epoch)
    |> Result.map_error ~f:(fun _ -> P.Error.invalid_request "invalid ingress epoch")
  in
  let%bind namespace = get "namespace" P.Json_codec.string in
  let%bind payload = P.Json_codec.required fields "data" in
  let%bind () =
    P.Json_codec.validate_limits ~max_depth:128 ~max_bytes:(1024 * 1024) payload
  in
  match
    epoch >= 0
    && String.is_prefix namespace ~prefix:"external."
    && String.length namespace > 9
    && String.length namespace <= 128
  with
  | true -> Ok { registration_id; event_id; subscription_id; epoch; namespace; payload }
  | false -> Error (P.Error.invalid_request "invalid ingress frame identity")
;;

let create ~registration_id ~event_id ~subscription_id ~epoch ~namespace ~payload =
  of_json (json { registration_id; event_id; subscription_id; epoch; namespace; payload })
  |> Result.map_error ~f:(fun error -> error.P.Error.message)
;;

let capture t =
  L.VVariant
    ( tag
    , [ L.VString (P.Id.Capability.to_string t.registration_id)
      ; L.VString (P.Id.Ingress_event.to_string t.event_id)
      ; L.VString (P.Id.Subscription.to_string t.subscription_id)
      ; L.VInt t.epoch
      ; L.VString t.namespace
      ; L.VString (Jsonaf.to_string t.payload)
      ] )
;;

let decode = function
  | L.VVariant (name, args) when String.equal name tag ->
    let open Result.Let_syntax in
    (match args with
     | [ L.VString registration
       ; L.VString event
       ; L.VString subscription
       ; L.VInt epoch
       ; L.VString namespace
       ; L.VString data
       ] ->
       let%bind payload =
         Chatmd_shell_spec.Tool_schema.parse_json data
         |> Result.map_error ~f:(fun _ -> "invalid ingress delivery JSON")
       in
       of_json
         (`Object
             [ "registration_id", `String registration
             ; "event_id", `String event
             ; "subscription_id", `String subscription
             ; "epoch", `String (Int.to_string epoch)
             ; "namespace", `String namespace
             ; "data", payload
             ])
       |> Result.map ~f:Option.some
       |> Result.map_error ~f:(fun error -> error.P.Error.message)
     | _ -> Error "invalid ingress delivery envelope")
  | _ -> Ok None
;;

let script_event value =
  let open Result.Let_syntax in
  let%map frame = decode value in
  match frame with
  | None -> value
  | Some frame ->
    let data =
      match json frame with
      | `Object fields -> `Object (("kind", `String "external_data") :: fields)
      | _ -> assert false
    in
    L.VVariant ("Internal_event", [ V.jsonaf_to_value data ])
;;
