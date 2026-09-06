open Core

module Request = struct
  type t = { payload : Jsonaf.t option } [@@deriving sexp]

  let to_json t =
    match t.payload with
    | None -> `Object []
    | Some payload -> `Object [ "payload", payload ]
  ;;

  let of_json json =
    let open Result.Let_syntax in
    let%map fields = Json_codec.fields json in
    { payload = Json_codec.optional fields "payload" }
  ;;
end

module Response = struct
  type t =
    { payload : Jsonaf.t option
    ; server_time : Timestamp.t
    ; ready : bool
    ; draining : bool
    }
  [@@deriving sexp]

  let to_json t =
    let fields =
      [ "server_time", Timestamp.to_json t.server_time
      ; ("ready", if t.ready then `True else `False)
      ; ("draining", if t.draining then `True else `False)
      ]
    in
    match t.payload with
    | None -> `Object fields
    | Some payload -> `Object (("payload", payload) :: fields)
  ;;

  let of_json json =
    let open Result.Let_syntax in
    let%bind fields = Json_codec.fields json in
    let payload = Json_codec.optional fields "payload" in
    let%bind server_time =
      Json_codec.required_as fields "server_time" Timestamp.of_json
    in
    let%bind ready = Json_codec.required_as fields "ready" Json_codec.bool in
    let%map draining = Json_codec.required_as fields "draining" Json_codec.bool in
    { payload; server_time; ready; draining }
  ;;
end
