open! Core
module D = Document_schema
module P = Agent_protocol
module X = Persistence_codec

type metadata =
  { history_id : P.History.Id.t
  ; generation : int
  ; owner : Pending_input_document.Owner.t
  ; binding : P.Pending_input.Binding.t
  }

type t = { carrier : metadata D.Extension_carrier.t }

let shape =
  X.shape_exn
    [ "id", D.Shape.value
    ; "generation", D.Shape.value
    ; "owner", Pending_input_document.Owner.shape
    ; "binding", Pending_input_document.binding_shape
    ]
;;

let decode json =
  let open Result.Let_syntax in
  let%bind fields = P.Json_codec.fields json in
  let%bind () =
    if Option.is_some (P.Json_codec.optional fields "entry")
    then
      Error (P.Error.invalid_request "pending metadata custody cannot contain an entry")
    else Ok ()
  in
  let%bind history_id = P.Json_codec.required_as fields "id" P.History.Id.of_json in
  let%bind generation =
    P.Json_codec.required_as
      fields
      "generation"
      (P.Json_codec.bounded_int ~min:0 ~max:Int.max_value)
  in
  let%bind owner =
    P.Json_codec.required_as fields "owner" Pending_input_document.Owner.of_json
  in
  let%bind binding =
    P.Json_codec.required_as fields "binding" P.Pending_input.Binding.of_json
  in
  let%map () =
    match binding with
    | Safe_boundary | Await_idle -> Ok ()
    | After_root { generation = bound; _ } ->
      if Int.equal generation bound
      then Ok ()
      else Error (P.Error.invalid_request "pending custody barrier generation differs")
  in
  { history_id; generation; owner; binding }
;;

let encode t =
  `Object
    [ "id", P.History.Id.to_json t.history_id
    ; "generation", `Number (Int.to_string t.generation)
    ; "owner", Pending_input_document.Owner.to_json t.owner
    ; "binding", P.Pending_input.Binding.to_json t.binding
    ]
;;

let codec ~limits =
  match
    D.Domain_codec.create
      ~limits
      ~kind:"session.pending_metadata_custody"
      ~version:1
      ~shape
      ~supported_semantics:[]
      ~decode:(fun json -> decode json |> X.document_result)
      ~encode:(fun value -> Ok (encode value))
  with
  | Ok codec -> codec
  | Error error -> raise_s [%sexp "invalid pending custody codec", (error : D.Error.t)]
;;

let of_jsonaf payload ~limits =
  let open Result.Let_syntax in
  let%bind document =
    D.Document.create ~limits ~kind:"session.pending_metadata_custody" ~version:1 ~payload
  in
  let%map carrier = D.Domain_codec.decode (codec ~limits) document in
  { carrier }
;;

let to_jsonaf t ~limits =
  D.Domain_codec.encode (codec ~limits) t.carrier |> Result.map ~f:D.Document.payload
;;

let of_pending document ~limits =
  let%bind.Result payload = Pending_input_document.metadata_jsonaf document ~limits in
  of_jsonaf payload ~limits
;;

let history_id t = (D.Extension_carrier.value t.carrier).history_id
let generation t = (D.Extension_carrier.value t.carrier).generation
let owner t = (D.Extension_carrier.value t.carrier).owner

let known_jsonaf t ~limits =
  let json = encode (D.Extension_carrier.value t.carrier) in
  D.Json.validate ~limits json |> Result.map ~f:(fun () -> json)
;;
