open! Core
module D = Document_schema
module P = Agent_protocol
module X = Persistence_codec

module Owner = struct
  type t =
    | Unknown
    | Submitting_principal of P.Id.Principal.t
    | Host_internal
  [@@deriving equal, sexp]

  let shape = X.fields_shape [ "kind"; "principal_id" ]

  let authorize t ~principal =
    match t with
    | Submitting_principal owner when P.Id.Principal.equal owner principal -> Ok ()
    | Submitting_principal _ | Unknown | Host_internal ->
      Error
        (P.Error.create
           Permission_denied
           ~message:"pending occurrence submitting ownership is not authorized"
           ~retryable:false
           ())
  ;;

  let to_json = function
    | Unknown -> `Object [ "kind", `String "unknown"; "principal_id", `Null ]
    | Host_internal -> `Object [ "kind", `String "host_internal"; "principal_id", `Null ]
    | Submitting_principal principal ->
      `Object
        [ "kind", `String "submitting_principal"
        ; "principal_id", P.Id.Principal.to_json principal
        ]
  ;;

  let of_json json =
    let open Result.Let_syntax in
    let%bind fields = P.Json_codec.fields json in
    let%bind kind = P.Json_codec.required_as fields "kind" P.Json_codec.string in
    let%bind principal = P.Json_codec.required fields "principal_id" in
    match kind, principal with
    | "unknown", `Null -> Ok Unknown
    | "host_internal", `Null -> Ok Host_internal
    | "submitting_principal", principal ->
      P.Id.Principal.of_json principal
      |> Result.map ~f:(fun principal -> Submitting_principal principal)
    | _ -> Error (P.Error.invalid_request "invalid pending submitting ownership")
  ;;
end

type stored =
  { input : P.Pending_input.t
  ; owner : Owner.t
  }

let stored_json stored =
  match P.Pending_input.to_json stored.input with
  | `Object fields -> `Object (fields @ [ "owner", Owner.to_json stored.owner ])
  | _ -> assert false
;;

let fields = X.fields_shape
let proof_shape = fields [ "operation_id"; "generation"; "outcome" ]

let binding_shape =
  match
    D.Shape.tagged_object
      ~discriminator:"kind"
      [ "safe_boundary", fields [ "kind" ]
      ; "await_idle", fields [ "kind" ]
      ; ( "after_root"
        , X.shape_exn
            [ "kind", D.Shape.value
            ; "operation_id", D.Shape.value
            ; "generation", D.Shape.value
            ; "terminal", D.Shape.nullable proof_shape
            ] )
      ]
  with
  | Ok shape -> shape
  | Error error -> raise_s [%sexp "invalid pending binding shape", (error : D.Error.t)]
;;

let shape =
  X.shape_exn
    [ "id", D.Shape.value
    ; "entry", Session_record_shapes.history_entry
    ; "generation", D.Shape.value
    ; "owner", Owner.shape
    ; "binding", binding_shape
    ]
;;

let codec ~limits =
  match
    D.Domain_codec.create
      ~limits
      ~kind:"session.pending_input"
      ~version:1
      ~shape
      ~supported_semantics:[]
      ~decode:(fun json ->
        let open Result.Let_syntax in
        let%bind input = P.Pending_input.of_json json |> X.document_result in
        let%map owner =
          Agent_store.Document_fields.required json "owner" (fun json ->
            Owner.of_json json |> X.document_result)
        in
        { input; owner })
      ~encode:(fun value -> Ok (stored_json value))
  with
  | Ok codec -> codec
  | Error error -> raise_s [%sexp "invalid pending input codec", (error : D.Error.t)]
;;

type t =
  { carrier : stored D.Extension_carrier.t
  ; encoded : Jsonaf.t
  }

let equal left right = Jsonaf.exactly_equal left.encoded right.encoded
let value t = (D.Extension_carrier.value t.carrier).input
let owner t = (D.Extension_carrier.value t.carrier).owner
let entry t = P.Pending_input.entry (value t)
let sexp_of_t t = Jsonaf.sexp_of_t t.encoded

let to_jsonaf t ~limits =
  D.Domain_codec.encode (codec ~limits) t.carrier |> Result.map ~f:D.Document.payload
;;

let of_jsonaf payload ~limits =
  let open Result.Let_syntax in
  let%bind document =
    D.Document.create ~limits ~kind:"session.pending_input" ~version:1 ~payload
  in
  let%map carrier = D.Domain_codec.decode (codec ~limits) document in
  { carrier; encoded = payload }
;;

let authored ?(owner = Owner.Unknown) input ~limits =
  let value = { input; owner } in
  let candidate =
    { carrier = D.Extension_carrier.of_authored_value value; encoded = stored_json value }
  in
  let open Result.Let_syntax in
  let%bind payload = to_jsonaf candidate ~limits in
  of_jsonaf payload ~limits
;;

let with_value t input ~limits =
  let value = { (D.Extension_carrier.value t.carrier) with input } in
  let candidate = { t with carrier = D.Extension_carrier.with_value t.carrier value } in
  let%map.Result encoded = to_jsonaf candidate ~limits in
  { candidate with encoded }
;;

let entry_jsonaf t ~limits =
  let%bind.Result payload = to_jsonaf t ~limits in
  Agent_store.Document_fields.required payload "entry" Result.return
;;

let metadata_jsonaf t ~limits =
  let%bind.Result payload = to_jsonaf t ~limits in
  match payload with
  | `Object fields ->
    Ok
      (`Object (List.filter fields ~f:(fun (name, _) -> not (String.equal name "entry"))))
  | _ ->
    Error
      (D.Error.Invalid_field { path = []; reason = "pending wrapper is not an object" })
;;

(* Native sexp replay uses the repository's bounded default admission. Production
   persistence uses the owning whole-state codec and its explicit limits. *)
let t_of_sexp sexp =
  match of_jsonaf (Jsonaf.t_of_sexp sexp) ~limits:D.Limits.default with
  | Ok value -> value
  | Error error -> raise_s [%sexp "invalid pending carrier", (error : D.Error.t)]
;;

let known_jsonaf t ~limits =
  let json = stored_json (D.Extension_carrier.value t.carrier) in
  D.Json.validate ~limits json |> Result.map ~f:(fun () -> json)
;;
