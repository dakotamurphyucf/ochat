open! Core
module P = Agent_protocol
module J = P.Json_codec
module X = Persistence_codec
module Shape = Document_schema.Shape
module Scope = P.Recovery_window.Scope
module Id = Inference.Observation.Observation_id
module Digest = Chatml_source.Digest

let invalid message = Error (P.Error.invalid_request message)

let id_of_json json =
  let%bind.Result value = J.string json in
  Id.of_string value
  |> Result.map_error ~f:(fun _ -> P.Error.invalid_request "invalid reminder identity")
;;

let id_to_json id = `String (Id.to_string id)

let digest_of_json json =
  let%bind.Result value = J.string json in
  Digest.of_hex value
  |> Result.map_error ~f:(fun _ -> P.Error.invalid_request "invalid reminder revision")
;;

module Claim = struct
  type t =
    { scope : Scope.t
    ; policy : Digest.t
    ; operation : P.Id.Operation.t
    ; preparation : Id.t
    }
  [@@deriving equal]

  let preparation t = Id.to_string t.preparation

  let create ~scope ~policy ~operation ~preparation =
    let%map.Result preparation = id_of_json (`String preparation) in
    { scope; policy; operation; preparation }
  ;;

  let key ~scope ~policy =
    `Array [ Scope.to_json scope; `String (Digest.to_hex policy) ]
    |> Jsonaf.to_string
    |> Digest.of_contents ~checkpoint:ignore
    |> Digest.to_hex
  ;;

  let to_json t =
    `Object
      [ "scope", Scope.to_json t.scope
      ; "policy", `String (Digest.to_hex t.policy)
      ; "operation", P.Id.Operation.to_json t.operation
      ; "preparation", id_to_json t.preparation
      ]
  ;;

  let of_json json =
    let open Result.Let_syntax in
    let%bind fields = J.fields json in
    let%bind scope = J.required_as fields "scope" Scope.of_json in
    let%bind policy = J.required_as fields "policy" digest_of_json in
    let%bind operation = J.required_as fields "operation" P.Id.Operation.of_json in
    let%map preparation = J.required_as fields "preparation" id_of_json in
    { scope; policy; operation; preparation }
  ;;

  let shape =
    X.shape_exn
      [ ( "scope"
        , X.shape_exn
            [ "session", Recovery_window_ledger.Source.owner_shape
            ; "generation", Shape.value
            ; "window", Shape.value
            ] )
      ; "policy", Shape.value
      ; "operation", Shape.value
      ; "preparation", Shape.value
      ]
  ;;
end

module Change = struct
  type t =
    | Claim of Claim.t
    | Accept of Claim.t * Id.t

  let claim = function
    | Claim claim | Accept (claim, _) -> claim
  ;;

  let to_json t =
    let state =
      match t with
      | Claim _ -> [ "kind", `String "claimed" ]
      | Accept (_, attempt) ->
        [ "kind", `String "accepted"; "attempt", id_to_json attempt ]
    in
    `Object (("claim", Claim.to_json (claim t)) :: state)
  ;;

  let of_json json =
    let open Result.Let_syntax in
    let%bind fields = J.fields json in
    let%bind claim = J.required_as fields "claim" Claim.of_json in
    match%bind J.required_as fields "kind" J.string with
    | "claimed" -> Ok (Claim claim)
    | "accepted" ->
      let%map attempt = J.required_as fields "attempt" id_of_json in
      Accept (claim, attempt)
    | _ -> invalid "invalid reminder change"
  ;;

  let shape =
    let fields = [ "kind", Shape.value; "claim", Claim.shape ] in
    X.tagged_shape_exn
      ~discriminator:"kind"
      [ "claimed", X.shape_exn fields
      ; "accepted", X.shape_exn (("attempt", Shape.value) :: fields)
      ]
  ;;
end

module Row = struct
  type t =
    { claim : Claim.t
    ; attempt : Id.t option
    }
  [@@deriving equal]

  let to_json key t =
    `Object
      [ "key", `String key
      ; "claim", Claim.to_json t.claim
      ; "attempt", Option.value_map t.attempt ~default:`Null ~f:id_to_json
      ]
  ;;

  let of_json json =
    let open Result.Let_syntax in
    let%bind fields = J.fields json in
    let%bind key = J.required_as fields "key" J.string in
    let%bind claim = J.required_as fields "claim" Claim.of_json in
    let%bind attempt = J.required_as fields "attempt" (X.nullable id_of_json) in
    if String.equal key (Claim.key ~scope:claim.scope ~policy:claim.policy)
    then Ok (key, { claim; attempt })
    else invalid "reminder key does not match the claimed window and policy"
  ;;

  let shape =
    X.shape_exn [ "key", Shape.value; "claim", Claim.shape; "attempt", Shape.value ]
  ;;
end

type t = Row.t String.Map.t [@@deriving equal]

let max_keys = 512
let rows t = Option.value t ~default:String.Map.empty

let can_claim t ~scope ~policy =
  let rows = rows t in
  match Map.find rows (Claim.key ~scope ~policy) with
  | None -> Map.length rows < max_keys
  | Some row -> Option.is_none row.Row.attempt
;;

let accepted_by t (claim : Claim.t) attempt =
  Option.exists
    (Map.find (rows t) (Claim.key ~scope:claim.scope ~policy:claim.policy))
    ~f:(fun row ->
      Claim.equal claim row.Row.claim
      && Option.value_map row.attempt ~default:false ~f:(Id.equal attempt))
;;

let apply t change ~scope ~operation =
  let claim = Change.claim change in
  let rows = rows t in
  let key = Claim.key ~scope:claim.scope ~policy:claim.policy in
  let previous = Map.find rows key in
  if not (Scope.equal scope claim.scope && P.Id.Operation.equal operation claim.operation)
  then invalid "reminder changed outside the current window or operation"
  else (
    match change, previous with
    | Claim _, _ when can_claim t ~scope:claim.scope ~policy:claim.policy ->
      Ok (Map.set rows ~key ~data:{ Row.claim; attempt = None })
    | Accept (_, attempt), Some row
      when Claim.equal claim row.claim && Option.is_none row.attempt ->
      Ok (Map.set rows ~key ~data:{ row with attempt = Some attempt })
    | _ -> invalid "reminder claim is exhausted, already accepted or superseded")
;;

let validate_state t ~session ~generation ~owner =
  let belongs row =
    let scope = row.Row.claim.scope in
    let reference = Scope.session scope in
    P.Id.Session.equal session (P.Session_ref.session_id reference)
    && Scope.generation scope <= generation
    && Option.for_all owner ~f:(P.Session_ref.equal reference)
  in
  if Map.for_all t ~f:belongs
  then Ok ()
  else invalid "reminder ledger belongs to a different session or generation"
;;

let check_unchanged previous ~incoming =
  if Option.equal equal previous incoming
  then Ok ()
  else invalid "replacement changed the private reminder ledger"
;;

let to_json t =
  `Array (Map.to_alist t |> List.map ~f:(fun (key, row) -> Row.to_json key row))
;;

let of_json = function
  | `Array rows when Stdlib.List.compare_length_with rows max_keys <= 0 ->
    let%bind.Result rows = List.map rows ~f:Row.of_json |> Result.all in
    (match String.Map.of_alist rows with
     | `Ok rows -> Ok rows
     | `Duplicate_key _ -> invalid "duplicate window and policy in reminder ledger")
  | _ -> invalid "invalid reminder ledger"
;;

let shape = X.array_shape_exn ~identity_field:"key" Row.shape
