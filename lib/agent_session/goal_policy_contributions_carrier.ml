open! Core
module P = Agent_protocol
module D = Document_schema
module X = Persistence_codec
module C = D.Extension_carrier
module Data = Goal_policy_contributions_data
module Capacity = Goal_policy_contributions_capacity
module R = Goal_policy_receipt
module PC = Policy_canonical_usage

let domain = R.domain
let invalid = R.invalid
let limits = Data.limits
let shape = Data.shape

let codec =
  X.codec_exn
    ~version:1
    ~limits
    ~kind:"ochat.goal_policy_contributions"
    ~shape
    ~decode:Data.of_json
    ~encode:(fun value ->
      let%map.Result () = Data.validate value in
      Data.to_json value)
;;

type t = Data.t C.t

let value (t : t) : Data.t = C.value t
let encoded t = D.Domain_codec.encode codec t |> domain

module Prepared_capacity = struct
  type carrier = t

  type t =
    { growth : Capacity_growth.t
    ; depth : int
    }

  let capture source ~current =
    let%map.Result growth, depth = Capacity.capture (value source) ~current in
    { growth; depth }
  ;;

  let prepare source =
    let%bind.Result current = encoded source in
    capture source ~current
  ;;

  let reserved_growth t = t.growth
  let required_depth t = t.depth

  let validate t raw =
    let open Result.Let_syntax in
    let%bind remaining = Capacity_growth.remaining_limits t.growth limits in
    let%bind () = D.Json.validate raw ~limits:remaining |> domain in
    Capacity_depth.validate t.depth ~ancestors:0 ~limits
  ;;
end

let reserved_growth t =
  let%map.Result prepared = Prepared_capacity.prepare t in
  Prepared_capacity.reserved_growth prepared
;;

let required_depth t =
  let%map.Result prepared = Prepared_capacity.prepare t in
  Prepared_capacity.required_depth prepared
;;

let validate_encoded_capacity t raw =
  let%bind.Result prepared = Prepared_capacity.prepare t in
  Prepared_capacity.validate prepared raw
;;

let to_document t =
  let open Result.Let_syntax in
  let%bind document = encoded t in
  let%bind () = Data.validate_original_children (D.Document.payload document) in
  let%bind prepared = Prepared_capacity.capture t ~current:document in
  let%map () = Prepared_capacity.validate prepared (D.Document.json document) in
  document
;;

let admit t =
  let%map.Result (_ : D.Document.t) = to_document t in
  t
;;

let of_document document =
  let open Result.Let_syntax in
  let%bind () = Data.validate_original_children (D.Document.payload document) in
  let%bind t = D.Domain_codec.decode codec document |> domain in
  let%bind t = admit t in
  let%map () = validate_encoded_capacity t (D.Document.json document) in
  t
;;

let with_value t value = admit (C.with_value t value)

let adopt previous ~incoming =
  let open Result.Let_syntax in
  if not (Data.equal (value previous) (value incoming))
  then invalid "causal known movement requires a closed Update"
  else (
    let%bind merged = D.Domain_codec.adopt codec ~previous ~incoming |> domain in
    admit merged)
;;

let inline ~owner ~canonical ~ledger ~session_revision =
  let%map.Result projections =
    Option.value_map canonical ~default:(Ok []) ~f:(fun canonical ->
      PC.projections canonical ~ledger ~session_revision)
  in
  List.filter projections ~f:(fun projection ->
    let stream = PC.Projection.stream projection in
    P.Session_ref.equal owner (PC.Stream.callee stream)
    && P.Session_ref.equal
         owner
         (Policy_cause_data.Goal_target.owner (PC.Stream.target stream)))
;;

let check_stream_capacity t ~inline =
  let remote =
    Option.value_map t ~default:0 ~f:(fun t ->
      List.sum
        (module Int)
        (value t).books
        ~f:(fun book -> List.length book.Data.Book.slots))
  in
  if remote + List.length inline <= Data.max_streams
  then Ok ()
  else invalid "combined inline and remote lifetime stream capacity exhausted"
;;

let validate_canonical_edge
      t
      ~owner
      ~catalog
      ~accounts
      ~canonical
      ~before
      ~after
      ~session_revision
  =
  let open Result.Let_syntax in
  let data = value t in
  let%bind () = Data.validate data in
  let session_id = P.Session_ref.session_id owner in
  let%bind () =
    if
      P.Session_ref.equal owner data.owner
      && P.Id.Session.equal session_id (Inference_ledger.session_id before)
      && P.Id.Session.equal session_id (Inference_ledger.session_id after)
      && Int64.(session_revision >= 0L)
    then Ok ()
    else invalid "causal Goal book owner, ledger session or revision differs"
  in
  let%bind inline = inline ~owner ~canonical ~ledger:after ~session_revision in
  let%bind () = check_stream_capacity (Some t) ~inline in
  List.fold_result data.books ~init:() ~f:(fun () book ->
    let%bind goal =
      Option.bind catalog ~f:(fun catalog ->
        Goal_catalog.find catalog book.Data.Book.goal_id)
      |> Result.of_option ~error:(P.Error.invalid_request "causal stable Goal is absent")
    in
    let%bind document =
      Option.bind accounts ~f:(fun accounts -> Goal_accounts.find accounts book.goal_id)
      |> Result.of_option
           ~error:(P.Error.invalid_request "causal stable Goal account is absent")
    in
    let%bind () =
      if P.Session_ref.equal owner (P.Goal.session goal)
      then Ok ()
      else invalid "causal Goal account has another full owner"
    in
    let%map (_ : Goal_accounting.t) =
      Goal_accounting_document.resolve document ~goal ~ledger:before
    in
    ())
;;

let validate_context t ~owner ~catalog ~accounts ~canonical ~ledger ~session_revision =
  validate_canonical_edge
    t
    ~owner
    ~catalog
    ~accounts
    ~canonical
    ~before:ledger
    ~after:ledger
    ~session_revision
;;

(* Full foreign child custody is established through the existing Book codec;
   native Slot encoding remains known-only. Same planned value is mandatory. *)
let retain_slot candidate ~stream ~projection =
  let open Result.Let_syntax in
  let%bind document = to_document candidate in
  let payload = D.Document.payload document in
  let%bind fields = X.object_ payload in
  let%bind books = X.required fields "books" (X.list X.raw) in
  let replace raw key value =
    match raw with
    | `Object fields ->
      `Object
        (List.map fields ~f:(fun (name, old) ->
           name, if String.equal name key then value else old))
    | _ -> assert false
  in
  let%bind books =
    List.map books ~f:(fun book ->
      let%bind fields = X.object_ book in
      let%bind slots = X.required fields "slots" (X.list X.raw) in
      let%bind slots =
        List.map slots ~f:(fun slot ->
          let%bind fields = X.object_ slot in
          let%bind id = X.required fields "id" P.Json_codec.string in
          if not (String.equal id (PC.Stream.id stream))
          then Ok slot
          else (
            let slot = replace slot "stream" (PC.Stream.to_json stream) in
            Ok
              (Option.value_map projection ~default:slot ~f:(fun projection ->
                 replace slot "projection" (PC.Projection.to_json projection)))))
        |> Result.all
      in
      Ok (replace book "slots" (`Array slots)))
    |> Result.all
  in
  let payload = replace payload "books" (`Array books) in
  let raw = replace (D.Document.json document) "payload" payload in
  let%bind document = D.Document.inspect raw ~limits |> domain in
  let%bind incoming = of_document document in
  adopt candidate ~incoming
;;
