open! Core
module P = Agent_protocol
module D = Document_schema
module X = Persistence_codec

type t =
  { growth : Capacity_growth.t
  ; additional : Capacity_growth.t
  ; projections : D.Document.t list
  }

let growth t = t.growth
let additional_growth t = t.additional
let projections t = t.projections

let replace json changes =
  match json with
  | `Object fields ->
    `Object
      (List.filter fields ~f:(fun (name, _) ->
         not (List.Assoc.mem changes name ~equal:String.equal))
       @ changes)
  | _ -> assert false
;;

let capture (state : Session_state.t) ~shape ~document ~limits =
  let open Result.Let_syntax in
  match state.run_state with
  | None ->
    Ok
      { growth = Capacity_growth.zero
      ; additional = Capacity_growth.zero
      ; projections = []
      }
  | Some index ->
    let%bind capacity = Run_state.terminal_capacity index |> X.document_result in
    let%bind bookkeeping =
      Run_job_capacity.bookkeeping_reserve index ~jobs:state.jobs |> X.document_result
    in
    let delivery = Run_state.reserved_delivery_bytes index in
    (* Retain the pre-existing byte credits additively. Each 4096-byte job
       bookkeeping unit bounds a work key/proof (<64 fields/128 nodes). A frame
       is a JSON STRING; each 256-byte disposition unit bounds <16 fields/32
       nodes. Charging per unit also covers introducing job_deliveries itself. *)
    let units = (bookkeeping + 4095) / 4096 in
    let disposition_units = (delivery + 255) / 256 in
    let%bind extra =
      Capacity_growth.create
        ~bytes:(bookkeeping + delivery)
        ~fields:((64 * units) + (16 * disposition_units))
        ~nodes:((128 * units) + (32 * disposition_units))
      |> X.document_result
    in
    if Run_terminal_capacity.is_empty capacity
    then Ok { growth = extra; additional = extra; projections = [] }
    else (
      (* Exactly the real owner's shape and merge algorithm, with identity JSON
         projection instead of a domain parser. Witnesses cannot mint domain
         evidence. Fixed three passes across the whole index, not per-Run copies. *)
      let%bind codec =
        D.Domain_codec.create
          ~limits
          ~kind:(D.Document.kind document)
          ~version:(D.Document.version document)
          ~shape
          ~supported_semantics:[]
          ~decode:(fun json -> Ok json)
          ~encode:(fun json -> Ok json)
      in
      let%bind carrier = D.Domain_codec.decode codec document in
      let payload = D.Extension_carrier.value carrier in
      let%bind original =
        Capacity_growth.measure (D.Document.json document) ~limits |> X.document_result
      in
      let run_limits =
        match
          D.Limits.create
            ~max_bytes:P.Run_limits.max_document_bytes
            ~max_depth:P.Run_limits.max_depth
            ~max_fields:100_000
            ~max_nodes:1_000_000
        with
        | Ok limits -> limits
        | Error error ->
          raise_s [%sexp "invalid terminal Run profile", (error : D.Error.t)]
      in
      let%bind child_limits =
        Capacity_growth.remaining_limits extra run_limits |> X.document_result
      in
      let%bind growth, projections =
        List.fold_result
          (Run_terminal_capacity.projections capacity ~index:(Run_state.to_jsonaf index))
          ~init:(Capacity_growth.zero, [])
          ~f:(fun (growth, projections) run ->
            let counters =
              match D.Json.field payload ~name:"counters" with
              | Value counters ->
                replace
                  counters
                  [ "revision", `String (Int64.to_string Int64.max_value)
                  ; "transaction_sequence", `String (Int64.to_string Int64.max_value)
                  ; "event_sequence", `String (Int64.to_string Int64.max_value)
                  ]
              | _ -> assert false
            in
            let identity =
              match D.Json.field payload ~name:"identity" with
              | Value identity ->
                replace
                  identity
                  [ "updated_at", `String "2099-12-31T23:59:59.999999999Z" ]
              | _ -> assert false
            in
            let projected =
              replace
                payload
                [ "run_state", run; "counters", counters; "identity", identity ]
            in
            let%bind projected =
              D.Domain_codec.encode
                codec
                (D.Extension_carrier.with_value carrier projected)
            in
            let%bind () =
              match D.Json.field (D.Document.payload projected) ~name:"run_state" with
              | Value raw -> D.Json.validate ~limits:child_limits raw
              | Absent | Null -> assert false
            in
            let%map measured =
              Capacity_growth.measure (D.Document.json projected) ~limits
              |> X.document_result
            in
            ( Capacity_growth.maximum
                growth
                (Capacity_growth.positive_difference measured ~previous:original)
            , projected :: projections ))
      in
      let%map growth = Capacity_growth.add growth extra |> X.document_result in
      { growth; additional = extra; projections = List.rev projections })
;;
