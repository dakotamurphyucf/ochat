open! Core
module P = Agent_protocol
module S = Child_usage_snapshot
module C = Child_contributions

let initialize_new (state : Session_state.t) ~max_children ~max_encoded_bytes =
  let open Result.Let_syntax in
  let%bind () =
    if
      Int64.equal state.counters.revision 0L
      && Int64.equal state.counters.transaction_sequence 0L
      && Int64.equal state.counters.event_sequence 0L
      && Option.is_none state.delegated_usage
      && Option.is_none state.child_contributions
    then Ok ()
    else
      Error
        (P.Error.invalid_request
           "child accounting birth requires an unpublished new State")
  in
  let%bind children =
    C.create ~inventory:Complete_inventory ~max_children ~max_encoded_bytes
  in
  let%bind delegated_usage =
    match state.spec.delegation with
    | None -> Ok None
    | Some reference ->
      Delegated_usage.create ~reference ~ledger:state.inference_ledger
      |> Result.map ~f:Option.some
  in
  let state = { state with delegated_usage; child_contributions = Some children } in
  let%map () = Session_state.validate state in
  state
;;

let snapshot (state : Session_state.t) =
  let open Result.Let_syntax in
  match state.delegated_usage with
  | None -> Ok None
  | Some own ->
    let%bind usage = Delegated_usage.usage own ~ledger:state.inference_ledger in
    let%bind marks =
      Canonical_usage_fold.Marks.of_ledger state.inference_ledger
      |> Result.map_error ~f:(fun error ->
        P.Error.invalid_request
          (Sexp.to_string_hum (Canonical_usage_fold.Error.sexp_of_t error)))
    in
    let%bind own_partition = S.Partition.create ~usage ~marks in
    let%bind descendants =
      match state.child_contributions with
      | Some children -> C.aggregate children
      | None ->
        let%map coverage =
          S.Coverage.create ~historical_children_unknown:true ~unobserved_children:0L
        in
        S.Descendants.create ~partition:S.Partition.empty ~coverage
    in
    let%map snapshot =
      S.create
        ~reference:(Delegated_usage.reference own)
        ~revision:state.counters.revision
        ~own:own_partition
        ~descendants
        ~canonical_coverage:
          (P.Inference_query.Summary.coverage
             (Inference_ledger.summary state.inference_ledger))
    in
    Some snapshot
;;

let register (state : Session_state.t) ~reference ~max_children ~max_encoded_bytes =
  let open Result.Let_syntax in
  let%bind children =
    match state.child_contributions with
    | Some children -> Ok children
    | None ->
      C.create ~inventory:Historical_children_unknown ~max_children ~max_encoded_bytes
  in
  C.register children ~reference
;;
