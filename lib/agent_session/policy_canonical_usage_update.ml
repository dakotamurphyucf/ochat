open! Core
module P = Agent_protocol
module D = Document_schema
module X = Persistence_codec
module C = D.Extension_carrier
module Data = Policy_canonical_usage_data
module Cause = Policy_cause_data
module Member = Cause.Member
module Work = Cause.Allocation_work
module Target = Cause.Goal_target
module Coverage = Cause.Coverage
module Context = Cause.Context
module L = Inference_ledger
module F = Canonical_usage_fold
module Canonical = Goal_descendants.Canonical
module Occurrence = Goal_descendant_occurrence
module U = Goal_usage
open Policy_canonical_usage_carrier
open Policy_canonical_usage_selection

type t = Policy_canonical_usage_carrier.t

module Selection = Policy_canonical_usage_selection.Selection

module Update = struct
  type accounting = t

  module Reason = struct
    type t =
      | Register_event of Cause.t
      | Register_work of
          { cause : Cause.Key.t
          ; member : Member.t
          }
      | Reconcile of Selection.t option
      | Prune_work

    let to_json = function
      | Register_event cause ->
        `Object [ "kind", `String "register_event"; "cause", Cause.to_json cause ]
      | Register_work { cause; member } ->
        `Object
          [ "kind", `String "register_work"
          ; "cause", Cause.Key.to_json cause
          ; "member", Member.to_json member
          ]
      | Reconcile selection ->
        `Object
          [ "kind", `String "reconcile"
          ; "selection", X.option_json Selection.to_json selection
          ]
      | Prune_work -> `Object [ "kind", `String "prune_work" ]
    ;;

    let of_json raw =
      let open Result.Let_syntax in
      let%bind fields = X.object_ raw in
      let%bind kind = X.required fields "kind" P.Json_codec.string in
      match kind with
      | "register_event" ->
        let%map cause = X.required fields "cause" Cause.of_json in
        Register_event cause
      | "register_work" ->
        let%bind cause = X.required fields "cause" Cause.Key.of_json in
        let%map member = X.required fields "member" Member.of_json in
        Register_work { cause; member }
      | "reconcile" ->
        let%map selected = X.required fields "selection" (X.nullable Selection.of_json) in
        Reconcile selected
      | "prune_work" -> Ok Prune_work
      | _ -> invalid "unknown policy Update reason"
    ;;

    let shape =
      X.tagged_shape_exn
        ~discriminator:"kind"
        [ "register_event", X.shape_exn [ "kind", D.Shape.value; "cause", Cause.shape ]
        ; ( "register_work"
          , X.shape_exn
              [ "kind", D.Shape.value; "cause", D.Shape.value; "member", Member.shape ] )
        ; ( "reconcile"
          , X.shape_exn
              [ "kind", D.Shape.value; "selection", D.Shape.nullable Selection.shape ] )
        ; "prune_work", X.fields_shape [ "kind" ]
        ]
    ;;
  end

  type t =
    { candidate : accounting
    ; reason : Reason.t
    }

  let candidate t = t.candidate

  let update_limits =
    X.limits ~max_bytes:(8 * 1024 * 1024)
    |> domain
    |> Result.map_error ~f:(fun error -> error.P.Error.message)
    |> Result.ok_or_failwith
  ;;

  let to_json t =
    let open Result.Let_syntax in
    let%bind candidate = to_document t.candidate in
    let raw =
      `Object
        [ "accounting", D.Document.json candidate; "reason", Reason.to_json t.reason ]
    in
    let%map () = D.Json.validate raw ~limits:update_limits |> domain in
    raw
  ;;

  let of_json raw =
    let open Result.Let_syntax in
    let%bind () = D.Json.validate raw ~limits:update_limits |> domain in
    let%bind fields = X.object_ raw in
    let%bind candidate =
      X.required fields "accounting" (fun raw ->
        let%bind document = D.Document.inspect raw ~limits |> domain in
        of_document document)
    in
    let%map reason = X.required fields "reason" Reason.of_json in
    { candidate; reason }
  ;;

  let shape = X.shape_exn [ "accounting", D.Shape.value; "reason", Reason.shape ]
end

let make_update candidate reason =
  let%map.Result candidate = admit candidate in
  { Update.candidate; reason }
;;

let member_birth ~memberships ~before ~after cause member =
  let open Result.Let_syntax in
  let%bind absent = Data.Source_work.capture ~memberships before cause member in
  let%bind () =
    if Option.is_none absent
    then Ok ()
    else invalid "policy source work was already present before registration"
  in
  let%bind present = Data.Source_work.capture ~memberships after cause member in
  Result.of_option
    present
    ~error:(P.Error.invalid_request "policy source work absent at birth prefix")
;;

let registered_member cause (current : Data.Source_work.t) =
  { Data.Membership.member = current.member
  ; cause = Cause.key cause
  ; parent = current.parent
  ; projection_sha256 = current.projection_sha256
  }
;;

let register_event original ~before ~after ~ledger ~cause =
  let open Result.Let_syntax in
  let source = Cause.callee_source cause in
  let%bind member =
    Member.event (Cause.parent_event cause) ~generation:source.generation
  in
  let%bind current =
    member_birth
      ~memberships:
        (Option.value_map original ~default:[] ~f:(fun original ->
           (value original).memberships))
      ~before
      ~after
      cause
      member
  in
  let%bind () =
    if
      current.ready
      && Option.is_none current.parent
      && P.Id.Session.equal
           after.Context.session_id
           (P.Session_ref.session_id (Cause.callee cause))
      && Option.exists after.current_source ~f:(P.Run_source.equal source)
    then Ok ()
    else invalid "policy seed is not this actual new Running delegated Event"
  in
  let%bind accounting =
    match original with
    | Some original ->
      let%map () = exact_current original ~context:before ~ledger in
      original
    | None ->
      let%bind marks = marks ledger in
      let data : Data.t =
        { callee = Cause.callee cause
        ; max_inherited_scopes = Cause.max_inherited_scopes cause
        ; basis_revision = L.revision ledger
        ; basis_marks = marks
        ; ledger_revision = L.revision ledger
        ; marks
        ; causes = []
        ; memberships = []
        ; streams = []
        ; bindings = []
        }
      in
      admit (C.of_authored_value data)
  in
  let data = value accounting in
  let%bind () =
    if
      P.Session_ref.equal data.callee (Cause.callee cause)
      && Int.equal data.max_inherited_scopes (Cause.max_inherited_scopes cause)
      && Option.is_none (Data.find_cause data (Cause.key cause))
      && not
           (List.exists data.memberships ~f:(fun row ->
              Member.equal row.Data.Membership.member member))
    then Ok ()
    else invalid "policy seed is duplicate or changes component identity"
  in
  let%bind candidate =
    with_value
      accounting
      { data with
        causes = data.causes @ [ cause ]
      ; memberships = data.memberships @ [ registered_member cause current ]
      }
  in
  let%bind candidate = retain_children candidate ~cause:(Some cause) in
  make_update candidate (Update.Reason.Register_event cause)
;;

let register_work accounting ~before ~after ~cause ~member =
  let open Result.Let_syntax in
  let data = value accounting in
  let%bind actual_cause =
    Result.of_option
      (Data.find_cause data cause)
      ~error:(P.Error.invalid_request "policy work cause absent")
  in
  let%bind () =
    if Option.is_none (Data.find_member data member)
    then Ok ()
    else invalid "policy member cannot be registered twice"
  in
  let%bind current =
    member_birth ~memberships:data.memberships ~before ~after actual_cause member
  in
  let%bind parent =
    Result.of_option
      current.parent
      ~error:(P.Error.invalid_request "policy nonseed work has no exact parent")
  in
  let%bind retained =
    Result.of_option
      (Data.find_member data parent)
      ~error:(P.Error.invalid_request "policy original parent has no accepted membership")
  in
  let%bind () =
    if Cause.Key.equal retained.cause cause
    then Ok ()
    else invalid "policy work original parent belongs to another cause"
  in
  let%bind candidate =
    with_value
      accounting
      { data with
        memberships = data.memberships @ [ registered_member actual_cause current ]
      }
  in
  make_update candidate (Update.Reason.Register_work { cause; member })
;;

let reconcile accounting ~before ~after ~selection =
  let open Result.Let_syntax in
  let data = value accounting in
  let%bind original_marks = marks before in
  let%bind () =
    if
      Int64.equal data.ledger_revision (L.revision before)
      && F.Marks.equal data.marks original_marks
    then Ok ()
    else invalid "policy reconciliation before-ledger differs"
  in
  let%bind () = L.validate_update before ~incoming:after |> Data.ledger in
  let%bind after_marks = marks after in
  let%bind all_handles =
    List.map data.bindings ~f:(fun binding ->
      let%bind.Result row =
        Result.of_option
          (L.find before ~ordinal:binding.Data.Binding.ordinal)
          ~error:(P.Error.invalid_request "policy bound canonical before-row absent")
      in
      let handle = L.Row.handle row in
      let frame = Occurrence.frame (Canonical.Fact.occurrence binding.canonical) in
      match
        ( Occurrence.kind (Canonical.Fact.occurrence binding.canonical)
        , Canonical.Fact.tracking binding.canonical )
      with
      | Attempt { ordinal; operation_id; invocation_id }, L.Tracked ->
        if
          Int64.equal ordinal (L.Handle.ordinal handle)
          && P.Id.Session.equal
               (P.Session_ref.session_id (Occurrence.Frame.session frame))
               (L.Handle.session_id handle)
          && Int.equal (Occurrence.Frame.generation frame) (L.Handle.generation handle)
          && Option.equal P.Id.Operation.equal operation_id (L.Handle.operation_id handle)
          && Option.equal
               P.Id.Invocation.equal
               invocation_id
               (L.Handle.invocation_id handle)
        then Ok handle
        else invalid "policy reconciliation bound canonical identity differs"
      | Host_turn _, _ | Attempt _, Untracked _ ->
        invalid "policy retained binding is not tracked Attempt")
    |> Result.all
  in
  let%bind (_ : F.Plan.t) =
    F.prepare U.empty ~before ~after ~handles:all_handles |> Data.fold
  in
  let%bind streams =
    List.map data.streams ~f:(fun partition ->
      let handles =
        List.filter_map data.bindings ~f:(fun binding ->
          if Data.selected data binding partition.Data.Partition.stream
          then Option.map (L.find before ~ordinal:binding.ordinal) ~f:L.Row.handle
          else None)
      in
      let%map.Result fold =
        F.prepare partition.settled ~before ~after ~handles |> Data.fold
      in
      { partition with settled = F.Plan.settled fold })
    |> Result.all
  in
  (* A missing before-row is always an error, even when a malformed selection
     would otherwise make it invisible to a stream-specific fold. *)
  let%bind () =
    List.fold_result data.bindings ~init:() ~f:(fun () binding ->
      if Option.is_some (L.find before ~ordinal:binding.Data.Binding.ordinal)
      then Ok ()
      else invalid "policy binding missing before actual retirement fold")
  in
  let retained =
    List.filter data.bindings ~f:(fun binding ->
      Option.is_some (L.find after ~ordinal:binding.Data.Binding.ordinal))
  in
  let base =
    { data with
      streams
    ; bindings = retained
    ; ledger_revision = L.revision after
    ; marks = after_marks
    }
  in
  let%bind candidate_data =
    match selection with
    | None -> Ok base
    | Some selected ->
      let%bind () =
        Canonical.validate_fact ~before ~after ~fact:selected.Selection.fact
      in
      let%bind cause =
        Result.of_option
          (Data.find_cause data selected.cause)
          ~error:(P.Error.invalid_request "policy selected cause absent")
      in
      let%bind ordinal = Data.ordinal selected.fact in
      let%bind () =
        if
          (not
             (List.exists retained ~f:(fun row ->
                Int64.equal row.Data.Binding.ordinal ordinal)))
          && List.equal
               Target.equal
               (Cause.targets cause)
               (List.map selected.choices ~f:Choice.target)
        then Ok ()
        else invalid "policy selected ordinal/targets already retained or changed"
      in
      let%bind streams =
        List.fold_result selected.choices ~init:base.streams ~f:(fun streams choice ->
          if not (Data.overlay choice)
          then Ok streams
          else (
            let stream = Stream.of_cause cause choice.Data.Choice.target in
            let old =
              List.find streams ~f:(fun row ->
                String.equal row.Data.Partition.stream.id stream.id)
            in
            let%bind () =
              match old with
              | Some old when not (Stream.equal old.stream stream) ->
                invalid "policy stream hash collision"
              | None | Some _ -> Ok ()
            in
            let initial : Data.Partition.t =
              Option.value
                old
                ~default:
                  { stream; settled = U.empty; admitted_attempts = 0L; head = None }
            in
            let%bind admitted_attempts = Data.increment initial.admitted_attempts in
            let%map settled =
              match Canonical.Fact.tracking selected.fact with
              | L.Tracked -> Ok initial.settled
              | Untracked _ -> U.add_untracked initial.settled |> Data.counter
            in
            let updated =
              { initial with
                admitted_attempts
              ; settled
              ; head =
                  Some
                    { Data.Head.cause = selected.cause
                    ; work = selected.work
                    ; canonical = selected.fact
                    }
              }
            in
            match old with
            | None -> streams @ [ updated ]
            | Some _ ->
              List.map streams ~f:(fun row ->
                if String.equal row.Data.Partition.stream.id stream.id
                then updated
                else row)))
      in
      let%map bindings =
        match Canonical.Fact.tracking selected.fact with
        | L.Tracked ->
          Ok
            (retained
             @ [ { Data.Binding.ordinal
                 ; cause = selected.cause
                 ; work = selected.work
                 ; canonical = selected.fact
                 ; choices = selected.choices
                 }
               ])
        | Untracked _ -> Ok retained
      in
      { base with streams; bindings }
  in
  let%bind candidate = with_value accounting candidate_data in
  let%bind candidate = retain_children candidate ~cause:None in
  make_update candidate (Update.Reason.Reconcile selection)
;;

(* Lifecycle replacement is a factual endpoint, never an admission. It may
   settle/retire actual retained rows or rotate the generation, but cannot
   introduce another Attempt/Host-turn or select a new causal route. *)
let reconcile_factual accounting ~before ~after =
  let open Result.Let_syntax in
  let%bind before_marks = marks before in
  let%bind after_marks = marks after in
  if not (F.Marks.equal before_marks after_marks)
  then invalid "policy lifecycle replacement cannot introduce canonical admissions"
  else (
    let%map update = reconcile accounting ~before ~after ~selection:None in
    Update.candidate update)
;;

let prune_work accounting ~context =
  let open Result.Let_syntax in
  let data = value accounting in
  let keep =
    List.fold data.bindings ~init:[] ~f:(fun retained row ->
      (row.Data.Binding.cause, row.work) :: retained)
  in
  let keep =
    List.fold data.streams ~init:keep ~f:(fun retained row ->
      match row.Data.Partition.head with
      | None -> retained
      | Some head -> (head.cause, head.work) :: retained)
  in
  let%bind required =
    List.map keep ~f:(fun (cause_key, work) ->
      let%bind.Result cause =
        Result.of_option
          (Data.find_cause data cause_key)
          ~error:(P.Error.invalid_request "policy retained cause absent")
      in
      Work.member work ~generation:(Cause.callee_source cause).generation)
    |> Result.all
  in
  let rec ancestors required frontier =
    match frontier with
    | [] -> required
    | member :: rest ->
      (match
         Option.bind (Data.find_member data member) ~f:(fun row ->
           row.Data.Membership.parent)
       with
       | None -> ancestors required rest
       | Some parent ->
         if List.mem required parent ~equal:Member.equal
         then ancestors required rest
         else ancestors (parent :: required) (parent :: rest))
  in
  let required = ancestors required required in
  let%bind memberships =
    List.map data.memberships ~f:(fun row ->
      let%bind.Result cause =
        Result.of_option
          (Data.find_cause data row.Data.Membership.cause)
          ~error:(P.Error.invalid_request "policy prune cause absent")
      in
      let%map.Result current =
        Data.Source_work.capture ~memberships:data.memberships context cause row.member
      in
      Option.some_if
        (Option.is_some current || List.mem required row.member ~equal:Member.equal)
        row)
    |> Result.all
    |> Result.map ~f:List.filter_opt
  in
  (* Preserve the ancestor chain of every still-present member as well. *)
  let all_required =
    ancestors
      (List.map memberships ~f:(fun row -> row.Data.Membership.member))
      (List.map memberships ~f:(fun row -> row.Data.Membership.member))
  in
  let memberships =
    List.filter data.memberships ~f:(fun row ->
      List.mem all_required row.Data.Membership.member ~equal:Member.equal)
  in
  let causes =
    List.filter data.causes ~f:(fun cause ->
      List.exists memberships ~f:(fun row ->
        Cause.Key.equal row.Data.Membership.cause (Cause.key cause)))
  in
  let candidate_data = { data with memberships; causes } in
  (* Whole-carrier encoding is the protected unknown-field decision. A rejected
     optional prune leaves the original untouched; callers may retain it. *)
  let%bind candidate = with_value accounting candidate_data in
  make_update candidate Update.Reason.Prune_work
;;
