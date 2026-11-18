open! Core
module P = Agent_protocol
module D = Agent_store.Delegation_store
module O = Agent_store.Delegation_goal_origin
module Cause = Policy_cause_data
module Goal_target = Cause.Goal_target
module PC = Policy_canonical_usage
module Frame = Policy_forwarding_frame.Frame
module Seed = Policy_forwarding_frame.Seed
module Witness = Policy_forwarding_route.Witness

let invalid message = Error (P.Error.create Conflict ~message ~retryable:false ())

module Evidence = struct
  type t =
    | Caller_local
    | Inherited_scope of O.Scope.t
    | Forwarded of Cause.Forwarding.t
end

module Target = struct
  type t =
    { target : Goal_target.t
    ; coverage : Cause.Coverage.t
    ; evidence : Evidence.t
    ; path : D.Reference.t list
    }

  let target t = t.target
  let coverage t = t.coverage
  let evidence t = t.evidence
end

type t =
  { references : D.Reference.t list
  ; suffix : D.record list
  ; targets : Target.t list
  }

let references t = t.references
let targets t = t.targets

let frame_path frames =
  List.concat_map frames ~f:(fun frame -> (Frame.seed frame).Seed.path)
;;

let validate_chain ~caller ~max_depth references =
  let open Result.Let_syntax in
  let%bind () =
    if List.is_empty references || List.length references > max_depth
    then invalid "causal route differs from its captured depth"
    else Ok ()
  in
  List.fold_result
    references
    ~init:(P.Session_ref.session_id caller, [ P.Session_ref.session_id caller ])
    ~f:(fun (child, seen) reference ->
      let%bind () =
        D.validate_reference reference
        |> Result.map_error ~f:Agent_store.Store_error.to_protocol_error
      in
      let parent = reference.D.Reference.key.parent_session_id in
      if
        P.Id.Session.equal reference.child_session_id child
        && not (List.mem seen parent ~equal:P.Id.Session.equal)
      then Ok (parent, parent :: seen)
      else invalid "causal route has a discontinuous or repeated session")
  |> Result.map ~f:ignore
;;

let suffix_records ~callee_delegation ~callee_ancestry =
  match callee_delegation, callee_ancestry with
  | None, None -> Ok []
  | Some reference, Some (first :: _ as records)
    when D.Reference.equal reference (D.reference first) -> Ok records
  | None, Some _ | Some _, (None | Some []) | Some _, Some (_ :: _) ->
    invalid "causal route suffix differs from the actual callee Spec"
;;

let local target (seed : Seed.t) = Policy_forwarding_frame.validate_local target seed

let target_path cause suffix choice =
  let open Result.Let_syntax in
  let target = PC.Choice.target choice in
  let suffix = List.map suffix ~f:D.reference in
  let%map evidence, prefix =
    match Cause.forwarding cause with
    | Some forwarded
      when List.mem (Cause.Forwarding.targets forwarded) target ~equal:Goal_target.equal
      ->
      let%bind witness = Cause.Forwarding.for_target forwarded ~target in
      let frames = Witness.frames witness in
      let%bind first =
        List.hd frames
        |> Result.of_option
             ~error:(P.Error.invalid_request "causal introduction is absent")
      in
      let%map () = local target (Frame.seed first) in
      Evidence.Forwarded forwarded, frame_path frames @ Cause.path cause
    | None | Some _ ->
      let%map () = local target (Cause.seed cause) in
      let evidence =
        match Goal_target.view target with
        | Local _ -> Evidence.Caller_local
        | Inherited scope -> Inherited_scope scope
      in
      evidence, Cause.path cause
  in
  { Target.target
  ; coverage = PC.Choice.coverage choice
  ; evidence
  ; path = prefix @ suffix
  }
;;

let capture cause ~selection ~callee_delegation ~callee_ancestry =
  let open Result.Let_syntax in
  let choices = PC.Selection.choices selection in
  let%bind () =
    if
      List.equal
        Goal_target.equal
        (Cause.targets cause)
        (List.map choices ~f:PC.Choice.target)
    then Ok ()
    else invalid "causal route selection does not cover its complete Cause"
  in
  let%bind suffix = suffix_records ~callee_delegation ~callee_ancestry in
  let frames =
    Option.value_map (Cause.forwarding cause) ~default:[] ~f:Cause.Forwarding.frames
  in
  let caller =
    match frames with
    | [] -> Cause.caller cause
    | frame :: _ -> (Frame.seed frame).caller
  in
  let references =
    frame_path frames @ Cause.path cause @ List.map suffix ~f:D.reference
  in
  let%bind () =
    validate_chain ~caller ~max_depth:(Cause.max_inherited_scopes cause) references
  in
  let%map targets = List.map choices ~f:(target_path cause suffix) |> Result.all in
  { references; suffix; targets }
;;

let resolved records reference =
  List.find records ~f:(fun record -> D.Reference.equal (D.reference record) reference)
  |> Result.of_option ~error:(P.Error.invalid_request "causal route record is absent")
;;

let inherited_records (target : Target.t) records scope =
  let open Result.Let_syntax in
  let%bind path = List.map target.path ~f:(resolved records) |> Result.all in
  let%bind branch =
    match
      List.filter
        path
        ~f:(Goal_descendant_root_route.anchor_matches (O.Scope.branch scope))
    with
    | [ branch ] -> Ok branch
    | [] -> invalid "causal inherited route has no exact first-child Anchor"
    | _ :: _ :: _ -> invalid "causal inherited route repeats its first-child Anchor"
  in
  let%bind () =
    if Int.equal branch.D.key.parent_generation (O.Scope.source scope).generation
    then Ok ()
    else invalid "causal inherited branch source generation differs"
  in
  let rec lower = function
    | [] -> invalid "causal inherited branch is outside its introducing path"
    | record :: rest ->
      let%bind () =
        match record.D.admission.goal_origin with
        | Some origin when List.mem (O.scopes origin) scope ~equal:O.Scope.equal -> Ok ()
        | None | Some _ -> invalid "causal inherited Scope changed below its branch"
      in
      if D.Reference.equal (D.reference record) (D.reference branch)
      then Ok ()
      else lower rest
  in
  lower path
;;

let validate_records t ~records =
  let open Result.Let_syntax in
  let%bind () =
    if
      List.equal D.Reference.equal t.references (List.map records ~f:D.reference)
      && List.for_all records ~f:(fun record ->
        D.equal_stage record.D.stage Linked
        && Option.is_none record.revocation
        && Option.is_none record.artifact_collection)
    then Ok ()
    else invalid "causal route records changed or are not linked and live"
  in
  let%bind () =
    List.fold_result t.suffix ~init:() ~f:(fun () expected ->
      let%bind actual = resolved records (D.reference expected) in
      if D.equal_record expected actual
      then Ok ()
      else invalid "causal actual callee ancestry changed")
  in
  List.fold_result t.targets ~init:() ~f:(fun () target ->
    match Goal_target.view (Target.target target) with
    | Local _ -> Ok ()
    | Inherited scope -> inherited_records target records scope)
;;

let owner_delegation t ~session ~delegation =
  let reference =
    List.find t.references ~f:(fun reference ->
      P.Id.Session.equal
        reference.D.Reference.child_session_id
        (P.Session_ref.session_id session))
  in
  match reference, delegation with
  | None, None -> Ok ()
  | Some expected, Some actual when D.Reference.equal expected actual -> Ok ()
  | None, Some _ | Some _, None | Some _, Some _ ->
    invalid "causal root Spec differs from the actual route vertex"
;;

let select t ~records ~session ~generation ~delegation ~catalog ~descendants ~children =
  let open Result.Let_syntax in
  let%bind () = validate_records t ~records in
  let selected =
    List.filter t.targets ~f:(fun target ->
      P.Session_ref.equal session (Goal_target.owner (Target.target target)))
  in
  let%bind () =
    if List.is_empty selected then Ok () else owner_delegation t ~session ~delegation
  in
  List.fold_result selected ~init:[] ~f:(fun selected target ->
    let value = Target.target target in
    let%bind goal =
      Option.bind catalog ~f:(fun catalog ->
        Goal_catalog.find catalog (Goal_target.goal_id value))
      |> Result.of_option ~error:(P.Error.invalid_request "causal root Goal is absent")
    in
    let%bind accounting =
      Result.of_option
        descendants
        ~error:(P.Error.invalid_request "causal root accounting is absent")
    in
    let%bind () =
      if
        P.Session_ref.equal (P.Goal.session goal) session
        && P.Session_ref.equal (Goal_descendants.session accounting) session
        && P.Run_source.equal (P.Goal.source goal) (Goal_target.source value)
        && Int.equal generation (Goal_target.source value).generation
        && List.mem
             (Goal_descendants.roots accounting)
             (Goal_target.goal_id value)
             ~equal:P.Id.Goal.equal
      then Ok ()
      else invalid "causal target differs from its actual root Goal/source"
    in
    let%map () =
      match Goal_target.view value with
      | Local _ -> Ok ()
      | Inherited scope ->
        let%bind children =
          Result.of_option
            children
            ~error:(P.Error.invalid_request "causal root child frontier is absent")
        in
        let%map (_ : Goal_descendant_root_route.t list) =
          Goal_descendant_root_route.select
            ~session
            ~scopes:[ scope ]
            ~records
            ~accounting
            ~children
        in
        ()
    in
    selected @ [ target ])
;;
