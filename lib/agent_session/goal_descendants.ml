open! Core
module P = Agent_protocol
module D = Document_schema
module X = Persistence_codec
module Carrier = D.Extension_carrier
module Data = Goal_descendants_data
module Profile = Data.Profile
module Branch = Goal_descendant_branch
module Canonical = Goal_descendants_canonical
module O = Agent_store.Delegation_goal_origin
module R = Agent_store.Delegation_store
module C = Child_contributions
module S = Child_usage_snapshot
module Pending = Goal_descendant_pending
module Included = Goal_descendant_included
module Occurrence = Goal_descendant_occurrence
module L = Inference_ledger
module Marks = Canonical_usage_fold.Marks

let invalid = Data.invalid
let domain = Data.domain
let limits = Data.limits

let codec =
  X.codec_exn
    ~version:1
    ~limits
    ~kind:"ochat.goal_descendants"
    ~shape:Data.shape
    ~decode:Data.of_json
    ~encode:(fun value ->
      let%map.Result () = Data.validate value in
      Data.to_json value)
;;

type t = Data.t Carrier.t

let profile t = (Carrier.value t).Data.profile
let session t = (Carrier.value t).Data.session
let origin t = (Carrier.value t).Data.origin

let local_included t =
  List.filter_map (Carrier.value t).Data.heads ~f:(fun head -> head.Data.included)
;;

let roots t = List.map (Carrier.value t).Data.roots ~f:(fun root -> root.Data.goal_id)

let branches t ~goal_id =
  Option.value_map
    (Data.find_root (Carrier.value t) goal_id)
    ~default:[]
    ~f:(fun root -> root.Data.branches)
;;

let encoded t = D.Domain_codec.encode codec t |> domain

let reserved_growth t =
  let%bind.Result current = encoded t in
  Goal_descendants_capacity.reserved_growth (Carrier.value t) ~current
;;

let required_depth t =
  let open Result.Let_syntax in
  let%bind current = encoded t in
  let%map capacity = Goal_descendants_capacity.capture (Carrier.value t) ~current in
  Goal_descendants_capacity.required_depth capacity
;;

let validate_capacity t current =
  let open Result.Let_syntax in
  let%bind growth =
    Goal_descendants_capacity.reserved_growth (Carrier.value t) ~current
  in
  let%bind remaining = Capacity_growth.remaining_limits growth limits in
  let%bind () = D.Document.validate current ~limits:remaining |> domain in
  let%bind measured = Capacity_growth.measure (D.Document.json current) ~limits in
  let size = Capacity_growth.encoded_bytes measured in
  let maximum = Profile.max_encoded_bytes (profile t) in
  if size <= maximum && Capacity_growth.encoded_bytes growth <= maximum - size
  then Ok ()
  else
    Error
      (P.Error.create
         Resource_limit
         ~message:"descendant accounting capacity exhausted"
         ~retryable:false
         ())
;;

let to_document t =
  let%bind.Result document = encoded t in
  let%map.Result () = validate_capacity t document in
  document
;;

let of_document document =
  let open Result.Let_syntax in
  let%bind t = D.Domain_codec.decode codec document |> domain in
  let%bind canonical = encoded t in
  let%bind () = validate_capacity t canonical in
  let%map () = validate_capacity t document in
  t
;;

let admit t =
  let%map.Result _ = to_document t in
  t
;;

let with_value t value = admit (Carrier.with_value t value)

let adopt previous ~incoming =
  let open Result.Let_syntax in
  if not (Data.equal (Carrier.value previous) (Carrier.value incoming))
  then invalid "descendant known movement requires a closed Update"
  else (
    let%bind candidate = D.Domain_codec.adopt codec ~previous ~incoming |> domain in
    admit candidate)
;;

let protocol_marks ledger =
  Marks.of_ledger ledger
  |> Result.map_error ~f:(fun error ->
    P.Error.invalid_request
      (Sexp.to_string_hum (Canonical_usage_fold.Error.sexp_of_t error)))
;;

let ledger_error result =
  Result.map_error result ~f:(fun error ->
    P.Error.invalid_request (Sexp.to_string_hum (L.Error.sexp_of_t error)))
;;

let create_new ~profile ~session ~origin ~ledger =
  let open Result.Let_syntax in
  let%bind () =
    L.validate
      ledger
      ~limits:L.Limits.default
      ~session_id:(P.Session_ref.session_id session)
      ~generation:(L.generation ledger)
    |> ledger_error
  in
  let%bind marks = protocol_marks ledger in
  let coverage = P.Inference_query.Summary.coverage (L.summary ledger) in
  if
    (not (Int64.equal marks.attempt 0L && Int64.equal marks.turn 0L))
    || coverage.before_tracking_unknown
  then invalid "descendant birth requires exact known zero canonical history"
  else (
    let value : Data.t =
      { profile
      ; session
      ; origin = Some origin
      ; heads =
          List.map (O.scopes origin) ~f:(fun scope -> Data.{ scope; included = None })
      ; roots = []
      }
    in
    admit (Carrier.of_authored_value value))
;;

module Update = struct
  type accounting = t

  module Children = struct
    type t =
      | Preserve
      | Replace of C.t
  end

  module Reason = struct
    type t =
      | Register_new_root of
          { host_profile : Profile.t
          ; session : P.Session_ref.t
          ; goal_id : P.Id.Goal.t
          }
      | Register_child of
          { reference : R.Reference.t
          ; origin : O.t
          }
      | Register_branch of
          { scope : O.Scope.t
          ; reference : R.Reference.t
          ; origin : O.t
          }
      | Reserve of
          { scope : O.Scope.t
          ; occurrence : Occurrence.t
          ; local_count : int64
          ; ceiling : int64 option
          }
      | Record_included of
          { fact : Canonical.Fact.t
          ; receipts : Included.t list
          }
      | Import of R.Reference.t
      | Close_before_writer of Pending.t
  end

  type t =
    { accounting : accounting
    ; children : Children.t
    ; reason : Reason.t
    }

  let accounting t = t.accounting
  let children t = t.children
  let reason t = t.reason

  let update_limits =
    X.limits ~max_bytes:(64 * 1024 * 1024)
    |> domain
    |> Result.map_error ~f:(fun error -> error.P.Error.message)
    |> Result.ok_or_failwith
  ;;

  let reference_of_json raw =
    R.reference_of_jsonaf raw
    |> Result.map_error ~f:Agent_store.Store_error.to_protocol_error
  ;;

  let origin_of_json raw = O.of_json ~max_scopes:Int.max_value ~limits raw

  let receipt_to_json included =
    let scope = Pending.scope (Included.pending included) in
    `Object
      [ "scope_id", `String (O.Scope.root_id scope)
      ; "included", Included.to_json included
      ]
  ;;

  let receipt_of_json raw =
    let open Result.Let_syntax in
    let%bind fields = X.object_ raw in
    let%bind scope_id = X.required fields "scope_id" P.Json_codec.string in
    let%bind included = X.required fields "included" Included.of_json in
    if String.equal scope_id (O.Scope.root_id (Pending.scope (Included.pending included)))
    then Ok included
    else invalid "Update Included wrapper differs"
  ;;

  let reason_to_json = function
    | Reason.Register_new_root { host_profile; session; goal_id } ->
      `Object
        [ "kind", `String "register_new_root"
        ; "host_profile", Profile.to_json host_profile
        ; "session", P.Session_ref.to_json session
        ; "goal_id", P.Id.Goal.to_json goal_id
        ]
    | Register_child { reference; origin } ->
      `Object
        [ "kind", `String "register_child"
        ; "reference", R.reference_to_jsonaf reference
        ; "origin", O.to_json origin
        ]
    | Register_branch { scope; reference; origin } ->
      `Object
        [ "kind", `String "register_branch"
        ; "scope", O.Scope.to_json scope
        ; "reference", R.reference_to_jsonaf reference
        ; "origin", O.to_json origin
        ]
    | Reserve { scope; occurrence; local_count; ceiling } ->
      `Object
        [ "kind", `String "reserve"
        ; "scope", O.Scope.to_json scope
        ; "occurrence", Occurrence.to_json occurrence
        ; "local_count", X.int64_json local_count
        ; "ceiling", X.option_json X.int64_json ceiling
        ]
    | Record_included { fact; receipts } ->
      `Object
        [ "kind", `String "record_included"
        ; "fact", Canonical.Fact.to_json fact
        ; "receipts", `Array (List.map receipts ~f:receipt_to_json)
        ]
    | Import reference ->
      `Object [ "kind", `String "import"; "reference", R.reference_to_jsonaf reference ]
    | Close_before_writer pending ->
      `Object
        [ "kind", `String "close_before_writer"; "pending", Pending.to_json pending ]
  ;;

  let reason_of_json raw =
    let open Result.Let_syntax in
    let%bind fields = X.object_ raw in
    let%bind kind = X.required fields "kind" P.Json_codec.string in
    match kind with
    | "register_new_root" ->
      let%bind host_profile = X.required fields "host_profile" Profile.of_json in
      let%bind session = X.required fields "session" P.Session_ref.of_json in
      let%map goal_id = X.required fields "goal_id" P.Id.Goal.of_json in
      Reason.Register_new_root { host_profile; session; goal_id }
    | "register_child" ->
      let%bind reference = X.required fields "reference" reference_of_json in
      let%map origin = X.required fields "origin" origin_of_json in
      Reason.Register_child { reference; origin }
    | "register_branch" ->
      let%bind scope = X.required fields "scope" O.Scope.of_json in
      let%bind reference = X.required fields "reference" reference_of_json in
      let%map origin = X.required fields "origin" origin_of_json in
      Reason.Register_branch { scope; reference; origin }
    | "reserve" ->
      let%bind scope = X.required fields "scope" O.Scope.of_json in
      let%bind occurrence = X.required fields "occurrence" Occurrence.of_json in
      let%bind local_count = X.required fields "local_count" X.nonnegative_int64 in
      let%map ceiling = X.required fields "ceiling" (X.nullable X.nonnegative_int64) in
      Reason.Reserve { scope; occurrence; local_count; ceiling }
    | "record_included" ->
      let%bind fact = X.required fields "fact" Canonical.Fact.of_json in
      let%map receipts = X.required fields "receipts" (X.list receipt_of_json) in
      Reason.Record_included { fact; receipts }
    | "import" ->
      let%map reference = X.required fields "reference" reference_of_json in
      Reason.Import reference
    | "close_before_writer" ->
      let%map pending = X.required fields "pending" Pending.of_json in
      Reason.Close_before_writer pending
    | _ -> invalid "unknown descendant Update reason"
  ;;

  let to_json t =
    let open Result.Let_syntax in
    let%bind accounting = to_document t.accounting in
    let%bind children =
      match t.children with
      | Children.Preserve -> Ok (`Object [ "kind", `String "preserve" ])
      | Replace children ->
        let%map value = C.to_jsonaf children in
        `Object [ "kind", `String "replace"; "value", value ]
    in
    let raw =
      `Object
        [ "accounting", D.Document.json accounting
        ; "children", children
        ; "reason", reason_to_json t.reason
        ]
    in
    let%map () = D.Json.validate raw ~limits:update_limits |> domain in
    raw
  ;;

  let of_json raw =
    let open Result.Let_syntax in
    let%bind () = D.Json.validate raw ~limits:update_limits |> domain in
    let%bind fields = X.object_ raw in
    let%bind accounting =
      X.required fields "accounting" (fun raw ->
        let%bind document = D.Document.inspect raw ~limits |> domain in
        of_document document)
    in
    let%bind children =
      X.required fields "children" (fun raw ->
        let%bind fields = X.object_ raw in
        let%bind kind = X.required fields "kind" P.Json_codec.string in
        match kind with
        | "preserve" -> Ok Children.Preserve
        | "replace" ->
          let%map children = X.required fields "value" C.of_jsonaf in
          Children.Replace children
        | _ -> invalid "unknown descendant Update child action")
    in
    let%map reason = X.required fields "reason" reason_of_json in
    { accounting; children; reason }
  ;;

  let object_shape fields = X.shape_exn (("kind", D.Shape.value) :: fields)

  let receipt_shape =
    X.shape_exn [ "scope_id", D.Shape.value; "included", Included.shape ]
  ;;

  let reason_shape =
    X.tagged_shape_exn
      ~discriminator:"kind"
      [ ( "register_new_root"
        , object_shape
            [ "host_profile", Profile.shape
            ; "session", X.fields_shape [ "server_id"; "session_id" ]
            ; "goal_id", D.Shape.value
            ] )
      ; ( "register_child"
        , object_shape [ "reference", R.reference_shape; "origin", O.shape ] )
      ; ( "register_branch"
        , object_shape
            [ "scope", O.Scope.shape; "reference", R.reference_shape; "origin", O.shape ]
        )
      ; ( "reserve"
        , object_shape
            [ "scope", O.Scope.shape
            ; "occurrence", Occurrence.shape
            ; "local_count", D.Shape.value
            ; "ceiling", D.Shape.value
            ] )
      ; ( "record_included"
        , object_shape
            [ "fact", Canonical.Fact.shape
            ; "receipts", X.array_shape_exn ~identity_field:"scope_id" receipt_shape
            ] )
      ; "import", object_shape [ "reference", R.reference_shape ]
      ; "close_before_writer", object_shape [ "pending", Pending.shape ]
      ]
  ;;

  let children_shape =
    X.tagged_shape_exn
      ~discriminator:"kind"
      [ "preserve", object_shape []; "replace", object_shape [ "value", D.Shape.value ] ]
  ;;

  let shape =
    X.shape_exn
      [ "accounting", D.Shape.value; "children", children_shape; "reason", reason_shape ]
  ;;
end

let make_update accounting children reason =
  let%bind.Result accounting = admit accounting in
  let update : Update.t = { accounting; children; reason } in
  let%map.Result _ = Update.to_json update in
  update
;;

let same_children left right =
  let open Result.Let_syntax in
  let%bind left = C.to_jsonaf left in
  let%map right = C.to_jsonaf right in
  D.Json.equal left right
;;

let child_change previous incoming =
  match previous with
  | None -> Ok (Update.Children.Replace incoming)
  | Some previous ->
    let%map.Result same = same_children previous incoming in
    if same then Update.Children.Preserve else Replace incoming
;;

let register_new_root
      previous
      ~host_profile
      ~session
      ~children
      ~goal
      ~account
      ~before_ledger
  =
  let open Result.Let_syntax in
  let%bind bootstrap = Goal_accounting_document.create goal ~ledger:before_ledger in
  let%bind () =
    if
      P.Session_ref.equal session (P.Goal.session goal)
      && Goal_accounting.Stored.equal
           (Goal_accounting_document.stored bootstrap)
           (Goal_accounting_document.stored account)
    then Ok ()
    else invalid "new descendant root differs from actual zero account bootstrap"
  in
  let%bind actual_children =
    match children with
    | Some children -> Ok children
    | None ->
      C.create
        ~inventory:Historical_children_unknown
        ~max_children:(Profile.max_branches host_profile)
        ~max_encoded_bytes:(Profile.child_max_encoded_bytes host_profile)
  in
  let%bind accounting =
    match previous with
    | Some previous ->
      if P.Session_ref.equal session (Carrier.value previous).Data.session
      then Ok previous
      else invalid "new descendant root session differs"
    | None ->
      let%bind profile = Profile.restrict_to_children host_profile actual_children in
      admit
        (Carrier.of_authored_value
           Data.{ profile; session; origin = None; heads = []; roots = [] })
  in
  let%bind () =
    if Option.is_none (Data.find_root (Carrier.value accounting) (P.Goal.id goal))
    then Ok ()
    else invalid "descendant root already exists; registration is not reset"
  in
  let value =
    Data.set_root
      (Carrier.value accounting)
      Data.{ goal_id = P.Goal.id goal; branches = [] }
  in
  let%bind accounting = with_value accounting value in
  let%bind children = child_change children actual_children in
  make_update
    accounting
    children
    (Register_new_root { host_profile; session; goal_id = P.Goal.id goal })
;;

let ensure_child t children ~reference ~origin =
  let%bind.Result () =
    if
      P.Id.Session.equal
        reference.R.Reference.key.parent_session_id
        (P.Session_ref.session_id (session t))
      && List.length (O.scopes origin) <= Profile.max_scopes (profile t)
    then Ok ()
    else invalid "child registration differs from captured descendant owner/profile"
  in
  C.register_with_origin children ~reference ~origin
;;

let ensure_existing_local_branches t reference origin =
  List.fold_result (O.scopes origin) ~init:() ~f:(fun () scope ->
    if not (P.Session_ref.equal (O.Scope.owner scope) (session t))
    then Ok ()
    else (
      match Data.find_root (Carrier.value t) (O.Scope.goal_id scope) with
      | None -> invalid "child origin names an absent local descendant root"
      | Some root ->
        (match Data.find_branch root scope with
         | Some branch when R.Reference.equal reference (Branch.reference branch) -> Ok ()
         | Some _ | None ->
           invalid "local child origin requires paired branch registration")))
;;

let register_child t ~children ~reference ~origin =
  let open Result.Let_syntax in
  let%bind () = ensure_existing_local_branches t reference origin in
  let%bind incoming = ensure_child t children ~reference ~origin in
  let%bind changed = child_change (Some children) incoming in
  make_update t changed (Register_child { reference; origin })
;;

let register_branch t ~children ~scope ~reference ~origin =
  let open Result.Let_syntax in
  let%bind () =
    if
      P.Session_ref.equal (O.Scope.owner scope) (session t)
      && List.exists (O.scopes origin) ~f:(O.Scope.equal scope)
    then Ok ()
    else invalid "branch scope is absent from its exact frozen child origin"
  in
  let%bind incoming = ensure_child t children ~reference ~origin in
  let%bind value =
    List.fold_result (O.scopes origin) ~init:(Carrier.value t) ~f:(fun value scope ->
      if not (P.Session_ref.equal (O.Scope.owner scope) value.Data.session)
      then Ok value
      else (
        let%bind root =
          Result.of_option
            (Data.find_root value (O.Scope.goal_id scope))
            ~error:(P.Error.invalid_request "branch local root is absent")
        in
        match
          List.find root.branches ~f:(fun branch ->
            P.Id.Session.equal
              (Branch.reference branch).child_session_id
              reference.child_session_id)
        with
        | Some previous ->
          if
            O.Scope.equal scope (Branch.scope previous)
            && R.Reference.equal reference (Branch.reference previous)
          then Ok value
          else invalid "branch frozen attribution conflicts"
        | None ->
          let%bind branch = Branch.create ~scope ~reference in
          Data.set_branch value branch))
  in
  let%bind accounting = with_value t value in
  let%bind changed = child_change (Some children) incoming in
  make_update accounting changed (Register_branch { scope; reference; origin })
;;

let add left right =
  let open Result.Let_syntax in
  let counter result =
    Result.map_error result ~f:(fun _ ->
      P.Error.invalid_request "descendant count exhausted")
  in
  let%bind left = Goal_accounting_counter.of_int64 left |> counter in
  let%bind right = Goal_accounting_counter.of_int64 right |> counter in
  let%map total = Goal_accounting_counter.add left right |> counter in
  Goal_accounting_counter.to_int64 total
;;

let root t goal_id =
  Result.of_option
    (Data.find_root (Carrier.value t) goal_id)
    ~error:(P.Error.invalid_request "descendant root is absent")
;;

let root_descendants t children goal_id =
  let%bind.Result _ = root t goal_id in
  C.aggregate_root children ~owner:(session t) ~goal_id
;;

let accounting_coverage t ~children ~goal_id =
  let%map.Result descendants = root_descendants t children goal_id in
  S.Descendants.coverage descendants
;;

let dimension_count (marks : Marks.t) = function
  | Goal_admission_policy.Attempt -> marks.attempt
  | Host_turn -> marks.turn
;;

module Availability = struct
  type t =
    | Available
    | Reserved
    | Exhausted
    | Unknown
  [@@deriving equal, sexp_of]
end

let available t ~children ~goal_id ~local_count ~ceiling ~dimension =
  let open Result.Let_syntax in
  let%bind root = root t goal_id in
  let%bind () =
    if
      Int64.(local_count < 0L)
      || Option.exists ceiling ~f:(fun value -> Int64.(value < 0L))
    then invalid "negative descendant policy count/ceiling"
    else Ok ()
  in
  let%bind descendants = root_descendants t children goal_id in
  let%bind committed =
    add
      local_count
      (dimension_count
         (S.Partition.marks (S.Descendants.partition descendants))
         dimension)
  in
  let%bind pending =
    List.fold_result root.branches ~init:0L ~f:(fun count branch ->
      match Branch.pending branch with
      | Some pending
        when Goal_admission_policy.equal_dimension
               dimension
               (Occurrence.dimension (Pending.occurrence pending)) -> add count 1L
      | Some _ | None -> Ok count)
  in
  let%map occupied = add committed pending in
  match ceiling with
  | None -> Availability.Available
  | Some ceiling ->
    if Int64.(committed >= ceiling)
    then Exhausted
    else if Int64.(occupied >= ceiling)
    then Reserved
    else (
      let coverage = S.Descendants.coverage descendants in
      if
        S.Coverage.historical_children_unknown coverage
        || Int64.(S.Coverage.unobserved_children coverage > 0L)
      then Unknown
      else Available)
;;

module Reserve_result = struct
  type t =
    | Reserved of Update.t * Pending.t
    | Already_pending of Pending.t
    | Blocked of Availability.t
end

let branch_for_scope t scope =
  let%bind.Result root = root t (O.Scope.goal_id scope) in
  Result.of_option
    (Data.find_branch root scope)
    ~error:(P.Error.invalid_request "exact descendant branch is absent")
;;

let reserve t ~children ~scope ~occurrence ~local_count ~ceiling =
  let open Result.Let_syntax in
  let%bind branch = branch_for_scope t scope in
  match Branch.pending branch with
  | Some pending ->
    if Occurrence.equal occurrence (Pending.occurrence pending)
    then Ok (Reserve_result.Already_pending pending)
    else Ok (Reserve_result.Blocked Availability.Reserved)
  | None ->
    let%bind available =
      available
        t
        ~children
        ~goal_id:(O.Scope.goal_id scope)
        ~local_count
        ~ceiling
        ~dimension:(Occurrence.dimension occurrence)
    in
    (match available with
     | Availability.Reserved | Exhausted | Unknown ->
       Ok (Reserve_result.Blocked available)
     | Available ->
       let%bind reserved = Branch.reserve branch ~occurrence in
       (match reserved with
        | Already_pending pending -> Ok (Reserve_result.Already_pending pending)
        | Busy _ -> Ok (Reserve_result.Blocked Availability.Reserved)
        | Reserved (branch, pending) ->
          let%bind value = Data.set_branch (Carrier.value t) branch in
          let%bind accounting = with_value t value in
          (* A newly accepted pending must fit either known terminal outcome,
            including actual preserved fields at the old latest-outcome slot.
            This is one finite reservation edge, not a full scan per branch. *)
          let%bind closed = Branch.close_before_writer branch ~pending in
          let%bind closed_value = Data.set_branch value closed in
          let%bind _ = with_value accounting closed_value in
          let%bind confirmed =
            Branch.confirm branch ~included:(Included.create pending)
          in
          let%bind confirmed_value = Data.set_branch value confirmed in
          let%bind _ = with_value accounting confirmed_value in
          let%map update =
            make_update
              accounting
              Preserve
              (Reserve { scope; occurrence; local_count; ceiling })
          in
          Reserve_result.Reserved (update, pending)))
;;

let record_fact t ~children:_ ~before ~after ~fact ~receipts =
  let open Result.Let_syntax in
  let occurrence = Canonical.Fact.occurrence fact in
  let%bind () =
    if
      P.Session_ref.equal
        (session t)
        (Occurrence.Frame.session (Occurrence.frame occurrence))
    then Ok ()
    else invalid "local descendant canonical occurrence belongs to another session"
  in
  let%bind () = Canonical.validate_fact ~before ~after ~fact in
  let value = Carrier.value t in
  let%bind () =
    if
      List.equal
        O.Scope.equal
        (List.map value.heads ~f:(fun head -> head.Data.scope))
        (List.map receipts ~f:(fun receipt -> Pending.scope (Included.pending receipt)))
      && List.for_all receipts ~f:(fun receipt ->
        Occurrence.equal occurrence (Pending.occurrence (Included.pending receipt)))
    then Ok ()
    else
      invalid "canonical Included receipts differ from exact inherited scope vector/leaf"
  in
  let%bind heads =
    List.map2_exn value.heads receipts ~f:(fun head included ->
      let pending = Included.pending included in
      let%bind () =
        match head.Data.included with
        | None -> Ok ()
        | Some previous ->
          if Int64.(Pending.nonce pending > Pending.nonce (Included.pending previous))
          then Ok ()
          else invalid "new canonical inclusion reused an inherited nonce"
      in
      Data.advance_head head included)
    |> Result.all
  in
  let%bind accounting = with_value t { value with heads } in
  make_update accounting Preserve (Record_included { fact; receipts })
;;

let record_included t ~children ~before ~after ~canonical ~occurrence ~receipts =
  let%bind.Result fact = Canonical.check ~before ~after ~canonical ~occurrence in
  record_fact t ~children ~before ~after ~fact ~receipts
;;

let count_edge before after dimension =
  let increment before after =
    Int64.(before < Int64.max_value) && Int64.equal (Int64.succ before) after
  in
  let untracked before after = Int64.equal before after || increment before after in
  match dimension with
  | Goal_admission_policy.Attempt ->
    increment before.Marks.attempt after.Marks.attempt
    && Int64.equal before.turn after.turn
    && untracked before.untracked_attempts after.untracked_attempts
    && Int64.equal before.untracked_turns after.untracked_turns
  | Host_turn ->
    increment before.turn after.turn
    && Int64.equal before.attempt after.attempt
    && untracked before.untracked_turns after.untracked_turns
    && Int64.equal before.untracked_attempts after.untracked_attempts
;;

let aggregate_marks descendants = S.Partition.marks (S.Descendants.partition descendants)

let scoped_entry snapshot scope =
  Option.bind (S.goal_scopes snapshot) ~f:(fun entries ->
    List.find entries ~f:(fun entry -> O.Scope.equal scope (S.Goal_scope.scope entry)))
;;

let import t ~children ~snapshot =
  let open Result.Let_syntax in
  let%bind incoming, disposition = C.replace children ~snapshot in
  match disposition with
  | C.Duplicate | Stale -> make_update t Preserve (Import (S.reference snapshot))
  | Added | Replaced ->
    let reference = S.reference snapshot in
    let value = Carrier.value t in
    let%bind value =
      List.fold_result value.roots ~init:value ~f:(fun value original_root ->
        let%bind before =
          C.aggregate_root
            children
            ~owner:value.session
            ~goal_id:original_root.Data.goal_id
        in
        let%bind after =
          C.aggregate_root incoming ~owner:value.session ~goal_id:original_root.goal_id
        in
        let before_marks = aggregate_marks before in
        let after_marks = aggregate_marks after in
        let branch =
          List.find original_root.branches ~f:(fun branch ->
            R.Reference.equal reference (Branch.reference branch))
        in
        match branch with
        | None ->
          if Marks.equal before_marks after_marks
          then Ok value
          else invalid "root subtotal changed outside its registered branch"
        | Some branch ->
          let%bind entry =
            Result.of_option
              (scoped_entry snapshot (Branch.scope branch))
              ~error:
                (P.Error.invalid_request "root branch snapshot lacks its exact scope")
          in
          let included = S.Goal_scope.included entry in
          let matches_pending =
            match Branch.pending branch, included with
            | Some pending, Some included ->
              Pending.equal pending (Included.pending included)
            | (Some _ | None), (Some _ | None) -> false
          in
          if Marks.equal before_marks after_marks
          then
            if matches_pending
            then invalid "matching root receipt lacks its exact +1 subtotal"
            else Ok value
          else (
            let%bind included =
              Result.of_option
                included
                ~error:
                  (P.Error.invalid_request
                     "root subtotal advanced without Included receipt")
            in
            let pending = Included.pending included in
            let%bind () =
              if
                matches_pending
                && count_edge
                     before_marks
                     after_marks
                     (Occurrence.dimension (Pending.occurrence pending))
              then Ok ()
              else invalid "root confirmation is not exact pending/+1 dimension"
            in
            let%bind branch = Branch.confirm branch ~included in
            Data.set_branch value branch))
    in
    let%bind heads =
      List.map value.heads ~f:(fun head ->
        let%bind before = C.aggregate_scope children ~scope:head.Data.scope in
        let%bind after = C.aggregate_scope incoming ~scope:head.scope in
        let before_marks = aggregate_marks before in
        let after_marks = aggregate_marks after in
        let included =
          Option.bind (scoped_entry snapshot head.scope) ~f:S.Goal_scope.included
        in
        let newer =
          Option.exists included ~f:(fun included ->
            Option.value_map head.included ~default:true ~f:(fun previous ->
              Int64.(
                Pending.nonce (Included.pending included)
                > Pending.nonce (Included.pending previous))))
        in
        let%bind () =
          if Marks.equal before_marks after_marks
          then
            if newer
            then invalid "new inherited head lacks one exact scoped subtotal increment"
            else Ok ()
          else (
            match included with
            | Some included
              when newer
                   && count_edge
                        before_marks
                        after_marks
                        (Occurrence.dimension
                           (Pending.occurrence (Included.pending included))) -> Ok ()
            | Some _ | None ->
              invalid
                "inherited scoped subtotal advanced without a new exact Included head")
        in
        match included with
        | None -> Ok head
        | Some included -> Data.advance_head head included)
      |> Result.all
    in
    let%bind accounting = with_value t { value with heads } in
    make_update accounting (Replace incoming) (Import reference)
;;

let close_before_writer t ~children:_ ~pending =
  let open Result.Let_syntax in
  let%bind branch = branch_for_scope t (Pending.scope pending) in
  let%bind branch = Branch.close_before_writer branch ~pending in
  let%bind value = Data.set_branch (Carrier.value t) branch in
  let%bind accounting = with_value t value in
  make_update accounting Preserve (Close_before_writer pending)
;;

let validate_context t ~session_id ~ledger ~children ~catalog ~accounts ~delegation =
  let open Result.Let_syntax in
  let value = Carrier.value t in
  let%bind () = Data.validate value in
  let%bind () =
    if
      P.Id.Session.equal session_id (P.Session_ref.session_id value.session)
      && P.Id.Session.equal session_id (L.session_id ledger)
    then Ok ()
    else invalid "descendant component/actual State/ledger session differs"
  in
  let%bind () =
    L.validate
      ledger
      ~limits:L.Limits.default
      ~session_id
      ~generation:(L.generation ledger)
    |> ledger_error
  in
  let%bind () =
    match delegation with
    | None ->
      (match value.origin with
       | Some origin when not (List.is_empty (O.scopes origin)) ->
         invalid "inherited descendant origin lacks actual own delegation"
       | Some _ | None -> Ok ())
    | Some reference ->
      let%bind () =
        R.validate_reference reference
        |> Result.map_error ~f:Agent_store.Store_error.to_protocol_error
      in
      if P.Id.Session.equal session_id reference.child_session_id
      then Ok ()
      else invalid "descendant own delegation targets another session"
  in
  let%bind children =
    Result.of_option
      children
      ~error:
        (P.Error.invalid_request
           "descendant component lacks its actual child contributions")
  in
  let%bind () =
    if
      Profile.max_branches value.profile <= C.max_children children
      && Profile.child_max_encoded_bytes value.profile <= C.max_encoded_bytes children
    then Ok ()
    else invalid "descendant profile exceeds actual captured child profile"
  in
  let%bind own_marks = protocol_marks ledger in
  let%bind own_count = add own_marks.attempt own_marks.turn in
  let%bind () =
    List.fold_result value.heads ~init:() ~f:(fun () head ->
      let%bind descendants = C.aggregate_scope children ~scope:head.Data.scope in
      let marks = aggregate_marks descendants in
      let%bind descendant_count = add marks.attempt marks.turn in
      let%bind count = add own_count descendant_count in
      match head.included with
      | None ->
        if Int64.equal count 0L
        then Ok ()
        else invalid "positive inherited usage lacks its Included head"
      | Some included ->
        let pending = Included.pending included in
        let%bind () =
          if Int64.(count <= Pending.next_prefix pending)
          then Ok ()
          else invalid "known inherited subtree usage exceeds its root branch prefix"
        in
        if
          P.Id.Session.equal
            session_id
            (O.Anchor.child_session_id (O.Scope.branch head.scope))
        then (
          match delegation with
          | Some reference
            when R.Reference.equal reference (Pending.branch_reference pending) -> Ok ()
          | Some _ | None ->
            invalid "first-child Included differs from actual own delegation")
        else Ok ())
  in
  let%bind () =
    List.fold_result value.roots ~init:() ~f:(fun () root ->
      let%bind goal =
        Option.bind catalog ~f:(fun catalog ->
          Goal_catalog.find catalog root.Data.goal_id)
        |> Result.of_option
             ~error:(P.Error.invalid_request "descendant local root Goal is absent")
      in
      let%bind account =
        Option.bind accounts ~f:(fun accounts -> Goal_accounts.find accounts root.goal_id)
        |> Result.of_option
             ~error:(P.Error.invalid_request "descendant local root account is absent")
      in
      let%bind () =
        if
          P.Session_ref.equal value.session (P.Goal.session goal)
          && P.Session_ref.equal
               value.session
               (Goal_accounting.Stored.session (Goal_accounting_document.stored account))
        then Ok ()
        else invalid "descendant local root/account host-qualified session differs"
      in
      let%bind _ = Goal_accounting_document.resolve account ~goal ~ledger in
      let%bind prefixes =
        List.fold_result root.branches ~init:0L ~f:(fun total branch ->
          let%bind origin = C.origin children ~reference:(Branch.reference branch) in
          let%bind () =
            match origin with
            | Some origin
              when List.exists (O.scopes origin) ~f:(O.Scope.equal (Branch.scope branch))
              -> Ok ()
            | Some _ | None ->
              invalid "root branch differs from actual child slot frozen origin"
          in
          add total (Branch.confirmed_prefix branch))
      in
      let%bind descendants =
        C.aggregate_root children ~owner:value.session ~goal_id:root.goal_id
      in
      let marks = aggregate_marks descendants in
      let%bind count = add marks.attempt marks.turn in
      if Int64.equal prefixes count
      then Ok ()
      else
        invalid "root child subtotal and confirmed frontier do not conserve occurrences")
  in
  List.fold_result (C.references children) ~init:() ~f:(fun () reference ->
    let%bind origin = C.origin children ~reference in
    match origin with
    | None -> Ok ()
    | Some origin -> ensure_existing_local_branches t reference origin)
;;

let same_child_projection left right =
  let open Result.Let_syntax in
  if
    (not (C.Inventory.equal (C.inventory left) (C.inventory right)))
    || C.max_children left <> C.max_children right
    || C.max_encoded_bytes left <> C.max_encoded_bytes right
    || not (List.equal R.Reference.equal (C.references left) (C.references right))
  then Ok false
  else
    List.fold_result (C.references left) ~init:true ~f:(fun same reference ->
      let%bind left_origin = C.origin left ~reference in
      let%bind right_origin = C.origin right ~reference in
      let%bind left_snapshot = C.snapshot left ~reference in
      let%map right_snapshot = C.snapshot right ~reference in
      same
      && Option.equal O.equal left_origin right_origin
      && Option.equal
           (fun left right -> String.equal (S.digest left) (S.digest right))
           left_snapshot
           right_snapshot)
;;

let require_children children =
  Result.of_option
    children
    ~error:(P.Error.invalid_request "descendant update lacks actual before children")
;;

let require_accounting accounting =
  Result.of_option
    accounting
    ~error:(P.Error.invalid_request "descendant update lacks actual before component")
;;

let actual_root_limit t ~scope ~dimension ~ledger ~catalog ~accounts =
  let open Result.Let_syntax in
  let%bind _ = branch_for_scope t scope in
  let id = O.Scope.goal_id scope in
  let%bind goal =
    Option.bind catalog ~f:(fun catalog -> Goal_catalog.find catalog id)
    |> Result.of_option
         ~error:(P.Error.invalid_request "reserve actual root Goal is absent")
  in
  let%bind child =
    Option.bind accounts ~f:(fun accounts -> Goal_accounts.find accounts id)
    |> Result.of_option
         ~error:(P.Error.invalid_request "reserve actual root account is absent")
  in
  let%bind account = Goal_accounting_document.resolve child ~goal ~ledger in
  match dimension with
  | Goal_admission_policy.Attempt ->
    Ok
      ( Goal_accounting.admitted_attempts account
      , P.Goal.Budget.admitted_attempts (P.Goal.budget goal) )
  | Host_turn ->
    Ok
      ( Goal_accounting.admitted_turns account
      , P.Goal.Budget.admitted_turns (P.Goal.budget goal) )
;;

(* A new parent frontier must be committed while its actual creator episode is
   current, before the child can have effects. Retained exact branch replay may
   occur after later episode/source changes. *)
let validate_new_branch_scopes t ~origin ~catalog =
  let open Result.Let_syntax in
  List.fold_result (O.scopes origin) ~init:() ~f:(fun () scope ->
    if not (P.Session_ref.equal (O.Scope.owner scope) (session t))
    then Ok ()
    else (
      let%bind root = root t (O.Scope.goal_id scope) in
      match Data.find_branch root scope with
      | Some _ -> Ok ()
      | None ->
        let%bind goal =
          Option.bind catalog ~f:(fun catalog ->
            Goal_catalog.find catalog (O.Scope.goal_id scope))
          |> Result.of_option
               ~error:(P.Error.invalid_request "new branch actual root Goal is absent")
        in
        if
          P.Session_ref.equal (session t) (P.Goal.session goal)
          && P.Run_source.equal (O.Scope.source scope) (P.Goal.source goal)
          && Option.exists
               (P.Goal.episode goal)
               ~f:(P.Goal_episode.equal (O.Scope.creator_episode scope))
        then Ok ()
        else invalid "new branch differs from actual current creator Goal episode/source"))
;;

let validate_transition
      previous
      ~update
      ~before_ledger
      ~after_ledger
      ~before_session_revision
      ~after_session_revision
      ~before_children
      ~creation
      ~catalog
      ~accounts
  =
  let open Result.Let_syntax in
  let%bind expected =
    match Update.reason update with
    | Update.Reason.Register_new_root { host_profile; session; goal_id } ->
      let%bind goal, account =
        Result.of_option
          creation
          ~error:(P.Error.invalid_request "new root lacks actual ordered Create proof")
      in
      let%bind () =
        if P.Id.Goal.equal goal_id (P.Goal.id goal)
        then Ok ()
        else invalid "ordered Create proof names another root"
      in
      register_new_root
        previous
        ~host_profile
        ~session
        ~children:before_children
        ~goal
        ~account
        ~before_ledger
    | Register_child { reference; origin } ->
      let%bind previous = require_accounting previous in
      let%bind children = require_children before_children in
      register_child previous ~children ~reference ~origin
    | Register_branch { scope; reference; origin } ->
      let%bind previous = require_accounting previous in
      let%bind children = require_children before_children in
      let%bind () = validate_new_branch_scopes previous ~origin ~catalog in
      register_branch previous ~children ~scope ~reference ~origin
    | Reserve { scope; occurrence; local_count; ceiling } ->
      let%bind previous = require_accounting previous in
      let%bind children = require_children before_children in
      let%bind actual_count, actual_ceiling =
        actual_root_limit
          previous
          ~scope
          ~dimension:(Occurrence.dimension occurrence)
          ~ledger:before_ledger
          ~catalog
          ~accounts
      in
      let%bind () =
        if
          Int64.equal actual_count local_count
          && Option.equal Int64.equal actual_ceiling ceiling
        then Ok ()
        else invalid "reserve differs from actual root count policy"
      in
      let%bind result =
        reserve previous ~children ~scope ~occurrence ~local_count ~ceiling
      in
      (match result with
       | Reserve_result.Reserved (expected, _) -> Ok expected
       | Already_pending _ | Blocked _ ->
         invalid "serialized Reserve did not allocate one new pending")
    | Record_included { fact; receipts } ->
      let%bind previous = require_accounting previous in
      let%bind children = require_children before_children in
      let frame = Occurrence.frame (Canonical.Fact.occurrence fact) in
      let%bind () =
        if
          Int64.(
            before_session_revision >= 0L && before_session_revision < Int64.max_value)
          && Int64.equal (Int64.succ before_session_revision) after_session_revision
          && Int64.equal
               (Occurrence.Frame.candidate_session_revision frame)
               after_session_revision
        then Ok ()
        else invalid "canonical Included fact differs from actual adjacent State revision"
      in
      record_fact
        previous
        ~children
        ~before:before_ledger
        ~after:after_ledger
        ~fact
        ~receipts
    | Import reference ->
      let%bind previous = require_accounting previous in
      let%bind children = require_children before_children in
      let candidate_children =
        match Update.children update with
        | Preserve -> children
        | Replace incoming -> incoming
      in
      let%bind snapshot = C.snapshot candidate_children ~reference in
      let%bind snapshot =
        Result.of_option
          snapshot
          ~error:
            (P.Error.invalid_request
               "paired Import has no actual candidate child snapshot")
      in
      import previous ~children ~snapshot
    | Close_before_writer pending ->
      let%bind previous = require_accounting previous in
      let%bind children = require_children before_children in
      close_before_writer previous ~children ~pending
  in
  let%bind merged =
    adopt (Update.accounting expected) ~incoming:(Update.accounting update)
  in
  let%bind merged_document = to_document merged in
  let%bind actual_document = to_document (Update.accounting update) in
  let%bind () =
    if D.Json.equal (D.Document.json merged_document) (D.Document.json actual_document)
    then Ok ()
    else invalid "descendant Update omitted preserved accounting data"
  in
  match Update.children expected, Update.children update with
  | Preserve, Preserve -> Ok ()
  | Replace expected, Replace incoming ->
    let%bind same = same_child_projection expected incoming in
    let%bind () =
      if same
      then Ok ()
      else invalid "paired child candidate differs from exact finite reason"
    in
    let%bind merged = C.adopt expected ~incoming in
    let%bind merged_json = C.to_jsonaf merged in
    let%bind incoming_json = C.to_jsonaf incoming in
    if D.Json.equal merged_json incoming_json
    then Ok ()
    else invalid "descendant Update omitted preserved child data"
  | Preserve, Replace _ | Replace _, Preserve ->
    invalid "descendant Update changed its exact paired child action"
;;
