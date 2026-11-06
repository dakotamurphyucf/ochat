open! Core
module P = Agent_protocol
module D = Document_schema
module X = Persistence_codec
module C = D.Extension_carrier
module Data = Goal_descendants_data
module O = Agent_store.Delegation_goal_origin
module R = Agent_store.Delegation_store
module B = Goal_descendant_branch
module Pending = Goal_descendant_pending
module Included = Goal_descendant_included
module Occurrence = Goal_descendant_occurrence

type t =
  { growth : Capacity_growth.t
  ; required_depth : int
  }

let growth t = t.growth
let required_depth t = t.required_depth
let limits = Data.limits
let domain = Data.domain

let field raw name =
  let%bind.Result fields = X.object_ raw in
  X.required fields name X.raw
;;

let set raw name value =
  match raw with
  | `Object fields -> `Object (List.Assoc.add fields ~equal:String.equal name value)
  | `Null | `True | `False | `Number _ | `String _ | `Array _ -> assert false
;;

let maximum_pending ~scope ~reference ~current ~maximum_attempt =
  let raw = Pending.capacity_projection ~scope ~branch_reference:reference in
  match current with
  | None -> raw
  | Some current ->
    if maximum_attempt
    then raw
    else
      set
        raw
        "occurrence"
        (Occurrence.capacity_projection_for (Pending.occurrence current))
;;

let last_pending = function
  | B.Last.Included included -> Included.pending included
  | No_writer_closed pending -> pending
;;

let branch_projection branch ~maximum_pending_attempt ~maximum_last_attempt =
  let scope = B.scope branch in
  let reference = B.reference branch in
  let pending =
    maximum_pending
      ~scope
      ~reference
      ~current:(B.pending branch)
      ~maximum_attempt:maximum_pending_attempt
  in
  let previous = Option.map (B.last branch) ~f:last_pending in
  let last =
    maximum_pending
      ~scope
      ~reference
      ~current:previous
      ~maximum_attempt:maximum_last_attempt
  in
  `Object
    [ "scope", O.Scope.to_json scope
    ; "reference", R.reference_to_jsonaf reference
    ; "confirmed_prefix", X.int64_json Int64.max_value
    ; "last_nonce", X.int64_json Int64.max_value
    ; "pending", pending
    ; "last", `Object [ "kind", `String "no_writer_closed"; "pending", last ]
    ]
;;

let head_projection (head : Data.head) ~maximum_attempt =
  let%map.Result included =
    Child_usage_snapshot.Goal_scope.capacity_projection
      ~scope:head.scope
      ~included:head.included
      ~maximum_attempt
    |> fun raw -> field raw "included"
  in
  `Object
    [ "scope_id", `String (O.Scope.root_id head.scope)
    ; "scope", O.Scope.to_json head.scope
    ; "included", included
    ]
;;

let head_shape =
  X.shape_exn
    [ "scope_id", D.Shape.value
    ; "scope", O.Scope.shape
    ; "included", D.Shape.nullable Included.shape
    ]
;;

let kind = "ochat.goal_descendants"

let probe_codec =
  X.codec_exn
    ~version:1
    ~limits
    ~kind
    ~shape:Data.shape
    ~decode:Result.return
    ~encode:Result.return
;;

let local_capacity ~shape ~raw ~baseline ~alternative =
  let open Result.Let_syntax in
  let codec =
    X.codec_exn
      ~version:1
      ~limits
      ~kind:"ochat.goal_descendant_capacity"
      ~shape
      ~decode:Result.return
      ~encode:Result.return
  in
  let%bind document =
    D.Document.create
      ~limits
      ~kind:"ochat.goal_descendant_capacity"
      ~version:1
      ~payload:raw
    |> domain
  in
  let%bind carrier = D.Domain_codec.decode codec document |> domain in
  let encode raw = D.Domain_codec.encode codec (C.with_value carrier raw) in
  let%bind baseline = encode baseline |> domain in
  let baseline_raw = D.Document.payload baseline in
  let%bind measured = Capacity_growth.measure baseline_raw ~limits in
  let%bind baseline_depth = Capacity_depth.measure baseline_raw ~limits in
  match encode alternative with
  | Error (D.Error.Extension_conflict _) ->
    Ok { growth = Capacity_growth.zero; required_depth = baseline_depth }
  | Error error -> domain (Error error)
  | Ok alternative ->
    let raw = D.Document.payload alternative in
    let%bind after = Capacity_growth.measure raw ~limits in
    let%map depth = Capacity_depth.measure raw ~limits in
    { growth = Capacity_growth.positive_difference after ~previous:measured
    ; required_depth = Int.max baseline_depth depth
    }
;;

let raw_index current =
  let open Result.Let_syntax in
  let%bind fields = X.object_ (D.Document.payload current) in
  let%bind heads = X.required fields "heads" (X.list X.raw) in
  let%bind heads =
    List.fold_result heads ~init:String.Map.empty ~f:(fun index raw ->
      let%bind fields = X.object_ raw in
      let%map id = X.required fields "scope_id" P.Json_codec.string in
      Map.set index ~key:id ~data:raw)
  in
  let%bind roots = X.required fields "roots" (X.list X.raw) in
  let%map branches =
    List.fold_result roots ~init:String.Map.empty ~f:(fun index raw ->
      let%bind fields = X.object_ raw in
      let%bind id = X.required fields "goal_id" P.Id.Goal.of_json in
      let%bind raw_branches = X.required fields "branches" (X.list X.raw) in
      let%map children =
        List.fold_result raw_branches ~init:String.Map.empty ~f:(fun index raw ->
          let%bind fields = X.object_ raw in
          let%bind id = X.required fields "child_session_id" P.Id.Session.of_json in
          let%map branch = X.required fields "branch" X.raw in
          Map.set index ~key:(P.Id.Session.to_string id) ~data:branch)
      in
      Map.set index ~key:(P.Id.Goal.to_string id) ~data:children)
  in
  heads, branches
;;

let capture (value : Data.t) ~current =
  let open Result.Let_syntax in
  let%bind raw_heads, raw_branches = raw_index current in
  let%bind heads, extra, local_depth =
    List.fold_result
      value.heads
      ~init:([], Capacity_growth.zero, 1)
      ~f:(fun (heads, extra, local_depth) head ->
        let%bind baseline = head_projection head ~maximum_attempt:false in
        let%bind alternative = head_projection head ~maximum_attempt:true in
        let raw = Map.find_exn raw_heads (O.Scope.root_id head.scope) in
        let%bind additional =
          local_capacity ~shape:head_shape ~raw ~baseline ~alternative
        in
        let%map extra = Capacity_growth.add extra additional.growth in
        (* Actual head path: payload/heads/<head>. *)
        baseline :: heads, extra, Int.max local_depth (3 + additional.required_depth))
  in
  let%bind roots, extra, local_depth =
    List.fold_result
      value.roots
      ~init:([], extra, local_depth)
      ~f:(fun (roots, extra, local_depth) root ->
        let index = Map.find_exn raw_branches (P.Id.Goal.to_string root.Data.goal_id) in
        let%map branches, extra, local_depth =
          List.fold_result
            root.branches
            ~init:([], extra, local_depth)
            ~f:(fun (branches, extra, local_depth) branch ->
              let baseline =
                branch_projection
                  branch
                  ~maximum_pending_attempt:false
                  ~maximum_last_attempt:false
              in
              let raw =
                Map.find_exn
                  index
                  (P.Id.Session.to_string (B.reference branch).child_session_id)
              in
              let%bind pending_extra =
                local_capacity
                  ~shape:B.shape
                  ~raw
                  ~baseline
                  ~alternative:
                    (branch_projection
                       branch
                       ~maximum_pending_attempt:true
                       ~maximum_last_attempt:false)
              in
              let%bind last_extra =
                local_capacity
                  ~shape:B.shape
                  ~raw
                  ~baseline
                  ~alternative:
                    (branch_projection
                       branch
                       ~maximum_pending_attempt:false
                       ~maximum_last_attempt:true)
              in
              let%bind extra = Capacity_growth.add extra pending_extra.growth in
              let%map extra = Capacity_growth.add extra last_extra.growth in
              ( `Object
                  [ ( "child_session_id"
                    , P.Id.Session.to_json (B.reference branch).child_session_id )
                  ; "branch", baseline
                  ]
                :: branches
              , extra
                (* Actual branch path: payload/roots/<root>/branches/<wrapper>/branch. *)
              , Int.max
                  local_depth
                  (6 + Int.max pending_extra.required_depth last_extra.required_depth) ))
        in
        ( `Object
            [ "goal_id", P.Id.Goal.to_json root.goal_id
            ; "branches", `Array (List.rev branches)
            ]
          :: roots
        , extra
        , local_depth ))
  in
  let future =
    Data.to_json value
    |> fun raw ->
    set (set raw "heads" (`Array (List.rev heads))) "roots" (`Array (List.rev roots))
  in
  let%bind carrier = D.Domain_codec.decode probe_codec current |> domain in
  let%bind future =
    D.Domain_codec.encode probe_codec (C.with_value carrier future) |> domain
  in
  let%bind before = Capacity_growth.measure (D.Document.json current) ~limits in
  let%bind after = Capacity_growth.measure (D.Document.json future) ~limits in
  let%bind growth =
    Capacity_growth.add (Capacity_growth.positive_difference after ~previous:before) extra
  in
  let%bind current_depth = Capacity_depth.measure (D.Document.json current) ~limits in
  let%bind future_depth = Capacity_depth.measure (D.Document.json future) ~limits in
  let required_depth = Int.max local_depth (Int.max current_depth future_depth) in
  let%map () = Capacity_depth.validate required_depth ~ancestors:0 ~limits in
  { growth; required_depth }
;;

let reserved_growth value ~current =
  let%map.Result capacity = capture value ~current in
  capacity.growth
;;
