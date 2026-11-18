open! Core
module P = Agent_protocol
module D = Document_schema
module X = Persistence_codec
module O = Agent_store.Delegation_goal_origin
module R = Agent_store.Delegation_store
module B = Goal_descendant_branch
module Pending = Goal_descendant_pending
module Included = Goal_descendant_included

let invalid message = Error (P.Error.invalid_request message)

let domain result =
  Result.map_error result ~f:(fun error ->
    P.Error.invalid_request (Sexp.to_string_hum (D.Error.sexp_of_t error)))
;;

let limits =
  D.Limits.create ~max_bytes:262144 ~max_depth:64 ~max_fields:16384 ~max_nodes:32768
  |> domain
  |> Result.map_error ~f:(fun error -> error.P.Error.message)
  |> Result.ok_or_failwith
;;

module Profile = struct
  type t =
    { max_scopes : int
    ; max_roots : int
    ; max_branches : int
    ; max_encoded_bytes : int
    ; child_max_encoded_bytes : int
    }
  [@@deriving equal, sexp_of]

  let create
        ~max_scopes
        ~max_roots
        ~max_branches
        ~max_encoded_bytes
        ~child_max_encoded_bytes
    =
    if
      max_scopes < 0
      || max_roots <= 0
      || max_roots > 16
      || max_branches <= 0
      || max_encoded_bytes <= 0
      || max_encoded_bytes > 262144
      || child_max_encoded_bytes <= 0
      || child_max_encoded_bytes > 64 * 1024 * 1024
    then invalid "invalid descendant accounting profile"
    else
      Ok
        { max_scopes
        ; max_roots
        ; max_branches
        ; max_encoded_bytes
        ; child_max_encoded_bytes
        }
  ;;

  let max_scopes t = t.max_scopes
  let max_roots t = t.max_roots
  let max_branches t = t.max_branches
  let max_encoded_bytes t = t.max_encoded_bytes
  let child_max_encoded_bytes t = t.child_max_encoded_bytes

  let restrict_to_children t children =
    create
      ~max_scopes:t.max_scopes
      ~max_roots:t.max_roots
      ~max_branches:(Int.min t.max_branches (Child_contributions.max_children children))
      ~max_encoded_bytes:t.max_encoded_bytes
      ~child_max_encoded_bytes:
        (Int.min
           t.child_max_encoded_bytes
           (Child_contributions.max_encoded_bytes children))
  ;;

  let to_json t =
    `Object
      [ "max_scopes", X.integer_json t.max_scopes
      ; "max_roots", X.integer_json t.max_roots
      ; "max_branches", X.integer_json t.max_branches
      ; "max_encoded_bytes", X.integer_json t.max_encoded_bytes
      ; "child_max_encoded_bytes", X.integer_json t.child_max_encoded_bytes
      ]
  ;;

  let of_json raw =
    let open Result.Let_syntax in
    let%bind fields = X.object_ raw in
    let n name = X.required fields name X.integer in
    let%bind max_scopes = n "max_scopes" in
    let%bind max_roots = n "max_roots" in
    let%bind max_branches = n "max_branches" in
    let%bind max_encoded_bytes = n "max_encoded_bytes" in
    let%bind child_max_encoded_bytes = n "child_max_encoded_bytes" in
    create
      ~max_scopes
      ~max_roots
      ~max_branches
      ~max_encoded_bytes
      ~child_max_encoded_bytes
  ;;

  let shape =
    X.fields_shape
      [ "max_scopes"
      ; "max_roots"
      ; "max_branches"
      ; "max_encoded_bytes"
      ; "child_max_encoded_bytes"
      ]
  ;;
end

type head =
  { scope : O.Scope.t
  ; included : Included.t option
  }
[@@deriving equal]

type root =
  { goal_id : P.Id.Goal.t
  ; branches : B.t list
  }
[@@deriving equal]

type t =
  { profile : Profile.t
  ; session : P.Session_ref.t
  ; origin : O.t option
  ; heads : head list
  ; roots : root list
  }
[@@deriving equal]

let find_root t goal_id =
  List.find t.roots ~f:(fun root -> P.Id.Goal.equal root.goal_id goal_id)
;;

let find_branch root scope =
  List.find root.branches ~f:(fun branch -> O.Scope.equal scope (B.scope branch))
;;

let set_root t root =
  { t with
    roots =
      root
      :: List.filter t.roots ~f:(fun previous ->
        not (P.Id.Goal.equal previous.goal_id root.goal_id))
      |> List.sort ~compare:(fun left right ->
        P.Id.Goal.compare left.goal_id right.goal_id)
  }
;;

let set_branch t branch =
  let open Result.Let_syntax in
  let goal_id = O.Scope.goal_id (B.scope branch) in
  let%bind root =
    Result.of_option
      (find_root t goal_id)
      ~error:(P.Error.invalid_request "descendant root is absent")
  in
  let branches =
    branch
    :: List.filter root.branches ~f:(fun previous ->
      not
        (P.Id.Session.equal
           (B.reference previous).child_session_id
           (B.reference branch).child_session_id))
    |> List.sort ~compare:(fun left right ->
      P.Id.Session.compare
        (B.reference left).child_session_id
        (B.reference right).child_session_id)
  in
  Ok (set_root t { root with branches })
;;

let advance_head head included =
  let pending = Included.pending included in
  if not (O.Scope.equal head.scope (Pending.scope pending))
  then invalid "inherited Included scope differs"
  else (
    match head.included with
    | None -> Ok { head with included = Some included }
    | Some previous ->
      let previous_pending = Included.pending previous in
      if
        not
          (R.Reference.equal
             (Pending.branch_reference pending)
             (Pending.branch_reference previous_pending))
      then invalid "inherited Included branch reference changed"
      else if Int64.(Pending.nonce pending < Pending.nonce previous_pending)
      then Ok head
      else if Int64.equal (Pending.nonce pending) (Pending.nonce previous_pending)
      then
        if Included.equal previous included
        then Ok head
        else invalid "inherited Included nonce conflicts"
      else if Int64.(Pending.next_prefix pending <= Pending.next_prefix previous_pending)
      then invalid "inherited Included prefix did not advance"
      else Ok { head with included = Some included })
;;

let validate t =
  let open Result.Let_syntax in
  let scopes = Option.value_map t.origin ~default:[] ~f:O.scopes in
  let%bind () =
    if
      List.length scopes > Profile.max_scopes t.profile
      || (not
            (List.equal
               O.Scope.equal
               scopes
               (List.map t.heads ~f:(fun head -> head.scope))))
      || List.length t.roots > Profile.max_roots t.profile
    then invalid "descendant roots or inherited vector exceed frozen profile"
    else Ok ()
  in
  let%bind () =
    List.fold_result t.heads ~init:() ~f:(fun () head ->
      match head.included with
      | None -> Ok ()
      | Some included ->
        if O.Scope.equal head.scope (Pending.scope (Included.pending included))
        then Ok ()
        else invalid "inherited head attribution differs")
  in
  let%map _ =
    List.fold_result t.roots ~init:None ~f:(fun previous root ->
      let%bind () =
        if
          Option.exists previous ~f:(fun previous ->
            P.Id.Goal.compare previous root.goal_id >= 0)
          || List.length root.branches > Profile.max_branches t.profile
        then invalid "descendant root identities must be bounded, unique and ordered"
        else Ok ()
      in
      let%map _ =
        List.fold_result root.branches ~init:(None, []) ~f:(fun (previous, keys) branch ->
          let reference = B.reference branch in
          let scope = B.scope branch in
          let%bind () =
            if
              (not (P.Session_ref.equal t.session (O.Scope.owner scope)))
              || (not (P.Id.Goal.equal root.goal_id (O.Scope.goal_id scope)))
              || Option.exists previous ~f:(fun previous ->
                P.Id.Session.compare previous reference.child_session_id >= 0)
              || List.exists keys ~f:(R.Key.equal reference.key)
            then invalid "descendant branch identity or attribution differs"
            else Ok ()
          in
          Ok (Some reference.child_session_id, reference.key :: keys))
      in
      Some root.goal_id)
  in
  ()
;;

let to_json t =
  `Object
    [ "profile", Profile.to_json t.profile
    ; "session", P.Session_ref.to_json t.session
    ; "origin", X.option_json O.to_json t.origin
    ; ( "heads"
      , `Array
          (List.map t.heads ~f:(fun head ->
             `Object
               [ "scope_id", `String (O.Scope.root_id head.scope)
               ; "scope", O.Scope.to_json head.scope
               ; "included", X.option_json Included.to_json head.included
               ])) )
    ; ( "roots"
      , `Array
          (List.map t.roots ~f:(fun root ->
             `Object
               [ "goal_id", P.Id.Goal.to_json root.goal_id
               ; ( "branches"
                 , `Array
                     (List.map root.branches ~f:(fun branch ->
                        `Object
                          [ ( "child_session_id"
                            , P.Id.Session.to_json (B.reference branch).child_session_id )
                          ; "branch", B.to_json branch
                          ])) )
               ])) )
    ]
;;

let of_json raw =
  let open Result.Let_syntax in
  let%bind () = D.Json.validate raw ~limits |> domain in
  let%bind fields = X.object_ raw in
  let%bind profile = X.required fields "profile" Profile.of_json in
  let%bind session = X.required fields "session" P.Session_ref.of_json in
  let%bind origin =
    X.required
      fields
      "origin"
      (X.nullable (O.of_json ~max_scopes:(Profile.max_scopes profile) ~limits))
  in
  let%bind heads =
    X.required
      fields
      "heads"
      (X.list (fun raw ->
         let%bind fields = X.object_ raw in
         let%bind scope_id = X.required fields "scope_id" P.Json_codec.string in
         let%bind scope = X.required fields "scope" O.Scope.of_json in
         let%bind included = X.required fields "included" (X.nullable Included.of_json) in
         if String.equal scope_id (O.Scope.root_id scope)
         then Ok { scope; included }
         else invalid "inherited head wrapper differs"))
  in
  let%bind roots =
    X.required
      fields
      "roots"
      (X.list (fun raw ->
         let%bind fields = X.object_ raw in
         let%bind goal_id = X.required fields "goal_id" P.Id.Goal.of_json in
         let%map branches =
           X.required
             fields
             "branches"
             (X.list (fun raw ->
                let%bind fields = X.object_ raw in
                let%bind child =
                  X.required fields "child_session_id" P.Id.Session.of_json
                in
                let%bind branch = X.required fields "branch" B.of_json in
                if P.Id.Session.equal child (B.reference branch).child_session_id
                then Ok branch
                else invalid "descendant branch wrapper differs"))
         in
         { goal_id; branches }))
  in
  let t = { profile; session; origin; heads; roots } in
  let%map () = validate t in
  t
;;

let branch_shape = B.shape

let head_shape =
  X.shape_exn
    [ "scope_id", D.Shape.value
    ; "scope", O.Scope.shape
    ; "included", D.Shape.nullable Included.shape
    ]
;;

let shape =
  X.shape_exn
    [ "profile", Profile.shape
    ; "session", X.fields_shape [ "server_id"; "session_id" ]
    ; "origin", D.Shape.nullable O.shape
    ; "heads", X.array_shape_exn ~identity_field:"scope_id" head_shape
    ; ( "roots"
      , X.array_shape_exn
          ~identity_field:"goal_id"
          (X.shape_exn
             [ "goal_id", D.Shape.value
             ; ( "branches"
               , X.array_shape_exn
                   ~identity_field:"child_session_id"
                   (X.shape_exn
                      [ "child_session_id", D.Shape.value; "branch", branch_shape ]) )
             ]) )
    ]
;;
