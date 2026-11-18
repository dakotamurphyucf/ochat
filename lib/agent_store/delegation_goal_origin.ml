open! Core
module P = Agent_protocol
module J = P.Json_codec
module D = Document_schema
module F = Document_fields

let invalid message = Error (P.Error.invalid_request message)

let limits =
  D.Limits.create ~max_bytes:262144 ~max_depth:64 ~max_fields:8192 ~max_nodes:16384
  |> Result.map_error ~f:(fun error -> Sexp.to_string_hum (D.Error.sexp_of_t error))
  |> Result.ok_or_failwith
;;

let preflight json ~limits =
  D.Json.validate json ~limits
  |> Result.map_error ~f:(fun error ->
    P.Error.invalid_request (Sexp.to_string_hum (D.Error.sexp_of_t error)))
;;

let sexp_of_json json = Sexp.Atom (Jsonaf.to_string json)

let decode_sexp decode sexp =
  let decoded =
    match sexp with
    | Sexp.Atom bytes ->
      D.Json.decode ~limits bytes
      |> Result.map_error ~f:(fun error ->
        P.Error.invalid_request (Sexp.to_string_hum (D.Error.sexp_of_t error)))
      |> Result.bind ~f:decode
    | List _ -> invalid "Goal origin sexp must be a bounded JSON atom"
  in
  match decoded with
  | Ok value -> value
  | Error error -> Sexplib0.Sexp_conv.of_sexp_error error.P.Error.message sexp
;;

module Anchor = struct
  type t =
    { parent_session_id : P.Id.Session.t
    ; parent_generation : int
    ; principal_id : P.Id.Principal.t
    ; key : P.Idempotency_key.t
    ; child_session_id : P.Id.Session.t
    ; transaction_id : P.Id.Transaction.t
    }
  [@@deriving equal]

  let parent_session_id t = t.parent_session_id
  let parent_generation t = t.parent_generation
  let principal_id t = t.principal_id
  let key t = t.key
  let child_session_id t = t.child_session_id
  let transaction_id t = t.transaction_id

  let create
        ~parent_session_id
        ~parent_generation
        ~principal_id
        ~key
        ~child_session_id
        ~transaction_id
    =
    let open Result.Let_syntax in
    let%bind _ = P.Id.Session.of_string (P.Id.Session.to_string parent_session_id) in
    let%bind _ = P.Id.Principal.of_string (P.Id.Principal.to_string principal_id) in
    let%bind _ = P.Idempotency_key.of_string (P.Idempotency_key.to_string key) in
    let%bind _ = P.Id.Session.of_string (P.Id.Session.to_string child_session_id) in
    let%bind _ = P.Id.Transaction.of_string (P.Id.Transaction.to_string transaction_id) in
    if parent_generation < 0 || P.Id.Session.equal parent_session_id child_session_id
    then
      invalid "Goal branch anchor requires distinct sessions and nonnegative generation"
    else
      Ok
        { parent_session_id
        ; parent_generation
        ; principal_id
        ; key
        ; child_session_id
        ; transaction_id
        }
  ;;

  let to_json t =
    `Object
      [ "parent_session_id", P.Id.Session.to_json t.parent_session_id
      ; "parent_generation", `Number (Int.to_string t.parent_generation)
      ; "principal_id", P.Id.Principal.to_json t.principal_id
      ; "key", P.Idempotency_key.to_json t.key
      ; "child_session_id", P.Id.Session.to_json t.child_session_id
      ; "transaction_id", P.Id.Transaction.to_json t.transaction_id
      ]
  ;;

  let of_json json =
    let open Result.Let_syntax in
    let%bind () = preflight json ~limits in
    let%bind fields = J.fields json in
    let%bind parent_session_id =
      J.required_as fields "parent_session_id" P.Id.Session.of_json
    in
    let%bind parent_generation =
      J.required_as fields "parent_generation" (J.bounded_int ~min:0 ~max:Int.max_value)
    in
    let%bind principal_id = J.required_as fields "principal_id" P.Id.Principal.of_json in
    let%bind key = J.required_as fields "key" P.Idempotency_key.of_json in
    let%bind child_session_id =
      J.required_as fields "child_session_id" P.Id.Session.of_json
    in
    let%bind transaction_id =
      J.required_as fields "transaction_id" P.Id.Transaction.of_json
    in
    create
      ~parent_session_id
      ~parent_generation
      ~principal_id
      ~key
      ~child_session_id
      ~transaction_id
  ;;

  let shape =
    F.shape
      (List.map
         [ "parent_session_id"
         ; "parent_generation"
         ; "principal_id"
         ; "key"
         ; "child_session_id"
         ; "transaction_id"
         ]
         ~f:(fun name -> name, D.Shape.value))
  ;;

  let sexp_of_t t = sexp_of_json (to_json t)
  let t_of_sexp sexp = decode_sexp of_json sexp
end

module Scope = struct
  type t =
    { owner : P.Session_ref.t
    ; goal_id : P.Id.Goal.t
    ; source : P.Run_source.t
    ; creator_episode : P.Goal_episode.t
    ; branch : Anchor.t
    }
  [@@deriving equal]

  let owner t = t.owner
  let goal_id t = t.goal_id
  let source t = t.source
  let creator_episode t = t.creator_episode
  let branch t = t.branch

  let compare_root left right =
    match P.Session_ref.compare left.owner right.owner with
    | 0 -> P.Id.Goal.compare left.goal_id right.goal_id
    | order -> order
  ;;

  let root_id t =
    `Object
      [ "owner", P.Session_ref.to_json t.owner; "goal_id", P.Id.Goal.to_json t.goal_id ]
    |> Jsonaf.to_string
    |> Digestif.SHA256.digest_string
    |> Digestif.SHA256.to_hex
  ;;

  let create ~owner ~goal_id ~source ~creator_episode ~branch =
    let open Result.Let_syntax in
    let%bind _ = P.Session_ref.of_json (P.Session_ref.to_json owner) in
    let%bind _ = P.Id.Goal.of_json (P.Id.Goal.to_json goal_id) in
    let%bind () = P.Run_source.validate source in
    let%bind _ = P.Goal_episode.of_json (P.Goal_episode.to_json creator_episode) in
    if
      P.Session_ref.equal owner (P.Goal_episode.session creator_episode)
      && P.Id.Session.equal
           (P.Session_ref.session_id owner)
           (Anchor.parent_session_id branch)
      && P.Run_source.equal source (P.Goal_episode.admission creator_episode).source
    then Ok { owner; goal_id; source; creator_episode; branch }
    else invalid "Goal scope owner, branch and creator source disagree"
  ;;

  let to_json t =
    `Object
      [ "root_id", `String (root_id t)
      ; "owner", P.Session_ref.to_json t.owner
      ; "goal_id", P.Id.Goal.to_json t.goal_id
      ; "source", P.Run_source.to_json t.source
      ; "creator_episode", P.Goal_episode.to_json t.creator_episode
      ; "branch", Anchor.to_json t.branch
      ]
  ;;

  let of_json json =
    let open Result.Let_syntax in
    let%bind () = preflight json ~limits in
    let%bind fields = J.fields json in
    let%bind supplied_id = J.required_as fields "root_id" J.string in
    let%bind owner = J.required_as fields "owner" P.Session_ref.of_json in
    let%bind goal_id = J.required_as fields "goal_id" P.Id.Goal.of_json in
    let%bind source = J.required_as fields "source" P.Run_source.of_json in
    let%bind creator_episode =
      J.required_as fields "creator_episode" P.Goal_episode.of_json
    in
    let%bind branch = J.required_as fields "branch" Anchor.of_json in
    let%bind scope = create ~owner ~goal_id ~source ~creator_episode ~branch in
    if String.equal supplied_id (root_id scope)
    then Ok scope
    else invalid "Goal scope root identity differs from owner and Goal"
  ;;

  let shape =
    let scalar = D.Shape.value in
    let session = F.shape [ "server_id", scalar; "session_id", scalar ] in
    let source =
      F.shape
        [ "script_id", scalar
        ; "source_sha256", scalar
        ; "generation", scalar
        ; "installation_epoch", scalar
        ]
    in
    let receipt =
      F.shape
        [ "run_id", scalar
        ; "principal_id", scalar
        ; "source", source
        ; "key", scalar
        ; "request_sha256", scalar
        ; "kind", scalar
        ; "run_revision", scalar
        ; "session_revision", scalar
        ; "committed_at", scalar
        ]
    in
    F.shape
      [ "root_id", scalar
      ; "owner", session
      ; "goal_id", scalar
      ; "source", source
      ; "creator_episode", F.shape [ "session", session; "admission", receipt ]
      ; "branch", Anchor.shape
      ]
  ;;

  let sexp_of_t t = sexp_of_json (to_json t)
  let t_of_sexp sexp = decode_sexp of_json sexp
end

type t =
  | Unrelated
  | Associated of Scope.t list
[@@deriving equal]

let unrelated = Unrelated

let scopes = function
  | Unrelated -> []
  | Associated scopes -> scopes
;;

let to_json = function
  | Unrelated -> `Object [ "kind", `String "unrelated" ]
  | Associated scopes ->
    `Object
      [ "kind", `String "associated"
      ; "scopes", `Array (List.map scopes ~f:Scope.to_json)
      ]
;;

let associated ~max_scopes scopes =
  let open Result.Let_syntax in
  if
    max_scopes < 1
    || List.is_empty scopes
    || List.length scopes > max_scopes
    || not (List.is_sorted_strictly scopes ~compare:Scope.compare_root)
  then invalid "Goal origin requires a bounded, nonempty, strictly ordered root vector"
  else (
    let t = Associated scopes in
    let%map () = preflight (to_json t) ~limits in
    t)
;;

let native_limits = limits

let of_json ~max_scopes ~limits json =
  let open Result.Let_syntax in
  let%bind () = preflight json ~limits in
  let%bind () = preflight json ~limits:native_limits in
  let%bind () =
    if max_scopes >= 0
    then Ok ()
    else invalid "Goal origin scope bound must be nonnegative"
  in
  let%bind fields = J.fields json in
  match%bind J.required_as fields "kind" J.string with
  | "unrelated" ->
    (match J.optional fields "scopes" with
     | None -> Ok unrelated
     | Some _ -> invalid "Unrelated Goal origin cannot carry a scope vector")
  | "associated" ->
    let%bind raw = J.required fields "scopes" in
    (match raw with
     | `Array entries when List.length entries <= max_scopes ->
       let%bind scopes = List.map entries ~f:Scope.of_json |> Result.all in
       associated ~max_scopes scopes
     | `Array _ -> invalid "Goal origin scope vector exceeds captured bound"
     | _ -> invalid "Goal origin scopes must be an array")
  | _ -> invalid "unknown Goal origin kind"
;;

let shape =
  let scopes =
    D.Shape.array Scope.shape ~identity_field:(Some "root_id")
    |> Result.map_error ~f:(fun error -> Sexp.to_string_hum (D.Error.sexp_of_t error))
    |> Result.ok_or_failwith
  in
  D.Shape.tagged_object
    ~discriminator:"kind"
    [ "unrelated", F.shape [ "kind", D.Shape.value ]
    ; "associated", F.shape [ "kind", D.Shape.value; "scopes", scopes ]
    ]
  |> Result.map_error ~f:(fun error -> Sexp.to_string_hum (D.Error.sexp_of_t error))
  |> Result.ok_or_failwith
;;

let sexp_of_t t = sexp_of_json (to_json t)
let t_of_sexp sexp = decode_sexp (of_json ~max_scopes:Int.max_value ~limits) sexp
