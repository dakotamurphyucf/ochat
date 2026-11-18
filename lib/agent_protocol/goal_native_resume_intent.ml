open! Core
module J = Json_codec

module Error = struct
  include Protocol_error

  let equal left right = Jsonaf.exactly_equal (to_json left) (to_json right)
end

module Phase = struct
  type t =
    | Staged
    | Accepted of Timestamp.t
    | Dispatch_claimed of
        { accepted_at : Timestamp.t
        ; claimed_at : Timestamp.t
        }
    | Discarded of string
    | Rejected of Error.t
  [@@deriving equal, sexp_of]
end

type t =
  { request : Goal_resume_request.t
  ; principal_id : Id.Principal.t
  ; phase : Phase.t
  }
[@@deriving equal]

let persistence_semantics = "goal.native-resume.v1"
let invalid message = Error (Error.invalid_request message)
let request t = t.request
let principal_id t = t.principal_id
let phase t = t.phase

let phase_json = function
  | Phase.Staged -> `Object [ "kind", `String "staged" ]
  | Accepted at ->
    `Object [ "kind", `String "accepted"; "accepted_at", Timestamp.to_json at ]
  | Dispatch_claimed { accepted_at; claimed_at } ->
    `Object
      [ "kind", `String "dispatch_claimed"
      ; "accepted_at", Timestamp.to_json accepted_at
      ; "claimed_at", Timestamp.to_json claimed_at
      ]
  | Discarded reason -> `Object [ "kind", `String "discarded"; "reason", `String reason ]
  | Rejected error -> `Object [ "kind", `String "rejected"; "error", Error.to_json error ]
;;

let to_json t =
  `Object
    [ "request", Goal_resume_request.to_json t.request
    ; "principal_id", Id.Principal.to_json t.principal_id
    ; "phase", phase_json t.phase
    ]
;;

let validate t =
  let open Result.Let_syntax in
  let%bind _ = Id.Principal.of_json (Id.Principal.to_json t.principal_id) in
  let%bind _ = Goal_resume_request.of_json (Goal_resume_request.to_json t.request) in
  let%bind () = J.validate_limits ~max_bytes:2048 ~max_depth:12 (phase_json t.phase) in
  (* Admission reserves every later bounded phase, not merely Staged's bytes.
     This is carrier headroom only; it grants no execution permission. *)
  let%bind () =
    J.validate_limits
      ~max_bytes:8192
      ~max_depth:16
      (`Object
          [ "request", Goal_resume_request.to_json t.request
          ; "principal_id", Id.Principal.to_json t.principal_id
          ; "phase", `String (String.make 3072 'x')
          ])
  in
  J.validate_limits ~max_bytes:8192 ~max_depth:16 (to_json t)
;;

let create ~request ~principal_id =
  let t = { request; principal_id; phase = Staged } in
  Result.map (validate t) ~f:(fun () -> t)
;;

let change t phase =
  let next = { t with phase } in
  Result.map (validate next) ~f:(fun () -> next)
;;

let accept t ~now =
  match t.phase with
  | Staged -> change t (Accepted now)
  | Accepted _ | Dispatch_claimed _ | Discarded _ | Rejected _ ->
    invalid "native Goal action is not staged"
;;

let claim t ~now =
  match t.phase with
  | Accepted accepted_at -> change t (Dispatch_claimed { accepted_at; claimed_at = now })
  | Staged | Dispatch_claimed _ | Discarded _ | Rejected _ ->
    invalid "native Goal action is not accepted"
;;

let discard t ~reason =
  let reason =
    match J.validate_limits ~max_bytes:1024 ~max_depth:1 (`String reason) with
    | Ok () -> reason
    | Error _ -> "native Goal action discarded; detailed reason exceeds retention limit"
  in
  match t.phase with
  | Staged | Accepted _ -> change t (Discarded reason)
  | Discarded previous when String.equal previous reason -> Ok t
  | Dispatch_claimed _ | Discarded _ | Rejected _ ->
    invalid "claimed native Goal action cannot be discarded"
;;

let reject t error =
  let error =
    match J.validate_limits ~max_bytes:1800 ~max_depth:8 (Error.to_json error) with
    | Ok () -> error
    | Error _ ->
      Error.create
        error.code
        ~message:"native Goal dispatch rejected; detailed error exceeds retention limit"
        ~retryable:error.retryable
        ()
  in
  match t.phase with
  | Dispatch_claimed _ -> change t (Rejected error)
  | Staged | Accepted _ | Discarded _ | Rejected _ ->
    invalid "native Goal action was not claimed"
;;

let validate_update t ~incoming =
  let open Result.Let_syntax in
  let%bind () = validate incoming in
  if
    not
      (Goal_resume_request.equal t.request incoming.request
       && Id.Principal.equal t.principal_id incoming.principal_id)
  then invalid "native Goal action identity changed"
  else if equal t incoming
  then Ok ()
  else (
    match t.phase, incoming.phase with
    | Accepted accepted_at, Dispatch_claimed claimed ->
      if Timestamp.equal accepted_at claimed.accepted_at
      then Ok ()
      else invalid "native Goal action acceptance time changed"
    | Staged, (Accepted _ | Discarded _)
    | Accepted _, Discarded _
    | Dispatch_claimed _, Rejected _ -> Ok ()
    | (Staged | Accepted _ | Dispatch_claimed _ | Discarded _ | Rejected _), _ ->
      invalid "native Goal action phase cannot be rearmed")
;;

let closed fields names =
  if
    List.for_all (J.to_alist fields) ~f:(fun (name, _) ->
      List.mem names name ~equal:String.equal)
  then Ok ()
  else invalid "unknown native Goal action field"
;;

let of_json json =
  let open Result.Let_syntax in
  let%bind () = J.validate_limits ~max_bytes:8192 ~max_depth:16 json in
  let%bind f = J.fields json in
  let%bind () = closed f [ "request"; "principal_id"; "phase" ] in
  let%bind request = J.required_as f "request" Goal_resume_request.of_json in
  let%bind principal_id = J.required_as f "principal_id" Id.Principal.of_json in
  let%bind p = J.required_as f "phase" J.fields in
  let%bind kind = J.required_as p "kind" J.string in
  let%bind phase =
    match kind with
    | "staged" ->
      let%map () = closed p [ "kind" ] in
      Phase.Staged
    | "accepted" ->
      let%bind () = closed p [ "kind"; "accepted_at" ] in
      let%map at = J.required_as p "accepted_at" Timestamp.of_json in
      Phase.Accepted at
    | "dispatch_claimed" ->
      let%bind () = closed p [ "kind"; "accepted_at"; "claimed_at" ] in
      let%bind accepted_at = J.required_as p "accepted_at" Timestamp.of_json in
      let%map claimed_at = J.required_as p "claimed_at" Timestamp.of_json in
      Phase.Dispatch_claimed { accepted_at; claimed_at }
    | "discarded" ->
      let%bind () = closed p [ "kind"; "reason" ] in
      let%map reason = J.required_as p "reason" J.string in
      Phase.Discarded reason
    | "rejected" ->
      let%bind () = closed p [ "kind"; "error" ] in
      let%map error = J.required_as p "error" Error.of_json in
      Phase.Rejected error
    | _ -> invalid "unknown native Goal action phase"
  in
  let t = { request; principal_id; phase } in
  let%map () = validate t in
  t
;;

let sexp_of_t t = Jsonaf.sexp_of_t (to_json t)

let t_of_sexp sexp =
  match of_json (Jsonaf.t_of_sexp sexp) with
  | Ok t -> t
  | Error error -> Sexplib.Conv.of_sexp_error error.message sexp
;;
