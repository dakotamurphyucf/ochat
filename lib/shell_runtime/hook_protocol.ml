open! Core

type version = V1 [@@deriving sexp, compare, equal]

type kind =
  | Before_interceptor
  | After_interceptor
  | Reviewer
  | Effect_analyzer
  | Audit_filter
[@@deriving sexp, compare, equal]

type status =
  | Exited of int
  | Signaled of int
[@@deriving sexp, compare, equal]

type action =
  | Continue
  | Rewrite of string list
  | Respond of { status : status; stdout : string; stderr : string }
  | Reject of string
  | Replace_result of { status : status; stdout : string; stderr : string }
  | Defer
  | Approve_once
  | Approve_scope of { scope : string; expires_at : float option }
  | Deny of string
  | Reviewer_rewrite of string list
  | Add_effects of string list
  | Replace_effects of string list
  | Audit_keep
  | Audit_drop_field of string
  | Audit_replace_fields of string String.Map.t
[@@deriving sexp, compare, equal]

type request =
  { version : version
  ; request_id : string
  ; hook_id : string
  ; kind : kind
  ; payload : Jsonaf.t
  }
[@@deriving sexp]

type response =
  { version : version
  ; request_id : string
  ; action : action
  }
[@@deriving sexp, compare, equal]

exception Decode_error of string

let fail message = raise_notrace (Decode_error message)

let kind_to_string = function
  | Before_interceptor -> "before_interceptor"
  | After_interceptor -> "after_interceptor"
  | Reviewer -> "reviewer"
  | Effect_analyzer -> "effect_analyzer"
  | Audit_filter -> "audit_filter"
;;

let rec canonicalize = function
  | `Object fields ->
    `Object
      (List.map fields ~f:(fun (name, value) -> name, canonicalize value)
       |> List.sort ~compare:(fun (left, _) (right, _) -> String.compare left right))
  | `Array values -> `Array (List.map values ~f:canonicalize)
  | (`Null | `False | `True | `String _ | `Number _) as value -> value
;;

let encode_request request =
  `Object
    [ "hook_id", `String request.hook_id
    ; "kind", `String (kind_to_string request.kind)
    ; "payload", request.payload
    ; "request_id", `String request.request_id
    ; "version", `Number "1"
    ]
  |> canonicalize
  |> Jsonaf.to_string
;;

let validate_no_duplicates value =
  let rec validate = function
    | `Object fields ->
      let names = String.Hash_set.create () in
      List.iter fields ~f:(fun (name, value) ->
        if Hash_set.mem names name then fail ("duplicate field: " ^ name);
        Hash_set.add names name;
        validate value)
    | `Array values -> List.iter values ~f:validate
    | `Null | `False | `True | `String _ | `Number _ -> ()
  in
  try
    validate value;
    Ok ()
  with
  | Decode_error message -> Error message
;;

let is_valid_utf8 source =
  let decoder = Uutf.decoder ~encoding:`UTF_8 (`String source) in
  let rec loop () =
    match Uutf.decode decoder with
    | `Uchar _ -> loop ()
    | `End -> true
    | `Malformed _ -> false
    | `Await -> assert false
  in
  loop ()
;;

let object_ = function
  | `Object fields -> fields
  | _ -> fail "expected JSON object"
;;

let record fields ~allowed ~required =
  let names = List.map fields ~f:fst |> String.Set.of_list in
  let unknown = Set.diff names (String.Set.of_list allowed) in
  let missing = Set.diff (String.Set.of_list required) names in
  if not (Set.is_empty unknown) then fail ("unknown field: " ^ Set.min_elt_exn unknown);
  if not (Set.is_empty missing) then fail ("missing field: " ^ Set.min_elt_exn missing);
  fields
;;

let field fields name =
  List.Assoc.find fields name ~equal:String.equal
  |> Option.value_or_thunk ~default:(fun () -> fail ("missing field: " ^ name))
;;

let string = function
  | `String value -> value
  | _ -> fail "expected string"
;;

let int = function
  | `Number value ->
    (match Int.of_string_opt value with
     | Some value -> value
     | None -> fail "expected integer")
  | _ -> fail "expected integer"
;;

let float = function
  | `Number value ->
    (match Float.of_string_opt value with
     | Some value when Float.is_finite value -> value
     | Some _ | None -> fail "expected finite number")
  | _ -> fail "expected finite number"
;;

let strings = function
  | `Array values -> List.map values ~f:string
  | _ -> fail "expected string array"
;;

let argv fields =
  let program = field fields "program" |> string in
  let arguments = field fields "arguments" |> strings in
  let values = program :: arguments in
  if List.exists values ~f:(fun value -> String.mem value '\000') then fail "argv contains NUL";
  values
;;

let status value =
  let fields = object_ value in
  let fields = record fields ~allowed:[ "exited"; "signaled" ] ~required:[] in
  match List.Assoc.find fields "exited" ~equal:String.equal,
        List.Assoc.find fields "signaled" ~equal:String.equal
  with
  | Some value, None -> Exited (int value)
  | None, Some value -> Signaled (int value)
  | _ -> fail "status requires exactly one of exited or signaled"
;;

let result_fields fields constructor =
  let allowed = [ "action"; "status"; "stdout"; "stderr" ] in
  let fields = record fields ~allowed ~required:allowed in
  constructor
    ~status:(field fields "status" |> status)
    ~stdout:(field fields "stdout" |> string)
    ~stderr:(field fields "stderr" |> string)
;;

let before_action fields action =
  match action with
  | "continue" ->
    ignore (record fields ~allowed:[ "action" ] ~required:[ "action" ] : _ list);
    Continue
  | "rewrite" ->
    Rewrite (record fields ~allowed:[ "action"; "program"; "arguments" ]
               ~required:[ "action"; "program"; "arguments" ] |> argv)
  | "respond" ->
    result_fields fields (fun ~status ~stdout ~stderr -> Respond { status; stdout; stderr })
  | "reject" ->
    let fields = record fields ~allowed:[ "action"; "reason" ] ~required:[ "action"; "reason" ] in
    Reject (field fields "reason" |> string)
  | _ -> fail "action is not valid for a before interceptor"
;;

let after_action fields action =
  match action with
  | "replace_result" ->
    result_fields fields (fun ~status ~stdout ~stderr -> Replace_result { status; stdout; stderr })
  | "reject" ->
    let fields = record fields ~allowed:[ "action"; "reason" ] ~required:[ "action"; "reason" ] in
    Reject (field fields "reason" |> string)
  | "continue" ->
    ignore (record fields ~allowed:[ "action" ] ~required:[ "action" ] : _ list);
    Continue
  | _ -> fail "action is not valid for an after interceptor"
;;

let reviewer_action fields =
  let decision = field fields "decision" |> string in
  match decision with
  | "defer" -> Defer
  | "approve_once" -> Approve_once
  | "deny" -> Deny (field fields "reason" |> string)
  | "rewrite" -> Reviewer_rewrite (argv fields)
  | "approve_scope" ->
    let expires_at = Option.map (List.Assoc.find fields "expires_at" ~equal:String.equal) ~f:float in
    Approve_scope { scope = field fields "scope" |> string; expires_at }
  | _ -> fail "unknown reviewer decision"
;;

let validate_reviewer_fields fields action =
  let allowed, required =
    match action with
    | Defer | Approve_once -> [ "decision" ], [ "decision" ]
    | Deny _ -> [ "decision"; "reason" ], [ "decision"; "reason" ]
    | Reviewer_rewrite _ ->
      [ "decision"; "program"; "arguments" ], [ "decision"; "program"; "arguments" ]
    | Approve_scope _ ->
      [ "decision"; "scope"; "expires_at" ], [ "decision"; "scope" ]
    | _ -> assert false
  in
  ignore (record fields ~allowed ~required : _ list);
  action
;;

let effects_action fields action =
  let fields = record fields ~allowed:[ "action"; "effects" ] ~required:[ "action"; "effects" ] in
  let effects = field fields "effects" |> strings in
  match action with
  | "add_effects" -> Add_effects effects
  | "replace_effects" -> Replace_effects effects
  | _ -> fail "action is not valid for an effect analyzer"
;;

let audit_action fields action =
  match action with
  | "keep" ->
    ignore (record fields ~allowed:[ "action" ] ~required:[ "action" ] : _ list);
    Audit_keep
  | "drop_field" ->
    let fields =
      record fields ~allowed:[ "action"; "field" ] ~required:[ "action"; "field" ]
    in
    Audit_drop_field (field fields "field" |> string)
  | "replace_fields" ->
    let fields =
      record fields ~allowed:[ "action"; "fields" ] ~required:[ "action"; "fields" ]
    in
    let values = field fields "fields" |> object_ in
    let values = List.map values ~f:(fun (name, value) -> name, string value) in
    Audit_replace_fields (String.Map.of_alist_exn values)
  | _ -> fail "action is not valid for an audit filter"
;;

let decode_action kind fields =
  match kind with
  | Reviewer -> reviewer_action fields |> validate_reviewer_fields fields
  | Before_interceptor -> before_action fields (field fields "action" |> string)
  | After_interceptor -> after_action fields (field fields "action" |> string)
  | Effect_analyzer -> effects_action fields (field fields "action" |> string)
  | Audit_filter -> audit_action fields (field fields "action" |> string)
;;

let response kind json =
  let fields = object_ json in
  let common = [ "version"; "request_id" ] in
  let action_fields = List.filter fields ~f:(fun (name, _) -> not (List.mem common name ~equal:String.equal)) in
  let version = field fields "version" |> int in
  if not (Int.equal version 1) then fail "unsupported protocol version";
  { version = V1
  ; request_id = field fields "request_id" |> string
  ; action = decode_action kind action_fields
  }
;;

let decode_response ~kind source =
  try
    if not (is_valid_utf8 source) then fail "hook output is not valid UTF-8";
    let json = Jsonaf.parse source |> Or_error.ok_exn in
    (match validate_no_duplicates json with
     | Ok () -> ()
     | Error message -> fail message);
    Ok (response kind json)
  with
  | Decode_error message -> Error message
  | exn -> Error (Exn.to_string exn)
;;

module For_testing = struct
  let canonicalize = canonicalize
  let validate_no_duplicates = validate_no_duplicates
end
