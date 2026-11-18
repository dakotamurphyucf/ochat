open! Core
module P = Agent_protocol
module D = Document_schema
module X = Persistence_codec
module Cause = Policy_cause_data
module Member = Cause.Member
module Target = Cause.Goal_target
module Coverage = Cause.Coverage
module Work = Cause.Allocation_work
module Fact = Goal_descendants.Canonical.Fact
module Marks = Canonical_usage_fold.Marks
module U = Goal_usage

let invalid message = Error (P.Error.invalid_request message)

let domain result =
  Result.map_error result ~f:(fun error ->
    P.Error.invalid_request (Sexp.to_string_hum (D.Error.sexp_of_t error)))
;;

let digest raw = Digestif.SHA256.(to_hex (digest_string (Jsonaf.to_string raw)))
let max_causes = 256
let max_memberships = 1024
let max_streams = 64
let max_bindings = 256

let limits =
  D.Limits.create ~max_bytes:4194304 ~max_depth:80 ~max_fields:280000 ~max_nodes:560000
  |> domain
  |> Result.map_error ~f:(fun error -> error.P.Error.message)
  |> Result.ok_or_failwith
;;

let projection_limits =
  D.Limits.create ~max_bytes:262144 ~max_depth:80 ~max_fields:16384 ~max_nodes:32768
  |> domain
  |> Result.map_error ~f:(fun error -> error.P.Error.message)
  |> Result.ok_or_failwith
;;

let ledger result =
  Result.map_error result ~f:(fun error ->
    P.Error.invalid_request (Sexp.to_string_hum (Inference_ledger.Error.sexp_of_t error)))
;;

let fold result =
  Result.map_error result ~f:(fun error ->
    P.Error.invalid_request
      (Sexp.to_string_hum (Canonical_usage_fold.Error.sexp_of_t error)))
;;

let counter result =
  Result.map_error result ~f:(fun (_ : Goal_accounting_counter.error) ->
    P.Error.invalid_request "policy usage count exhausted")
;;

let add a b =
  let open Result.Let_syntax in
  let%bind a = Goal_accounting_counter.of_int64 a |> counter in
  let%bind b = Goal_accounting_counter.of_int64 b |> counter in
  let%map n = Goal_accounting_counter.add a b |> counter in
  Goal_accounting_counter.to_int64 n
;;

let increment n = add n 1L

let marks_json (t : Marks.t) =
  `Object
    [ "attempt", X.int64_json t.attempt
    ; "turn", X.int64_json t.turn
    ; "untracked_attempts", X.int64_json t.untracked_attempts
    ; "untracked_turns", X.int64_json t.untracked_turns
    ]
;;

let marks_of_json raw =
  let open Result.Let_syntax in
  let%bind f = X.object_ raw in
  let%bind attempt = X.required f "attempt" X.nonnegative_int64 in
  let%bind turn = X.required f "turn" X.nonnegative_int64 in
  let%bind untracked_attempts = X.required f "untracked_attempts" X.nonnegative_int64 in
  let%bind untracked_turns = X.required f "untracked_turns" X.nonnegative_int64 in
  if Int64.(untracked_attempts > attempt || untracked_turns > turn)
  then invalid "policy canonical marks differ"
  else Ok Marks.{ attempt; turn; untracked_attempts; untracked_turns }
;;

let marks_shape =
  X.fields_shape [ "attempt"; "turn"; "untracked_attempts"; "untracked_turns" ]
;;

module Stream = struct
  type t =
    { id : string
    ; caller : P.Session_ref.t
    ; caller_source : Policy_caller_source.t
    ; callee : P.Session_ref.t
    ; callee_source : P.Run_source.t
    ; path : Agent_store.Delegation_store.Reference.t list
    ; target : Target.t
    ; forwarding_route : Cause.Forwarding.Route.t option
    ; raw : Jsonaf.t option
    }

  let descriptor t =
    `Object
      ([ "caller", P.Session_ref.to_json t.caller
       ; "caller_source", Policy_caller_source.to_json t.caller_source
       ; "callee", P.Session_ref.to_json t.callee
       ; "callee_source", P.Run_source.to_json t.callee_source
       ; ( "path"
         , `Array (List.map t.path ~f:Agent_store.Delegation_store.reference_to_jsonaf) )
       ; "target", Target.to_json t.target
       ]
       @ Option.to_list
           (Option.map t.forwarding_route ~f:(fun route ->
              "forwarding_route", Cause.Forwarding.Route.known_json route)))
  ;;

  let known_json t =
    match descriptor t with
    | `Object fields -> `Object (("id", `String t.id) :: fields)
    | _ -> assert false
  ;;

  (* A complete raw carrier needs no reconstruction of its known children. *)
  let to_json t =
    match t.raw with
    | Some raw -> raw
    | None -> known_json t
  ;;

  let id t = t.id
  let caller t = t.caller
  let callee t = t.callee
  let callee_source t = t.callee_source
  let path t = t.path
  let target t = t.target
  let forwarding_route t = t.forwarding_route
  let equal a b = String.equal a.id b.id && D.Json.equal (descriptor a) (descriptor b)

  let of_json raw =
    let open Result.Let_syntax in
    let%bind () = D.Json.validate raw ~limits:Cause.limits |> domain in
    let%bind f = X.object_ raw in
    let%bind id =
      X.required f "id" Cause.Key.of_json |> Result.map ~f:Cause.Key.to_string
    in
    let%bind caller = X.required f "caller" P.Session_ref.of_json in
    let%bind caller_source = X.required f "caller_source" Policy_caller_source.of_json in
    let%bind callee = X.required f "callee" P.Session_ref.of_json in
    let%bind callee_source = X.required f "callee_source" P.Run_source.of_json in
    let%bind path =
      X.required
        f
        "path"
        (X.list (fun raw ->
           Agent_store.Delegation_store.reference_of_jsonaf raw
           |> Result.map_error ~f:Agent_store.Store_error.to_protocol_error))
    in
    let%bind target = X.required f "target" Target.of_json in
    let%bind forwarding_route =
      P.Json_codec.optional_as f "forwarding_route" Cause.Forwarding.Route.of_json
    in
    let%bind () = P.Run_source.validate callee_source in
    let%bind () = Cause.validate_path ~caller ~callee ~callee_source ~path in
    let%bind () =
      match forwarding_route with
      | Some route ->
        let%bind () =
          if Target.equal target (Cause.Forwarding.Route.target route)
          then Ok ()
          else invalid "forwarded stream target differs"
        in
        Cause.Forwarding.Route.check_next
          route
          ~caller
          ~caller_source
          ~callee
          ~callee_source
          ~path
      | None ->
        (match Target.view target with
         | Inherited _ -> Ok ()
         | Local _ ->
           if
             P.Session_ref.equal caller (Target.owner target)
             && Policy_caller_source.matches_installed
                  caller_source
                  (Target.source target)
           then Ok ()
           else invalid "policy stream caller-local target differs")
    in
    let t =
      { id
      ; caller
      ; caller_source
      ; callee
      ; callee_source
      ; path
      ; target
      ; forwarding_route
      ; raw = Some raw
      }
    in
    if String.equal id (digest (descriptor t))
    then Ok t
    else invalid "policy stream descriptor identity differs"
  ;;

  let of_cause cause target =
    let forwarding_route =
      Option.bind (Cause.forwarding cause) ~f:(fun forwarded ->
        if List.mem (Cause.Forwarding.targets forwarded) target ~equal:Target.equal
        then
          Some
            (Cause.Forwarding.for_target forwarded ~target
             |> Result.map ~f:Cause.Forwarding.Witness.route
             |> Result.map_error ~f:(fun error -> error.P.Error.message)
             |> Result.ok_or_failwith)
        else None)
    in
    let t =
      { id = ""
      ; caller = Cause.caller cause
      ; caller_source = Cause.caller_source cause
      ; callee = Cause.callee cause
      ; callee_source = Cause.callee_source cause
      ; path = Cause.path cause
      ; target
      ; forwarding_route
      ; raw = None
      }
    in
    let t = { t with id = digest (descriptor t) } in
    (* Native identity is known-only. Copy required complete source children into
       the immutable raw descriptor; carriers admit these through decode/adopt. *)
    let assembled =
      let open Result.Let_syntax in
      let%bind source = X.object_ (Cause.to_json cause) in
      let%bind fields = X.object_ (known_json t) in
      let%bind raw_targets = X.required source "targets" (X.list (fun raw -> Ok raw)) in
      let%bind target_children =
        List.map raw_targets ~f:(fun raw ->
          let%map value = Target.of_json raw in
          value, raw)
        |> Result.all
      in
      let%bind target_raw =
        List.find target_children ~f:(fun (value, _) -> Target.equal value target)
        |> Result.of_option
             ~error:(P.Error.invalid_request "stream target absent from validated cause")
        |> Result.map ~f:snd
      in
      let%bind copied =
        List.map
          [ "caller"; "caller_source"; "callee"; "callee_source"; "path" ]
          ~f:(fun key ->
            let%map raw = X.required source key X.raw in
            key, raw)
        |> Result.all
      in
      let replacements =
        (("target", target_raw) :: copied)
        @ Option.to_list
            (Option.map forwarding_route ~f:(fun route ->
               "forwarding_route", Cause.Forwarding.Route.to_json route))
      in
      let raw =
        `Object
          (List.map (P.Json_codec.to_alist fields) ~f:(fun (key, value) ->
             ( key
             , Option.value
                 (List.Assoc.find replacements key ~equal:String.equal)
                 ~default:value )))
      in
      of_json raw
    in
    assembled
    |> Result.map_error ~f:(fun error -> error.P.Error.message)
    |> Result.ok_or_failwith
  ;;

  let shape =
    X.shape_exn
      [ "id", D.Shape.value
      ; "caller", X.fields_shape [ "server_id"; "session_id" ]
      ; "caller_source", Policy_caller_source.shape
      ; "callee", X.fields_shape [ "server_id"; "session_id" ]
      ; "callee_source", Run_record_shapes.source
      ; "path", X.array_shape_exn Agent_store.Delegation_store.reference_shape
      ; "target", Target.shape
      ; "forwarding_route", Cause.Forwarding.Route.shape
      ]
  ;;
end

module Choice = struct
  type t =
    { target : Target.t
    ; coverage : Coverage.t
    }

  let target t = t.target
  let coverage t = t.coverage
  let equal a b = Target.equal a.target b.target && Coverage.equal a.coverage b.coverage

  let to_json t =
    `Object [ "target", Target.to_json t.target; "coverage", Coverage.to_json t.coverage ]
  ;;

  let of_json raw =
    let open Result.Let_syntax in
    let%bind f = X.object_ raw in
    let%bind target = X.required f "target" Target.of_json in
    let%map coverage = X.required f "coverage" Coverage.of_json in
    { target; coverage }
  ;;

  let shape = X.shape_exn [ "target", Target.shape; "coverage", D.Shape.value ]
end

module Head = struct
  type t =
    { cause : Cause.Key.t
    ; work : Work.t
    ; canonical : Fact.t
    }

  let to_json t =
    `Object
      [ "cause", Cause.Key.to_json t.cause
      ; "work", Work.to_json t.work
      ; "canonical", Fact.to_json t.canonical
      ]
  ;;

  let of_json raw =
    let open Result.Let_syntax in
    let%bind f = X.object_ raw in
    let%bind cause = X.required f "cause" Cause.Key.of_json in
    let%bind work = X.required f "work" Work.of_json in
    let%map canonical = X.required f "canonical" Fact.of_json in
    { cause; work; canonical }
  ;;

  let shape =
    X.shape_exn [ "cause", D.Shape.value; "work", Work.shape; "canonical", Fact.shape ]
  ;;
end

module Membership = struct
  type t =
    { member : Member.t
    ; cause : Cause.Key.t
    ; parent : Member.t option
    ; projection_sha256 : string
    }

  let to_json t =
    `Object
      [ "id", `String (Member.key t.member)
      ; "member", Member.to_json t.member
      ; "cause", Cause.Key.to_json t.cause
      ; "parent", X.option_json Member.to_json t.parent
      ; "projection_sha256", `String t.projection_sha256
      ]
  ;;

  let of_json raw =
    let open Result.Let_syntax in
    let%bind f = X.object_ raw in
    let%bind member = X.required f "member" Member.of_json in
    let%bind id = X.required f "id" P.Json_codec.string in
    let%bind cause = X.required f "cause" Cause.Key.of_json in
    let%bind parent = X.required f "parent" (X.nullable Member.of_json) in
    let%bind projection_sha256 =
      X.required f "projection_sha256" Cause.Key.of_json
      |> Result.map ~f:Cause.Key.to_string
    in
    if String.equal id (Member.key member)
    then Ok { member; cause; parent; projection_sha256 }
    else invalid "policy membership key differs"
  ;;

  let shape =
    X.shape_exn
      [ "id", D.Shape.value
      ; "member", Member.shape
      ; "cause", D.Shape.value
      ; "parent", D.Shape.nullable Member.shape
      ; "projection_sha256", D.Shape.value
      ]
  ;;
end

module Binding = struct
  type t =
    { ordinal : int64
    ; cause : Cause.Key.t
    ; work : Work.t
    ; canonical : Fact.t
    ; choices : Choice.t list
    }

  let ordinal t = t.ordinal
  let cause t = t.cause
  let choices t = t.choices

  let to_json t =
    `Object
      [ "id", X.int64_json t.ordinal
      ; "cause", Cause.Key.to_json t.cause
      ; "work", Work.to_json t.work
      ; "canonical", Fact.to_json t.canonical
      ; "choices", `Array (List.map t.choices ~f:Choice.to_json)
      ]
  ;;

  let of_json raw =
    let open Result.Let_syntax in
    let%bind f = X.object_ raw in
    let%bind ordinal = X.required f "id" X.nonnegative_int64 in
    let%bind cause = X.required f "cause" Cause.Key.of_json in
    let%bind work = X.required f "work" Work.of_json in
    let%bind canonical = X.required f "canonical" Fact.of_json in
    let%bind choices = X.required f "choices" (X.list Choice.of_json) in
    if Int64.(ordinal > 0L)
    then Ok { ordinal; cause; work; canonical; choices }
    else invalid "nonpositive policy ordinal"
  ;;

  let shape =
    X.shape_exn
      [ "id", D.Shape.value
      ; "cause", D.Shape.value
      ; "work", Work.shape
      ; "canonical", Fact.shape
      ; "choices", X.array_shape_exn Choice.shape
      ]
  ;;
end

module Partition = struct
  type t =
    { stream : Stream.t
    ; settled : U.t
    ; admitted_attempts : int64
    ; head : Head.t option
    }

  let to_json t =
    `Object
      [ "id", `String t.stream.id
      ; "stream", Stream.known_json t.stream
      ; "settled", U.Persistence.to_json t.settled
      ; "admitted_attempts", X.int64_json t.admitted_attempts
      ; "head", X.option_json Head.to_json t.head
      ]
  ;;

  let of_json raw =
    let open Result.Let_syntax in
    let%bind f = X.object_ raw in
    let%bind id = X.required f "id" P.Json_codec.string in
    let%bind stream = X.required f "stream" Stream.of_json in
    let%bind settled = X.required f "settled" U.Persistence.of_json in
    let%bind admitted_attempts = X.required f "admitted_attempts" X.nonnegative_int64 in
    let%bind head = X.required f "head" (X.nullable Head.of_json) in
    if String.equal id stream.id
    then Ok { stream; settled; admitted_attempts; head }
    else invalid "policy partition key differs"
  ;;

  let shape =
    X.shape_exn
      [ "id", D.Shape.value
      ; "stream", Stream.shape
      ; "settled", U.Persistence.shape
      ; "admitted_attempts", D.Shape.value
      ; "head", D.Shape.nullable Head.shape
      ]
  ;;
end

type t =
  { callee : P.Session_ref.t
  ; max_inherited_scopes : int
  ; basis_revision : int64
  ; basis_marks : Marks.t
  ; ledger_revision : int64
  ; marks : Marks.t
  ; causes : Cause.t list
  ; memberships : Membership.t list
  ; streams : Partition.t list
  ; bindings : Binding.t list
  }

let to_json t =
  `Object
    [ "callee", P.Session_ref.to_json t.callee
    ; "max_inherited_scopes", X.host_counter_to_json t.max_inherited_scopes
    ; "basis_revision", X.int64_json t.basis_revision
    ; "basis_marks", marks_json t.basis_marks
    ; "ledger_revision", X.int64_json t.ledger_revision
    ; "marks", marks_json t.marks
    ; "causes", `Array (List.map t.causes ~f:Cause.known_json)
    ; "memberships", `Array (List.map t.memberships ~f:Membership.to_json)
    ; "streams", `Array (List.map t.streams ~f:Partition.to_json)
    ; "bindings", `Array (List.map t.bindings ~f:Binding.to_json)
    ]
;;

let equal a b = D.Json.equal (to_json a) (to_json b)

let find_cause t key =
  List.find t.causes ~f:(fun cause -> Cause.Key.equal (Cause.key cause) key)
;;

let find_member t member =
  List.find t.memberships ~f:(fun row -> Member.equal row.Membership.member member)
;;

let find_stream t id =
  List.find t.streams ~f:(fun row -> String.equal row.Partition.stream.id id)
;;

let ordinal fact =
  match Goal_descendant_occurrence.kind (Fact.occurrence fact) with
  | Attempt { ordinal; _ } -> Ok ordinal
  | Host_turn _ -> invalid "policy usage cannot charge a host Turn"
;;

let overlay (choice : Choice.t) = Coverage.equal choice.coverage Causal_overlay

let selected_with ~stream_of_cause t (binding : Binding.t) stream =
  match find_cause t binding.cause with
  | None -> false
  | Some cause ->
    List.exists binding.choices ~f:(fun choice ->
      overlay choice && Stream.equal (stream_of_cause cause choice.target) stream)
;;

let selected t binding stream =
  selected_with ~stream_of_cause:Stream.of_cause t binding stream
;;

type stream_derivation =
  { cause : Cause.Key.t
  ; target : Target.t
  ; stream : Stream.t
  }

let validate t =
  let open Result.Let_syntax in
  let%bind () =
    if
      t.max_inherited_scopes < 0
      || t.max_inherited_scopes = Int.max_value
      || List.length t.causes > max_causes
      || List.length t.memberships > max_memberships
      || List.length t.streams > max_streams
      || List.length t.bindings > max_bindings
      || Int64.(
           t.basis_revision < 0L
           || t.ledger_revision < t.basis_revision
           || t.marks.attempt < t.basis_marks.attempt
           || t.marks.turn < t.basis_marks.turn
           || t.marks.untracked_attempts < t.basis_marks.untracked_attempts
           || t.marks.untracked_turns < t.basis_marks.untracked_turns)
    then invalid "policy capacity or canonical frontier differs"
    else Ok ()
  in
  let unique values ~key =
    if Option.is_none (List.find_a_dup (List.map values ~f:key) ~compare:String.compare)
    then Ok ()
    else invalid "duplicate policy identity"
  in
  let%bind () =
    unique t.causes ~key:(fun cause -> Cause.Key.to_string (Cause.key cause))
  in
  let%bind () =
    unique t.memberships ~key:(fun row ->
      Sexp.to_string (Member.sexp_of_kind (Member.kind row.Membership.member))
      ^ ":"
      ^ Member.id row.member)
  in
  let%bind () = unique t.streams ~key:(fun row -> row.Partition.stream.id) in
  let%bind () = unique t.bindings ~key:(fun row -> Int64.to_string row.Binding.ordinal) in
  let%bind () =
    List.fold_result t.causes ~init:() ~f:(fun () cause ->
      if
        P.Session_ref.equal t.callee (Cause.callee cause)
        && Int.equal t.max_inherited_scopes (Cause.max_inherited_scopes cause)
      then Ok ()
      else invalid "policy cause host profile differs")
  in
  let%bind () =
    List.fold_result t.memberships ~init:() ~f:(fun () row ->
      let%bind cause =
        Result.of_option
          (find_cause t row.cause)
          ~error:(P.Error.invalid_request "policy membership cause absent")
      in
      if
        not
          (Int.equal
             (Member.generation row.member)
             (Cause.callee_source cause).generation)
      then invalid "policy member generation differs"
      else (
        match row.parent with
        | None ->
          let%bind expected =
            Member.event
              (Cause.parent_event cause)
              ~generation:(Cause.callee_source cause).generation
          in
          if Member.equal row.member expected
          then Ok ()
          else invalid "nonseed policy member lacks parent"
        | Some parent ->
          (match find_member t parent with
           | Some parent
             when Cause.Key.equal parent.cause row.cause
                  && not (Member.equal parent.member row.member) -> Ok ()
           | Some _ | None -> invalid "policy member parent differs")))
  in
  let rec chain seen row =
    if List.mem seen row.Membership.member ~equal:Member.equal
    then invalid "cyclic policy membership"
    else (
      match row.parent with
      | None -> Ok ()
      | Some parent ->
        (match find_member t parent with
         | None -> invalid "policy member parent absent"
         | Some parent -> chain (row.member :: seen) parent))
  in
  let%bind () = List.fold_result t.memberships ~init:() ~f:(fun () row -> chain [] row) in
  let check_work cause_key work fact =
    let%bind cause =
      Result.of_option
        (find_cause t cause_key)
        ~error:(P.Error.invalid_request "policy head/binding cause absent")
    in
    let%bind member =
      Work.member work ~generation:(Cause.callee_source cause).generation
    in
    let%bind () =
      match find_member t member with
      | Some row when Cause.Key.equal row.cause cause_key -> Ok ()
      | Some _ | None -> invalid "policy head/binding member absent"
    in
    let%bind n = ordinal fact in
    let frame = Goal_descendant_occurrence.frame (Fact.occurrence fact) in
    if
      P.Session_ref.equal t.callee (Goal_descendant_occurrence.Frame.session frame)
      && Int.equal
           (Cause.callee_source cause).generation
           (Goal_descendant_occurrence.Frame.generation frame)
      && Int64.(n > t.basis_marks.attempt && n <= t.marks.attempt)
    then Ok cause
    else invalid "policy canonical fact scope differs"
  in
  (* All causes come from this same immutable input after the existing unique-key
     and membership checks. Retain only successful, actually visited pairs; no
     result survives this validation or substitutes for a raw child validator. *)
  let derivations : stream_derivation list ref = ref [] in
  let stream_of_cause cause target =
    let key = Cause.key cause in
    match
      List.find !derivations ~f:(fun derived ->
        Cause.Key.equal derived.cause key && Target.equal derived.target target)
    with
    | Some derived -> derived.stream
    | None ->
      let stream = Stream.of_cause cause target in
      derivations := { cause = key; target; stream } :: !derivations;
      stream
  in
  let%bind () =
    List.fold_result t.bindings ~init:() ~f:(fun () binding ->
      let%bind cause = check_work binding.cause binding.work binding.canonical in
      let%bind n = ordinal binding.canonical in
      if
        (not (Int64.equal n binding.ordinal))
        || not
             (List.equal
                Target.equal
                (Cause.targets cause)
                (List.map binding.choices ~f:Choice.target))
      then invalid "policy immutable binding target vector differs"
      else
        List.fold_result binding.choices ~init:() ~f:(fun () choice ->
          if
            (not (overlay choice))
            || Option.is_some (find_stream t (stream_of_cause cause choice.target).id)
          then Ok ()
          else invalid "policy overlay stream absent"))
  in
  List.fold_result t.streams ~init:() ~f:(fun () row ->
    let%bind () =
      match row.head with
      | None -> invalid "policy stream lacks latest actual head"
      | Some head ->
        let%bind cause = check_work head.cause head.work head.canonical in
        if
          not
            (List.exists (Cause.targets cause) ~f:(fun target ->
               Stream.equal row.stream (stream_of_cause cause target)))
        then invalid "policy head stream/cause differs"
        else Ok ()
    in
    let retained =
      List.count t.bindings ~f:(fun binding ->
        selected_with ~stream_of_cause t binding row.stream)
      |> Int64.of_int
    in
    let%bind count = add (U.attempts row.settled) retained in
    if
      Int64.equal count row.admitted_attempts
      && Int64.(
           count > 0L
           && count <= t.marks.attempt - t.basis_marks.attempt
           && U.untracked_attempts row.settled
              <= t.marks.untracked_attempts - t.basis_marks.untracked_attempts)
    then Ok ()
    else invalid "policy stream count differs from exact subset")
;;

let of_json raw =
  let open Result.Let_syntax in
  let%bind () = D.Json.validate raw ~limits |> domain in
  let%bind f = X.object_ raw in
  let%bind callee = X.required f "callee" P.Session_ref.of_json in
  let%bind max_inherited_scopes =
    X.required f "max_inherited_scopes" X.host_counter_of_json
  in
  let%bind basis_revision = X.required f "basis_revision" X.nonnegative_int64 in
  let%bind basis_marks = X.required f "basis_marks" marks_of_json in
  let%bind ledger_revision = X.required f "ledger_revision" X.nonnegative_int64 in
  let%bind marks = X.required f "marks" marks_of_json in
  let%bind causes = X.required f "causes" (X.list Cause.of_json) in
  let%bind memberships = X.required f "memberships" (X.list Membership.of_json) in
  let%bind streams = X.required f "streams" (X.list Partition.of_json) in
  let%bind bindings = X.required f "bindings" (X.list Binding.of_json) in
  let t =
    { callee
    ; max_inherited_scopes
    ; basis_revision
    ; basis_marks
    ; ledger_revision
    ; marks
    ; causes
    ; memberships
    ; streams
    ; bindings
    }
  in
  let%map () = validate t in
  t
;;

let shape =
  X.shape_exn
    [ "callee", X.fields_shape [ "server_id"; "session_id" ]
    ; "max_inherited_scopes", D.Shape.value
    ; "basis_revision", D.Shape.value
    ; "basis_marks", marks_shape
    ; "ledger_revision", D.Shape.value
    ; "marks", marks_shape
    ; "causes", X.array_shape_exn ~identity_field:"id" Cause.shape
    ; "memberships", X.array_shape_exn ~identity_field:"id" Membership.shape
    ; "streams", X.array_shape_exn ~identity_field:"id" Partition.shape
    ; "bindings", X.array_shape_exn ~identity_field:"id" Binding.shape
    ]
;;

(* Only immutable native associations enter a membership witness. Mutable outcome,
   input/result/configuration bodies are deliberately absent. The actual live row
   is resolved again at admission; this digest never becomes execution custody. *)
module Source_work = struct
  type t =
    { member : Member.t
    ; parent : Member.t option
    ; projection_sha256 : string
    ; ready : bool
    ; attempt : int option
    ; operation_id : P.Id.Operation.t option
    ; invocation_id : P.Id.Invocation.t option
    }

  let observer_json (observer : P.Moderator_observer.t) =
    `Object
      [ "script_id", `String observer.script_id
      ; "source_sha256", `String observer.source_sha256
      ]
  ;;

  let retained_queued_parent ~memberships cause queued =
    let open Result.Let_syntax in
    let matching =
      List.filter memberships ~f:(fun (row : Membership.t) ->
        String.equal
          (Member.id row.member)
          (P.Id.Job.to_string (Policy_native_ancestry.Queued_job.id queued))
        && Int.equal
             (Member.generation row.member)
             (Policy_native_ancestry.Queued_job.generation queued)
        &&
        match Member.kind row.member with
        | Model_job | Async_job -> true
        | Event | Invocation -> false)
    in
    let%bind selected =
      match matching with
      | [ row ] when Member.equal_kind (Member.kind row.member) Async_job -> Ok row
      | [] | _ :: _ ->
        invalid "retained queued Job lacks one exact typed Async membership"
    in
    let%bind () =
      if
        Cause.Key.equal selected.cause (Cause.key cause)
        && P.Run_source.equal
             (Cause.callee_source cause)
             (Policy_native_ancestry.Queued_job.source queued)
        && Option.is_some (Policy_native_ancestry.Queued_job.retained_delivery queued)
      then Ok ()
      else invalid "retained queued Job differs from its original Cause/source"
    in
    let%bind () =
      match Policy_native_ancestry.Queued_job.actual_job queued with
      | Some actual when Int.equal actual.generation (Member.generation selected.member)
        ->
        let%bind actual_member = Policy_native_ancestry.member_of_job actual in
        if Member.equal actual_member selected.member
        then Ok ()
        else invalid "retained queued Job kind differs from its native lifetime"
      | None | Some _ -> Ok ()
    in
    let rec chain seen (row : Membership.t) =
      if List.mem seen row.member ~equal:Member.equal
      then invalid "retained queued Job Cause chain is cyclic"
      else if not (Cause.Key.equal row.cause (Cause.key cause))
      then invalid "retained queued Job Cause chain changes owner"
      else (
        match row.parent with
        | None ->
          let%bind expected =
            Member.event
              (Cause.parent_event cause)
              ~generation:(Cause.callee_source cause).generation
          in
          if Member.equal expected row.member
          then Ok ()
          else invalid "retained queued Job Cause chain lost its seed"
        | Some parent ->
          (match
             List.filter memberships ~f:(fun (candidate : Membership.t) ->
               Member.equal candidate.member parent)
           with
           | [ parent ] -> chain (row.member :: seen) parent
           | [] | _ :: _ ->
             invalid "retained queued Job Cause parent is absent or duplicate"))
    in
    let%map () = chain [] selected in
    selected.member
  ;;

  let event_parent ~memberships cause (ancestry : Policy_native_ancestry.Event_parent.t) =
    match ancestry with
    | Root -> Ok None
    | Native member -> Ok (Some member)
    | Retained_job queued ->
      retained_queued_parent ~memberships cause queued |> Result.map ~f:Option.some
    | Unavailable -> invalid "policy queue producer history is unavailable"
  ;;

  let capture_native ~memberships (context : Policy_native_ancestry.t) cause member =
    let open Result.Let_syntax in
    let generation = Member.generation member in
    let source = Cause.callee_source cause in
    let%bind () =
      if
        P.Id.Session.equal
          context.session_id
          (P.Session_ref.session_id (Cause.callee cause))
        && Int.equal generation source.generation
        && generation <= context.generation
      then Ok ()
      else invalid "policy source-work frame differs from its Cause"
    in
    let source_matches observer = P.Moderator_observer.equal source.observer observer in
    let generation_matches actual =
      Int.equal generation actual && Int.equal generation source.generation
    in
    let project parent ready attempt operation_id invocation_id raw =
      Ok
        (Some
           { member
           ; parent
           ; projection_sha256 = digest raw
           ; ready
           ; attempt
           ; operation_id
           ; invocation_id
           })
    in
    match Member.kind member with
    | Member.Event ->
      (match
         List.find context.events ~f:(fun row ->
           String.equal
             (Member.id member)
             (P.Id.Moderator_execution.to_string row.P.Moderator_execution.context.id))
       with
       | None -> Ok None
       | Some _ ->
         let%bind row = Policy_native_ancestry.event context (Member.id member) in
         let c = row.context in
         let%bind () =
           if generation_matches c.generation && source_matches c.source
           then Ok ()
           else invalid "policy Event source/generation differs"
         in
         let%bind ancestry = Policy_native_ancestry.event_ancestry context row in
         (match ancestry with
          | Unavailable -> Ok None
          | Root | Native _ | Retained_job _ ->
            let%bind parent =
              match row.delegation, c.job with
              | Some _, None ->
                if Cause.matches_event cause row
                then Ok None
                else invalid "policy seed Event differs"
              | None, Some _ -> event_parent ~memberships cause ancestry
              | None, None when Option.is_some row.queue_occurrence ->
                let%bind () =
                  match row.queue_occurrence with
                  | Some occurrence
                    when P.Run_source.equal
                           (P.Moderator_execution.Queue_occurrence.source occurrence)
                           source -> Ok ()
                  | Some _ | None ->
                    invalid "policy queue Event installation differs from its Cause"
                in
                let%bind parent = event_parent ~memberships cause ancestry in
                if Option.is_some parent
                then Ok parent
                else invalid "external queue root cannot acquire a delegated Cause"
              | None, None | Some _, Some _ ->
                invalid "policy Event has no exact original parent"
            in
            let job =
              Option.map c.job ~f:(fun job ->
                `Object
                  [ "id", P.Id.Job.to_json job.job_id
                  ; "attempt", X.host_counter_to_json job.attempt
                  ])
            in
            let delegation =
              Option.map row.delegation ~f:(fun d ->
                `Object
                  [ "child_session_id", P.Id.Session.to_json d.child_session_id
                  ; "child_generation", X.host_counter_to_json d.child_generation
                  ; "child_invocation_id", P.Id.Invocation.to_json d.child_invocation_id
                  ; "admission_sha256", `String d.admission_sha256
                  ])
            in
            project
              parent
              (match row.status with
               | Running -> true
               | Completed _ | Failed _ | Interrupted _ -> false)
              None
              c.operation_id
              None
              (`Object
                  ([ "kind", `String "event"
                   ; "id", P.Id.Moderator_execution.to_json c.id
                   ; "session_id", P.Id.Session.to_json c.session_id
                   ; "generation", X.host_counter_to_json c.generation
                   ; "source", observer_json c.source
                   ; ( "phase"
                     , `String
                         (Sexp.to_string (P.Moderator_execution.sexp_of_phase c.phase)) )
                   ; "operation_id", X.option_json P.Id.Operation.to_json c.operation_id
                   ; "job", Option.value job ~default:`Null
                   ; "delegation", Option.value delegation ~default:`Null
                   ]
                   @
                   match row.queue_occurrence with
                   | None -> []
                   | Some occurrence ->
                     [ ( "queue_occurrence"
                       , P.Moderator_execution.Queue_occurrence.to_json occurrence )
                     ]))))
    | Invocation ->
      (match
         List.find context.invocations ~f:(fun row ->
           String.equal
             (Member.id member)
             (P.Id.Invocation.to_string row.P.Invocation.context.id))
       with
       | None -> Ok None
       | Some _ ->
         let%bind row = Policy_native_ancestry.invocation context (Member.id member) in
         let c = row.context in
         let%bind () =
           if generation_matches c.generation
           then Ok ()
           else invalid "policy Invocation generation differs"
         in
         let%bind parent = Policy_native_ancestry.invocation_parent context row in
         let ready =
           match row.status with
           | Dispatching -> true
           | Admitted | Resolved _ | Published _ -> false
         in
         project
           parent
           ready
           None
           None
           (Some c.id)
           (`Object
               [ "kind", `String "invocation"
               ; "id", P.Id.Invocation.to_json c.id
               ; "session_id", P.Id.Session.to_json c.session_id
               ; "generation", X.host_counter_to_json c.generation
               ; ( "parent_event"
                 , X.option_json P.Id.Moderator_execution.to_json row.parent_event )
               ; ( "parent_invocation"
                 , X.option_json P.Id.Invocation.to_json c.parent_invocation )
               ; "parent_job", X.option_json P.Id.Job.to_json c.parent_job
               ; ( "observer"
                 , X.option_json
                     (fun o -> observer_json o.P.Invocation.observer)
                     row.observation )
               ; ( "inference_origin"
                 , X.option_json P.Inference_origin.to_json row.inference_origin )
               ]))
    | (Model_job | Async_job) as kind ->
      (match
         List.find context.jobs ~f:(fun row ->
           String.equal (Member.id member) (P.Id.Job.to_string row.P.Job.id))
       with
       | None -> Ok None
       | Some _ ->
         let%bind row = Policy_native_ancestry.job context (Member.id member) in
         let%bind actual = Policy_native_ancestry.member_of_job row in
         let%bind () =
           if generation_matches row.generation && Member.equal actual member
           then Ok ()
           else invalid "policy Job kind/generation differs"
         in
         let%bind launch =
           Result.of_option
             row.launch
             ~error:(P.Error.invalid_request "policy Model Job launch absent")
         in
         let%bind () =
           if Option.for_all launch.moderator_source ~f:source_matches
           then Ok ()
           else invalid "policy Model Job source differs"
         in
         let%bind parent = Policy_native_ancestry.job_parent context row in
         let ready =
           match row.status with
           | Running -> true
           | Queued
           | Waiting_permission _
           | Waiting_completion _
           | Succeeded
           | Failed _
           | Cancelled
           | Interrupted _ -> false
         in
         let launch_json =
           match D.Json.field (P.Job.to_json row) ~name:"launch" with
           | Value raw -> raw
           | Absent | Null -> `Null
         in
         project
           parent
           ready
           (Some row.attempt)
           None
           None
           (`Object
               [ ( "kind"
                 , `String
                     (match kind with
                      | Model_job -> "model_job"
                      | Async_job -> "async_job"
                      | Event | Invocation -> assert false) )
               ; "id", P.Id.Job.to_json row.id
               ; "session_id", P.Id.Session.to_json row.session_id
               ; "generation", X.host_counter_to_json row.generation
               ; "launch", launch_json
               ; ( "inference_origin"
                 , X.option_json P.Inference_origin.to_json row.inference_origin )
               ]))
  ;;

  let capture ~memberships context cause member =
    capture_native ~memberships (Policy_native_ancestry.of_context context) cause member
  ;;

  let matches (row : Membership.t) t =
    Member.equal row.member t.member
    && Option.equal Member.equal row.parent t.parent
    && String.equal row.projection_sha256 t.projection_sha256
  ;;
end
