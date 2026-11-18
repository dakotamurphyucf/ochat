open! Core
module P = Agent_protocol
module D = Document_schema
module X = Persistence_codec
module PC = Policy_canonical_usage
module Receipt = Goal_policy_receipt
module Pending = Receipt.Pending

let invalid = Receipt.invalid
let domain = Receipt.domain
let max_books = 16
let max_streams = 64

let make_limits ~bytes ~fields ~nodes =
  D.Limits.create ~max_bytes:bytes ~max_depth:96 ~max_fields:fields ~max_nodes:nodes
  |> domain
  |> Result.map_error ~f:(fun error -> error.P.Error.message)
  |> Result.ok_or_failwith
;;

let limits = make_limits ~bytes:4194304 ~fields:280000 ~nodes:560000
let slot_limits = make_limits ~bytes:1048576 ~fields:70000 ~nodes:140000

module History = struct
  type t =
    | Known_from_creation
    | Historical_unknown
  [@@deriving equal, sexp_of]

  let to_json = function
    | Known_from_creation -> `String "known_from_creation"
    | Historical_unknown -> `String "historical_unknown"
  ;;

  let of_json = function
    | `String "known_from_creation" -> Ok Known_from_creation
    | `String "historical_unknown" -> Ok Historical_unknown
    | _ -> invalid "unknown causal Goal history coverage"
  ;;
end

module Last = struct
  type t =
    | Confirmed of Pending.t
    | No_writer_closed of Pending.t

  let pending = function
    | Confirmed p | No_writer_closed p -> p
  ;;

  let equal a b =
    match a, b with
    | Confirmed a, Confirmed b | No_writer_closed a, No_writer_closed b ->
      Pending.equal a b
    | Confirmed _, No_writer_closed _ | No_writer_closed _, Confirmed _ -> false
  ;;

  let to_json t =
    `Object
      [ ( "kind"
        , `String
            (match t with
             | Confirmed _ -> "confirmed"
             | No_writer_closed _ -> "no_writer_closed") )
      ; "pending", Pending.to_json (pending t)
      ]
  ;;

  let of_json raw =
    let open Result.Let_syntax in
    let%bind f = X.object_ raw in
    let%bind p = X.required f "pending" Pending.of_json in
    let%bind kind = X.required f "kind" P.Json_codec.string in
    match kind with
    | "confirmed" -> Ok (Confirmed p)
    | "no_writer_closed" -> Ok (No_writer_closed p)
    | _ -> invalid "unknown causal terminal receipt"
  ;;

  let shape = X.shape_exn [ "kind", D.Shape.value; "pending", Pending.shape ]
end

module Slot = struct
  type t =
    { stream : PC.Stream.t
    ; confirmed_prefix : int64
    ; last_nonce : int64
    ; pending : Pending.t option
    ; last : Last.t option
    ; projection : PC.Projection.t option
    }

  let to_json t =
    `Object
      [ "id", `String (PC.Stream.id t.stream)
      ; "kind", `String "remote"
      ; "stream", PC.Stream.known_json t.stream
      ; "confirmed_prefix", X.int64_json t.confirmed_prefix
      ; "last_nonce", X.int64_json t.last_nonce
      ; "pending", X.option_json Pending.to_json t.pending
      ; "last", X.option_json Last.to_json t.last
      ; "projection", X.option_json PC.Projection.known_json t.projection
      ]
  ;;

  let equal a b = D.Json.equal (to_json a) (to_json b)

  let validate t ~owner ~goal_id =
    let open Result.Let_syntax in
    let target = PC.Stream.target t.stream in
    let%bind () =
      if
        P.Session_ref.equal owner (Policy_cause_data.Goal_target.owner target)
        && P.Id.Goal.equal goal_id (Policy_cause_data.Goal_target.goal_id target)
        && (not (P.Session_ref.equal owner (PC.Stream.callee t.stream)))
        && Int64.(t.confirmed_prefix >= 0L && t.last_nonce >= t.confirmed_prefix)
      then Ok ()
      else invalid "remote causal slot identity or prefix differs"
    in
    let%bind () =
      match t.projection with
      | None ->
        if Int64.equal t.confirmed_prefix 0L
        then Ok ()
        else invalid "positive causal prefix lacks its full projection"
      | Some projection ->
        if
          PC.Stream.equal t.stream (PC.Projection.stream projection)
          && Int64.equal t.confirmed_prefix (PC.Projection.admitted_attempts projection)
        then Ok ()
        else invalid "causal projection differs from retained stream prefix"
    in
    let%bind () =
      match t.last with
      | None ->
        if
          Int64.equal t.last_nonce 0L
          && Int64.equal t.confirmed_prefix 0L
          && Option.is_some t.pending
        then Ok ()
        else invalid "causal terminal frontier lacks receipt or initial pending"
      | Some last ->
        let p = Last.pending last in
        let%bind () = Receipt.validate_pending p ~stream:t.stream in
        let%bind () =
          if Int64.equal (Pending.nonce p) t.last_nonce
          then Ok ()
          else invalid "causal last nonce differs"
        in
        (match last with
         | Confirmed p ->
           (match t.projection with
            | Some projection -> Receipt.confirms p ~projection
            | None -> invalid "confirmed receipt lacks projection")
         | No_writer_closed p ->
           let%bind next = Receipt.add t.confirmed_prefix 1L in
           if Int64.equal (Pending.next_prefix p) next
           then Ok ()
           else invalid "no-writer receipt changed confirmed prefix")
    in
    match t.pending with
    | None -> Ok ()
    | Some pending ->
      let%bind () = Receipt.validate_pending pending ~stream:t.stream in
      let%bind next = Receipt.add t.confirmed_prefix 1L in
      let%bind nonce = Receipt.add t.last_nonce 1L in
      if
        Int64.equal (Pending.next_prefix pending) next
        && Int64.equal (Pending.nonce pending) nonce
      then Ok ()
      else invalid "causal pending does not follow retained frontier"
  ;;

  let of_json raw =
    let open Result.Let_syntax in
    let%bind () = D.Json.validate raw ~limits:slot_limits |> domain in
    let%bind f = X.object_ raw in
    let%bind kind = X.required f "kind" P.Json_codec.string in
    let%bind () =
      if String.equal kind "remote"
      then Ok ()
      else invalid "unsupported causal stream kind"
    in
    let%bind stream = X.required f "stream" PC.Stream.of_json in
    let%bind id = X.required f "id" P.Json_codec.string in
    let%bind () =
      if String.equal id (PC.Stream.id stream)
      then Ok ()
      else invalid "causal slot key differs from full stream"
    in
    let%bind confirmed_prefix = X.required f "confirmed_prefix" X.nonnegative_int64 in
    let%bind last_nonce = X.required f "last_nonce" X.nonnegative_int64 in
    let%bind pending = X.required f "pending" (X.nullable Pending.of_json) in
    let%bind last = X.required f "last" (X.nullable Last.of_json) in
    let%map projection = X.required f "projection" (X.nullable PC.Projection.of_json) in
    { stream; confirmed_prefix; last_nonce; pending; last; projection }
  ;;

  let shape =
    X.shape_exn
      [ "id", D.Shape.value
      ; "kind", D.Shape.value
      ; "stream", PC.Stream.shape
      ; "confirmed_prefix", D.Shape.value
      ; "last_nonce", D.Shape.value
      ; "pending", X.nullable_shape Pending.shape
      ; "last", X.nullable_shape Last.shape
      ; "projection", X.nullable_shape PC.Projection.shape
      ]
  ;;
end

module Book = struct
  type t =
    { goal_id : P.Id.Goal.t
    ; history : History.t
    ; slots : Slot.t list
    }

  let to_json t =
    `Object
      [ "goal_id", P.Id.Goal.to_json t.goal_id
      ; "history", History.to_json t.history
      ; "slots", X.list_json Slot.to_json t.slots
      ]
  ;;

  let equal a b = D.Json.equal (to_json a) (to_json b)

  let of_json raw =
    let open Result.Let_syntax in
    let%bind f = X.object_ raw in
    let%bind goal_id = X.required f "goal_id" P.Id.Goal.of_json in
    let%bind history = X.required f "history" History.of_json in
    let%map slots = X.required f "slots" (X.list Slot.of_json) in
    { goal_id; history; slots }
  ;;

  (* Keyed raw preservation follows the existing595 partition carrier. The
     redundant key must equal its complete validated Stream identity. *)
  let shape =
    X.shape_exn
      [ "goal_id", D.Shape.value
      ; "history", D.Shape.value
      ; "slots", X.array_shape_exn ~identity_field:"id" Slot.shape
      ]
  ;;
end

type t =
  { owner : P.Session_ref.t
  ; books : Book.t list
  }

let to_json t =
  `Object
    [ "owner", P.Session_ref.to_json t.owner; "books", X.list_json Book.to_json t.books ]
;;

let equal a b = D.Json.equal (to_json a) (to_json b)

let find_book t id =
  List.find t.books ~f:(fun book -> P.Id.Goal.equal book.Book.goal_id id)
;;

let find_slot t id =
  List.find_map t.books ~f:(fun book ->
    Option.map
      (List.find book.Book.slots ~f:(fun slot ->
         String.equal (PC.Stream.id slot.Slot.stream) id))
      ~f:(fun slot -> book, slot))
;;

let set_book t book =
  if Option.is_some (find_book t book.Book.goal_id)
  then
    { t with
      books =
        List.map t.books ~f:(fun old ->
          if P.Id.Goal.equal old.Book.goal_id book.goal_id then book else old)
    }
  else { t with books = t.books @ [ book ] }
;;

let set_slot t ~goal_id slot =
  match find_book t goal_id with
  | None -> invalid "causal slot has no stable Goal book"
  | Some book ->
    let id = PC.Stream.id slot.Slot.stream in
    let slots =
      if
        List.exists book.slots ~f:(fun old ->
          String.equal id (PC.Stream.id old.Slot.stream))
      then
        List.map book.slots ~f:(fun old ->
          if String.equal id (PC.Stream.id old.Slot.stream) then slot else old)
      else book.slots @ [ slot ]
    in
    Ok (set_book t { book with slots })
;;

let validate t =
  let open Result.Let_syntax in
  let%bind () =
    if List.length t.books <= max_books
    then Ok ()
    else invalid "causal Goal book capacity exhausted"
  in
  let%bind () =
    if
      List.length (List.concat_map t.books ~f:(fun book -> book.Book.slots))
      <= max_streams
    then Ok ()
    else invalid "causal lifetime stream capacity exhausted"
  in
  let goals = List.map t.books ~f:(fun book -> P.Id.Goal.to_string book.Book.goal_id) in
  let streams =
    List.concat_map t.books ~f:(fun book ->
      List.map book.Book.slots ~f:(fun slot -> PC.Stream.id slot.Slot.stream))
  in
  let%bind () =
    if
      List.contains_dup goals ~compare:String.compare
      || List.contains_dup streams ~compare:String.compare
    then invalid "duplicate causal Goal or stream identity"
    else Ok ()
  in
  List.fold_result t.books ~init:() ~f:(fun () book ->
    List.fold_result book.Book.slots ~init:() ~f:(fun () slot ->
      Slot.validate slot ~owner:t.owner ~goal_id:book.goal_id))
;;

let shape =
  X.shape_exn
    [ "owner", D.Shape.value
    ; "books", X.array_shape_exn ~identity_field:"goal_id" Book.shape
    ]
;;

let of_json raw =
  let open Result.Let_syntax in
  let%bind f = X.object_ raw in
  let%bind owner = X.required f "owner" P.Session_ref.of_json in
  let%bind books = X.required f "books" (X.list Book.of_json) in
  let value = { owner; books } in
  let%map () = validate value in
  value
;;

let validate_original_children raw =
  let open Result.Let_syntax in
  let%bind fields = X.object_ raw in
  X.required
    fields
    "books"
    (X.list (fun raw ->
       let%bind f = X.object_ raw in
       X.required
         f
         "slots"
         (X.list (fun raw ->
            let%bind () = D.Json.validate raw ~limits:slot_limits |> domain in
            let%bind f = X.object_ raw in
            let%bind () =
              X.required f "stream" (fun raw ->
                D.Json.validate raw ~limits:Policy_cause_data.limits |> domain)
            in
            let%bind () =
              X.required
                f
                "projection"
                (X.nullable (fun raw ->
                   D.Json.validate raw ~limits:Receipt.pending_limits |> domain))
              |> Result.map ~f:ignore
            in
            let%bind () =
              X.required
                f
                "pending"
                (X.nullable (fun raw ->
                   D.Json.validate raw ~limits:Receipt.pending_limits |> domain))
              |> Result.map ~f:ignore
            in
            X.required
              f
              "last"
              (X.nullable (fun raw ->
                 let%bind f = X.object_ raw in
                 X.required f "pending" (fun raw ->
                   D.Json.validate raw ~limits:Receipt.pending_limits |> domain)))
            |> Result.map ~f:ignore))
       |> Result.map ~f:ignore))
  |> Result.map ~f:ignore
;;
