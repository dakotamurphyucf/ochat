open! Core
include Recovery_note_share_document

let error code message = P.Error.create code ~message ~retryable:false ()
let unavailable () = error Conflict "shared recovery note is unavailable"
let unavailable_result () = Error (unavailable ())

let incarnation t ~generation =
  let state = D.Extension_carrier.value t in
  Incarnation.create ~session:state.owner ~nonce:state.nonce ~generation
;;

let owned_reference state (reference : Reference.t) =
  Reference.matches reference ~source:state.owner ~nonce:state.nonce
;;

let find_grant state (reference : Reference.t) =
  if owned_reference state reference
  then
    List.find state.grants ~f:(fun grant ->
      Int64.equal (Revision.to_int64 grant.id) (Reference.id reference))
  else None
;;

let resolve t ~source_generation ~recipient ~reference ~now =
  let state = D.Extension_carrier.value t in
  match find_grant state reference with
  | Some grant
    when Int.equal source_generation grant.source_generation
         && Incarnation.equal recipient grant.recipient
         && P.Timestamp.compare now grant.issued_at >= 0
         && P.Timestamp.compare now grant.expires_at < 0 -> Ok grant.selection
  | Some _ | None -> unavailable_result ()
;;

let check_unchanged previous ~incoming =
  let changed () = Error (invalid "replacement changed recovery note share custody") in
  let open Result.Let_syntax in
  match previous, incoming with
  | None, None -> Ok ()
  | Some old, Some next when phys_equal old next -> Ok ()
  | Some old, Some next ->
    let%bind old = to_json old in
    let%bind next = to_json next in
    if D.Json.equal old next then Ok () else changed ()
  | None, Some _ | Some _, None -> changed ()
;;

module Journal = struct
  type ledger = t

  type mutation = Mutation.t =
    | Create of Incarnation.t * create
    | Revoke of revoke

  let mutation_to_json = Mutation.to_json
  let mutation_of_json = Mutation.of_json

  let establish ~owner ~nonce =
    let open Result.Let_syntax in
    let%map _ = Incarnation.create ~session:owner ~nonce ~generation:0 in
    D.Extension_carrier.of_authored_value
      { owner; nonce; revision = Revision.initial; grants = []; receipts = [] }
  ;;

  let receipt state (record : recorded_receipt) =
    let%map.Result route =
      Reference.create
        state.owner
        ~nonce:state.nonce
        ~id:(Revision.to_int64 record.grant_id)
    in
    Receipt.{ reference = route; revision = record.revision }
  ;;

  (* A writer retry retains its original request, even if server time advanced. *)
  let digest ~source mutation =
    Jsonaf.to_string
      (`Array
          [ `String "ochat.recovery-note-share.v1"
          ; Incarnation.to_json source
          ; Mutation.to_json mutation
          ])
    |> Digestif.SHA256.digest_string
    |> Digestif.SHA256.to_hex
  ;;

  let catalog_read catalog ~owner selection =
    match catalog with
    | Some catalog when P.Session_ref.equal (C.owner catalog) owner ->
      C.read catalog selection |> Result.map_error ~f:(fun _ -> unavailable ())
    | Some _ | None -> unavailable_result ()
  ;;

  let live_grants state ~source ~catalog ~now =
    List.filter state.grants ~f:(fun grant ->
      P.Timestamp.compare now grant.expires_at < 0
      && Int.equal grant.source_generation source.Incarnation.generation
      && Result.is_ok (catalog_read catalog ~owner:state.owner grant.selection))
  ;;

  let advance state ~source ~catalog ~now mutation =
    let open Result.Let_syntax in
    let expected =
      match mutation with
      | Create (_, request) -> request.expected_revision
      | Revoke request -> request.expected_revision
    in
    if not (Revision.equal expected state.revision)
    then Error (error Conflict "recovery note share revision changed")
    else if Int64.equal state.revision Int64.max_value
    then Error (error Resource_limit "recovery note share revision exhausted")
    else (
      let next = Int64.succ state.revision in
      let grants = live_grants state ~source ~catalog ~now in
      match mutation with
      | Create (recipient, request) ->
        if
          not
            (recipient_matches_owner state.owner recipient
             && valid_ttl ~now request.expires_at)
        then Error (invalid "invalid recovery note share recipient or expiry")
        else if List.length grants >= max_grants
        then Error (error Resource_limit "recovery note share capacity exceeded")
        else (
          let%bind () =
            match catalog with
            | Some catalog
              when C.Catalog_revision.equal
                     (C.revision catalog)
                     request.expected_catalog_revision -> Ok ()
            | Some _ | None ->
              Error (error Conflict "recovery note catalog revision changed")
          in
          let%map _ = catalog_read catalog ~owner:state.owner request.selection in
          let grant =
            { id = next
            ; source_generation = source.generation
            ; recipient
            ; selection = request.selection
            ; issued_at = now
            ; expires_at = request.expires_at
            }
          in
          next, next, grant :: grants)
      | Revoke { reference; expected_revision = _ } ->
        (match find_grant state reference with
         | None -> unavailable_result ()
         | Some grant ->
           let grants =
             List.filter grants ~f:(fun active -> not (Revision.equal active.id grant.id))
           in
           Ok (next, grant.id, grants)))
  ;;

  let mutate t ~source ~catalog ~principal ~key ~now mutation =
    let open Result.Let_syntax in
    let state = D.Extension_carrier.value t in
    let%bind actual = incarnation t ~generation:source.Incarnation.generation in
    let%bind () =
      if Incarnation.equal actual source then Ok () else unavailable_result ()
    in
    let%bind principal = P.Id.Principal.of_json (P.Id.Principal.to_json principal) in
    let%bind key = P.Idempotency_key.of_json (P.Idempotency_key.to_json key) in
    let digest = digest ~source mutation in
    let same_key receipt =
      P.Id.Principal.equal principal receipt.principal
      && P.Idempotency_key.equal key receipt.key
    in
    match List.find state.receipts ~f:same_key with
    | Some record ->
      if String.equal record.digest digest
      then Result.map (receipt state record) ~f:(fun receipt -> `Replayed receipt)
      else
        Error
          (error Idempotency_conflict "recovery note share key has different arguments")
    | None ->
      let%bind revision, grant_id, grants =
        advance state ~source ~catalog ~now mutation
      in
      let record = { principal; key; digest; grant_id; revision } in
      let receipts = List.take (record :: state.receipts) max_receipts in
      let%bind receipt = receipt state record in
      let%map next = with_state t { state with revision; grants; receipts } in
      `Applied (next, receipt)
  ;;
end
