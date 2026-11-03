open! Core
module P = Agent_protocol
module G = P.Goal
module C = Goal_command

type decision =
  | Retained of Goal_command_receipt.t
  | Commit of
      { catalog : Goal_catalog.t
      ; receipt : Goal_command_receipt.t
      }

let conflict message = Error (P.Error.create Conflict ~message ~retryable:false ())

let domain result =
  Result.map_error result ~f:(fun error ->
    P.Error.invalid_request (Sexp.to_string_hum (Document_schema.Error.sexp_of_t error)))
;;

let prepare
      catalog
      ~(request : C.t)
      ~principal_id
      ~session
      ~generation
      ~current_source
      ~now
  =
  let open Result.Let_syntax in
  let%bind request_sha256 = C.digest request in
  let%bind retained =
    Goal_catalog.receipt catalog ~principal_id ~key:request.key ~request_sha256
  in
  match retained with
  | Some receipt -> Ok (Retained receipt)
  | None ->
    let%bind () =
      if
        P.Id.Session.equal request.session_id (P.Session_ref.session_id session)
        && request.generation = generation
        && Int64.equal request.expected_catalog_revision (Goal_catalog.revision catalog)
        && ((not (C.Mutation.needs_source request.mutation))
            || Option.equal P.Run_source.equal request.source current_source)
      then Ok ()
      else conflict "goal command session, source, generation or catalog frontier changed"
    in
    let id = C.Mutation.goal_id request.mutation in
    let%bind goal =
      match request.mutation with
      | Create { objective; budget; _ } ->
        let%bind source =
          Result.of_option
            current_source
            ~error:(P.Error.invalid_request "goal create needs actual installed source")
        in
        if Option.is_some (Goal_catalog.find catalog id)
        then conflict "goal identity already exists"
        else G.create ~id ~session ~creator:principal_id ~source ~objective ~budget
      | ( Revise_objective _
        | Record_progress _
        | Hold _
        | Change_budget _
        | Adopt_source _
        | Abandon _ ) as mutation ->
        let%bind old =
          Result.of_option
            (Goal_catalog.find catalog id)
            ~error:(P.Error.invalid_request "goal not found")
        in
        let%bind () =
          if
            Option.equal
              Int64.equal
              request.expected_goal_revision
              (Some (G.revision old))
            && P.Session_ref.equal session (G.session old)
          then Ok ()
          else conflict "goal revision or host session changed"
        in
        let%bind () =
          match mutation with
          | Adopt_source _ | Hold _ | Abandon _ -> Ok ()
          | Revise_objective _ | Record_progress _ | Change_budget _ ->
            if Option.equal P.Run_source.equal (Some (G.source old)) current_source
            then Ok ()
            else conflict "goal needs explicit current-source adoption"
          | Create _ -> assert false
        in
        (match mutation with
         | Revise_objective (_, objective) -> G.revise_objective old objective
         | Record_progress (_, progress) -> G.record_progress old progress
         | Hold (_, hold) -> G.hold old hold
         | Change_budget (_, budget) -> G.change_budget old budget
         | Adopt_source _ ->
           (match current_source with
            | Some source -> G.adopt_source old source
            | None -> conflict "source adoption needs installed source")
         | Abandon (_, reason) -> G.abandon old reason
         | Create _ -> assert false)
    in
    let%bind document =
      match Goal_catalog.document catalog id with
      | None -> Goal_document.create goal |> domain
      | Some previous -> Goal_document.update previous goal |> domain
    in
    let%bind () =
      if Int64.equal (Goal_catalog.revision catalog) Int64.max_value
      then conflict "goal catalog revision exhausted"
      else Ok ()
    in
    let%bind receipt =
      Goal_command_receipt.create
        ~principal_id
        ~key:request.key
        ~request_sha256
        ~goal_id:id
        ~goal_revision:(G.revision goal)
        ~catalog_revision:(Int64.succ (Goal_catalog.revision catalog))
        ~source:request.source
        ~accepted_at:now
    in
    let%map catalog = Goal_catalog.commit catalog ~goal:document ~receipt in
    Commit { catalog; receipt }
;;
