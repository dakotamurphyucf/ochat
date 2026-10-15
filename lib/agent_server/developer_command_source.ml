open! Core
module P = Agent_protocol
module Original = Developer_command_original
module Source = Authoring_context_service.Developer
module Root = Authoring_source_host.Root

let mismatch () =
  P.Error.invalid_request "developer command source does not match request"
;;

let prepare source ~original ~fresh ~actor ~clock ~checkpoint =
  let open Result.Let_syntax in
  let%bind request = Original.capture_request original in
  match Original.route original, fresh with
  | ( Original.Route.Local_project
        { root_index; dependency_root_indices; project; selection }
    , Original.Fresh_source.Local_project { root; dependency_roots } ) ->
    if
      (not (Int.equal root_index (Root.index root)))
      || not
           (List.equal
              Int.equal
              dependency_root_indices
              (List.map dependency_roots ~f:Root.index))
    then Error (mismatch ())
    else
      Source.prepare_local_project
        source
        ~original_actor:actor
        ~actor
        ~local:(Source.Local_project.create ~root ~dependency_roots ~project ~selection)
        ~request
        ~clock
        ~checkpoint
  | ( Original.Route.Local_source { root_index; member }
    , Original.Fresh_source.Local_source { root } ) ->
    if not (Int.equal root_index (Root.index root))
    then Error (mismatch ())
    else
      Source.prepare_local_source
        source
        ~original_actor:actor
        ~actor
        ~source:(Source.Local_source.create ~root ~member)
        ~request
        ~checkpoint
  | ( Original.Route.Authoring_project { expected }
    , Original.Fresh_source.Authoring_project { connection } ) ->
    Source.prepare_authoring_project
      source
      ~actor
      ~connection
      ~expected
      ~request
      ~checkpoint
  | ( Original.Route.Document_script { expected; selector }
    , Original.Fresh_source.Document_script { connection } ) ->
    Source.prepare_document_script
      source
      ~actor
      ~connection
      ~expected
      ~script:(Source.Document_script.create ~selector)
      ~request
      ~checkpoint
  | ( Original.Route.Local_project _
    , ( Original.Fresh_source.Local_source _
      | Original.Fresh_source.Authoring_project _
      | Original.Fresh_source.Document_script _ ) )
  | ( Original.Route.Local_source _
    , ( Original.Fresh_source.Local_project _
      | Original.Fresh_source.Authoring_project _
      | Original.Fresh_source.Document_script _ ) )
  | ( Original.Route.Authoring_project _
    , ( Original.Fresh_source.Local_project _
      | Original.Fresh_source.Local_source _
      | Original.Fresh_source.Document_script _ ) )
  | ( Original.Route.Document_script _
    , ( Original.Fresh_source.Local_project _
      | Original.Fresh_source.Local_source _
      | Original.Fresh_source.Authoring_project _ ) ) -> Error (mismatch ())
;;
