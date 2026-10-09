module Profile_policy = Profile_policy
open! Core
module P = Agent_protocol
module Actor = Operator_authorization
module DTO = P.Provider_operator
module M = Credential_registry_model
module C = Credential_registry
module Bridge = Inference_host.Credential_bridge
module Service = Provider_operator
module Admin = Service.Profile_admin
module Intents = Service.Command_intents
module Runtime = Provider_runtime

let unavailable _ = DTO.Error.Store_unavailable

let missing_registry = function
  | C.Error.Model M.Error.Missing_registry | C.Error.Storage Missing -> true
  | _ -> false
;;

let create
      ~sw
      ~env
      ~server_id
      ~anchor
      ~components
      ~host
      ~secret_namespace
      ~driver
      ~templates
      ~mappings
      ~compatible_profiles
      ~default_profile
      ~environment
      ~environment_sources
      ~oauth
      ~oauth_lease
      ~start_login
      ~inference_principal
      ~authorize_bridge
      ~authorize
      ~authorize_setup
      ~authorize_status
      ~new_operation
      ~new_revision
      ~maximum_wait
      ~limits
      ~inference_limits
      ~transport_policy
  =
  let cleanups = ref [] in
  let retained = ref false in
  let rec cleanup = function
    | [] -> ()
    | release :: rest -> Exn.protect ~f:release ~finally:(fun () -> cleanup rest)
  in
  Exn.protect
    ~finally:(fun () -> if not !retained then cleanup !cleanups)
    ~f:(fun () ->
      let open Result.Let_syntax in
      let%bind metadata_admission =
        C.Metadata_admission.wait ~clock:(Eio.Stdenv.mono_clock env) ~maximum_wait
        |> Result.map_error ~f:(fun _ -> DTO.Error.Invalid_request)
      in
      let%bind directory =
        Private_storage.Directory.open_or_create ~sw ~anchor ~components
        |> Result.map_error ~f:unavailable
      in
      cleanups := (fun () -> Private_storage.Directory.close directory) :: !cleanups;
      let%bind secrets =
        Provider_secret_store.open_private_files
          ~sw
          ~directory
          ~namespace:secret_namespace
        |> Result.map_error ~f:unavailable
      in
      cleanups := (fun () -> Provider_secret_store.close secrets) :: !cleanups;
      let%bind intents =
        Intents.create directory ~host ~maximum_records:1024
        |> Result.map_error ~f:unavailable
      in
      let environment_port = Option.map environment ~f:Bridge.Environment.port in
      let open_registry () =
        C.open_existing
          ~metadata_admission
          ~sw
          ~wall_clock:(Eio.Stdenv.clock env)
          ~new_operation
          ~directory
          ~secrets
          ~environment:environment_port
          ~host
      in
      let setup_identity ~actor (request : DTO.Setup_request.t) =
        let command = P.Command.Provider_setup request in
        ( (Actor.principal actor).id
        , P.Command.method_name command
        , P.Command.params command )
      in
      let setup_receipt ~actor request =
        let actor, method_name, params = setup_identity ~actor request in
        let%bind intent =
          Intents.lookup
            intents
            ~principal:actor
            ~key:request.idempotency_key
            ~method_name
            ~params
          |> Result.map_error ~f:unavailable
        in
        match Option.bind intent ~f:Intents.Intent.committed with
        | Some (P.Command_receipt.Provider_setup result) ->
          Ok (P.Command_receipt.Committed (P.Command_receipt.Provider_setup result))
        | Some _ -> Error DTO.Error.Store_unavailable
        | None ->
          Ok (if Option.is_none intent then P.Command_receipt.Missing else Unavailable)
      in
      let build registry =
        let failed = ref true in
        Exn.protect
          ~finally:(fun () -> if !failed then C.close registry)
          ~f:(fun () ->
            let%bind incarnation =
              C.incarnation registry |> Result.map_error ~f:unavailable
            in
            let%bind revision =
              DTO.Revision.of_string (M.Id.to_string incarnation)
              |> Result.map_error ~f:unavailable
            in
            let%bind bridge =
              Bridge.create
                ?oauth:oauth_lease
                driver
                ~registry
                ~mappings
                ~compatible_profiles
                ~approved_profiles:
                  (List.map templates ~f:(fun template ->
                     DTO.Profile_id.to_string (Admin.Template.profile template)))
                ~authorize:authorize_bridge
                ~clock:(Eio.Stdenv.mono_clock env)
                ~maximum_wait
                ~transport_policy
                ~limits:inference_limits
              |> Result.map_error ~f:unavailable
            in
            let%bind profiles =
              Admin.open_
                directory
                ~incarnation
                ~registry
                ~templates
                ~publish:(Bridge.publish_mapping bridge)
                ~new_revision
              |> Result.map_error ~f:unavailable
            in
            let%bind records =
              Service.Owner_records.create directory ~incarnation ~maximum_records:128
              |> Result.map_error ~f:unavailable
            in
            let%map service =
              Service.create
                ~sw
                ~server_id
                ~host
                ~incarnation
                ~registry
                ~bridge
                ~profiles
                ~oauth
                ~owner_records:records
                ~command_intents:intents
                ~start_login
                ~authorize
                ~clock:(Eio.Stdenv.mono_clock env)
                ~maximum_wait
                ~now:(fun () ->
                  P.Timestamp.of_time_ns
                    (Time_ns.of_span_since_epoch
                       (Time_ns.Span.of_sec (Eio.Time.now (Eio.Stdenv.clock env)))))
                ~new_operation
                ~limits
                ~environment:environment_sources
            in
            failed := false;
            Runtime.Opened.create
              ~service
              ~backend:(Admin.backend profiles ~bridge ~principal:inference_principal)
              ~incarnation:revision
              ~setup_receipt:(fun ~actor request ->
                let principal_id, method_name, params = setup_identity ~actor request in
                let%bind intent =
                  Intents.lookup
                    intents
                    ~principal:principal_id
                    ~key:request.idempotency_key
                    ~method_name
                    ~params
                  |> Result.map_error ~f:unavailable
                in
                match intent with
                | Some intent
                  when M.Id.equal (Intents.Intent.operation intent) incarnation ->
                  let%map () =
                    Intents.complete
                      intents
                      intent
                      (P.Command_receipt.Provider_setup { server_id; revision })
                    |> Result.map_error ~f:unavailable
                  in
                  revision
                | _ -> Error DTO.Error.Submission_uncertain)
              ~close:(fun () ->
                Exn.protect
                  ~f:(fun () -> Service.close service)
                  ~finally:(fun () -> C.close registry)))
      in
      let existing ~sw:_ =
        match open_registry () with
        | Ok registry ->
          let metadata =
            Private_storage.Name.create "operator-profiles.json"
            |> Result.map_error ~f:unavailable
          in
          (match metadata with
           | Error error ->
             C.close registry;
             Error error
           | Ok metadata ->
             (match
                Private_storage.Directory.read_bounded
                  directory
                  metadata
                  ~max_bytes:(256 * 1024)
              with
              | Ok _ -> build registry |> Result.map ~f:Option.some
              | Error error
                when Private_storage.Error.equal_code
                       (Private_storage.Error.code error)
                       Missing ->
                Exn.protect
                  ~finally:(fun () -> C.close registry)
                  ~f:(fun () ->
                    let%bind incarnation =
                      C.incarnation registry |> Result.map_error ~f:unavailable
                    in
                    let%bind original =
                      Intents.has_operation
                        intents
                        ~method_name:"provider.setup"
                        ~operation:incarnation
                      |> Result.map_error ~f:unavailable
                    in
                    if original then Ok None else Error DTO.Error.Store_unavailable)
              | Error _ ->
                C.close registry;
                Error DTO.Error.Store_unavailable))
        | Error error when missing_registry error -> Ok None
        | Error _ -> Error DTO.Error.Store_unavailable
      in
      let initialize ~sw:_ ~actor request =
        let%bind () =
          if
            Actor.is_current actor
            && P.Principal.has_scope (Actor.principal actor) Provider_manage
            && authorize_setup actor
          then Ok ()
          else Error DTO.Error.Denied
        in
        let principal_id, method_name, params = setup_identity ~actor request in
        let%bind admission =
          Intents.begin_
            intents
            ~principal:principal_id
            ~key:request.idempotency_key
            ~method_name
            ~params
            ~operation:(new_operation ())
          |> Result.map_error ~f:unavailable
        in
        let intent =
          match admission with
          | Fresh intent | Existing intent -> intent
        in
        let operation = Intents.Intent.operation intent in
        let%bind registry =
          match admission, open_registry () with
          | _, Ok registry ->
            (* A retry can open only its original incarnation. Another setup owner
           or pre-existing authority is never overwritten/adopted as success. *)
            (match C.incarnation registry with
             | Ok incarnation when M.Id.equal incarnation operation -> Ok registry
             | _ ->
               C.close registry;
               Error DTO.Error.Submission_uncertain)
          | (Intents.Existing _ | Fresh _), Error error when missing_registry error ->
            let%bind () =
              if
                Actor.is_current actor
                && P.Principal.has_scope (Actor.principal actor) Provider_manage
                && authorize_setup actor
              then Ok ()
              else Error DTO.Error.Denied
            in
            C.initialize_new
              ~metadata_admission
              ~sw
              ~wall_clock:(Eio.Stdenv.clock env)
              ~new_operation
              ~directory
              ~secrets
              ~environment:environment_port
              ~host
              ~incarnation:operation
            |> Result.map_error ~f:unavailable
          | _, Error _ -> Error DTO.Error.Store_unavailable
        in
        let retained = ref false in
        let opened_owner = ref None in
        Exn.protect
          ~finally:(fun () ->
            if not !retained
            then (
              match !opened_owner with
              | Some opened -> Runtime.Opened.close opened
              | None -> C.close registry))
          ~f:(fun () ->
            let%bind () =
              if
                Actor.is_current actor
                && P.Principal.has_scope (Actor.principal actor) Provider_manage
                && authorize_setup actor
              then Ok ()
              else Error DTO.Error.Denied
            in
            let%bind () =
              match
                Admin.initialize
                  directory
                  ~incarnation:operation
                  ~templates
                  ~default_profile
                  ~initial_revision:(new_revision ())
              with
              | Ok () -> Ok ()
              | Error (Admin.Error.Storage error)
                when Private_storage.Error.equal_code
                       (Private_storage.Error.code error)
                       Exists -> Ok ()
              | Error _ -> Error DTO.Error.Submission_uncertain
            in
            let%bind opened = build registry in
            opened_owner := Some opened;
            let%bind revision =
              DTO.Revision.of_string (M.Id.to_string operation)
              |> Result.map_error ~f:unavailable
            in
            let result = P.Command_receipt.Provider_setup { server_id; revision } in
            let%map () =
              Intents.complete intents intent result
              |> Result.map_error ~f:(fun _ -> DTO.Error.Submission_uncertain)
            in
            retained := true;
            opened)
      in
      match
        Runtime.create
          ~sw
          ~server_id
          ~authorize_setup
          ~authorize_status
          ~setup_receipt
          ~existing
          ~initialize
      with
      | Ok runtime ->
        cleanups := (fun () -> Runtime.close runtime) :: !cleanups;
        Eio.Switch.on_release sw (fun () -> cleanup !cleanups);
        retained := true;
        Ok runtime
      | Error error -> Error error)
;;
