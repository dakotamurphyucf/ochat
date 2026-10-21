open Core
module P = Agent_protocol
module Host_policy = Authoring_installation.Host_policy
module Config = Authoring_upload_access_configuration
module Bound = Authoring_configured_upload_policy

type t = { host_policy : Host_policy.t option }

module Limits = struct
  type t =
    { store : Agent_store.Authoring_upload_store.Limits.t
    ; validation : Chatml_authoring_bundle_project.Limits.t
    }

  let invalid () = P.Error.invalid_request "invalid configured upload limits"
  let create ~store ~validation = Ok { store; validation }

  let standard =
    let open Result.Let_syntax in
    let admitted result = Result.map_error result ~f:(fun _ -> invalid ()) in
    let mib = 1024 * 1024 in
    let%bind store =
      Agent_store.Authoring_upload_store.Limits.create
        ~max_active_uploads:8
        ~max_active_per_principal:4
        ~max_members:64
        ~max_manifest_bytes:mib
        ~max_member_bytes:(8 * mib)
        ~max_bundle_bytes:(32 * mib)
        ~max_chunk_bytes:(64 * 1024)
        ~max_reserved_bytes:(64 * mib)
        ~max_reserved_per_principal:(32 * mib)
        ~max_published_records:8
        ~max_installed_sources:512
        ~max_installed_bytes_per_bundle:(8 * mib)
        ~max_installed_bytes:(64 * mib)
        ~max_installed_per_principal:(32 * mib)
        ~max_publication_metadata_bytes:(8 * mib)
        ~max_intent_bytes:(4 * mib)
        ~max_marker_bytes:mib
        ~max_work_steps:1_000_000
        ~max_journal_records:256
        ~max_journal_bytes:(8 * mib)
        ~max_idempotency_records:64
        ~max_tombstones:64
        ~max_tombstone_bytes:(16 * mib)
        ~max_recovery_entries:1024
        ~max_recovery_metadata_bytes:(16 * mib)
        ~max_recovery_source_bytes:(128 * mib)
        ~staging_ttl:(Time_ns.Span.of_hr 1.)
        ~receipt_ttl:(Time_ns.Span.of_day 1.)
        ~expired_tombstone_ttl:(Time_ns.Span.of_day 2.)
      |> admitted
    in
    let%bind validation =
      Chatml_authoring_bundle_project.Limits.create
        ~max_work_steps:1_000_000
        ~max_retained_input_bytes:(64 * mib)
        ~document:Document_schema.Limits.default
      |> admitted
    in
    create ~store ~validation
  ;;
end

let invalid message = P.Error.invalid_request message

let create
      ~base
      ~configured
      ~native_config
      ~contracts
      ~initialize_limits
      ~document_dir
      ~(limits : Limits.t)
  =
  match configured with
  | None -> Ok { host_policy = base }
  | Some configuration ->
    let open Result.Let_syntax in
    let%bind bound =
      Bound.bind configuration ~contracts
      |> Result.map_error ~f:(function
        | Bound.Error.Invalid_configuration -> invalid "invalid configured upload access"
        | Missing_standard_target -> invalid "upload target was not admitted"
        | Selection_limit -> invalid "upload selection limit")
    in
    let upload =
      Host_policy.Upload.create
        ~limits:limits.store
        ~grant:(Bound.upload_policy bound)
        ~publication:
          (Some
             (Authoring_upload_host.Publication.create
                ~policy:(Bound.publication_policy bound)
                ~limits:limits.validation
                ~document_dir))
        ~rebind:
          (Some
             (Authoring_upload_host.Rebind.create
                ~policy:(Bound.rebind_policy bound)
                ~limits:limits.validation
                ~document_dir))
    in
    let%bind policy =
      match configuration.mode, native_config, base with
      | Config.Upload_only, Some _, _ ->
        Error (invalid "upload-only conflicts with native authoring access")
      | Upload_only, None, _ -> Host_policy.upload_only ~initialize_limits ~upload
      | Augment_core, _, None -> Error (invalid "configured upload needs core")
      | Augment_core, _, Some base ->
        let%bind () = Host_policy.validate_initialize base initialize_limits in
        Host_policy.with_configured_upload base ~upload
    in
    Ok { host_policy = Some policy }
;;

let host_policy t = t.host_policy
