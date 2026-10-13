open Core
module Captured = Chatml_source
module Prepared = Chatml_prepared_program
module Build = Chatml_project_build
module Identity = Authoring_context_types.Identity
module Limits = Authoring_context_types.Limits
module Generation = Authoring_context_types.Generation
module Editor_revision = Authoring_context_types.Editor_revision
module Error = Authoring_context_types.Error
module Version = Authoring_context_types.Version
module View = Authoring_context_types.View
module Basis = Authoring_context_types.Basis
module Document = Authoring_context_types.Document
module Inputs = Authoring_context_types.Inputs
module Analysis_key = Authoring_context_types.Analysis_key
module Refresh = Authoring_context_types.Refresh
module Contexts = Map.M (Identity)
module Sources = Map.M (Captured.Id)
module Source_ids = Set.M (Captured.Id)

type context =
  { identity : Identity.t
  ; basis : basis
  ; base_sources : Captured.t list
  ; base_by_id : Captured.t Sources.t
  ; editable : Source_ids.t
  ; editable_list : Captured.Id.t list
  ; source_count : int
  ; generation : Generation.t
  ; overlays : Captured.t Sources.t
  ; revisions : Editor_revision.t Sources.t
  ; open_document_count : int
  ; charged_source_bytes : int
  ; effective_input_bytes : int
  ; analysis_key : Analysis_key.t option
  }

and basis =
  | Project of Build.t
  | Document of
      { base : Captured.t
      ; requested_environment : string option
      ; static_build : Build.t option
      }

type t =
  { limits : Limits.t
  ; contexts : context Contexts.t
  ; charged_source_bytes : int
  }

let create ~limits =
  { limits; contexts = Map.empty (module Identity); charged_source_bytes = 0 }
;;

let context_count t = Map.length t.contexts
let charged_source_bytes t = t.charged_source_bytes

let context_view context base =
  let prepared = Build.prepared base in
  { View.identity = context.identity
  ; generation = context.generation
  ; base_graph = Prepared.fingerprint prepared
  ; environment = Prepared.Environment.name (Prepared.environment prepared)
  ; compiler_contract = Prepared.compiler_contract prepared
  ; source_count = context.source_count
  ; editable_sources = context.editable_list
  ; open_document_count = context.open_document_count
  ; charged_source_bytes = context.charged_source_bytes
  }
;;

let find t ~identity =
  Map.find t.contexts identity
  |> Option.bind ~f:(fun context ->
    match context.basis with
    | Project base -> Some (context_view context base)
    | Document _ -> None)
;;

let basis t ~identity =
  Map.find t.contexts identity
  |> Option.map ~f:(fun context ->
    match context.basis with
    | Project _ -> Basis.Project
    | Document _ -> Basis.Document)
;;

let find_document t ~identity =
  Map.find t.contexts identity
  |> Option.bind ~f:(fun context ->
    match context.basis with
    | Project _ -> None
    | Document { base; requested_environment; static_build } ->
      let source =
        Map.find context.overlays (Captured.id base) |> Option.value ~default:base
      in
      let static = Option.map static_build ~f:Build.prepared in
      Some
        { Document.View.identity = context.identity
        ; generation = context.generation
        ; source
        ; requested_environment
        ; selected_environment =
            Option.map static ~f:(fun prepared ->
              Prepared.environment prepared |> Prepared.Environment.name)
        ; compiler_contract = Option.map static ~f:Prepared.compiler_contract
        ; static_graph = Option.map static ~f:Prepared.fingerprint
        ; open_document_count = context.open_document_count
        ; charged_source_bytes = context.charged_source_bytes
        })
;;

let current_version context ~source =
  match Map.find context.base_by_id source with
  | None -> None
  | Some base ->
    let overlay = Map.find context.overlays source in
    Some
      { Version.identity = context.identity
      ; generation = context.generation
      ; source
      ; digest = Captured.digest (Option.value overlay ~default:base)
      ; editor_revision = Map.find context.revisions source
      ; overlay_open = Option.is_some overlay
      }
;;

let version t ~identity ~source =
  Map.find t.contexts identity
  |> Option.bind ~f:(fun context -> current_version context ~source)
;;

let analysis_key t ~identity =
  Map.find t.contexts identity
  |> Option.bind ~f:(fun context ->
    match context.basis with
    | Project _ -> context.analysis_key
    | Document _ -> None)
;;

let preflight_count list ~maximum ~checkpoint =
  let rec loop count = function
    | [] -> Ok ()
    | _ when count >= maximum -> Error Error.Limit_exceeded
    | _ :: rest ->
      checkpoint ();
      loop (count + 1) rest
  in
  loop 0 list
;;

let within_add ~limit left right =
  left >= 0 && right >= 0 && left <= limit && right <= limit - left
;;

let next_generation context =
  Generation.next context.generation |> Result.of_option ~error:Error.Generation_overflow
;;

let base_context t ~identity ~base ~editable_sources ~checkpoint =
  let open Result.Let_syntax in
  let prepared = Build.prepared base in
  let accounting = Prepared.accounting prepared in
  let source_count = Prepared.Accounting.source_count accounting in
  let source_bytes = Prepared.Accounting.source_bytes accounting in
  let input_bytes = Prepared.Accounting.input_bytes accounting in
  let%bind () =
    if
      source_count > t.limits.max_sources
      || source_bytes > t.limits.max_context_source_bytes
      || input_bytes > t.limits.max_effective_input_bytes
    then Error Error.Limit_exceeded
    else Ok ()
  in
  let%bind () =
    preflight_count editable_sources ~maximum:t.limits.max_sources ~checkpoint
  in
  let sources = Prepared.sources prepared in
  let base_by_id =
    List.fold
      sources
      ~init:(Map.empty (module Captured.Id))
      ~f:(fun map source ->
        checkpoint ();
        Map.set map ~key:(Captured.id source) ~data:source)
  in
  let lock_id = Captured.id (Build.lock_source base) in
  let%map editable =
    List.fold_result
      editable_sources
      ~init:(Set.empty (module Captured.Id))
      ~f:(fun set id ->
        checkpoint ();
        if Captured.Id.equal id lock_id
        then Error (Error.Read_only_source id)
        else if not (Map.mem base_by_id id)
        then Error (Error.Unknown_source id)
        else if Set.mem set id
        then Error (Error.Duplicate_editable_source id)
        else Ok (Set.add set id))
  in
  { identity
  ; basis = Project base
  ; base_sources = sources
  ; base_by_id
  ; editable
  ; editable_list =
      Set.fold editable ~init:[] ~f:(fun reversed id ->
        checkpoint ();
        id :: reversed)
      |> List.rev
  ; source_count
  ; generation = Generation.zero
  ; overlays = Map.empty (module Captured.Id)
  ; revisions = Map.empty (module Captured.Id)
  ; open_document_count = 0
  ; charged_source_bytes = source_bytes
  ; effective_input_bytes = input_bytes
  ; analysis_key = None
  }
;;

let admit t ~identity ~base ~editable_sources ~checkpoint =
  let open Result.Let_syntax in
  if Map.mem t.contexts identity
  then Error Error.Duplicate_context
  else if context_count t >= t.limits.max_contexts
  then Error Error.Limit_exceeded
  else (
    let base_source_bytes =
      Build.prepared base |> Prepared.accounting |> Prepared.Accounting.source_bytes
    in
    let%bind () =
      if
        within_add
          ~limit:t.limits.max_total_source_bytes
          t.charged_source_bytes
          base_source_bytes
      then Ok ()
      else Error Error.Limit_exceeded
    in
    let%bind context = base_context t ~identity ~base ~editable_sources ~checkpoint in
    if
      not
        (within_add
           ~limit:t.limits.max_total_source_bytes
           t.charged_source_bytes
           context.charged_source_bytes)
    then Error Error.Limit_exceeded
    else
      Ok
        { t with
          contexts = Map.set t.contexts ~key:identity ~data:context
        ; charged_source_bytes = t.charged_source_bytes + context.charged_source_bytes
        })
;;

let admit_document t ~identity ~base ~requested_environment ~checkpoint =
  let bytes = Captured.byte_length base in
  let valid_environment =
    Option.for_all requested_environment ~f:(fun name ->
      let length = String.length name in
      length > 0 && length <= 256)
  in
  if not valid_environment
  then Error Error.Invalid_requested_environment
  else if Map.mem t.contexts identity
  then Error Error.Duplicate_context
  else if Map.length t.contexts >= t.limits.max_contexts
  then Error Error.Limit_exceeded
  else if
    1 > t.limits.max_sources
    || bytes > t.limits.max_document_bytes
    || bytes > t.limits.max_context_source_bytes
    || bytes > t.limits.max_effective_input_bytes
    || not
         (within_add ~limit:t.limits.max_total_source_bytes t.charged_source_bytes bytes)
  then Error Error.Limit_exceeded
  else (
    checkpoint ();
    let source_id = Captured.id base in
    let context =
      { identity
      ; basis = Document { base; requested_environment; static_build = None }
      ; base_sources = [ base ]
      ; base_by_id = Map.singleton (module Captured.Id) source_id base
      ; editable = Set.singleton (module Captured.Id) source_id
      ; editable_list = [ source_id ]
      ; source_count = 1
      ; generation = Generation.zero
      ; overlays = Map.empty (module Captured.Id)
      ; revisions = Map.empty (module Captured.Id)
      ; open_document_count = 0
      ; charged_source_bytes = bytes
      ; effective_input_bytes = bytes
      ; analysis_key = None
      }
    in
    Ok
      { t with
        contexts = Map.set t.contexts ~key:identity ~data:context
      ; charged_source_bytes = t.charged_source_bytes + bytes
      })
;;

let attach_document_static t ~identity ~expected ~build ~checkpoint =
  let open Result.Let_syntax in
  let%bind context =
    Map.find t.contexts identity |> Result.of_option ~error:Error.Unknown_context
  in
  if not (Generation.equal context.generation expected)
  then Error Error.Stale_generation
  else (
    match context.basis with
    | Project _ -> Error Error.Unsupported_basis
    | Document { static_build = Some _; _ } -> Error Error.Static_selection_unavailable
    | Document ({ base; requested_environment; static_build = None } as document) ->
      let prepared = Build.prepared build in
      let accounting = Prepared.accounting prepared in
      let extra_sources = Prepared.Accounting.source_count accounting in
      let extra_bytes = Prepared.Accounting.source_bytes accounting in
      let extra_input = Prepared.Accounting.input_bytes accounting in
      let selected_environment =
        Prepared.Environment.name (Prepared.environment prepared)
      in
      if
        Option.exists requested_environment ~f:(fun name ->
          not (String.equal name selected_environment))
      then Error Error.Static_selection_unavailable
      else if
        (not (within_add ~limit:t.limits.max_sources context.source_count extra_sources))
        || (not
              (within_add
                 ~limit:t.limits.max_context_source_bytes
                 context.charged_source_bytes
                 extra_bytes))
        || (not
              (within_add
                 ~limit:t.limits.max_effective_input_bytes
                 context.effective_input_bytes
                 extra_input))
        || not
             (within_add
                ~limit:t.limits.max_total_source_bytes
                t.charged_source_bytes
                extra_bytes)
      then Error Error.Limit_exceeded
      else if Option.is_some (Prepared.find_source prepared (Captured.id base))
      then Error Error.Static_selection_unavailable
      else (
        let%map generation = next_generation context in
        checkpoint ();
        let context =
          { context with
            basis = Document { document with static_build = Some build }
          ; source_count = context.source_count + extra_sources
          ; generation
          ; charged_source_bytes = context.charged_source_bytes + extra_bytes
          ; effective_input_bytes = context.effective_input_bytes + extra_input
          ; analysis_key = None
          }
        in
        { t with
          contexts = Map.set t.contexts ~key:identity ~data:context
        ; charged_source_bytes = t.charged_source_bytes + extra_bytes
        }))
;;

let detach_document_static t ~identity ~expected =
  let open Result.Let_syntax in
  let%bind context =
    Map.find t.contexts identity |> Result.of_option ~error:Error.Unknown_context
  in
  if not (Generation.equal context.generation expected)
  then Error Error.Stale_generation
  else (
    match context.basis with
    | Project _ -> Error Error.Unsupported_basis
    | Document { static_build = None; _ } -> Error Error.Static_selection_unavailable
    | Document ({ static_build = Some build; _ } as document) ->
      let accounting = Build.prepared build |> Prepared.accounting in
      let extra_sources = Prepared.Accounting.source_count accounting in
      let extra_bytes = Prepared.Accounting.source_bytes accounting in
      let extra_input = Prepared.Accounting.input_bytes accounting in
      let%map generation = next_generation context in
      let context =
        { context with
          basis = Document { document with static_build = None }
        ; source_count = context.source_count - extra_sources
        ; generation
        ; charged_source_bytes = context.charged_source_bytes - extra_bytes
        ; effective_input_bytes = context.effective_input_bytes - extra_input
        ; analysis_key = None
        }
      in
      { t with
        contexts = Map.set t.contexts ~key:identity ~data:context
      ; charged_source_bytes = t.charged_source_bytes - extra_bytes
      })
;;

let checked_context t version =
  let open Result.Let_syntax in
  let%bind context =
    Map.find t.contexts version.Version.identity
    |> Result.of_option ~error:Error.Unknown_context
  in
  if not (Generation.equal context.generation version.generation)
  then Error Error.Stale_generation
  else (
    let%bind actual =
      current_version context ~source:version.source
      |> Result.of_option ~error:(Error.Unknown_source version.source)
    in
    if Version.equal actual version then Ok context else Error Error.Stale_document)
;;

let replace t ~expected ~next_revision ~contents ~checkpoint =
  let open Result.Let_syntax in
  let%bind context = checked_context t expected in
  let source = expected.Version.source in
  if not (Set.mem context.editable source)
  then Error (Error.Read_only_source source)
  else if
    Option.exists (Map.find context.revisions source) ~f:(fun previous ->
      Editor_revision.compare next_revision previous <= 0)
  then Error Error.Revision_not_increasing
  else (
    let old_overlay = Map.find context.overlays source in
    if
      Option.is_none old_overlay
      && context.open_document_count >= t.limits.max_open_documents
    then Error Error.Limit_exceeded
    else (
      let new_bytes = String.length contents in
      let old_overlay_bytes =
        Option.value_map old_overlay ~default:0 ~f:Captured.byte_length
      in
      let old_selected_bytes =
        match old_overlay with
        | Some old -> Captured.byte_length old
        | None -> Map.find_exn context.base_by_id source |> Captured.byte_length
      in
      let charged_without_old = context.charged_source_bytes - old_overlay_bytes in
      let effective_without_old = context.effective_input_bytes - old_selected_bytes in
      let total_without_context = t.charged_source_bytes - context.charged_source_bytes in
      if
        new_bytes > t.limits.max_document_bytes
        || (not
              (within_add
                 ~limit:t.limits.max_context_source_bytes
                 charged_without_old
                 new_bytes))
        || (not
              (within_add
                 ~limit:t.limits.max_effective_input_bytes
                 effective_without_old
                 new_bytes))
        || not
             (within_add
                ~limit:t.limits.max_total_source_bytes
                total_without_context
                (charged_without_old + new_bytes))
      then Error Error.Limit_exceeded
      else (
        let%bind generation = next_generation context in
        let%map captured =
          Captured.create
            source
            ~contents
            ~expected_digest:None
            ~max_bytes:(Some t.limits.max_document_bytes)
            ~checkpoint
          |> Result.map_error ~f:(fun error -> Error.Capture error)
        in
        let context =
          { context with
            generation
          ; overlays = Map.set context.overlays ~key:source ~data:captured
          ; revisions = Map.set context.revisions ~key:source ~data:next_revision
          ; open_document_count =
              (context.open_document_count + if Option.is_some old_overlay then 0 else 1)
          ; charged_source_bytes = charged_without_old + new_bytes
          ; effective_input_bytes = effective_without_old + new_bytes
          ; analysis_key = None
          }
        in
        { t with
          contexts = Map.set t.contexts ~key:context.identity ~data:context
        ; charged_source_bytes = total_without_context + context.charged_source_bytes
        })))
;;

let close_document t ~expected =
  let open Result.Let_syntax in
  let%bind context = checked_context t expected in
  let source = expected.Version.source in
  let%bind overlay =
    Map.find context.overlays source |> Result.of_option ~error:Error.Document_not_open
  in
  let%bind generation = next_generation context in
  let restored_bytes = Map.find_exn context.base_by_id source |> Captured.byte_length in
  let effective_without_old =
    context.effective_input_bytes - Captured.byte_length overlay
  in
  if
    not
      (within_add
         ~limit:t.limits.max_effective_input_bytes
         effective_without_old
         restored_bytes)
  then Error Error.Limit_exceeded
  else (
    let new_effective = effective_without_old + restored_bytes in
    let context =
      { context with
        generation
      ; overlays = Map.remove context.overlays source
      ; open_document_count = context.open_document_count - 1
      ; charged_source_bytes = context.charged_source_bytes - Captured.byte_length overlay
      ; effective_input_bytes = new_effective
      ; analysis_key = None
      }
    in
    Ok
      { t with
        contexts = Map.set t.contexts ~key:context.identity ~data:context
      ; charged_source_bytes = t.charged_source_bytes - Captured.byte_length overlay
      })
;;

let same_selection first second =
  let first = Build.prepared first in
  let second = Build.prepared second in
  String.equal
    (Prepared.Environment.name (Prepared.environment first))
    (Prepared.Environment.name (Prepared.environment second))
  && Captured.Digest.equal
       (Prepared.compiler_contract first)
       (Prepared.compiler_contract second)
  && Captured.Digest.equal
       (Prepared.Environment.required_contract (Prepared.environment first))
       (Prepared.Environment.required_contract (Prepared.environment second))
;;

let refresh
      t
      ~identity
      ~expected
      ~new_identity
      ~base
      ~editable_sources
      ~policy
      ~checkpoint
  =
  let open Result.Let_syntax in
  let%bind context =
    Map.find t.contexts identity |> Result.of_option ~error:Error.Unknown_context
  in
  let%bind old_base =
    match context.basis with
    | Project base -> Ok base
    | Document _ -> Error Error.Unsupported_basis
  in
  if not (Generation.equal context.generation expected)
  then Error Error.Stale_generation
  else if
    Identity.equal identity new_identity
    || (not (String.equal identity.principal new_identity.principal))
    || (not (String.equal identity.host_incarnation new_identity.host_incarnation))
    || not (String.equal identity.connection_lease new_identity.connection_lease)
  then Error Error.Invalid_refresh_identity
  else if Map.mem t.contexts new_identity
  then Error Error.Duplicate_context
  else if
    context.open_document_count > 0 && Refresh.equal_policy policy Reject_open_documents
  then Error Error.Open_documents
  else if not (same_selection old_base base)
  then Error Error.Incompatible_environment
  else (
    let without_old =
      { t with
        contexts = Map.remove t.contexts identity
      ; charged_source_bytes = t.charged_source_bytes - context.charged_source_bytes
      }
    in
    admit without_old ~identity:new_identity ~base ~editable_sources ~checkpoint)
;;

let refresh_document t ~identity ~expected ~new_identity ~base ~policy ~checkpoint =
  let open Result.Let_syntax in
  let%bind context =
    Map.find t.contexts identity |> Result.of_option ~error:Error.Unknown_context
  in
  let%bind old_base, requested_environment =
    match context.basis with
    | Project _ -> Error Error.Unsupported_basis
    | Document { base; requested_environment; _ } -> Ok (base, requested_environment)
  in
  if not (Generation.equal context.generation expected)
  then Error Error.Stale_generation
  else if
    Identity.equal identity new_identity
    || (not (String.equal identity.principal new_identity.principal))
    || (not (String.equal identity.host_incarnation new_identity.host_incarnation))
    || not (String.equal identity.connection_lease new_identity.connection_lease)
  then Error Error.Invalid_refresh_identity
  else if Map.mem t.contexts new_identity
  then Error Error.Duplicate_context
  else if not (Captured.Id.equal (Captured.id old_base) (Captured.id base))
  then Error (Error.Unknown_source (Captured.id base))
  else if
    context.open_document_count > 0 && Refresh.equal_policy policy Reject_open_documents
  then Error Error.Open_documents
  else (
    let without_old =
      { t with
        contexts = Map.remove t.contexts identity
      ; charged_source_bytes = t.charged_source_bytes - context.charged_source_bytes
      }
    in
    admit_document
      without_old
      ~identity:new_identity
      ~base
      ~requested_environment
      ~checkpoint)
;;

let inputs t ~identity ~checkpoint =
  let open Result.Let_syntax in
  let%bind context =
    Map.find t.contexts identity |> Result.of_option ~error:Error.Unknown_context
  in
  match context.basis with
  | Document _ -> Error Error.Unsupported_basis
  | Project base ->
    let sources =
      List.map context.base_sources ~f:(fun source ->
        checkpoint ();
        Option.value (Map.find context.overlays (Captured.id source)) ~default:source)
    in
    Ok
      { Inputs.identity = context.identity
      ; generation = context.generation
      ; base
      ; sources
      ; changed_sources =
          Map.fold context.overlays ~init:[] ~f:(fun ~key ~data:_ reversed ->
            checkpoint ();
            key :: reversed)
          |> List.rev
      ; effective_input_bytes = context.effective_input_bytes
      }
;;

let document_inputs t ~identity ~checkpoint =
  let open Result.Let_syntax in
  let%bind context =
    Map.find t.contexts identity |> Result.of_option ~error:Error.Unknown_context
  in
  match context.basis with
  | Project _ -> Error Error.Unsupported_basis
  | Document { base; static_build; _ } ->
    checkpoint ();
    let source =
      Map.find context.overlays (Captured.id base) |> Option.value ~default:base
    in
    Ok
      { Document.Inputs.identity = context.identity
      ; generation = context.generation
      ; source
      ; base
      ; static_build
      ; effective_input_bytes = context.effective_input_bytes
      }
;;

let note_analysis t ~identity ~expected ~input_fingerprint =
  let open Result.Let_syntax in
  let%bind context =
    Map.find t.contexts identity |> Result.of_option ~error:Error.Unknown_context
  in
  if
    match context.basis with
    | Project _ -> false
    | Document _ -> true
  then Error Error.Unsupported_basis
  else if not (Generation.equal context.generation expected)
  then Error Error.Stale_generation
  else (
    let analysis_key =
      { Analysis_key.identity; generation = expected; input_fingerprint }
    in
    Ok
      { t with
        contexts =
          Map.set
            t.contexts
            ~key:identity
            ~data:{ context with analysis_key = Some analysis_key }
      })
;;

let close_context t ~identity =
  let open Result.Let_syntax in
  let%map context =
    Map.find t.contexts identity |> Result.of_option ~error:Error.Unknown_context
  in
  { t with
    contexts = Map.remove t.contexts identity
  ; charged_source_bytes = t.charged_source_bytes - context.charged_source_bytes
  }
;;
