open Core
module Metadata = Chatmd_shell_spec.Authoring_metadata
module Spec = Chatmd_shell_spec.Extension_spec
module Caps = Tool_capability

type error =
  { code : string
  ; message : string
  }
[@@deriving sexp]

let error code message = Error { code; message }

let cap_result value =
  Result.map_error value ~f:(fun (e : Caps.error) ->
    { code = e.code; message = e.message })
;;

let digest = Chatmd_shell_spec.Source_ref.digest

type catalog =
  { identity : string
  ; packages : Metadata.help String.Map.t
  ; topics : Metadata.task list String.Map.t
  ; fingerprint : string
  }

let catalog ~identity ~packages ~topics =
  let valid_tasks tasks =
    (not (List.is_empty tasks))
    && List.length tasks <= 5
    && Option.is_none (List.find_a_dup tasks ~compare:Metadata.compare_task)
  in
  if
    String.length identity <> 64
    || (not
          (String.for_all identity ~f:(function
             | '0' .. '9' | 'a' .. 'f' -> true
             | _ -> false)))
    || List.length packages > 128
    || List.length topics > 512
    || (not (List.for_all packages ~f:(fun p -> Result.is_ok (Metadata.validate_help p))))
    || not
         (List.for_all topics ~f:(fun (id, tasks) ->
            Metadata.valid_topic id && valid_tasks tasks))
  then error "authoring.invalid_catalog" "invalid installed authoring catalog"
  else (
    match
      ( String.Map.of_alist (List.map packages ~f:(fun p -> p.Metadata.package, p))
      , String.Map.of_alist topics )
    with
    | `Duplicate_key _, _ | _, `Duplicate_key _ ->
      error "authoring.invalid_catalog" "duplicate package or topic"
    | `Ok packages, `Ok topics ->
      let compatible =
        Map.for_all packages ~f:(fun package ->
          List.for_all package.Metadata.topics ~f:(fun id ->
            match Map.find topics id with
            | None -> false
            | Some tasks ->
              List.exists package.tasks ~f:(fun task ->
                List.mem tasks task ~equal:Metadata.equal_task)))
      in
      if not compatible
      then error "authoring.invalid_catalog" "package has missing or incompatible topics"
      else (
        let fingerprint =
          [%sexp
            (identity : string)
          , (Map.data packages : Metadata.help list)
          , (Map.to_alist topics : (string * Metadata.task list) list)]
          |> Sexp.to_string
          |> digest
        in
        Ok { identity; packages; topics; fingerprint }))
;;

type t =
  { policy : Spec.policy
  ; policy_source : Chatmd_shell_spec.Source_ref.t option
  ; capabilities : Caps.t
  ; added_helpers : Caps.reference list
  ; helper_pointers : (Metadata.helper * string) list
  ; authoring_tools : (Caps.reference * Metadata.help) list
  ; inject_primer : bool
  ; preload_topics : string list
  ; corpus_identity : string option
  ; fingerprint : string
  }

let policy t = t.policy
let policy_source t = t.policy_source
let capabilities t = t.capabilities
let added_helpers t = t.added_helpers
let helper_pointers t = t.helper_pointers
let authoring_tools t = t.authoring_tools
let inject_primer t = t.inject_primer
let preload_topics t = t.preload_topics
let corpus_identity t = t.corpus_identity
let fingerprint t = t.fingerprint

let authentic_helper registry helper =
  match Caps.find registry ~name:(Metadata.helper_name helper) with
  | Ok binding
    when Option.equal Metadata.equal_helper (Caps.metadata binding).helper (Some helper)
    -> Ok binding
  | _ ->
    error
      "authoring.helper_unavailable"
      "required authoring helper is absent from the permitted capability set"
;;

let resolve ?(policy = Spec.Auto) ?catalog ~ceiling ~selected_names () =
  let open Result.Let_syntax in
  let%bind selected = Caps.select ceiling ~names:selected_names |> cap_result in
  let authoring_tools =
    Caps.references selected
    |> List.filter_map ~f:(fun reference ->
      match Caps.find selected ~name:reference.name with
      | Error _ -> assert false
      | Ok binding ->
        Option.map (Caps.metadata binding).authoring ~f:(fun help -> reference, help))
  in
  let active = not (List.is_empty authoring_tools) in
  let automatic = active && not (Spec.equal_policy policy Spec.Manual) in
  let%bind preload_topics =
    match policy with
    | Spec.Auto | Manual -> Ok []
    | Preload topics
      when active
           && (not (List.is_empty topics))
           && List.length topics <= 32
           && List.for_all topics ~f:Metadata.valid_topic
           && Option.is_none (List.find_a_dup topics ~compare:String.compare) -> Ok topics
    | Preload _ ->
      error
        "authoring.invalid_preload"
        "preload requires authoring tools and unique valid topic IDs"
  in
  let tasks =
    List.concat_map authoring_tools ~f:(fun (_, help) -> help.Metadata.tasks)
    |> List.dedup_and_sort ~compare:Metadata.compare_task
  in
  let%bind used_catalog =
    if not automatic
    then Ok None
    else (
      match catalog with
      | None ->
        error
          "authoring.catalog_unavailable"
          "automatic guidance requires the compatible installed corpus"
      | Some catalog ->
        let valid_help help =
          match Map.find catalog.packages help.Metadata.package with
          | None -> false
          | Some package ->
            List.for_all help.tasks ~f:(fun task ->
              List.mem package.tasks task ~equal:Metadata.equal_task)
            && List.for_all help.topics ~f:(fun topic ->
              List.mem package.topics topic ~equal:String.equal
              && Option.value_map
                   (Map.find catalog.topics topic)
                   ~default:false
                   ~f:(fun supported ->
                     List.exists help.tasks ~f:(fun task ->
                       List.mem supported task ~equal:Metadata.equal_task)))
        in
        let valid_topic id =
          match Map.find catalog.topics id with
          | None -> false
          | Some supported ->
            List.exists tasks ~f:(fun task ->
              List.mem supported task ~equal:Metadata.equal_task)
        in
        if
          (not (List.for_all authoring_tools ~f:(fun (_, help) -> valid_help help)))
          || not (List.for_all preload_topics ~f:valid_topic)
        then
          error
            "authoring.incompatible_help"
            "help package or preloaded topic is unavailable for the selected authoring \
             tasks"
        else Ok (Some catalog))
  in
  let required =
    List.concat_map authoring_tools ~f:(fun (_, help) -> help.Metadata.required_helpers)
  in
  let requested_helpers =
    (if automatic then [ Metadata.Reference; Validation ] else []) @ required
    |> List.dedup_and_sort ~compare:Metadata.compare_helper
  in
  let%bind helper_bindings =
    List.fold requested_helpers ~init:(Ok []) ~f:(fun acc helper ->
      let%bind bindings = acc in
      let%map binding =
        authentic_helper (if automatic then ceiling else selected) helper
      in
      binding :: bindings)
  in
  let names =
    selected_names
    @ List.map helper_bindings ~f:(fun binding -> (Caps.reference binding).name)
    |> List.dedup_and_sort ~compare:String.compare
  in
  let%bind capabilities = Caps.select ceiling ~names |> cap_result in
  let added_helpers =
    Caps.references capabilities
    |> List.filter ~f:(fun reference ->
      not (List.mem selected_names reference.name ~equal:String.equal))
  in
  let helper_pointers =
    List.filter_map [ Metadata.Reference; Validation ] ~f:(fun helper ->
      match authentic_helper capabilities helper with
      | Ok binding -> Some (helper, (Caps.reference binding).name)
      | Error _ -> None)
  in
  let corpus_identity = Option.map used_catalog ~f:(fun catalog -> catalog.identity) in
  let fingerprint =
    [%sexp
      ("ochat.authoring-policy.v1" : string)
    , (policy : Spec.policy)
    , (Caps.fingerprint selected : string)
    , (Caps.fingerprint capabilities : string)
    , (Option.map used_catalog ~f:(fun c -> c.fingerprint) : string option)
    , (preload_topics : string list)]
    |> Sexp.to_string
    |> digest
  in
  Ok
    { policy
    ; policy_source = None
    ; capabilities
    ; added_helpers
    ; helper_pointers
    ; authoring_tools
    ; inject_primer = automatic
    ; preload_topics
    ; corpus_identity
    ; fingerprint
    }
;;

let resolve_context ?context ?catalog ~ceiling ~selected_names () =
  let open Result.Let_syntax in
  let%bind () =
    match context with
    | Some (config : Spec.authoring_context) when config.version <> 1 ->
      error "authoring.invalid_policy_version" "unsupported authoring-context version"
    | _ -> Ok ()
  in
  let%map plan =
    resolve
      ?policy:(Option.map context ~f:(fun c -> c.Spec.policy))
      ?catalog
      ~ceiling
      ~selected_names
      ()
  in
  let policy_source = Option.map context ~f:(fun c -> c.Spec.source_ref) in
  let fingerprint =
    [%sexp (plan.fingerprint : string), (context : Spec.authoring_context option)]
    |> Sexp.to_string
    |> digest
  in
  { plan with policy_source; fingerprint }
;;
