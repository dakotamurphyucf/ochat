open Core
module Bundle = Agent_protocol.Authoring_bundle.Bundle
module Captured = Chatml_source
module Build = Chatml_project_build
module Prepared = Chatml_prepared_program
module Project = Chatml_authoring_bundle_project
module Document = Chatml_authoring_bundle_document
module Binding = Chatml_prompt_binding
module Intent = Authoring_upload_document.Intent
module Wire_source = Agent_protocol.Authoring_common.Source

module Proof = struct
  type t =
    | Project of Project.Validated.t
    | Chatmd_document of Document.Validated.t
end

module Graph = struct
  type t =
    { selector : Bundle.Manifest.Script.Selector.t option
    ; lock_source : Captured.t
    ; sources : Captured.t list
    }

  let selector t = t.selector
  let lock_source t = t.lock_source
  let sources t = t.sources
end

type t =
  { basis : Bundle.Receipt.basis
  ; graphs : Graph.t list
  }

let basis t = t.basis
let graphs t = t.graphs
let ( let* ) result f = Result.bind result ~f

let project_result validated ~selected_script ~checkpoint =
  checkpoint ();
  let build = Project.Validated.build validated in
  let prepared = Build.prepared build in
  let lock_source = Build.lock_source build in
  let lock_id = Captured.id lock_source in
  let selected_lock = Prepared.find_source prepared lock_id in
  match selected_lock with
  | None -> Error ()
  | Some selected_lock
    when not
           (Captured.Digest.equal
              (Captured.digest selected_lock)
              (Captured.digest lock_source)
            && Int.equal
                 (Captured.byte_length selected_lock)
                 (Captured.byte_length lock_source)) -> Error ()
  | Some _ ->
    let sources =
      Prepared.sources prepared
      |> List.filter ~f:(fun source ->
        not (Captured.Id.equal (Captured.id source) lock_id))
    in
    let environment = Prepared.Environment.name (Prepared.environment prepared) in
    if
      List.is_empty sources
      || not
           (Captured.Digest.equal
              (Project.Validated.input_fingerprint validated)
              (Prepared.fingerprint prepared))
    then Error ()
    else
      let* receipt =
        Bundle.Receipt.Project.create
          ~root_source:(Project.Validated.root_source validated)
          ~entry_source:(Prepared.entry prepared)
          ~selected_lock:lock_id
          ~input_fingerprint:(Prepared.fingerprint prepared)
          ~compiler_contract:(Prepared.compiler_contract prepared)
          ~environment
        |> Result.map_error ~f:(fun _ -> ())
      in
      Ok (receipt, { Graph.selector = selected_script; lock_source; sources })
;;

let of_proof manifest proof ~checkpoint =
  checkpoint ();
  let fingerprint = Bundle.Manifest.fingerprint manifest in
  match Bundle.Manifest.basis manifest, proof with
  | Project intent, Proof.Project validated ->
    if
      (not
         (Captured.Digest.equal
            fingerprint
            (Project.Validated.manifest_fingerprint validated)))
      || not
           (Captured.Id.equal
              (Bundle.Manifest.Project.root_source intent)
              (Project.Validated.root_source validated))
    then Error ()
    else
      let* receipt, graph = project_result validated ~selected_script:None ~checkpoint in
      Ok { basis = Bundle.Receipt.Project receipt; graphs = [ graph ] }
  | Chatmd_document intent, Proof.Chatmd_document validated ->
    if
      (not
         (Captured.Digest.equal
            fingerprint
            (Document.Validated.manifest_fingerprint validated)))
      || not
           (Captured.Id.equal
              (Bundle.Manifest.Chatmd_document.root_source intent)
              (Document.Validated.root_source validated))
    then Error ()
    else (
      let expected = Bundle.Manifest.Chatmd_document.scripts intent in
      let actual = Document.Validated.scripts validated in
      let* scripts, graphs =
        if List.length expected <> List.length actual
        then Error ()
        else
          List.fold
            (List.zip_exn expected actual)
            ~init:(Ok ([], []))
            ~f:(fun result (authored, script) ->
              let* scripts, graphs = result in
              checkpoint ();
              let selector = Bundle.Manifest.Script.selector authored in
              if
                (not
                   (Bundle.Manifest.Script.Selector.equal
                      selector
                      (Document.Validated.Script.selector script)))
                || not
                     (Captured.Digest.equal
                        (Bundle.Manifest.Script.Selector.body_digest selector)
                        (Binding.script_digest (Document.Validated.Script.binding script)))
              then Error ()
              else (
                match
                  ( Bundle.Manifest.Script.choice authored
                  , Document.Validated.Script.project script )
                with
                | Source_only, None ->
                  Ok
                    ( Bundle.Receipt.Script.Source_only
                        { selector
                        ; body_digest =
                            Binding.script_digest
                              (Document.Validated.Script.binding script)
                        }
                      :: scripts
                    , graphs )
                | Selected_project _, Some project ->
                  let* receipt, graph =
                    project_result project ~selected_script:(Some selector) ~checkpoint
                  in
                  Ok
                    ( Bundle.Receipt.Script.Selected_project
                        { selector
                        ; body_digest =
                            Binding.script_digest
                              (Document.Validated.Script.binding script)
                        ; project = receipt
                        }
                      :: scripts
                    , graph :: graphs )
                | Source_only, Some _ | Selected_project _, None -> Error ()))
      in
      let* receipt =
        Bundle.Receipt.Chatmd_document.create
          ~root_source:(Document.Validated.root_source validated)
          ~closure_fingerprint:(Document.Validated.document_bundle_fingerprint validated)
          ~scripts:(List.rev scripts)
        |> Result.map_error ~f:(fun _ -> ())
      in
      Ok { basis = Bundle.Receipt.Chatmd_document receipt; graphs = List.rev graphs })
  | Project _, Proof.Chatmd_document _ | Chatmd_document _, Proof.Project _ -> Error ()
;;

module Installed = struct
  type t =
    { intent : Intent.t
    ; intent_bytes : string
    ; receipt : Bundle.Receipt.t
    ; supplemental : (int * Captured.t) list
    }

  let intent t = t.intent
  let intent_bytes t = t.intent_bytes
  let receipt t = t.receipt
  let supplemental t = t.supplemental
end

let same_bytes left right ~checkpoint =
  if Captured.byte_length left <> Captured.byte_length right
  then false
  else if not (Captured.Digest.equal (Captured.digest left) (Captured.digest right))
  then false
  else (
    let left = Captured.contents left in
    let right = Captured.contents right in
    let same = ref true in
    let index = ref 0 in
    while !same && !index < String.length left do
      if !index mod 4096 = 0 then checkpoint ();
      same := Char.equal left.[!index] right.[!index];
      incr index
    done;
    !same)
;;

let wire_source source =
  Wire_source.create
    ~id:(Captured.id source)
    ~digest:(Captured.digest source)
    ~byte_length:(Captured.byte_length source)
    ~editor_revision:None
  |> Result.map_error ~f:(fun _ -> ())
;;

let install_plan
      candidate
      ~manifest
      ~uploaded
      ~owner
      ~upload_id
      ~commit_key
      ~payload_digest
      ~bundle_id
      ~expires_at
      ~max_graphs
      ~max_sources
      ~max_supplemental_bytes
      ~max_intent_bytes
      ~checkpoint
  =
  checkpoint ();
  if
    max_graphs < 0
    || max_sources < 0
    || max_supplemental_bytes < 0
    || max_intent_bytes <= 0
    || List.length candidate.graphs > max_graphs
    || List.length uploaded <> List.length (Bundle.Manifest.members manifest)
  then Error ()
  else
    let* receipt =
      Bundle.Receipt.create
        ~owner
        ~upload_id
        ~bundle_id
        ~manifest_fingerprint:(Bundle.Manifest.fingerprint manifest)
        ~basis:candidate.basis
        ~expires_at
      |> Result.map_error ~f:(fun _ -> ())
    in
    let* uploaded_index =
      List.foldi
        uploaded
        ~init:(Ok (Map.empty (module Captured.Id)))
        ~f:(fun ordinal result source ->
          let* map = result in
          checkpoint ();
          if Map.mem map (Captured.id source)
          then Error ()
          else Ok (Map.set map ~key:(Captured.id source) ~data:(ordinal, source)))
    in
    let next_ordinal = ref 0 in
    let remaining_sources = ref max_sources in
    let remaining_bytes = ref max_supplemental_bytes in
    let supplemental = ref [] in
    let reference source =
      checkpoint ();
      if !remaining_sources <= 0
      then Error ()
      else (
        decr remaining_sources;
        let* metadata = wire_source source in
        let location =
          match Map.find uploaded_index (Captured.id source) with
          | Some (ordinal, uploaded_source)
            when same_bytes source uploaded_source ~checkpoint ->
            Ok (Intent.Source_ref.Uploaded_member ordinal)
          | None | Some _ ->
            let bytes = Captured.byte_length source in
            if bytes > !remaining_bytes
            then Error ()
            else (
              remaining_bytes := !remaining_bytes - bytes;
              let ordinal = !next_ordinal in
              incr next_ordinal;
              supplemental := (ordinal, source) :: !supplemental;
              Ok (Intent.Source_ref.Supplemental_file ordinal))
        in
        let* location = location in
        Intent.Source_ref.create ~source:metadata ~location
        |> Result.map_error ~f:(fun _ -> ()))
    in
    let* graphs =
      List.fold candidate.graphs ~init:(Ok []) ~f:(fun result graph ->
        let* completed = result in
        let* lock = reference graph.lock_source in
        let* sources =
          List.fold graph.sources ~init:(Ok []) ~f:(fun result source ->
            let* sources = result in
            let* source = reference source in
            Ok (source :: sources))
        in
        let* graph =
          Intent.Graph.create ~selector:graph.selector ~lock ~sources:(List.rev sources)
          |> Result.map_error ~f:(fun _ -> ())
        in
        Ok (graph :: completed))
    in
    let* intent =
      Intent.create
        ~owner
        ~upload_id
        ~commit_key
        ~payload_digest
        ~manifest_fingerprint:(Bundle.Manifest.fingerprint manifest)
        ~receipt
        ~graphs:(List.rev graphs)
        ~max_graphs
        ~max_sources
        ~max_bytes:max_intent_bytes
      |> Result.map_error ~f:(fun _ -> ())
    in
    let* intent_bytes =
      Intent.to_bytes intent ~max_bytes:max_intent_bytes
      |> Result.map_error ~f:(fun _ -> ())
    in
    Ok { Installed.intent; intent_bytes; receipt; supplemental = List.rev !supplemental }
;;
