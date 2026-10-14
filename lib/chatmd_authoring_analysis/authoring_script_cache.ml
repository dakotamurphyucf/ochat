open Core
module Context = Chatml_authoring_context
module Captured = Chatml_source
module Prepared = Chatml_prepared_program
module Analysis = Chatml_authoring_analysis
module Admission = Authoring_script_admission
module Error = Admission.Error
module Document_stamp = Admission.Document_stamp
module Script = Admission.Script

module Candidate = struct
  type t =
    { script : Script.t
    ; checked : Analysis.Checked.t
    ; policy : Analysis.Policy.t
    }

  let stamp t = Script.document_stamp t.script
  let checked t = t.checked

  let analyze_using script ~policy ~checkpoint ~analyze =
    let open Result.Let_syntax in
    let%bind checked =
      analyze script.Script.selected
      |> Result.map_error ~f:(fun error -> Error.Compiler error)
    in
    let stamp = Script.document_stamp script in
    if
      (not
         (Captured.Digest.equal
            (Analysis.Checked.input_fingerprint checked)
            (Document_stamp.input_fingerprint stamp)))
      || (not
            (Captured.Digest.equal
               (Analysis.Checked.required_contract checked)
               (Document_stamp.required_contract stamp)))
      || (not
            (Captured.Digest.equal
               (Analysis.Checked.selected_contract checked)
               (Document_stamp.selected_contract stamp)))
      || not
           (String.equal
              (Prepared.Environment.name (Analysis.Checked.environment checked))
              (Prepared.Environment.name (Document_stamp.environment stamp)))
    then Error Error.Selection_mismatch
    else Ok { script; checked; policy }
  ;;

  let analyze script ~policy ~env ~target ~checkpoint =
    analyze_using script ~policy ~checkpoint ~analyze:(fun build ->
      Analysis.Checked.analyze_build build ~policy ~env ~target ~checkpoint)
  ;;

  let analyze_with_contract script ~contract ~policy ~env ~checkpoint =
    analyze_using script ~policy ~checkpoint ~analyze:(fun build ->
      Analysis.Checked.analyze_build_with_contract
        build
        ~contract
        ~policy
        ~env
        ~checkpoint)
  ;;
end

module Document_cache = struct
  type t =
    { policy : Analysis.Policy.t
    ; results :
        (Context.Identity.t, Candidate.t, Context.Identity.comparator_witness) Map.t
    }

  let create ~policy = { policy; results = Map.empty (module Context.Identity) }
  let retained_count t = Map.length t.results

  let same_source stamp source =
    Captured.Id.equal (Document_stamp.document_source stamp) (Captured.id source)
    && Captured.Digest.equal
         (Document_stamp.document_digest stamp)
         (Captured.digest source)
    && Int.equal stamp.Document_stamp.document_byte_length (Captured.byte_length source)
  ;;

  let same_inputs stamp inputs =
    Context.Identity.equal
      (Document_stamp.identity stamp)
      (Context.Document.Inputs.identity inputs)
    && Context.Generation.equal
         (Document_stamp.generation stamp)
         (Context.Document.Inputs.generation inputs)
    && same_source stamp (Context.Document.Inputs.source inputs)
  ;;

  let same_view stamp view =
    Context.Identity.equal
      (Document_stamp.identity stamp)
      (Context.Document.View.identity view)
    && Context.Generation.equal
         (Document_stamp.generation stamp)
         (Context.Document.View.generation view)
    && same_source stamp (Context.Document.View.source view)
  ;;

  let transition t ~contexts =
    let results =
      Map.filter t.results ~f:(fun candidate ->
        match
          Context.find_document
            contexts
            ~identity:(Document_stamp.identity (Candidate.stamp candidate))
        with
        | Some view -> same_view (Candidate.stamp candidate) view
        | None -> false)
    in
    { t with results }
  ;;

  let publish t ~current_inputs ~occupied_elsewhere candidate ~checkpoint =
    let stamp = Candidate.stamp candidate in
    let identity = Document_stamp.identity stamp in
    checkpoint ();
    if not (phys_equal t.policy candidate.Candidate.policy)
    then Error Error.Policy_mismatch
    else if occupied_elsewhere < 0
    then Error Error.Invalid_occupancy
    else if not (same_inputs stamp current_inputs)
    then Error Error.Stale_document
    else if
      occupied_elsewhere > Analysis.Policy.max_results t.policy
      ||
      let available = Analysis.Policy.max_results t.policy - occupied_elsewhere in
      Map.length t.results > available
      || ((not (Map.mem t.results identity)) && Map.length t.results >= available)
    then Error Error.Retention_limit
    else Ok { t with results = Map.set t.results ~key:identity ~data:candidate }
  ;;

  let retire t ~identity =
    if Map.mem t.results identity
    then { t with results = Map.remove t.results identity }
    else t
  ;;

  let current t ~contexts ~identity =
    match Map.find t.results identity, Context.find_document contexts ~identity with
    | Some candidate, Some view when same_view (Candidate.stamp candidate) view ->
      Some candidate
    | None, _ | _, None | Some _, Some _ -> None
  ;;
end
