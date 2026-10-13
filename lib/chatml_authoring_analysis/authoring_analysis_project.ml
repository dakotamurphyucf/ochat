open Core
module Captured = Chatml_source
module Prepared = Chatml_prepared_program
module Build = Chatml_project_build
module Context = Chatml_authoring_context
module Effective = Chatml_authoring_effective
module Policy = Authoring_analysis_checked.Policy
module Error = Authoring_analysis_checked.Error
module Checked = Authoring_analysis_checked.Checked

let equal_source left right ~checkpoint =
  if
    (not (Captured.Id.equal (Captured.id left) (Captured.id right)))
    || (not (Captured.Digest.equal (Captured.digest left) (Captured.digest right)))
    || not (Int.equal (Captured.byte_length left) (Captured.byte_length right))
  then false
  else (
    let left_bytes = Captured.contents left in
    let right_bytes = Captured.contents right in
    let rec check offset =
      checkpoint ();
      if offset >= String.length left_bytes
      then true
      else (
        let stop = Int.min (String.length left_bytes) (offset + 4096) in
        let rec compare_byte index =
          if index >= stop
          then check stop
          else if Char.equal left_bytes.[index] right_bytes.[index]
          then compare_byte (index + 1)
          else false
        in
        compare_byte offset)
    in
    check 0)
;;

let same_sources left right ~checkpoint =
  let sorted sources =
    List.sort sources ~compare:(fun left right ->
      checkpoint ();
      Captured.Id.compare (Captured.id left) (Captured.id right))
  in
  let rec compare left right =
    checkpoint ();
    match left, right with
    | [], [] -> true
    | left :: left_tail, right :: right_tail ->
      equal_source left right ~checkpoint && compare left_tail right_tail
    | [], _ :: _ | _ :: _, [] -> false
  in
  compare (sorted left) (sorted right)
;;

let same_source_revisions left right =
  let sorted sources =
    List.sort sources ~compare:(fun left right ->
      Captured.Id.compare (Captured.id left) (Captured.id right))
  in
  let rec compare left right =
    match left, right with
    | [], [] -> true
    | left :: left_tail, right :: right_tail ->
      Captured.Id.equal (Captured.id left) (Captured.id right)
      && Captured.Digest.equal (Captured.digest left) (Captured.digest right)
      && Int.equal (Captured.byte_length left) (Captured.byte_length right)
      && compare left_tail right_tail
    | [], _ :: _ | _ :: _, [] -> false
  in
  compare (sorted left) (sorted right)
;;

module Project_stamp = struct
  type t =
    { identity : Context.Identity.t
    ; generation : Context.Generation.t
    ; input_fingerprint : Captured.Digest.t
    ; required_contract : Captured.Digest.t
    ; selected_contract : Captured.Digest.t
    ; environment : Prepared.Environment.t
    }

  let identity t = t.identity
  let generation t = t.generation
  let input_fingerprint t = t.input_fingerprint
  let required_contract t = t.required_contract
  let selected_contract t = t.selected_contract
  let environment t = t.environment

  let equal left right =
    Context.Identity.equal left.identity right.identity
    && Context.Generation.equal left.generation right.generation
    && Captured.Digest.equal left.input_fingerprint right.input_fingerprint
    && Captured.Digest.equal left.required_contract right.required_contract
    && Captured.Digest.equal left.selected_contract right.selected_contract
    && Authoring_analysis_checked.same_environment left.environment right.environment
  ;;

  let sexp_of_t t =
    [%sexp
      { identity = (t.identity : Context.Identity.t)
      ; generation = (t.generation : Context.Generation.t)
      ; input_fingerprint = (t.input_fingerprint : Captured.Digest.t)
      ; required_contract = (t.required_contract : Captured.Digest.t)
      ; selected_contract = (t.selected_contract : Captured.Digest.t)
      ; environment = (Prepared.Environment.name t.environment : string)
      }]
  ;;
end

module Project_payload = struct
  type t =
    { stamp : Project_stamp.t
    ; checked : Checked.t
    }

  let stamp t = t.stamp
  let checked t = t.checked
end

module Project_candidate = struct
  type t =
    { payload : Project_payload.t
    ; inputs : Context.Inputs.t
    ; policy : Policy.t
    }

  let payload t = t.payload

  let exact_receipt inputs receipt ~checkpoint =
    let base = Context.Inputs.base inputs in
    let old_lock = Build.lock_source base in
    let old_lock_id = Captured.id old_lock in
    let effective_sources = Context.Inputs.sources inputs in
    let old_locks =
      List.filter effective_sources ~f:(fun source ->
        checkpoint ();
        Captured.Id.equal (Captured.id source) old_lock_id)
    in
    match old_locks with
    | [ source ] when equal_source source old_lock ~checkpoint ->
      let build = Effective.Overlay.build receipt in
      let new_lock_id = Captured.id (Build.lock_source build) in
      if
        List.exists effective_sources ~f:(fun source ->
          checkpoint ();
          Captured.Id.equal (Captured.id source) new_lock_id)
      then false
      else (
        let without_old =
          List.filter effective_sources ~f:(fun source ->
            checkpoint ();
            not (Captured.Id.equal (Captured.id source) old_lock_id))
        in
        let without_new =
          Prepared.sources (Build.prepared build)
          |> List.filter ~f:(fun source ->
            checkpoint ();
            not (Captured.Id.equal (Captured.id source) new_lock_id))
        in
        same_sources without_old without_new ~checkpoint)
    | [] | _ :: _ -> false
  ;;

  let analyze_validated inputs ~receipt ~policy ~env ~target ~checkpoint =
    if
      (not
         (Context.Identity.equal
            (Context.Inputs.identity inputs)
            (Effective.Overlay.identity receipt)))
      || (not
            (Context.Generation.equal
               (Context.Inputs.generation inputs)
               (Effective.Overlay.generation receipt)))
      || not (exact_receipt inputs receipt ~checkpoint)
    then Error Error.Receipt_mismatch
    else (
      let build = Effective.Overlay.build receipt in
      match Checked.analyze_build build ~policy ~env ~target ~checkpoint with
      | Error error -> Error error
      | Ok checked ->
        let stamp =
          { Project_stamp.identity = Context.Inputs.identity inputs
          ; generation = Context.Inputs.generation inputs
          ; input_fingerprint = Checked.input_fingerprint checked
          ; required_contract = Checked.required_contract checked
          ; selected_contract = Checked.selected_contract checked
          ; environment = Checked.environment checked
          }
        in
        Ok { payload = { Project_payload.stamp; checked }; inputs; policy })
  ;;
end

module Project_cache = struct
  type t =
    { contexts : Context.t
    ; policy : Policy.t
    ; results :
        (Context.Identity.t, Project_payload.t, Context.Identity.comparator_witness) Map.t
    }

  let create ~contexts ~policy =
    { contexts; policy; results = Map.empty (module Context.Identity) }
  ;;

  let contexts t = t.contexts

  let current_key_matches contexts payload =
    let stamp = Project_payload.stamp payload in
    let identity = Project_stamp.identity stamp in
    match Context.analysis_key contexts ~identity, Context.find contexts ~identity with
    | Some key, Some view ->
      Context.Identity.equal (Context.Analysis_key.identity key) identity
      && Context.Generation.equal
           (Context.Analysis_key.generation key)
           (Project_stamp.generation stamp)
      && Captured.Digest.equal
           (Context.Analysis_key.input_fingerprint key)
           (Project_stamp.input_fingerprint stamp)
      && String.equal
           (Context.View.environment view)
           (Prepared.Environment.name (Project_stamp.environment stamp))
      && Captured.Digest.equal
           (Context.View.compiler_contract view)
           (Project_stamp.required_contract stamp)
    | None, _ | _, None -> false
  ;;

  let transition t ~contexts =
    let results =
      Map.filter t.results ~f:(fun payload -> current_key_matches contexts payload)
    in
    { t with contexts; results }
  ;;

  let current t ~identity =
    match Map.find t.results identity with
    | Some payload when current_key_matches t.contexts payload -> Some payload
    | None | Some _ -> None
  ;;

  let publish t candidate ~checkpoint =
    let open Result.Let_syntax in
    let payload = Project_candidate.payload candidate in
    let stamp = Project_payload.stamp payload in
    let identity = Project_stamp.identity stamp in
    if not (phys_equal t.policy candidate.policy)
    then Error Error.Policy_mismatch
    else (
      let%bind current_inputs =
        Context.inputs t.contexts ~identity ~checkpoint
        |> Result.map_error ~f:(fun _ -> Error.Stale_generation)
      in
      let current_view = Context.find t.contexts ~identity in
      if
        (not
           (Context.Generation.equal
              (Context.Inputs.generation current_inputs)
              (Project_stamp.generation stamp)))
        || (not
              (same_source_revisions
                 (Context.Inputs.sources current_inputs)
                 (Context.Inputs.sources candidate.inputs)))
        || (not
              (Captured.Digest.equal
                 (Prepared.fingerprint
                    (Build.prepared (Context.Inputs.base current_inputs)))
                 (Prepared.fingerprint
                    (Build.prepared (Context.Inputs.base candidate.inputs)))))
        || not
             (match current_view with
              | None -> false
              | Some view ->
                String.equal
                  (Context.View.environment view)
                  (Prepared.Environment.name (Project_stamp.environment stamp))
                && Captured.Digest.equal
                     (Context.View.compiler_contract view)
                     (Project_stamp.required_contract stamp)
                && Captured.Digest.equal
                     (Project_stamp.required_contract stamp)
                     (Project_stamp.selected_contract stamp))
      then Error Error.Stale_generation
      else if
        (not (Map.mem t.results identity)) && Map.length t.results >= t.policy.max_results
      then Error Error.Retention_limit
      else (
        let%map contexts =
          Context.note_analysis
            t.contexts
            ~identity
            ~expected:(Project_stamp.generation stamp)
            ~input_fingerprint:(Project_stamp.input_fingerprint stamp)
          |> Result.map_error ~f:(fun _ -> Error.Stale_generation)
        in
        { t with contexts; results = Map.set t.results ~key:identity ~data:payload }))
  ;;
end
