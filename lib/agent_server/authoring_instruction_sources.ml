open! Core
module P = Agent_protocol
module I = Instruction_content
module Request = Agent_session.Instruction_source_request
module Port = Agent_session.Instruction_source_port
module Sources = Instruction_sources

exception Work_limit

let error code message = Error (P.Error.create code ~message ~retryable:false ())
let denied () = error Permission_denied "instruction root is unavailable or denied"
let exhausted () = error Resource_limit "instruction source request limit"

let source_error = function
  | Authoring_source_host.Error.Unauthorized | Expired_actor | Foreign_root ->
    P.Error.create
      Permission_denied
      ~message:"instruction source permission denied"
      ~retryable:false
      ()
  | Closed ->
    P.Error.create
      Server_shutting_down
      ~message:"instruction source is closed"
      ~retryable:true
      ()
  | Busy ->
    P.Error.create Conflict ~message:"instruction source is busy" ~retryable:true ()
  | Foreign_domain
  | Context_limit
  | Result_limit
  | Unsupported_source_selection
  | Sources _
  | Root _
  | Local _
  | Project _
  | Source_capture _
  | Document_parser _
  | Document_bundle_limit
  | Project_declaration_unavailable
  | Invalid_project_declaration
  | Selected_script_unavailable ->
    P.Error.create
      Resource_limit
      ~message:"instruction source admission failed"
      ~retryable:false
      ()
;;

let lower_error = function
  | Sources.Error.Source failure -> source_error failure
  | Limit_exceeded ->
    P.Error.create
      Resource_limit
      ~message:"instruction source request limit"
      ~retryable:false
      ()
  | Foreign_candidate | Missing | Content _ ->
    P.Error.create
      Conflict
      ~message:"instruction source changed; refresh explicitly"
      ~retryable:false
      ()
;;

let limits =
  I.Limits.create
    ~max_files:64
    ~max_file_bytes:262144
    ~max_total_bytes:262144
    ~max_work_steps:131072
  |> Or_error.ok_exn
;;

type root =
  { id : I.Root_id.t
  ; source : Authoring_source_host.Root.t
  ; binding : Sources.Root.t
  }

type discovered =
  { candidates : (root * Sources.Candidate.t) list
  ; issues : (I.Root_id.t * Source.Portable_path.t * string) list
  }

let create source ~configured ~is_current =
  Port.create ~open_use:(fun actor ~checkpoint ->
    let open Result.Let_syntax in
    if not (is_current ())
    then denied ()
    else (
      let%bind admitted =
        Authoring_source_host.roots source ~actor ~checkpoint
        |> Result.map_error ~f:source_error
      in
      let%bind roots =
        List.map admitted ~f:(fun root ->
          match Authoring_configured_project_policy.instruction_root configured ~root with
          | None -> denied ()
          | Some id ->
            Sources.bind_root source ~actor ~root ~id
            |> Result.map_error ~f:lower_error
            |> Result.map ~f:(fun binding -> { id; source = root; binding }))
        |> Result.all
      in
      let find id = List.find roots ~f:(fun root -> I.Root_id.equal root.id id) in
      let check_now ids =
        if not (is_current ())
        then denied ()
        else
          List.fold_result ids ~init:() ~f:(fun () id ->
            match find id with
            | None -> denied ()
            | Some root ->
              Authoring_source_host.check_root_current source ~actor root.source
              |> Result.map_error ~f:source_error)
      in
      let bounded ~checkpoint f =
        let work = ref 0 in
        let poll () =
          checkpoint ();
          if !work >= 131072 then raise Work_limit;
          Int.incr work
        in
        try f poll with
        | Work_limit -> exhausted ()
      in
      let discover selection ~checkpoint =
        let%bind () = Request.Selection.validate selection in
        let%bind () = check_now (Request.Selection.roots selection) in
        let count = ref 0 in
        let bytes = ref 0 in
        let%bind result =
          List.fold_result
            (Request.Selection.roots selection)
            ~init:{ candidates = []; issues = [] }
            ~f:(fun acc id ->
              match find id with
              | None -> denied ()
              | Some root ->
                let result =
                  match selection with
                  | Request.Selection.Project { scope; _ } ->
                    Sources.discover_project
                      root.binding
                      ~actor
                      ~scope
                      ~limits
                      ~checkpoint
                  | Skills _ ->
                    Sources.discover_skills root.binding ~actor ~limits ~checkpoint
                in
                let%bind result = Result.map_error result ~f:lower_error in
                let candidates = Sources.Discovery.candidates result in
                let issues = Sources.Discovery.issues result in
                let added = List.length candidates + List.length issues in
                if added > 64 - !count
                then exhausted ()
                else (
                  count := !count + added;
                  let%bind () =
                    List.fold_result candidates ~init:() ~f:(fun () candidate ->
                      checkpoint ();
                      let length =
                        I.Observed.byte_length (Sources.Candidate.observed candidate)
                      in
                      if length < 0 || length > 262144 - !bytes
                      then exhausted ()
                      else (
                        bytes := !bytes + length;
                        Ok ()))
                  in
                  Ok
                    { candidates =
                        acc.candidates
                        @ List.map candidates ~f:(fun candidate -> root, candidate)
                    ; issues =
                        acc.issues
                        @ List.map issues ~f:(fun issue ->
                          id, Sources.Issue.member issue, "invalid_skill")
                    }))
        in
        let%bind _ =
          Request.Discovery.create
            ~roots:(Request.Selection.roots selection)
            ~observations:
              (List.map result.candidates ~f:(fun (_, c) -> Sources.Candidate.observed c))
            ~issues:result.issues
        in
        Ok result
      in
      let refresh selection ~checkpoint =
        bounded ~checkpoint (fun checkpoint ->
          let%bind result = discover selection ~checkpoint in
          Request.Discovery.create
            ~roots:(Request.Selection.roots selection)
            ~observations:
              (List.map result.candidates ~f:(fun (_, c) -> Sources.Candidate.observed c))
            ~issues:result.issues)
      in
      let capture request ~checkpoint =
        bounded ~checkpoint (fun checkpoint ->
          let selection = Request.selection request in
          let%bind result = discover selection ~checkpoint in
          let observed_member (_, candidate) =
            Request.Member.of_observed (Sources.Candidate.observed candidate)
          in
          let%bind candidates =
            match selection with
            | Request.Selection.Project _ ->
              if
                List.equal
                  Request.Member.equal
                  (Request.members request)
                  (List.map result.candidates ~f:observed_member)
              then Ok result.candidates
              else error Conflict "project guidance changed; refresh explicitly"
            | Skills _ ->
              List.map (Request.members request) ~f:(fun expected ->
                List.find result.candidates ~f:(fun candidate ->
                  Request.Member.equal expected (observed_member candidate))
                |> Result.of_option
                     ~error:
                       (P.Error.create
                          Conflict
                          ~message:"skill changed; refresh explicitly"
                          ~retryable:false
                          ()))
              |> Result.all
          in
          let%bind loaded =
            List.map candidates ~f:(fun (root, candidate) ->
              Sources.load root.binding ~actor candidate ~limits ~checkpoint
              |> Result.map_error ~f:lower_error)
            |> Result.all
          in
          let%bind snapshot =
            I.Snapshot.capture loaded ~limits ~checkpoint
            |> Result.map_error ~f:(fun _ ->
              P.Error.invalid_request "instruction capture is invalid")
          in
          match selection with
          | Project { root; scope } ->
            Agent_session.Instruction_capture.project ~root ~scope snapshot
          | Skills _ -> Agent_session.Instruction_capture.skills snapshot)
      in
      Ok
        (Port.Backend.create
           ~roots:(List.map roots ~f:(fun root -> root.id))
           ~check_now
           ~refresh
           ~capture)))
;;
