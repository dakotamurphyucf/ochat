open! Core
module Content = Instruction_content
module Sources = Authoring_source_host
module Local = Chatml_local_source

module Root = struct
  type t =
    { host : Sources.t
    ; root : Sources.Root.t
    ; id : Content.Root_id.t
    }
end

module Candidate = struct
  type t =
    { root : Root.t
    ; observed : Content.Observed.t
    }

  let observed t = t.observed
end

module Error = struct
  type t =
    | Foreign_candidate
    | Limit_exceeded
    | Missing
    | Source of Sources.Error.t
    | Content of Content.Error.t

  let sexp_of_t = function
    | Foreign_candidate -> Sexp.Atom "Foreign_candidate"
    | Limit_exceeded -> Sexp.Atom "Limit_exceeded"
    | Missing -> Sexp.Atom "Missing"
    | Source value -> Sexp.List [ Sexp.Atom "Source"; Sources.Error.sexp_of_t value ]
    | Content value -> Sexp.List [ Sexp.Atom "Content"; Content.Error.sexp_of_t value ]
  ;;
end

module Issue = struct
  type t =
    { member : Source.Portable_path.t
    ; error : Error.t
    }

  let member t = t.member
  let error t = t.error
end

module Discovery = struct
  type t =
    { candidates : Candidate.t list
    ; issues : Issue.t list
    }

  let candidates t = t.candidates
  let issues t = t.issues
end

let source result = Result.map_error result ~f:(fun error -> Error.Source error)
let content result = Result.map_error result ~f:(fun error -> Error.Content error)

let current root actor =
  source (Sources.check_root_current root.Root.host ~actor root.root)
;;

let bind_root host ~actor ~root ~id =
  Result.map
    (source (Sources.check_root_current host ~actor root))
    ~f:(fun () -> Root.{ host; root; id })
;;

exception Exhausted

let bounded root actor limits ~checkpoint f =
  let work = ref 0 in
  let poll () =
    checkpoint ();
    if !work >= Content.Limits.work_steps limits then raise Exhausted;
    Int.incr work
  in
  let open Result.Let_syntax in
  let%bind () = current root actor in
  let result =
    try f poll with
    | Exhausted -> Error Error.Limit_exceeded
  in
  let%bind () = current root actor in
  result
;;

let missing_leaf = function
  | Sources.Error.Local error ->
    Local.Error.equal_code (Local.Error.code error) Missing
    && Local.Error.equal_operation (Local.Error.operation error) Open_file
  | _ -> false
;;

let global_error = function
  | Sources.Error.Local error ->
    Local.Error.equal_code (Local.Error.code error) Entry_limit
  | Unauthorized
  | Expired_actor
  | Foreign_root
  | Closed
  | Busy
  | Foreign_domain
  | Result_limit
  | Sources _ -> true
  | Context_limit
  | Unsupported_source_selection
  | Root _
  | Project _
  | Source_capture _
  | Document_parser _
  | Document_bundle_limit
  | Project_declaration_unavailable
  | Invalid_project_declaration
  | Selected_script_unavailable -> false
;;

let observe root actor member kind ~limits ~remaining ~poll =
  if !remaining <= 0
  then Error Error.Limit_exceeded
  else (
    let maximum = Int.min !remaining (Content.Limits.file_bytes limits) in
    match
      Sources.read_root_member
        ~max_bytes:maximum
        root.Root.host
        ~actor
        ~root:root.root
        ~member
        ~checkpoint:poll
    with
    | Error error ->
      if missing_leaf error
      then Error Error.Missing
      else (
        (* Failed reads return no byte receipt. Conservatively charge their entire
         attempted allowance; only exact absent leaves cost zero read bytes. *)
        remaining := !remaining - maximum;
        match error with
        | Local value
          when Local.Error.equal_code (Local.Error.code value) Too_large
               && maximum < Content.Limits.file_bytes limits -> Error Error.Limit_exceeded
        | _ -> Error (Error.Source error))
    | Ok captured ->
      let source = Sources.Document_capture.source captured in
      remaining := !remaining - Chatml_source.byte_length source;
      let open Result.Let_syntax in
      let%map observed =
        content (Content.observe source ~root:root.id ~kind ~limits ~checkpoint:poll)
      in
      { Candidate.root; observed })
;;

let path value =
  Source.Portable_path.create value
  |> Result.map_error ~f:(fun _ -> Error.Content Invalid_identity)
;;

let discover_project root ~actor ~scope ~limits ~checkpoint =
  bounded root actor limits ~checkpoint (fun poll ->
    let open Result.Let_syntax in
    let prefixes =
      match Content.Scope.path scope with
      | None -> []
      | Some path -> String.split (Source.Portable_path.to_string path) ~on:'/'
    in
    if List.length prefixes >= Content.Limits.files limits
    then Error Error.Limit_exceeded
    else (
      let remaining = ref (Content.Limits.total_bytes limits) in
      let rec chain prefix names output =
        poll ();
        let%bind member = path (prefix ^ "AGENTS.md") in
        let%bind output =
          match observe root actor member Project_guidance ~limits ~remaining ~poll with
          | Ok candidate -> Ok (candidate :: output)
          | Error Missing -> Ok output
          | Error error -> Error error
        in
        match names with
        | [] -> Ok Discovery.{ candidates = List.rev output; issues = [] }
        | name :: rest -> chain (prefix ^ name ^ "/") rest output
      in
      chain "" prefixes []))
;;

let discover_skills root ~actor ~limits ~checkpoint =
  bounded root actor limits ~checkpoint (fun poll ->
    let open Result.Let_syntax in
    let%bind entries =
      source
        (Sources.list_root_directory
           ~max_entries:(Content.Limits.files limits)
           root.Root.host
           ~actor
           ~root:root.root
           ~directory:None
           ~checkpoint:poll)
    in
    let remaining = ref (Content.Limits.total_bytes limits) in
    let%map candidates, issues =
      List.fold_result entries ~init:([], []) ~f:(fun (candidates, issues) entry ->
        poll ();
        let directory = Sources.Entry.member entry in
        let name = Source.Portable_path.to_string directory in
        if Result.is_error (Content.Skill_slug.of_string name)
        then
          Ok
            ( candidates
            , Issue.{ member = directory; error = Content Invalid_identity } :: issues )
        else (
          let%bind member = path (name ^ "/SKILL.md") in
          match observe root actor member Skill ~limits ~remaining ~poll with
          | Ok candidate -> Ok (candidate :: candidates, issues)
          | Error Missing -> Ok (candidates, issues)
          | Error (Source error) when global_error error -> Error (Error.Source error)
          | Error Limit_exceeded -> Error Error.Limit_exceeded
          | Error error -> Ok (candidates, { Issue.member; error } :: issues)))
    in
    { Discovery.candidates = List.rev candidates; issues = List.rev issues })
;;

let load root ~actor candidate ~limits ~checkpoint =
  if not (phys_equal root candidate.Candidate.root)
  then Error Error.Foreign_candidate
  else
    bounded root actor limits ~checkpoint (fun poll ->
      let open Result.Let_syntax in
      let member = Content.Id.member (Content.Observed.id candidate.observed) in
      let%bind captured =
        Sources.read_root_member
          ~max_bytes:
            (Int.min
               (Content.Limits.file_bytes limits)
               (Content.Limits.total_bytes limits))
          root.Root.host
          ~actor
          ~root:root.root
          ~member
          ~checkpoint:poll
        |> Result.map_error ~f:(fun error ->
          if missing_leaf error then Error.Missing else Error.Source error)
      in
      content
        (Content.load
           candidate.observed
           ~source:(Sources.Document_capture.source captured)
           ~limits
           ~checkpoint:poll))
;;
