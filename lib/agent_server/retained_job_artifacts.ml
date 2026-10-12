open Core
module P = Agent_protocol
module Request = Chat_response.Background_request

module Revision_key = struct
  include P.Id.Prompt_revision
  include Comparator.Make (P.Id.Prompt_revision)
end

let limited message = P.Error.create Resource_limit ~message ~retryable:false ()

module Limits = struct
  type t =
    { max_sessions : int
    ; max_archives : int
    ; max_jobs : int
    ; max_work_steps : int
    }

  let create ~max_sessions ~max_archives ~max_jobs ~max_work_steps =
    if max_sessions < 0 || max_archives < 0 || max_jobs < 0 || max_work_steps < 0
    then Error (P.Error.invalid_request "negative retained-artifact collection limit")
    else Ok { max_sessions; max_archives; max_jobs; max_work_steps }
  ;;
end

module Receipt = struct
  type t =
    { sessions : int
    ; archives : int
    ; jobs : int
    ; work_steps : int
    }

  let sessions t = t.sessions
  let archives t = t.archives
  let jobs t = t.jobs
  let work_steps t = t.work_steps
end

type t =
  { revisions : P.Id.Prompt_revision.t list
  ; receipt : Receipt.t
  }

let revisions t = t.revisions
let receipt t = t.receipt

type json_frame =
  | Value of Jsonaf.t * int
  | Array of Jsonaf.t list * int
  | Object of (string * Jsonaf.t) list * int

let collect ~factory ~entries ~authorize ~request_policy ~limits ~checkpoint =
  let module Abort = struct
    exception Rejected of P.Error.t
  end
  in
  let open Result.Let_syntax in
  let sessions = ref 0 in
  let archives = ref 0 in
  let jobs = ref 0 in
  let work_steps = ref 0 in
  let poll () =
    if !work_steps >= limits.Limits.max_work_steps
    then raise (Abort.Rejected (limited "retained artifact traversal work exhausted"));
    incr work_steps;
    Eio.Fiber.check ();
    checkpoint ();
    Eio.Fiber.check ()
  in
  let admit count maximum message =
    poll ();
    if !count >= maximum then raise (Abort.Rejected (limited message));
    incr count
  in
  let bytes value =
    let rec loop offset =
      if offset < String.length value
      then (
        poll ();
        loop (offset + Int.min 1024 (String.length value - offset)))
    in
    loop 0
  in
  let admit_json json =
    let maximum_depth =
      request_policy.Chat_response.One_off_request.execution.max_depth
    in
    if maximum_depth < 0 || maximum_depth > Int.max_value - 8
    then raise (Abort.Rejected (limited "invalid retained request depth ceiling"));
    let rec loop = function
      | [] -> ()
      | frame :: rest ->
        poll ();
        (match frame with
         | Value (json, depth) ->
           (* The stored request wraps admitted input in a fixed shallow carrier.
              Bound construction depth before the existing codec is entered. *)
           if depth > maximum_depth && depth - maximum_depth > 8
           then raise (Abort.Rejected (limited "retained background request is too deep"));
           (match json with
            | `Null | `True | `False -> loop rest
            | `String value | `Number value ->
              bytes value;
              loop rest
            | `Array items -> loop (Array (items, depth + 1) :: rest)
            | `Object fields -> loop (Object (fields, depth + 1) :: rest))
         | Array ([], _) | Object ([], _) -> loop rest
         | Array (item :: items, depth) ->
           loop (Value (item, depth) :: Array (items, depth) :: rest)
         | Object ((name, value) :: fields, depth) ->
           bytes name;
           loop (Value (value, depth) :: Object (fields, depth) :: rest))
    in
    loop [ Value (json, 0) ]
  in
  let add revisions id =
    poll ();
    (* Domain IDs are at most 96 bytes. Revalidate the persisted value before a
       comparator-backed key enters the retained set; no unbounded comparator
       or borrowed checkpoint can escape this attempt. *)
    let%map id = P.Id.Prompt_revision.of_string (P.Id.Prompt_revision.to_string id) in
    Set.add revisions id
  in
  let inspect revisions (state : Agent_session.Session_state.t) =
    let%bind revisions = add revisions state.spec.prompt_revision_id in
    let rec preflight = function
      | [] -> ()
      | _ :: rest ->
        admit jobs limits.max_jobs "retained job inventory exceeds its count limit";
        preflight rest
    in
    preflight state.jobs;
    List.fold_result state.jobs ~init:revisions ~f:(fun revisions job ->
      poll ();
      match job.P.Job.kind with
      | Model_call | Nested_agent | Scheduled_event | Shell_process | Compaction ->
        Ok revisions
      | Async_tool ->
        admit_json job.payload;
        let%bind request = Request.of_json ~policy:request_policy job.payload in
        (match Request.agent_artifact request with
         | None -> Ok revisions
         | Some artifact -> add revisions (Request.Agent_artifact.revision_id artifact)))
  in
  try
    let rec preflight = function
      | [] -> ()
      | _ :: rest ->
        admit
          sessions
          limits.max_sessions
          "retained session inventory exceeds its count limit";
        preflight rest
    in
    preflight entries;
    let%map revisions =
      List.fold_result
        entries
        ~init:(Set.empty (module Revision_key))
        ~f:(fun revisions entry ->
          poll ();
          Session_factory.fold_indexed_retained_states
            factory
            entry
            ~authorize
            ~checkpoint:poll
            ~admit_archive:(fun () ->
              admit
                archives
                limits.max_archives
                "retained archive inventory exceeds its count limit";
              Ok ())
            ~init:revisions
            ~f:inspect)
    in
    let revisions =
      Set.fold_right revisions ~init:[] ~f:(fun id revisions ->
        poll ();
        id :: revisions)
    in
    { revisions
    ; receipt =
        { sessions = !sessions
        ; archives = !archives
        ; jobs = !jobs
        ; work_steps = !work_steps
        }
    }
  with
  | Abort.Rejected error -> Error error
;;
