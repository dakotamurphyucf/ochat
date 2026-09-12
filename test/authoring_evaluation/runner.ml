open Core
open Jsonaf.Export

type policy =
  | Minimal
  | Automatic_retrieval
  | Selected_preload
[@@deriving equal, sexp, jsonaf]

let policies = [ Minimal; Automatic_retrieval; Selected_preload ]

type provenance =
  | Offline_transcript
  | Real_model
[@@deriving equal, sexp, jsonaf]

type category =
  | Primer
  | Tool_descriptions
  | Documentation
  | Conversation
[@@deriving equal, sexp, jsonaf]

type message =
  { category : category
  ; text : string
  }
[@@deriving sexp, jsonaf]

type failure =
  | Syntax
  | Semantics
  | Capability
  | Infrastructure
[@@deriving equal, sexp, jsonaf]

type validation =
  | Valid
  | Invalid of failure * string
[@@deriving sexp, jsonaf]

type execution =
  | Passed
  | Failed of failure * string
  | Not_run of string
[@@deriving sexp, jsonaf]

type action =
  | Retrieve of Jsonaf.t
  | Submit of Jsonaf.t
  | Decline of string
[@@deriving sexp, jsonaf]

type answer =
  { action : action
  ; provider_input_tokens : int option
  }

type limits =
  { max_steps : int
  ; max_attempts : int
  }
[@@deriving equal, sexp, jsonaf]

type task =
  { id : string
  ; family : string
  ; prompt : string
  ; preload_topics : string list
  ; compaction_after_step : int option
  }
[@@deriving sexp, jsonaf]

(* A backend owns real validation, execution and reference access. Candidate
   JSON cannot supply a score or turn static acceptance into runtime success. *)
type backend =
  { primer : message
  ; tool_descriptions : message
  ; prepare : task -> message list
  ; preload : task -> message list
  ; retrieve : Jsonaf.t -> message list
  ; validate : Jsonaf.t -> validation
  ; execute : Jsonaf.t -> execution
  }

type attempt =
  { validation : validation
  ; execution : execution option
  }
[@@deriving sexp, jsonaf]

type termination =
  | Succeeded
  | Declined
  | Attempts_exhausted
  | Steps_exhausted
[@@deriving equal, sexp, jsonaf]

type tokens =
  { primer : int
  ; tool_descriptions : int
  ; documentation_delivered : int
  ; documentation_effective_input : int
  ; total_effective_input : int
  ; provider_input : int option
  }
[@@deriving sexp, jsonaf]

type result =
  { task_id : string
  ; family : string
  ; policy : policy
  ; provenance : provenance
  ; model : string
  ; suite_revision : string
  ; runtime_revision : string
  ; limits : limits
  ; termination : termination
  ; attempts : attempt list
  ; first_pass_compile : bool
  ; runtime_success : bool
  ; repairs : int
  ; provider_steps : int
  ; retrieval_calls : int
  ; initial_reference_roots : int
  ; compactions : int
  ; tokens : tokens
  ; latency_seconds : float
  }
[@@deriving sexp, jsonaf]

(* Same labelled approximation as installed authoring context. These are not
   tokenizer counts, upper bounds, or provider billing measurements. *)
let estimate text = (String.length text + 2) / 3
let tokens messages = List.sum (module Int) messages ~f:(fun m -> estimate m.text)

let documentation_tokens messages =
  List.filter messages ~f:(fun m -> equal_category m.category Documentation) |> tokens
;;

let run
      ~now
      ~provenance
      ~model
      ~suite_revision
      ~runtime_revision
      ~limits
      ~policy
      ~backend
      ~provider
      task
  =
  match limits.max_steps > 0 && limits.max_attempts > 0 with
  | false -> invalid_arg "evaluation limits must be positive"
  | true ->
    let started = now () in
    let references, initial_reference_roots =
      match policy with
      | Minimal -> [], 0
      | Automatic_retrieval -> backend.prepare task, 1
      | Selected_preload -> backend.preload task, List.length task.preload_topics
    in
    let initial =
      [ backend.primer
      ; backend.tool_descriptions
      ; { category = Conversation; text = task.prompt }
      ]
      @ references
    in
    let history = ref initial in
    let attempts = ref [] in
    let steps = ref 0 in
    let retrieval_calls = ref 0 in
    let compactions = ref 0 in
    let documentation_delivered = ref (documentation_tokens references) in
    let documentation_effective_input = ref 0 in
    let total_effective_input = ref 0 in
    let provider_input = ref (Some 0) in
    let append messages = history := !history @ messages in
    let record value =
      append [ { category = Conversation; text = Jsonaf.to_string value } ]
    in
    let rec loop () =
      match !steps >= limits.max_steps with
      | true -> Steps_exhausted
      | false ->
        incr steps;
        total_effective_input := !total_effective_input + tokens !history;
        documentation_effective_input
        := !documentation_effective_input + documentation_tokens !history;
        let answer = provider ~step:!steps ~messages:!history in
        (match answer.provider_input_tokens with
         | Some n when n < 0 -> invalid_arg "negative provider token measurement"
         | _ -> ());
        provider_input
        := Option.both !provider_input answer.provider_input_tokens
           |> Option.map ~f:(fun (total, n) -> total + n);
        record (jsonaf_of_action answer.action);
        (* An evaluation stimulus, not a claim about durable runtime compaction:
           drop retrieved text while keeping the task, tools and a rediscovery
           instruction. The provider must retrieve exact contracts again. *)
        (match task.compaction_after_step with
         | Some step when step = !steps ->
           incr compactions;
           history
           := [ backend.primer
              ; backend.tool_descriptions
              ; { category = Conversation; text = task.prompt }
              ; { category = Conversation
                ; text =
                    "Earlier reference text was compacted. Retrieve the exact topics \
                     needed for your next candidate; remembered names are not content."
                }
              ]
         | _ -> ());
        (match answer.action with
         | Decline _ -> Declined
         | Retrieve query ->
           incr retrieval_calls;
           let messages = backend.retrieve query in
           documentation_delivered
           := !documentation_delivered + documentation_tokens messages;
           append messages;
           loop ()
         | Submit candidate ->
           let validation = backend.validate candidate in
           let execution =
             match validation with
             | Invalid _ -> None
             | Valid -> Some (backend.execute candidate)
           in
           let attempt = { validation; execution } in
           attempts := attempt :: !attempts;
           record (jsonaf_of_attempt attempt);
           (match execution with
            | Some Passed -> Succeeded
            | Some (Failed _ | Not_run _) | None ->
              (match List.length !attempts >= limits.max_attempts with
               | true -> Attempts_exhausted
               | false -> loop ())))
    in
    let termination = loop () in
    let attempts = List.rev !attempts in
    let first_pass_compile =
      match attempts with
      | { validation = Valid; _ } :: _ -> true
      | [] | { validation = Invalid _; _ } :: _ -> false
    in
    { task_id = task.id
    ; family = task.family
    ; policy
    ; provenance
    ; model
    ; suite_revision
    ; runtime_revision
    ; limits
    ; termination
    ; attempts
    ; first_pass_compile
    ; runtime_success = equal_termination termination Succeeded
    ; repairs = Int.max 0 (List.length attempts - 1)
    ; provider_steps = !steps
    ; retrieval_calls = !retrieval_calls
    ; initial_reference_roots
    ; compactions = !compactions
    ; tokens =
        { primer = estimate backend.primer.text
        ; tool_descriptions = estimate backend.tool_descriptions.text
        ; documentation_delivered = !documentation_delivered
        ; documentation_effective_input = !documentation_effective_input
        ; total_effective_input = !total_effective_input
        ; provider_input = !provider_input
        }
    ; latency_seconds = now () -. started
    }
;;

type score =
  { policy : policy
  ; tasks : int
  ; first_pass_compile_rate : float
  ; runtime_success_rate : float
  ; repair_count : int
  ; retrieval_calls : int
  ; documentation_tokens : int
  ; effective_input_tokens : int
  }
[@@deriving sexp, jsonaf]

(* Refuse cherry-picked/mixed comparisons. Expected IDs come from the independent
   suite manifest, not from whichever results happened to finish successfully. *)
let compare ~task_ids results =
  let invalid message = Error ("incomparable evaluation: " ^ message) in
  match task_ids, results with
  | [], _ | _, [] -> invalid "empty suite or results"
  | _, first :: _ ->
    let expected = List.sort task_ids ~compare:String.compare in
    let same_run (row : result) =
      equal_provenance row.provenance first.provenance
      && String.equal row.model first.model
      && String.equal row.suite_revision first.suite_revision
      && String.equal row.runtime_revision first.runtime_revision
      && equal_limits row.limits first.limits
    in
    let partitions =
      List.map policies ~f:(fun policy ->
        policy, List.filter results ~f:(fun row -> equal_policy row.policy policy))
    in
    (match List.find_a_dup expected ~compare:String.compare with
     | Some _ -> invalid "duplicate manifest task"
     | None ->
       (match List.for_all results ~f:same_run with
        | false -> invalid "provenance, model, revisions or limits differ"
        | true ->
          (match
             List.for_all partitions ~f:(fun (_, rows) ->
               List.equal
                 String.equal
                 expected
                 (List.map rows ~f:(fun row -> row.task_id)
                  |> List.sort ~compare:String.compare))
           with
           | false -> invalid "each policy must have exactly the full task set"
           | true ->
             Ok
               (List.map partitions ~f:(fun (policy, rows) ->
                  let count = List.length rows in
                  let rate f = Float.of_int (List.count rows ~f) /. Float.of_int count in
                  { policy
                  ; tasks = count
                  ; first_pass_compile_rate = rate (fun r -> r.first_pass_compile)
                  ; runtime_success_rate = rate (fun r -> r.runtime_success)
                  ; repair_count = List.sum (module Int) rows ~f:(fun r -> r.repairs)
                  ; retrieval_calls =
                      List.sum (module Int) rows ~f:(fun r -> r.retrieval_calls)
                  ; documentation_tokens =
                      List.sum
                        (module Int)
                        rows
                        ~f:(fun r -> r.tokens.documentation_delivered)
                  ; effective_input_tokens =
                      List.sum
                        (module Int)
                        rows
                        ~f:(fun r -> r.tokens.total_effective_input)
                  })))))
;;
