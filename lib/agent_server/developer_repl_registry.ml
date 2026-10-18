open! Core
module P = Agent_protocol
module Wire = P.Developer_repl_wire
module Repl = Developer_repl_epoch

let ( let* ) result f = Result.bind result ~f
let stale message = P.Error.create P.Error.Lease_stale ~message ~retryable:false ()
let limit message = P.Error.create P.Error.Resource_limit ~message ~retryable:false ()

type epoch_entry =
  { physical : Repl.Epoch.t
  ; wire : Wire.Wire_id.Epoch.t
  }

type revision_entry =
  { physical : Repl.Revision.t
  ; epoch : Repl.Epoch.t
  ; wire : Wire.Wire_id.Revision.t
  }

type instance_entry =
  { physical : Repl.Instance.t
  ; wire : Wire.Wire_id.Instance.t
  ; mutable epochs : epoch_entry list
  ; mutable revisions : revision_entry list
  }

type t =
  { limits : Wire.Limits.t
  ; namespace : string
  ; domain : int
  ; capacity : int
  ; mutable issued : int
  ; mutable instances : instance_entry list
  }

let on_domain t = Int.equal t.domain (Stdlib.Domain.self () :> int)

let check_domain t =
  if on_domain t
  then Ok ()
  else Error (stale "REPL comparison registry used from another domain")
;;

let create ~host ~limits =
  let* capacity = Developer_host.repl_registry_required_entries host in
  if capacity > Wire.Limits.max_registry_entries limits
  then Error (limit "REPL comparison registry is below installed owner capacity")
  else (
    let namespace =
      (* Transaction IDs are [txn_] plus 24 URI-safe base64 characters from
         18 random bytes. Hex encoding preserves that random identity while
         satisfying the lowercase REPL comparison-ID grammar. At the maximal
         wire ordinal, each emitted ID is at most 3 + 56 + 1 + 16 = 76 bytes,
         below the 96-byte wire-ID limit. *)
      P.Id.Transaction.create ()
      |> P.Id.Transaction.to_string
      |> String.concat_map ~f:(fun character ->
        Printf.sprintf "%02x" (Char.to_int character))
    in
    Ok
      { limits
      ; namespace
      ; domain = (Stdlib.Domain.self () :> int)
      ; capacity
      ; issued = 0
      ; instances = []
      })
;;

let next_id t prefix decode =
  if t.issued >= t.capacity
  then Error (limit "REPL comparison registry capacity was exhausted")
  else (
    let value = prefix ^ t.namespace ^ "_" ^ Int.to_string (t.issued + 1) in
    let* wire = decode value in
    t.issued <- t.issued + 1;
    Ok wire)
;;

let instance_by_physical t physical =
  List.find t.instances ~f:(fun entry -> Repl.Instance.equal entry.physical physical)
;;

let instance_by_wire t wire =
  List.find t.instances ~f:(fun entry -> Wire.Wire_id.Instance.equal entry.wire wire)
;;

let ensure_instance t physical =
  match instance_by_physical t physical with
  | Some entry -> Ok entry
  | None ->
    let* wire = next_id t "ri_" Wire.Wire_id.Instance.of_string in
    let entry = { physical; wire; epochs = []; revisions = [] } in
    t.instances <- entry :: t.instances;
    Ok entry
;;

let ensure_epoch t entry physical =
  match
    List.find entry.epochs ~f:(fun item -> Repl.Epoch.equal item.physical physical)
  with
  | Some item -> Ok item.wire
  | None ->
    let* wire = next_id t "re_" Wire.Wire_id.Epoch.of_string in
    entry.epochs <- { physical; wire } :: entry.epochs;
    Ok wire
;;

let ensure_revision t entry ~epoch physical =
  match
    List.find entry.revisions ~f:(fun item -> Repl.Revision.equal item.physical physical)
  with
  | Some item when Repl.Epoch.equal item.epoch epoch -> Ok item.wire
  | Some _ -> Error (stale "REPL revision changed its issuing epoch")
  | None ->
    let* wire = next_id t "rv_" Wire.Wire_id.Revision.of_string in
    entry.revisions <- { physical; epoch; wire } :: entry.revisions;
    Ok wire
;;

let resolve_instance t wire =
  let* () = check_domain t in
  instance_by_wire t wire
  |> Result.of_option ~error:(stale "REPL instance comparison ID is stale")
  |> Result.map ~f:(fun entry -> entry.physical)
;;

let resolve_epoch t ~instance wire =
  let* () = check_domain t in
  let* entry =
    instance_by_physical t instance
    |> Result.of_option ~error:(stale "REPL instance comparison ID is stale")
  in
  List.find entry.epochs ~f:(fun item -> Wire.Wire_id.Epoch.equal item.wire wire)
  |> Result.of_option ~error:(stale "REPL epoch comparison ID is stale")
  |> Result.map ~f:(fun item -> item.physical)
;;

let resolve_revision t ~instance ~epoch wire =
  let* () = check_domain t in
  let* entry =
    instance_by_physical t instance
    |> Result.of_option ~error:(stale "REPL instance comparison ID is stale")
  in
  List.find entry.revisions ~f:(fun item ->
    Wire.Wire_id.Revision.equal item.wire wire && Repl.Epoch.equal item.epoch epoch)
  |> Result.of_option ~error:(stale "REPL revision comparison ID is stale")
  |> Result.map ~f:(fun item -> item.physical)
;;

let project_view t view =
  let* () = check_domain t in
  let* entry = ensure_instance t (Repl.View.instance view) in
  let epoch = Repl.View.epoch view in
  let* epoch_wire = ensure_epoch t entry epoch in
  let* revision_wire = ensure_revision t entry ~epoch (Repl.View.revision view) in
  Wire.View.create
    ~limits:t.limits
    ~instance:entry.wire
    ~epoch:epoch_wire
    ~revision:revision_wire
    ~run_id:(Repl.View.run_id view)
    ~capture_fingerprint:(Repl.View.capture_fingerprint view)
;;

let project_location t location =
  let span = Chatml_source.Location.span location in
  Wire.Submission.Location.create
    ~limits:t.limits
    ~source:(Chatml_source.Location.source location)
    ~digest:(Chatml_source.Location.source_digest location)
    ~start_byte:span.left.offset
    ~end_byte:span.right.offset
;;

let project_status_diagnostic t diagnostic =
  let* location =
    Option.value_map
      (Repl.Submission.Diagnostic.location diagnostic)
      ~default:(Ok None)
      ~f:(fun location -> Result.map (project_location t location) ~f:Option.some)
  in
  Wire.Submission.Diagnostic.create
    ~limits:t.limits
    ~message:(Repl.Submission.Diagnostic.message diagnostic)
    ~location
;;

let project_evaluator_diagnostic t diagnostic =
  let* location =
    Option.value_map
      (Chatml_repl_execution.Diagnostic.location diagnostic)
      ~default:(Ok None)
      ~f:(fun location -> Result.map (project_location t location) ~f:Option.some)
  in
  let* () =
    match location, Chatml_repl_execution.Diagnostic.source_bytes diagnostic with
    | Some location, Some source_bytes
      when Wire.Submission.Location.end_byte location <= source_bytes -> Ok ()
    | None, None -> Ok ()
    | _ -> Error (stale "REPL diagnostic lost its owned source range")
  in
  Wire.Submission.Diagnostic.create
    ~limits:t.limits
    ~message:(Chatml_repl_execution.Diagnostic.message diagnostic)
    ~location
;;

let project_rendered t rendered =
  let kind =
    match Chatml_type_display.kind rendered with
    | Type -> Wire.Submission.Rendered_type.Type
    | Signature -> Signature
  in
  let completeness =
    match Chatml_type_display.completeness rendered with
    | Complete -> Wire.Submission.Rendered_type.Complete
    | Truncated reason ->
      let reason =
        match reason with
        | Node_limit -> Wire.Submission.Rendered_type.Node_limit
        | Edge_limit -> Edge_limit
        | Depth_limit -> Depth_limit
        | Work_limit -> Work_limit
        | Byte_limit -> Byte_limit
      in
      Truncated reason
  in
  Wire.Submission.Rendered_type.create
    ~limits:t.limits
    ~kind
    ~completeness
    ~text:(Chatml_type_display.text rendered)
;;

let project_summary t summary =
  let outcome =
    match Repl.Submission.Statement_summary.outcome summary with
    | Chatml_repl_execution.Receipt.Succeeded -> Wire.Submission.Summary.Succeeded
    | Failed reason ->
      let reason =
        match reason with
        | Initializer -> Wire.Submission.Summary.Initializer
        | Runtime -> Runtime
        | Budget -> Budget
      in
      Failed reason
  in
  let* diagnostic =
    Option.value_map
      (Repl.Submission.Statement_summary.diagnostic summary)
      ~default:(Ok None)
      ~f:(fun diagnostic ->
        Result.map (project_evaluator_diagnostic t diagnostic) ~f:Option.some)
  in
  let* expression_type =
    Option.value_map
      (Repl.Submission.Statement_summary.expression_type summary)
      ~default:(Ok None)
      ~f:(fun rendered -> Result.map (project_rendered t rendered) ~f:Option.some)
  in
  let task_retention =
    match Repl.Submission.Statement_summary.task_retention summary with
    | Not_a_task -> Wire.Submission.Summary.Not_a_task
    | Retained -> Retained
    | Limit_reached -> Limit_reached
  in
  Wire.Submission.Summary.create
    ~limits:t.limits
    ~outcome
    ~value_preview:(Repl.Submission.Statement_summary.value_preview summary)
    ~diagnostic
    ~expression_type
    ~binding_count:(Repl.Submission.Statement_summary.binding_count summary)
    ~task_retention
    ~text_limited:(Repl.Submission.Statement_summary.text_limited summary)
;;

let project_status t = function
  | Repl.Submission.Need_more (reason, location) ->
    let reason =
      match reason with
      | Chatml.Chatml_parse.Submission.Need_more.Parser_eof -> Wire.Submission.Parser_eof
      | Comment_eof -> Comment_eof
      | String_eof -> String_eof
    in
    let* location =
      Option.value_map location ~default:(Ok None) ~f:(fun location ->
        Result.map (project_location t location) ~f:Option.some)
    in
    Ok (Wire.Submission.Need_more (reason, location))
  | Rejected diagnostic ->
    let* diagnostic = project_status_diagnostic t diagnostic in
    Ok (Wire.Submission.Rejected diagnostic)
  | Finished -> Ok Wire.Submission.Finished
  | Stopped diagnostic ->
    let* diagnostic =
      Option.value_map diagnostic ~default:(Ok None) ~f:(fun diagnostic ->
        Result.map (project_status_diagnostic t diagnostic) ~f:Option.some)
    in
    Ok (Wire.Submission.Stopped diagnostic)
;;

let project_submission t ~instance submission =
  let* () = check_domain t in
  let* entry =
    instance_by_physical t instance
    |> Result.of_option ~error:(stale "REPL instance comparison ID is stale")
  in
  let epoch = Repl.Submission.origin_epoch submission in
  let* epoch_wire = ensure_epoch t entry epoch in
  let* revision_wire =
    ensure_revision t entry ~epoch (Repl.Submission.revision submission)
  in
  let* status = project_status t (Repl.Submission.status submission) in
  let details =
    match Repl.Submission.details submission with
    | Available -> Wire.Submission.Available
    | Output_limited -> Output_limited
  in
  let* summaries =
    Result.all (List.map (Repl.Submission.summaries submission) ~f:(project_summary t))
  in
  Wire.Submission.create
    ~limits:t.limits
    ~status
    ~details
    ~completed_statements:(Repl.Submission.completed_statements submission)
    ~statement_count:(Repl.Submission.statement_count submission)
    ~epoch:epoch_wire
    ~revision:revision_wire
    ~summaries
    ~task_reference_count:(Repl.Submission.task_reference_count submission)
    ~task_references:
      (List.take
         (Repl.Submission.task_references submission)
         (Wire.Limits.max_page_items t.limits)
       |> List.map ~f:Repl.Task_reference.to_string)
;;

let project_type_inspection t ~instance ~epoch inspection =
  let* () = check_domain t in
  let* entry =
    instance_by_physical t instance
    |> Result.of_option ~error:(stale "REPL instance comparison ID is stale")
  in
  let* revision =
    List.find entry.revisions ~f:(fun item ->
      Repl.Epoch.equal item.epoch epoch
      && Repl.Revision.equal item.physical (Repl.Type_inspection.revision inspection))
    |> Result.of_option ~error:(stale "REPL inspected revision is not registered")
  in
  let* rendered = project_rendered t (Repl.Type_inspection.rendered inspection) in
  Wire.Type_inspection.create
    ~limits:t.limits
    ~revision:revision.wire
    ~binding_name:(Repl.Type_inspection.binding_name inspection)
    ~rendered
;;

let project_reference_page t page =
  let* () = check_domain t in
  Wire.Reference_page.create
    ~limits:t.limits
    ~items:
      (List.map (Repl.Task_reference_page.items page) ~f:Repl.Task_reference.to_string)
    ~next:
      (Option.map (Repl.Task_reference_page.next page) ~f:Repl.Task_reference.to_string)
    ~total:(Repl.Task_reference_page.total page)
;;
