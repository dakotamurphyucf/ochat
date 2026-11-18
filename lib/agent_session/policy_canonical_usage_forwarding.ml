open! Core
module P = Agent_protocol
module D = Document_schema
module X = Persistence_codec
module Cause = Policy_cause_data
module Data = Policy_canonical_usage_data
module Carrier = Policy_canonical_usage_carrier
module Work = Cause.Allocation_work

let invalid = Data.invalid

let capture accounting ~(context : Cause.Context.t) ~work =
  let open Result.Let_syntax in
  let data = Carrier.value accounting in
  let%bind () =
    if P.Id.Session.equal context.session_id (P.Session_ref.session_id data.callee)
    then Ok ()
    else invalid "forwarding context session differs from actual callee"
  in
  let%bind member = Work.member work ~generation:context.generation in
  let%bind retained =
    Data.find_member data member
    |> Result.of_option
         ~error:(P.Error.invalid_request "forwarding work has no actual birth membership")
  in
  let%bind cause =
    Data.find_cause data retained.cause
    |> Result.of_option
         ~error:(P.Error.invalid_request "forwarding member cause is absent")
  in
  let%bind () =
    if
      Int.equal context.generation (Cause.callee_source cause).generation
      && Option.exists
           context.current_source
           ~f:(P.Run_source.equal (Cause.callee_source cause))
    then Ok ()
    else invalid "forwarding source is not the actual current installation"
  in
  let%bind current =
    Data.Source_work.capture ~memberships:data.memberships context cause member
  in
  let%bind current =
    current
    |> Result.of_option
         ~error:(P.Error.invalid_request "forwarding source work is absent")
  in
  let%bind () =
    if
      Cause.Key.equal retained.cause (Cause.key cause)
      && Data.Source_work.matches retained current
      && current.ready
    then Ok ()
    else invalid "forwarding source work is not the actual ready caused prefix"
  in
  let%bind () =
    match work, current.attempt with
    | ( (Work.Model_job_attempt { attempt; _ } | Async_job_attempt { attempt; _ })
      , Some actual )
      when Int.equal attempt actual -> Ok ()
    | (Event _ | Invocation _), None -> Ok ()
    | (Model_job_attempt _ | Async_job_attempt _), (None | Some _)
    | (Event _ | Invocation _), Some _ -> invalid "forwarding source Job attempt differs"
  in
  (* Encoding CURRENT carrier is essential: its authored template can predate
     later registered Cause rows and their separately retained raw children. *)
  let%bind document = Carrier.to_document accounting in
  let%bind fields = X.object_ (D.Document.payload document) in
  let%bind children = X.required fields "causes" (X.list X.raw) in
  let%bind raw =
    List.find children ~f:(fun raw ->
      match D.Json.field raw ~name:"id" with
      | Value (`String id) -> String.equal id (Cause.Key.to_string (Cause.key cause))
      | Absent | Null | Value _ -> false)
    |> Result.of_option
         ~error:(P.Error.invalid_request "current complete source Cause child is absent")
  in
  let%bind child = Cause.Source_child.of_json raw in
  let%bind () =
    if Cause.equal cause (Cause.Source_child.value child)
    then Ok ()
    else invalid "current raw source child differs from actual registered Cause"
  in
  Cause.append_forwarding child ~work ~member_projection_sha256:current.projection_sha256
;;

let check_current accounting ~context ~work supplied =
  let open Result.Let_syntax in
  let%bind actual = capture accounting ~context ~work in
  if
    Cause.Forwarding.equal actual supplied
    && D.Json.equal (Cause.Forwarding.to_json actual) (Cause.Forwarding.to_json supplied)
  then Ok ()
  else invalid "forwarding export differs from complete actual known/raw source prefix"
;;
