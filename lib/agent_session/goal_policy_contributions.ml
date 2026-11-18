open! Core
module PC = Policy_canonical_usage
module Cause = Policy_cause_data
module P = Agent_protocol
module D = Document_schema
module Data = Goal_policy_contributions_data
module Carrier = Goal_policy_contributions_carrier
module Changed = Goal_policy_contributions_update

type t = Carrier.t

let limits = Data.limits
let max_books = Data.max_books
let max_streams = Data.max_streams
let owner t = (Carrier.value t).Data.owner

module History = Data.History

module Admission_context = struct
  type t =
    | Ordinary_existing
    | Delegated_policy_causal
  [@@deriving equal, sexp_of]
end

module Pending = Goal_policy_receipt.Pending
module Request = Goal_policy_receipt.Request
module Disposition = Changed.Disposition
module Update = Changed.Update
module Reserve_result = Changed.Reserve_result

let initialize_goal = Changed.initialize_goal
let reserve = Changed.reserve
let import = Changed.import
let close_before_writer = Changed.close_before_writer
let validate_context = Carrier.validate_context
let adopt = Carrier.adopt
let reserved_growth = Carrier.reserved_growth
let required_depth = Carrier.required_depth
let validate_encoded_capacity = Carrier.validate_encoded_capacity
let of_document = Carrier.of_document
let to_document = Carrier.to_document
let shape = Carrier.shape

module Subtotal = struct
  type t =
    { usage : Goal_usage.t
    ; history : History.t
    ; pending : Goal_descendant_occurrence.t list
    }

  let usage t = t.usage
  let history t = t.history
  let pending t = t.pending
end

let subtotal
      original
      ~owner
      ~goal_id
      ~catalog
      ~accounts
      ~canonical
      ~ledger
      ~session_revision
  =
  let open Result.Let_syntax in
  let%bind goal =
    Option.bind catalog ~f:(fun catalog -> Goal_catalog.find catalog goal_id)
    |> Result.of_option ~error:(P.Error.invalid_request "causal subtotal Goal is absent")
  in
  let%bind account =
    Option.bind accounts ~f:(fun accounts -> Goal_accounts.find accounts goal_id)
    |> Result.of_option
         ~error:(P.Error.invalid_request "causal subtotal account is absent")
  in
  let%bind () =
    if
      P.Session_ref.equal owner (P.Goal.session goal)
      && P.Id.Session.equal
           (P.Session_ref.session_id owner)
           (Inference_ledger.session_id ledger)
      && Int64.(session_revision >= 0L)
    then Ok ()
    else
      Goal_policy_receipt.invalid
        "causal subtotal owner, ledger session or revision differs"
  in
  let%bind (_ : Goal_accounting.t) =
    Goal_accounting_document.resolve account ~goal ~ledger
  in
  let%bind () =
    Option.value_map original ~default:(Ok ()) ~f:(fun t ->
      Carrier.validate_context
        t
        ~owner
        ~catalog
        ~accounts
        ~canonical
        ~ledger
        ~session_revision)
  in
  let%bind inline = Carrier.inline ~owner ~canonical ~ledger ~session_revision in
  let%bind () = Carrier.check_stream_capacity original ~inline in
  let book =
    Option.bind original ~f:(fun t -> Data.find_book (Carrier.value t) goal_id)
  in
  let history =
    Option.value_map book ~default:History.Historical_unknown ~f:(fun book ->
      book.Data.Book.history)
  in
  let remote = Option.value_map book ~default:[] ~f:(fun book -> book.Data.Book.slots) in
  let projections =
    List.filter inline ~f:(fun projection ->
      P.Id.Goal.equal
        goal_id
        (Cause.Goal_target.goal_id (PC.Stream.target (PC.Projection.stream projection))))
    @ List.filter_map remote ~f:(fun slot -> slot.Data.Slot.projection)
  in
  let%bind usage =
    List.fold_result projections ~init:Goal_usage.empty ~f:(fun usage projection ->
      Goal_usage.add_disjoint usage (PC.Projection.usage projection)
      |> Result.map_error ~f:(fun (_ : Goal_accounting_counter.error) ->
        P.Error.invalid_request "causal subtotal count exhausted"))
  in
  let pending =
    List.filter_map remote ~f:(fun slot ->
      Option.map slot.Data.Slot.pending ~f:Pending.occurrence)
  in
  Ok { Subtotal.usage; history; pending }
;;
