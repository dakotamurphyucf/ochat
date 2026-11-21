open! Core
module P = Agent_protocol
module Ledger = Recovery_window_ledger
module Window = Recovery_window_record
module Scope = P.Recovery_window.Scope
module Hash = Digestif.SHA256

type summary = P.Recovery_query.Summary.t =
  { scope : Scope.t
  ; state : Window.state
  }
[@@deriving sexp_of]

type signer = string

let create () = P.Id.Transaction.(to_string (create ()))

let expired () =
  P.Error.create
    Cursor_expired
    ~message:"recovery window cursor expired; request the first page again"
    ~retryable:false
    ~data:(`Object [ "snapshot_required", `True ])
    ()
;;

let page_position signer ~basis ~length = function
  | None -> Ok 0
  | Some encoded ->
    let valid =
      let open Option.Let_syntax in
      match String.split (P.Page.Cursor.to_string encoded) ~on:'.' with
      | [ position; mac ] ->
        let%bind next = Int.of_string_opt position in
        let%bind received = Hash.consistent_of_hex_opt mac in
        let payload = basis ^ "." ^ position in
        let expected = Hash.hmac_string ~key:signer payload in
        Option.some_if (next > 0 && next < length && Hash.equal expected received) next
      | _ -> None
    in
    Result.of_option valid ~error:(expired ())
;;

let list_for_origin
      signer
      (source : Session_state.t)
      ~origin
      ~operation
      ~generation
      ~state
      page
  =
  let open Result.Let_syntax in
  let current = source.identity.generation in
  let selected = Option.value generation ~default:current in
  if
    page.P.Page.Request.limit <= 0
    || page.limit > P.Recovery_query.max_page_size
    || selected < 0
  then Error (P.Error.invalid_request "invalid recovery window generation or page size")
  else (
    let%bind ledger =
      Result.of_option
        source.recovery_windows
        ~error:
          (P.Error.create
             Invalid_state
             ~message:"recovery windows are unavailable"
             ~retryable:false
             ())
    in
    let rows =
      List.filter_map (Ledger.windows ledger) ~f:(fun window ->
        let scope = Window.scope window in
        let status = Window.state window in
        Option.some_if
          (Scope.generation scope = selected
           && Option.for_all state ~f:(Window.equal_state status))
          { scope; state = status })
    in
    let basis =
      [%sexp
        ("ochat.window-list.v2" : string)
      , (origin : int)
      , (operation : P.Id.Operation.t option)
      , (source.identity.session_id : P.Id.Session.t)
      , (current : int)
      , (source.spec.prompt_revision_id : P.Id.Prompt_revision.t)
      , (source.spec.configuration_revision : int64)
      , (source.counters.owner_lease_generation : int64)
      , (source.stop_epoch : int64)
      , (source.parent_stop_epoch : int64 option)
      , (generation : int option)
      , (state : Window.state option)
      , (page.limit : int)
      , (rows : summary list)]
      |> Sexp.to_string_mach
      |> Hash.digest_string
      |> Hash.to_hex
    in
    let length = List.length rows in
    let%bind start = page_position signer ~basis ~length page.cursor in
    let items = List.take (List.drop rows start) page.limit in
    let next = start + List.length items in
    let%map next_cursor =
      if next = length
      then Ok None
      else (
        let position = Int.to_string next in
        let signature =
          Hash.hmac_string ~key:signer (basis ^ "." ^ position) |> Hash.to_hex
        in
        P.Page.Cursor.of_string (position ^ "." ^ signature) |> Result.map ~f:Option.some)
    in
    { P.Page.items; next_cursor })
;;

let list signer source ~actor ~generation ~state page =
  if Operator_authorization.is_current actor
  then
    list_for_origin
      signer
      source
      ~origin:(Operator_authorization.origin actor)
      ~operation:None
      ~generation
      ~state
      page
  else
    Error
      (P.Error.create
         Permission_denied
         ~message:"recovery authority expired"
         ~retryable:false
         ())
;;
