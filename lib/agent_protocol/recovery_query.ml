open! Core
module J = Json_codec
module W = Recovery_wire

let max_page_size = 50
let max_page_bytes = 65536

module Features = struct
  let windows = "recovery.windows"
end

module State = struct
  type t =
    | Open
    | Sealed
    | Expired
  [@@deriving equal, sexp_of]

  let to_json = function
    | Open -> `String "open"
    | Sealed -> `String "sealed"
    | Expired -> `String "expired"
  ;;

  let of_json =
    J.enum
      ~name:"recovery window state"
      [ "open", Open; "sealed", Sealed; "expired", Expired ]
  ;;
end

let fields json names =
  let%bind.Result () = W.admit_coordinate json in
  W.fields json names
;;

let session fields =
  J.required_as fields "session" (fun json ->
    let%bind.Result _ = W.fields json [ "server_id"; "session_id" ] in
    Session_ref.of_json json)
;;

let generation = J.bounded_int ~min:0 ~max:Int.max_value
let number i = `Number (Int.to_string i)

module Summary = struct
  type t =
    { scope : Recovery_window.Scope.t
    ; state : State.t
    }
  [@@deriving sexp_of]

  let to_json t =
    `Object
      [ "scope", Recovery_window.Scope.to_json t.scope; "state", State.to_json t.state ]
  ;;

  let of_json json =
    let open Result.Let_syntax in
    let%bind f = fields json [ "scope"; "state" ] in
    let%bind scope = J.required_as f "scope" Recovery_window.Scope.of_json in
    let%map state = J.required_as f "state" State.of_json in
    { scope; state }
  ;;

  let page_to_json page = Page.to_json to_json page
  let page_limits = W.limits ~max_bytes:max_page_bytes ~max_fields:512 ~max_nodes:1024

  let page_of_json json =
    let open Result.Let_syntax in
    let%bind _ = W.measure ~limits:page_limits json in
    let%bind _ = W.fields json [ "items"; "next_cursor" ] in
    let%bind page = Page.of_json of_json json in
    if List.length page.items > max_page_size
    then Error (Protocol_error.invalid_request "too many recovery windows in page")
    else Ok page
  ;;
end

module Enable = struct
  let method_name = "session.recovery.enable"

  type t =
    { session : Session_ref.t
    ; attachment_id : Id.Attachment.t
    ; expected_generation : int
    }

  let to_json t =
    `Object
      [ "session", Session_ref.to_json t.session
      ; "attachment_id", Id.Attachment.to_json t.attachment_id
      ; "expected_generation", number t.expected_generation
      ]
  ;;

  let of_json json =
    let open Result.Let_syntax in
    let%bind f = fields json [ "session"; "attachment_id"; "expected_generation" ] in
    let%bind session = session f in
    let%bind attachment_id = J.required_as f "attachment_id" Id.Attachment.of_json in
    let%map expected_generation = J.required_as f "expected_generation" generation in
    { session; attachment_id; expected_generation }
  ;;
end

module List_windows = struct
  let method_name = "session.recovery.windows"

  type t =
    { session : Session_ref.t
    ; generation : int option
    ; state : State.t option
    ; page : Page.Request.t
    }

  let to_json t =
    `Object
      ((("session", Session_ref.to_json t.session)
        :: List.filter_opt
             [ Option.map t.generation ~f:(fun g -> "generation", number g)
             ; Option.map t.state ~f:(fun s -> "state", State.to_json s)
             ])
       @ Page.Request.to_fields t.page)
  ;;

  let of_json json =
    let open Result.Let_syntax in
    let%bind f = fields json [ "session"; "generation"; "state"; "limit"; "cursor" ] in
    let%bind session = session f in
    let%bind generation = J.optional_as f "generation" generation in
    let%bind state = J.optional_as f "state" State.of_json in
    let%bind page = Page.Request.of_fields f in
    if page.limit > max_page_size
    then Error (Protocol_error.invalid_request "recovery window page exceeds 50")
    else Ok { session; generation; state; page }
  ;;
end
