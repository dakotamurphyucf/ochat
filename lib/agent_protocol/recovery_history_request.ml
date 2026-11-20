open! Core
module J = Json_codec
module W = Recovery_wire

let method_name = "session.recovery.query"
let feature = "recovery.history_query"

type family =
  | Canonical
  | Effective
[@@deriving equal]

type view =
  | Ordinary
  | Selected_developer
  | Selected_tool_result
[@@deriving equal]

type role =
  | User
  | Assistant
  | Developer
  | Tool_result
[@@deriving equal, sexp_of]

type action =
  | List_items
  | Search of Search_term.t

let family_name = function
  | Canonical -> "canonical"
  | Effective -> "effective"
;;

let view_name = function
  | Ordinary -> "ordinary"
  | Selected_developer -> "selected_developer"
  | Selected_tool_result -> "selected_tool_result"
;;

let role_name = function
  | User -> "user"
  | Assistant -> "assistant"
  | Developer -> "developer"
  | Tool_result -> "tool_result"
;;

let enum name encode values =
  J.enum ~name (List.map values ~f:(fun value -> encode value, value))
;;

let role_of_json =
  enum "recovery role" role_name [ User; Assistant; Developer; Tool_result ]
;;

type t =
  { session : Session_ref.t
  ; window : Recovery_window.Scope.t option
  ; family : family
  ; view : view
  ; role : role option
  ; ordinals : (int * int) option
  ; page : Page.Request.t
  ; max_reply_bytes : int
  ; action : action
  }

let invalid message = Error (Protocol_error.invalid_request message)

let create
      ~session
      ?window
      ~family
      ?(view = Ordinary)
      ?role
      ?ordinals
      ~page
      ?(max_reply_bytes = Recovery_query.max_page_bytes)
      action
  =
  let window_ok =
    match window, action with
    | None, List_items -> false
    | None, Search _ -> true
    | Some scope, _ -> Session_ref.equal session (Recovery_window.Scope.session scope)
  in
  if
    (not window_ok)
    || page.Page.Request.limit < 1
    || page.limit > Recovery_query.max_page_size
    || max_reply_bytes < 1
    || max_reply_bytes > Recovery_query.max_page_bytes
    || Option.exists ordinals ~f:(fun (lo, hi) ->
      lo < 0 || hi < lo || hi > Recovery_window.max_members)
  then invalid "invalid recovery history request bounds or window"
  else Ok { session; window; family; view; role; ordinals; page; max_reply_bytes; action }
;;

let number n = `Number (Int.to_string n)

let to_json t =
  let action =
    match t.action with
    | List_items -> [ "action", `String "list_items" ]
    | Search term -> [ "action", `String "search"; "term", Search_term.to_json term ]
  in
  `Object
    ([ "session", Session_ref.to_json t.session
     ; "family", `String (family_name t.family)
     ; "view", `String (view_name t.view)
     ; "max_reply_bytes", number t.max_reply_bytes
     ]
     @ action
     @ List.filter_opt
         [ Option.map t.window ~f:(fun x -> "window", Recovery_window.Scope.to_json x)
         ; Option.map t.role ~f:(fun x -> "role", `String (role_name x))
         ; Option.map t.ordinals ~f:(fun (lo, hi) ->
             "ordinals", `Array [ number lo; number hi ])
         ]
     @ Page.Request.to_fields t.page)
;;

let ordinals_of_json = function
  | `Array [ lo; hi ] ->
    let open Result.Let_syntax in
    let index = J.bounded_int ~min:0 ~max:Recovery_window.max_members in
    let%bind lo = index lo in
    let%map hi = index hi in
    lo, hi
  | _ -> invalid "expected two half-open recovery ordinals"
;;

let of_json json =
  let open Result.Let_syntax in
  let%bind () = W.admit_coordinate json in
  let%bind f =
    W.fields
      json
      [ "session"
      ; "window"
      ; "family"
      ; "view"
      ; "role"
      ; "ordinals"
      ; "limit"
      ; "cursor"
      ; "max_reply_bytes"
      ; "action"
      ; "term"
      ]
  in
  let%bind session =
    J.required_as f "session" (fun j ->
      let%bind _ = W.fields j [ "server_id"; "session_id" ] in
      Session_ref.of_json j)
  in
  let%bind window = J.optional_as f "window" Recovery_window.Scope.of_json in
  let%bind family =
    J.required_as f "family" (enum "recovery family" family_name [ Canonical; Effective ])
  in
  let%bind view =
    J.optional_as
      f
      "view"
      (enum
         "recovery view"
         view_name
         [ Ordinary; Selected_developer; Selected_tool_result ])
  in
  let%bind role = J.optional_as f "role" role_of_json in
  let%bind ordinals = J.optional_as f "ordinals" ordinals_of_json in
  let%bind page = Page.Request.of_fields f in
  let%bind max_reply_bytes =
    J.optional_as
      f
      "max_reply_bytes"
      (J.bounded_int ~min:1 ~max:Recovery_query.max_page_bytes)
  in
  let%bind kind = J.required_as f "action" J.string in
  let%bind action =
    match kind, J.optional f "term" with
    | "list_items", None -> Ok List_items
    | "search", Some json ->
      Result.map (Search_term.of_json json) ~f:(fun term -> Search term)
    | _ -> invalid "invalid recovery history action or term"
  in
  create ~session ?window ~family ?view ?role ?ordinals ~page ?max_reply_bytes action
;;
