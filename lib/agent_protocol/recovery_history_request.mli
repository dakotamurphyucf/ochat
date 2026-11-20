(** A bounded query of one session's local canonical or prepared history.
    Omitting the window selects the current generation for a search. Listing
    always selects an exact window. A cursor conveys no authorization. *)
open! Core

val method_name : string
val feature : string

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

val role_name : role -> string
val view_name : view -> string
val role_of_json : Jsonaf.t -> (role, Protocol_error.t) result

type t = private
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

val create
  :  session:Session_ref.t
  -> ?window:Recovery_window.Scope.t
  -> family:family
  -> ?view:view
  -> ?role:role
  -> ?ordinals:int * int
  -> page:Page.Request.t
  -> ?max_reply_bytes:int
  -> action
  -> (t, Protocol_error.t) result

val to_json : t -> Jsonaf.t
val of_json : Jsonaf.t -> (t, Protocol_error.t) result
