(** Closed, bounded projection from an authorized history query. A terminal
    [Source_unavailable] page records incomplete source coverage. A continuation
    can accompany an empty page. *)
open! Core

type projection =
  | Text_parts of int list
  | Match of
      { part : int
      ; start : int
      ; stop : int
      ; snippet : Search_snippet.t
      }

type row = private
  { citation : Recovery_window.Citation.t
  ; preparation_revision : int64 option
  ; ordinal : int
  ; role : Recovery_history_request.role
  ; attempt : Recovery_attempt_evidence.t option
  ; projection : projection
  }

type coverage =
  | Complete
  | Source_unavailable
  | Row_limit
  | Position_limit
  | Work_limit
  | Reply_limit

type t = private
  { rows : row list
  ; next : Page.Cursor.t option
  ; coverage : coverage
  }

val of_json : Jsonaf.t -> (t, Protocol_error.t) result
val to_json : t -> Jsonaf.t
val validate : t -> request:Recovery_history_request.t -> (unit, Protocol_error.t) result
