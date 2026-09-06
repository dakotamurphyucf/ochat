open! Core

type t =
  { top_left : string
  ; top_right : string
  ; bottom_left : string
  ; bottom_right : string
  ; horizontal : string
  ; vertical : string
  }

let unicode =
  { top_left = "╭"
  ; top_right = "╮"
  ; bottom_left = "╰"
  ; bottom_right = "╯"
  ; horizontal = "─"
  ; vertical = "│"
  }
;;

let ascii =
  { top_left = "+"
  ; top_right = "+"
  ; bottom_left = "+"
  ; bottom_right = "+"
  ; horizontal = "-"
  ; vertical = "|"
  }
;;

let current () =
  match Sys.getenv "OCHAT_TUI_ASCII" |> Option.map ~f:String.lowercase with
  | Some ("1" | "true" | "yes") -> ascii
  | None | Some _ -> unicode
;;
