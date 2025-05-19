(*********************************************************************
     Small utility around the <config/> element that appears at most
     once in a ChatMarkdown document.  Most call-sites only need the
     *first* occurrence or a sensible default when the element is
     missing, leading to the same boiler-plate in three different
     places.

     We expose a single helper [of_elements] that collapses that logic
     into one reusable function.
  *********************************************************************)

open Core
module CM = Prompt_template.Chat_markdown

type t = CM.config

let default : t =
  { max_tokens = None
  ; model = None
  ; reasoning_effort = None
  ; temperature = None
  ; show_tool_call = false
  ; id = None
  }
;;

(* [of_elements els] returns either the first <config/> element found
     in [els] or a default record when none is present. *)
let of_elements (els : CM.top_level_elements list) : t =
  List.find_map els ~f:(function
    | CM.Config c -> Some c
    | _ -> None)
  |> Option.value ~default
;;
