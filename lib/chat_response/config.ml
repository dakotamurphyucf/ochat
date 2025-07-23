(** Configuration helpers.

    ChatMarkdown documents can contain **at most one** [<config/>] element that
    tweaks generation parameters such as the model variant, temperature, or
    maximum number of output tokens.  Most callers only care about the first
    such element – or a sensible default when none is present – but the search
    and fallback logic was previously duplicated across several modules.

    This tiny helper collapses that boiler-plate into a single reusable
    function {!of_elements}.  The concrete record type {!t} is re-exported from
    {!Prompt.Chat_markdown.config}.  The module is *pure* and performs no I/O. *)

open Core
module CM = Prompt.Chat_markdown

type t = CM.config

(** Default configuration (all optional fields unset).

    The value mirrors the defaults used by the OpenAI API client and can be
    shared safely between requests. *)
let default : t =
  { max_tokens = None
  ; model = None
  ; reasoning_effort = None
  ; temperature = None
  ; show_tool_call = false
  ; id = None
  }
;;

(** [of_elements els] extracts the first [<config/>] element from the list of
    parsed ChatMarkdown [els].  When no such element is present the
    {!default} configuration record is returned instead. *)

let of_elements (els : CM.top_level_elements list) : t =
  List.find_map els ~f:(function
    | CM.Config c -> Some c
    | _ -> None)
  |> Option.value ~default
;;
