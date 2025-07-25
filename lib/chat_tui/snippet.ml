(** Snippet expansion table for the Chat-TUI.

    A *snippet* is a short mnemonic (lower-case) that expands to a template –
    potentially multiple lines – which is pasted verbatim into the composer
    when the user types:

    {v
      /expand <name>
    v}

    The table is intentionally minimal; extend it by editing the internal
    {!val:snippets} association list and rebuilding the project.

    {1 API overview}

    • {!val:find}  – resolve a snippet by name.
    • {!val:available} – list all defined snippet names.

    Names are case-sensitive; the convention is to keep them lower-case. *)

open Core

(** Association table [name ✦ template].

    Keep [name] lower-case to avoid surprising case-sensitivity issues. *)
let snippets : (string * string) list =
  [ "sig", "module type S = sig\n  (** TODO: contents *)\nend"
  ; "code", "```ocaml\n(* Write your code here *)\n```"
  ]
;;

(** [find name] returns the template associated with [name] or [None] if
    the snippet is unknown.

    Example using the default table:
    {[
      match Snippet.find "sig" with
      | Some t -> Stdio.print_endline t
      | None   -> Stdio.eprintf "unknown snippet\n"
    ]} *)
let find (name : string) : string option =
  List.Assoc.find snippets ~equal:String.equal name
;;

(** [available ()] lists all snippet names in declaration order.  Useful for
    auto-completion menus.

    {[
      Snippet.available () = ["sig"; "code"]
    ]} *)
let available () = List.map snippets ~f:fst
