
(* Simple snippet expansion table used by the Chat-TUI.

   A "snippet" is identified by a short, lowercase name and expands to a
   multi-line string that is inserted verbatim into the current draft when
   the user invokes

     /expand <name>

   inside the composer.  The snippets below are intentionally minimal – they
   merely serve as examples and can be extended freely without touching the
   rest of the code-base.  To add or modify snippets, edit the [snippets]
   association list below. *)

open Core

(* Association table [name, template].  Keep [name] all-lowercase to avoid
   surprising case-sensitivity issues. *)
let snippets : (string * string) list =
  [ ( "sig"
    , "module type S = sig\n  (** TODO: contents *)\nend" )
  ; ( "code"
    , "```ocaml\n(* Write your code here *)\n```" )
  ]

let find (name : string) : string option =
  List.Assoc.find snippets ~equal:String.equal name

let available () = List.map snippets ~f:fst

