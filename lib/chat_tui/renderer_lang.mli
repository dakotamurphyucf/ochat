(** Language inference helpers for tool output rendering. *)

(** [lang_of_path path] maps [path]'s file extension to a TextMate language id.

    The mapping is intentionally small and geared towards making
    [read_file]-style tool outputs readable. Unknown extensions yield [None].

    Current mappings:
    {ul
    {- [".ml"], [".mli"] → ["ocaml"]}
    {- [".md"] → ["markdown"]}
    {- [".json"] → ["json"]}
    {- [".sh"] → ["bash"]}}
*)
val lang_of_path : string -> string option
