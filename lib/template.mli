(** Lightweight string templating with typed substitutions.

    This module provides a simple, *zero-dependency* (other than
    {!Re2}) mechanism for substituting variables of the form
    {[ {{variable}} ]} inside a string.  The public API is exposed
    through two functors:

    • {!Make_Template} — renders a template using values supplied by a
      user-defined module that satisfies {!RENDERABLE}.

    • {!Make_parser} — performs the inverse operation: given the
      rendered text, recover a structured value whose shape is defined
      by a module that satisfies {!PARSABLE}.

    The snippet below shows a *round-trip* — render a value, parse it
    back, and inspect the result:

    {[
      open Core

      module Items = struct
        type t = string list

        let to_key_value_pairs items =
          let items =
            List.map items ~f:(Printf.sprintf "- %s")
            |> String.concat ~sep:"\n"
          in
          [ "items", items ]
      end

      module Items_template = Make_Template (Items)

      module Person = struct
        type t =
          { name  : string
          ; age   : int
          ; items : string list
          }

        let items_template =
          Items_template.create {|items
-----------
{{items}}|}

        let to_key_value_pairs p =
          [ "name", p.name
          ; "age", Int.to_string p.age
          ; "items", Items_template.render items_template p.items
          ]
      end

      module Person_template   = Make_Template (Person)
      module Person_parser     = struct
        include Person

        let parse_patterns =
          [ "Hello,\\s+(\\w+\\s+\\w+)!", "name"
          ; "Your age is\\s+(\\d+)\\.", "age"
          ; ( "What do You need from this list\\s+items\\n-----------\\n((?:-\\s+\\w+\\n?)+)",
              "items" )
          ]

        let from_key_value_pairs kv =
          let find k = List.Assoc.find_exn kv ~equal:String.equal k in
          { name  = find "name"
          ; age   = Int.of_string (find "age")
          ; items =
              String.split_lines (find "items")
              |> List.map ~f:String.strip
              |> List.map ~f:(String.chop_prefix_exn ~prefix:"- ")
          }
      end

      module P = Make_parser (Person_parser)

      let () =
        let t = Person_template.create {|Hello, {{ name }}!\nYour age is {{ age }}.\nWhat do You need from this list\n{{items}}|} in
        let person =
          { Person.name = "John Doe"; age = 30; items = [ "milk"; "eggs"; "toast" ] }
        in
        let rendered   = Person_template.render t person in
        match P.parse rendered with
        | Some p ->
          printf "%%s (%%d) → %%s\n" p.name p.age (String.concat p.items ~sep:", ")
        | None -> printf "Failed to parse\n"
    ]}

    Expected output:

    {[
      John Doe (30) → milk, eggs, toast
    ]}
*)

(** The RENDERABLE module type defines an interface for types that can be
    converted to key-value pairs for use in templating. *)
module type RENDERABLE = sig
  type t

  (** [to_key_value_pairs t] converts [t] to a list of key-value pairs. *)
  val to_key_value_pairs : t -> (string * string) list
end

(** {1 Simple one-shot helper API}

    The original functor interface is convenient when you have a
    well-typed record to render.  For quick substitutions we expose a
    minimal helper trio compatible with the needs of
    [Meta_prompting.Make].  This keeps legacy code working while
    avoiding another template library. *)

(** [of_string s] is identity.  It exists solely for API symmetry with
    {!load}. *)
val of_string : string -> string

(** [load ?search_dirs path] reads template contents from disk.

    Search order:
    1. iterate over [?search_dirs] (defaults to [["."]]);
    2. fall back to the literal [path] if no directory matched.

    @raise Failure if no readable file is found in the candidate
      locations. *)
val load : ?search_dirs:string list -> string -> string

(** [render tmpl mapping] substitutes every occurrence of {{key}} in
    [tmpl] with its value from [mapping].  Keys that are absent from
    [mapping] are replaced by the empty string. *)
val render : string -> (string * string) list -> string

(** The [Make_Template] functor instantiates a templating module for a
    given {!RENDERABLE}.  Placeholders follow the `{{variable}}`
    convention. *)
module Make_Template : functor (R : RENDERABLE) -> sig
  type t

  (** [create s] returns a template whose literal content is [s].

      The string may contain placeholders such as {[ {{user}} ]} that
      correspond to keys produced by {!R.to_key_value_pairs}. *)
  val create : string -> t

  (** [render tmpl v] substitutes every placeholder in [tmpl] with the
      value associated with the same key in [v].  Placeholders that do
      not have a corresponding key are replaced by the empty string.

      Example:
      {[
        let module T = Make_Template (struct
          type t = unit
          let to_key_value_pairs () = [ "x", "42" ]
        end) in
        T.render (T.create "value = {{x}}") () = "value = 42"
      ]} *)
  val render : t -> R.t -> string

  (** [to_string tmpl] returns the literal representation of [tmpl]
      without performing any substitution.  This is equivalent to the
      argument passed to {!create}. *)
  val to_string : t -> string
end

module type PARSABLE = sig
  type t

  (** [parse_patterns] is a list of regex patterns and corresponding keys to extract key-value pairs from a rendered template. *)
  val parse_patterns : (string * string) list

  (** [from_key_value_pairs kv_pairs] converts a list of key-value pairs [kv_pairs] to a value of type [t]. *)
  val from_key_value_pairs : (string * string) list -> t
end

module Make_parser : functor (P : PARSABLE) -> sig
  (** [parse s] attempts to recover a value of type [P.t] from the
      fully-rendered template [s].  It applies each `(pattern, key)`
      entry in {!P.parse_patterns} sequentially, collects all captured
      substrings, and finally delegates to {!P.from_key_value_pairs}.

      The function returns [None] if *any* of the patterns fail to
      match.  This conservative strategy avoids silently dropping
      information.  *)
  val parse : string -> P.t option
end
