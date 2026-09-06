(** Literal and typed string templates. The standalone and functor renderers
    intentionally have different substitution semantics. *)

module type RENDERABLE = sig
  type t

  val to_key_value_pairs : t -> (string * string) list
end

(** [of_string text] returns [text] unchanged. *)
val of_string : string -> string

(** [load ?search_dirs path] searches directories in order (default ["."]).
    There is no final literal-path fallback. Uses blocking filesystem calls,
    not Eio; missing candidates and read errors raise. *)
val load : ?search_dirs:string list -> string -> string

(** [render template mapping] substitutes exact [{{key}}] strings in mapping
    order. Missing keys remain, whitespace is not normalized, and later mappings
    may replace text inserted earlier. No escaping or validation is performed.

    {[
      let result = Template.render "{{x}}/{{ missing }}" [ "x", "yes" ]
      let () = assert (String.equal result "yes/{{ missing }}")
    ]} *)
val render : string -> (string * string) list -> string

module Make_Template : functor (R : RENDERABLE) -> sig
  type t

  (** [create text] stores the raw template. *)
  val create : string -> t

  (** [render template value] replaces whitespace-tolerant placeholders whose
      names contain letters, digits or underscores. Missing keys become empty
      strings. Inserted values are not recursively expanded by this replacement.
      The first matching association wins. *)
  val render : t -> R.t -> string

  (** [to_string template] returns the raw, unsubstituted text. *)
  val to_string : t -> string
end

module type PARSABLE = sig
  type t

  (** Patterns and output keys; each pattern must provide capture group one. *)
  val parse_patterns : (string * string) list

  val from_key_value_pairs : (string * string) list -> t
end

module Make_parser : functor (P : PARSABLE) -> sig
  (** [parse text] collects capture group one from each pattern and calls
      [from_key_value_pairs] only when every extraction succeeds. Failed matches
      print diagnostics and return [None]. Invalid regexes or conversion callback
      exceptions can raise; this is not an exception-free validation boundary. *)
  val parse : string -> P.t option
end
