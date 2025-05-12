open Core

(** The RENDERABLE module type defines an interface for types that can be
    converted to key-value pairs for use in templating. *)
module type RENDERABLE = sig
  type t

  (** [to_key_value_pairs t] converts [t] to a list of key-value pairs. *)
  val to_key_value_pairs : t -> (string * string) list
end

(** The Make_Template functor creates a templating module for a given RENDERABLE type. 
    Supports variable replacement with the syntax {{variable}} *)
module Make_Template (R : RENDERABLE) : sig
  type t

  (** [create s] creates a new template with the given string [s]. *)
  val create : string -> t

  (** [render t r] renders the template [t] with the RENDERABLE data [r]. *)
  val render : t -> R.t -> string

  (** [to_string t] converts the template [t] to a string. *)
  val to_string : t -> string
end = struct
  type t = string

  (** [replace_variables template data] replaces variables in the [template]
      with their corresponding values from the [data]. *)
  let replace_variables template data =
    let regex = Re2.create_exn "{{\\s*([a-zA-Z0-9_]+)\\s*}}" in
    let variables = R.to_key_value_pairs data in
    let replacer match_obj =
      let var_name = Re2.Match.get_exn ~sub:(`Index 1) match_obj in
      match List.Assoc.find ~equal:String.equal variables var_name with
      | Some value -> value
      | None -> ""
    in
    Re2.replace_exn ~f:replacer regex template
  ;;

  let render t data = replace_variables t data
  let to_string t = t
  let create t = t
end

module Person = struct
  module Items = struct
    type t = string list

    let to_key_value_pairs items =
      let items =
        List.map ~f:(fun item -> Printf.sprintf "- %s" item) items
        |> String.concat ~sep:"\n"
      in
      [ "items", items ]
    ;;
  end

  module ItemsTemplate = Make_Template (Items)

  type t =
    { name : string
    ; age : int
    ; items : string list
    }

  let items_template =
    ItemsTemplate.create
      {|items
-----------
{{items}}|}
  ;;

  let to_key_value_pairs person =
    [ "name", person.name
    ; "age", Int.to_string person.age
    ; "items", ItemsTemplate.render items_template person.items
    ]
  ;;
end

let%expect_test "template_test" =
  let module PersonTemplate = Make_Template (Person) in
  let template =
    PersonTemplate.create
      {|
Hello, {{ name }}! 
Your age is {{ age }}.
What do You need from this list 
{{items}}
|}
  in
  let person =
    { Person.name = "John Doe"; age = 30; items = [ "milk"; "eggs"; "toast" ] }
  in
  let rendered = PersonTemplate.render template person in
  printf "%s\n" rendered;
  [%expect
    {|
    Hello, John Doe!
    Your age is 30.
    What do You need from this list
    items
    -----------
    - milk
    - eggs
    - toast |}]
;;

module type PARSABLE = sig
  type t

  (** [parse_patterns] is a list of regex patterns and corresponding keys to extract key-value pairs from a rendered template. *)
  val parse_patterns : (string * string) list

  (** [from_key_value_pairs kv_pairs] converts a list of key-value pairs [kv_pairs] to a value of type [t]. *)
  val from_key_value_pairs : (string * string) list -> t
end

module Make_parser (P : PARSABLE) : sig
  (** [parse s] extracts the data of type [P.t] from the rendered template string [s]. *)
  val parse : string -> P.t option
end = struct
  let parse_patterns = P.parse_patterns

  let parse s =
    let extract_key_value_pair (pattern, key) =
      let regex = Re2.create_exn pattern in
      match Re2.find_first ~sub:(`Index 1) regex s with
      | Ok value -> Some (key, value)
      | Error err ->
        print_endline @@ Error.to_string_hum err;
        None
    in
    let kv_pairs = List.filter_map ~f:extract_key_value_pair parse_patterns in
    if List.length kv_pairs = List.length parse_patterns
    then Some (P.from_key_value_pairs kv_pairs)
    else None
  ;;
end

let%expect_test "template_parser_test" =
  let module PersonParser = struct
    include Person

    let parse_patterns =
      [ "Hello,\\s+(\\w+\\s+\\w+)!", "name"
      ; "Your age is\\s+(\\d+)\\.", "age"
      ; ( "What do You need from this list\\s+items\\n-----------\\n((?:-\\s+\\w+\\n?)+)"
        , "items" )
      ]
    ;;

    let from_key_value_pairs kv_pairs =
      let find_value key = List.Assoc.find_exn ~equal:String.equal kv_pairs key in
      let items_str = find_value "items" in
      let items =
        String.split_lines items_str
        |> List.map ~f:String.strip
        |> List.map ~f:(String.chop_prefix_exn ~prefix:"- ")
      in
      { name = find_value "name"; age = Int.of_string (find_value "age"); items }
    ;;
  end
  in
  let module PersonTemplateParser = Make_parser (PersonParser) in
  let rendered =
    "Hello, John Doe!\n\
     Your age is 30.\n\
     What do You need from this list\n\
     items\n\
     -----------\n\
     - milk\n\
     - eggs\n\
     - toast"
  in
  (match PersonTemplateParser.parse rendered with
   | Some person ->
     printf
       "Name: %s\nAge: %d\nItems: %s\n"
       person.Person.name
       person.Person.age
       (String.concat ~sep:", " person.Person.items)
   | None -> printf "Failed to parse the template\n");
  [%expect
    {|
    Name: John Doe
    Age: 30
    Items: milk, eggs, toast |}]
;;
