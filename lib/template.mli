(** Example use of this module
    {[
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
        
        module ItemsTemplate = Make (Items) 
        module Person = struct
            type t =
            { name : string
            ; age : int
            ; items : string list
            }

            let items_template = ItemsTemplate.create {|items
        -----------
        {{items}}|}

            let to_key_value_pairs person =
            [ "name", person.name
            ; "age", Int.to_string person.age
            ; "items", ItemsTemplate.render items_template person.items
            ]
            ;;
        end
        
        module PersonTemplate = Make (Person) 
        let template =
            PersonTemplate.create
            {|
        Hello, {{ name }}! 
        Your age is {{ age }}.
        What do You need from this list 
        {{items}}
        |}
        
        let person =
            { Person.name = "John Doe"; age = 30; items = [ "milk"; "eggs"; "toast" ] }
        
        let rendered = PersonTemplate.render template person
        let () = printf "%s\n" rendered;
        
       "Hello, John Doe!
        Your age is 30.
        What do You need from this list
        items
        -----------
        - milk
        - eggs
        - toast"
   
    ]} *)

(** The RENDERABLE module type defines an interface for types that can be
    converted to key-value pairs for use in templating. *)
module type RENDERABLE = sig
  type t

  (** [to_key_value_pairs t] converts [t] to a list of key-value pairs. *)
  val to_key_value_pairs : t -> (string * string) list
end

(** The Make functor creates a templating module for a given RENDERABLE type. 
    Supports variable replacement with the syntax {{variable}} *)
module Make_Template : functor (R : RENDERABLE) -> sig
  type t

  (** [create s] creates a new template with the given string [s]. *)
  val create : string -> t

  (** [render t r] renders the template [t] with the RENDERABLE data [r]. *)
  val render : t -> R.t -> string

  (** [to_string t] converts the template [t] to a string. *)
  val to_string : t -> string
end


module type PARSABLE = sig
    type t
  
    (** [parse_patterns] is a list of regex patterns and corresponding keys to extract key-value pairs from a rendered template. *)
    val parse_patterns : (string * string) list
  
    (** [from_key_value_pairs kv_pairs] converts a list of key-value pairs [kv_pairs] to a value of type [t]. *)
    val from_key_value_pairs : (string * string) list -> t
  end

  module Make_parser : functor (P : PARSABLE)
  -> sig

    (** [parse s] extracts the data of type [P.t] from the rendered template string [s]. *)
    val parse : string -> P.t option
end

