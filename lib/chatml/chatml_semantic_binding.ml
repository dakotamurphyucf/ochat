open Core
module Captured = Chatml_source
module Module_id = Chatml_prepared_program.Module_id

module Owner = struct
  type t = { seal : string }

  let create () = { seal = String.init 1 ~f:(fun _ -> '\000') }
  let equal = phys_equal
end

module Id = struct
  type t =
    { owner : Owner.t
    ; index : int
    }

  let create owner index = { owner; index }

  let equal left right =
    Owner.equal left.owner right.owner && Int.equal left.index right.index
  ;;

  let sexp_of_t t = [%sexp (t.index : int)]
  let owns owner t = Owner.equal owner t.owner
end

module Symbol_id = Id
module Group_id = Id
module Root_id = Id
module Context_id = Id

module Namespace = struct
  type t =
    | Value
    | Type
    | Type_parameter
  [@@deriving equal, sexp_of]
end

module Kind = struct
  type t =
    | Value
    | Parameter
    | Module
    | Type_alias
    | Type_parameter
  [@@deriving equal, sexp_of]
end

module Definition = struct
  type t =
    | Authored of Captured.Location.t
    | Implicit_specification_parameter of Captured.Location.t
    | Host of
        { contract : Captured.Digest.t
        ; member_path : string list
        }
end

type t =
  { symbol : Symbol_id.t
  ; name : string
  ; kind : Kind.t
  ; definition : Definition.t
  ; scheme : (Group_id.t * Root_id.t) option
  ; members : members option
  }

and members =
  { values : (string * t) list
  ; types : (string * t) list
  }

let name t = t.name
let symbol t = t.symbol
let kind t = t.kind
let definition t = t.definition
let group t = Option.map t.scheme ~f:fst
let scheme t = Option.map t.scheme ~f:snd
let members t = t.members

module Members = struct
  type binding = t
  type t = members

  let values t = t.values
  let types t = t.types

  let equal_name left right checkpoint =
    if String.length left <> String.length right
    then false
    else (
      let index = ref 0 in
      let equal = ref true in
      while !equal && !index < String.length left do
        if !index % 4096 = 0 then checkpoint ();
        if not (Char.equal left.[!index] right.[!index]) then equal := false;
        incr index
      done;
      !equal)
  ;;

  let find t ~namespace ~name ~checkpoint =
    let entries =
      match namespace with
      | Namespace.Value -> t.values
      | Type -> t.types
      | Type_parameter -> []
    in
    List.find_map entries ~f:(fun (candidate, binding) ->
      checkpoint ();
      if equal_name name candidate checkpoint then Some binding else None)
  ;;
end

module Private = struct
  module Owner = Owner

  let symbol = Symbol_id.create
  let group = Group_id.create
  let root = Root_id.create
  let context = Context_id.create
  let owns_symbol = Symbol_id.owns
  let owns_group = Group_id.owns
  let owns_root = Root_id.owns
  let owns_context = Context_id.owns
  let root_index (root : Root_id.t) = root.Id.index
  let symbol_index (symbol : Symbol_id.t) = symbol.Id.index
  let context_index (context : Context_id.t) = context.Id.index
  let equal_name left right ~checkpoint = Members.equal_name left right checkpoint

  let create symbol ~name ~kind ~definition ~scheme ~members =
    { symbol; name; kind; definition; scheme; members }
  ;;

  let with_scheme t ~scheme = { t with scheme = Some scheme }
  let with_members t members = { t with members = Some members }
  let public_projection t ~scheme ~members = { t with scheme = Some scheme; members }
  let members ~values ~types = { values; types }
end
