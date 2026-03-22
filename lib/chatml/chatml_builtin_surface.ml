open Core
module Builtin_spec = Chatml_builtin_spec

type builtin_type_alias =
  { name : string
  ; body : Builtin_spec.ty
  }

type surface =
  { globals : Builtin_spec.builtin list
  ; modules : Builtin_spec.builtin_module list
  ; type_aliases : builtin_type_alias list
  }

let empty : surface = { globals = []; modules = []; type_aliases = [] }

let dedupe_by_name (entries : 'a list) ~(name_of : 'a -> string) : 'a list =
  let seen = Hash_set.create (module String) in
  List.filter entries ~f:(fun entry ->
    let name = name_of entry in
    if Hash_set.mem seen name
    then false
    else (
      Hash_set.add seen name;
      true))
;;

let merge (left : surface) (right : surface) : surface =
  { globals =
      dedupe_by_name (left.globals @ right.globals) ~name_of:(fun builtin -> builtin.name)
  ; modules =
      dedupe_by_name (left.modules @ right.modules) ~name_of:(fun builtin_module ->
        builtin_module.name)
  ; type_aliases =
      dedupe_by_name (left.type_aliases @ right.type_aliases) ~name_of:(fun alias ->
        alias.name)
  }
;;

let of_modules (modules : Builtin_spec.builtin_module list) : surface =
  { empty with modules }
;;

let core_type_aliases : builtin_type_alias list =
  [ { name = "json"; body = Builtin_spec.json_ty } ]
;;

let moderator_type_aliases : builtin_type_alias list =
  [ { name = "item"; body = Builtin_spec.item_ty }
  ; { name = "tool_desc"; body = Builtin_spec.tool_desc_ty }
  ; { name = "tool_call"; body = Builtin_spec.tool_call_ty }
  ; { name = "tool_result"; body = Builtin_spec.tool_result_ty }
  ; { name = "context"; body = Builtin_spec.context_ty }
  ]
;;

let core_surface : surface =
  { globals = Builtin_spec.builtins
  ; modules = Builtin_spec.core_modules
  ; type_aliases = core_type_aliases
  }
;;

let moderator_surface : surface =
  merge
    core_surface
    { globals = []
    ; modules = Builtin_spec.moderator_modules
    ; type_aliases = moderator_type_aliases
    }
;;
