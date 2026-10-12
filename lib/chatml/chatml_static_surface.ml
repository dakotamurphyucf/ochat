open Core
module Spec = Chatml_builtin_spec

module Binding = struct
  type t =
    { name : string
    ; scheme : Spec.ty
    }
end

module Module = struct
  type t =
    { name : string
    ; exports : Binding.t list
    }

  let scheme ?(checkpoint = fun () -> ()) t =
    Spec.TModule
      (Spec.TRow_extend
         ( List.map t.exports ~f:(fun (b : Binding.t) ->
             checkpoint ();
             b.name, b.scheme)
         , Spec.TRow_empty ))
  ;;
end

type t =
  { globals : Binding.t list
  ; modules : Module.t list
  ; type_aliases : Chatml_builtin_surface.builtin_type_alias list
  }

let of_runtime_surface (surface : Chatml_builtin_surface.surface) ~checkpoint =
  let binding (b : Spec.builtin) =
    checkpoint ();
    { Binding.name = b.name; scheme = b.scheme }
  in
  let globals = List.map surface.globals ~f:binding in
  let modules =
    List.map surface.modules ~f:(fun (m : Spec.builtin_module) ->
      checkpoint ();
      { Module.name = m.name; exports = List.map m.exports ~f:binding })
  in
  let type_aliases =
    List.map surface.type_aliases ~f:(fun alias ->
      checkpoint ();
      alias)
  in
  checkpoint ();
  { globals; modules; type_aliases }
;;

let globals t = t.globals
let modules t = t.modules
let type_aliases t = t.type_aliases
