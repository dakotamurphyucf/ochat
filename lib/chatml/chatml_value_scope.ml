open Core
module Origin = Chatml_configured_lookup.Origin

module Binding = struct
  type t =
    { scheme : Chatml_inferred_type.typ
    ; origin : Origin.t option
    }

  let scheme t = t.scheme
  let origin t = t.origin
end

type t = Binding.t String.Map.t

let empty = String.Map.empty

let of_schemes schemes =
  Map.map schemes ~f:(fun scheme -> { Binding.scheme; origin = None })
;;

let find t ~name = Map.find t name

let add_local t ~name ~scheme =
  Map.set t ~key:name ~data:{ Binding.scheme; origin = None }
;;

let add_imported t ~name ~scheme ~origin =
  Map.set t ~key:name ~data:{ Binding.scheme; origin = Some origin }
;;
