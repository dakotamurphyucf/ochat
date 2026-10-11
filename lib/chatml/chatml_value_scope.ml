open Core
module Origin = Chatml_configured_lookup.Origin

module Binding = struct
  type t =
    { scheme : Chatml_inferred_type.typ
    ; origin : Origin.t option
    ; span : Source.span option
    }

  let scheme t = t.scheme
  let origin t = t.origin
  let span t = t.span
end

type t = Binding.t String.Map.t

let empty = String.Map.empty

let of_schemes schemes =
  Map.map schemes ~f:(fun scheme -> { Binding.scheme; origin = None; span = None })
;;

let find t ~name = Map.find t name

let add_local t ~name ~scheme =
  Map.set t ~key:name ~data:{ Binding.scheme; origin = None; span = None }
;;

let add_imported t ~name ~scheme ~origin =
  Map.set t ~key:name ~data:{ Binding.scheme; origin = Some origin; span = None }
;;

let with_span t ~name ~span =
  match Map.find t name with
  | None -> t
  | Some binding -> Map.set t ~key:name ~data:{ binding with Binding.span = Some span }
;;
