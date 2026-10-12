open Core
module Origin = Chatml_configured_lookup.Origin

module Binding = struct
  type t =
    { scheme : Chatml_inferred_type.typ
    ; semantic : Chatml_semantic_binding.t option
    ; origin : Origin.t option
    ; span : Source.span option
    }

  let semantic t = t.semantic
  let scheme t = t.scheme
  let origin t = t.origin
  let span t = t.span
end

type t =
  { bindings : Binding.t String.Map.t
  ; semantic_context : Chatml_semantic_recorder.Context.t option
  }

let empty = { bindings = String.Map.empty; semantic_context = None }

let of_schemes schemes =
  { bindings =
      Map.map schemes ~f:(fun scheme ->
        { Binding.scheme; semantic = None; origin = None; span = None })
  ; semantic_context = None
  }
;;

let find t ~name = Map.find t.bindings name

let add_local t ~name ~scheme =
  { t with
    bindings =
      Map.set
        t.bindings
        ~key:name
        ~data:{ Binding.scheme; semantic = None; origin = None; span = None }
  }
;;

let add_imported t ~name ~scheme ~origin =
  { t with
    bindings =
      Map.set
        t.bindings
        ~key:name
        ~data:{ Binding.scheme; semantic = None; origin = Some origin; span = None }
  }
;;

let with_span t ~name ~span =
  match Map.find t.bindings name with
  | None -> t
  | Some binding ->
    { t with
      bindings =
        Map.set t.bindings ~key:name ~data:{ binding with Binding.span = Some span }
    }
;;

let semantic_context t = t.semantic_context

let with_semantic_exn t ~name ~binding ~context =
  match Map.find t.bindings name with
  | None -> invalid_arg "Value_scope.with_semantic_exn: missing checked binding"
  | Some existing ->
    { bindings =
        Map.set
          t.bindings
          ~key:name
          ~data:{ existing with Binding.semantic = Some binding }
    ; semantic_context = Some context
    }
;;
