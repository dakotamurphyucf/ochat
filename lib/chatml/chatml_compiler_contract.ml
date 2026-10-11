open Core
module Static = Chatml_static_surface

module Target = struct
  type t =
    | One_off_v1
    | Tool_v1
    | Moderator_v1
    | Delegated_moderator_v1
  [@@deriving equal, sexp]
end

module Kind = struct
  type t =
    | Standard of Target.t
    | Native
end

type t =
  { kind : Kind.t
  ; surface : Static.t
  ; required_bindings : (string * Chatml_builtin_spec.ty) list
  }

let for_target target ~checkpoint =
  let module X = Chatml_extension_surface in
  let surface, required_bindings =
    match target with
    | Target.One_off_v1 -> X.one_off_v1, X.one_off_entrypoints
    | Tool_v1 -> X.tool_v1, X.tool_entrypoints
    | Moderator_v1 -> X.moderator_v1, X.moderator_entrypoints
    | Delegated_moderator_v1 -> X.delegated_moderator_v1, X.moderator_entrypoints
  in
  { kind = Kind.Standard target
  ; surface = Static.of_runtime_surface surface ~checkpoint
  ; required_bindings
  }
;;

let kind t = t.kind
let static_surface t = t.surface
let required_bindings t = t.required_bindings

let for_static_surface surface ~required_bindings =
  { kind = Kind.Native; surface; required_bindings }
;;

(* Conversion and emission poll their actual work. Standard identity preserves
   compiler.v1 bytes; native static metadata has a distinct target/version tag. *)
let identity t ~checkpoint =
  let binding name scheme =
    checkpoint ();
    Sexp.List [ Sexp.Atom name; Chatml_contract_encoding.type_scheme scheme ~checkpoint ]
  in
  let bindings entries =
    Sexp.List
      (List.map entries ~f:(fun (b : Static.Binding.t) -> binding b.name b.scheme))
  in
  let globals = bindings (Static.globals t.surface) in
  let modules =
    Sexp.List
      (List.map (Static.modules t.surface) ~f:(fun (m : Static.Module.t) ->
         checkpoint ();
         Sexp.List [ Sexp.Atom m.name; bindings m.exports ]))
  in
  let aliases =
    Sexp.List
      (List.map
         (Static.type_aliases t.surface)
         ~f:(fun (a : Chatml_builtin_surface.builtin_type_alias) -> binding a.name a.body))
  in
  let requirements =
    Sexp.List
      (List.map t.required_bindings ~f:(fun (name, scheme) -> binding name scheme))
  in
  let tag, target =
    match t.kind with
    | Standard target -> "ochat.chatml.compiler.v1", Target.sexp_of_t target
    | Native -> "ochat.chatml.compiler.native.v1", Sexp.Atom "Native"
  in
  checkpoint ();
  Sexp.List [ Sexp.Atom tag; target; globals; modules; aliases; requirements ]
;;

let fingerprint t ~checkpoint =
  let encoded = Chatml_contract_encoding.to_string (identity t ~checkpoint) ~checkpoint in
  Chatml_source.Digest.of_contents encoded ~checkpoint
;;
