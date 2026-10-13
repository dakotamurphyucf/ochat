open Core
module Lookup = Chatml_configured_lookup
module Origin = Lookup.Origin
module Reference = Lookup.Reference
module Inferred = Chatml_inferred_type
module Alias = Chatml_type_alias
module Scope = Chatml_value_scope
module Diagnostic = Chatml_diagnostic

module Public_module = struct
  type t =
    { value_scheme : Inferred.typ
    ; types : Alias.t
    ; members : Chatml_semantic_binding.Members.t option
    }

  let create ~values ~types =
    { value_scheme = Inferred.Module (Inferred.Row (values, Inferred.Empty_row))
    ; types
    ; members = None
    }
  ;;

  let value_scheme t = t.value_scheme
  let types t = t.types
  let members t = t.members
  let with_members t members = { t with members = Some members }

  let with_value_scheme t value_scheme =
    match value_scheme with
    | Inferred.Module (Inferred.Row (_, Inferred.Empty_row)) -> Ok { t with value_scheme }
    | _ -> Error `Invalid_module_scheme
  ;;
end

module Annotation = struct
  type t =
    | Value_read of Origin.t
    | Inert_open of Origin.t
end

type t =
  { context : Lookup.t
  ; require : source:Lookup.t -> Reference.t -> (Public_module.t, Diagnostic.t) Result.t
  ; checkpoint : unit -> unit
  }

let create context ~require ~checkpoint = { context; require; checkpoint }
let with_checkpoint t ~checkpoint = { t with checkpoint }
let require t reference = t.require ~source:t.context reference

let reference t ?origin ~reason ~span name =
  match origin with
  | None -> Lookup.resolve t.context ~name ~reason ~span ~checkpoint:t.checkpoint
  | Some origin -> Lookup.at_use t.context origin ~reason ~span ~checkpoint:t.checkpoint
;;

module Selection = struct
  type t =
    { scheme : Inferred.typ
    ; origin : Origin.t option
    ; members : Chatml_semantic_binding.Members.t option
    }
end

let value_with_metadata t ~env ~name ~span =
  let open Result.Let_syntax in
  match Scope.find env ~name with
  | Some binding ->
    (match Scope.Binding.origin binding with
     | None ->
       Ok
         { Selection.scheme = Scope.Binding.scheme binding
         ; origin = None
         ; members =
             Option.bind
               (Scope.Binding.semantic binding)
               ~f:Chatml_semantic_binding.members
         }
     | Some origin ->
       let%bind reference =
         reference t ~origin ~reason:Lookup.Reason.Value_reference ~span name
       in
       let%map _ = require t reference in
       { Selection.scheme = Scope.Binding.scheme binding
       ; origin = Some origin
       ; members =
           Option.bind (Scope.Binding.semantic binding) ~f:Chatml_semantic_binding.members
       })
  | None ->
    let%bind reference = reference t ~reason:Lookup.Reason.Value_reference ~span name in
    let%map exports = require t reference in
    { Selection.scheme = Public_module.value_scheme exports
    ; origin = Some (Reference.origin reference)
    ; members = Public_module.members exports
    }
;;

let value t ~env ~name ~span =
  Result.map (value_with_metadata t ~env ~name ~span) ~f:(fun selected ->
    selected.Selection.scheme, selected.origin)
;;

let type_lookup t ~env =
  let resolve ~name ~member_path ~span =
    match Scope.find env ~name with
    | Some _ ->
      let diagnostic =
        Diagnostic.create ~span ("Unknown lexical type namespace '" ^ name ^ "'")
      in
      Error
        (Chatml.Chatml_owned_source.attach
           (Lookup.source t.context)
           diagnostic
           ~checkpoint:t.checkpoint)
    | None ->
      let open Result.Let_syntax in
      let%bind reference = reference t ~reason:Lookup.Reason.Qualified_type ~span name in
      let origin = Reference.origin reference in
      let selected =
        List.fold member_path ~init:origin ~f:(fun origin name ->
          t.checkpoint ();
          Origin.member origin ~name)
      in
      let%bind reference =
        Lookup.at_use
          t.context
          selected
          ~reason:Lookup.Reason.Qualified_type
          ~span
          ~checkpoint:t.checkpoint
      in
      let%map exports = require t reference in
      { Alias.Lookup.exports = Public_module.types exports; origin }
  in
  let observe origin ~span =
    let open Result.Let_syntax in
    let%bind reference =
      Lookup.at_use
        t.context
        origin
        ~reason:Lookup.Reason.Qualified_type
        ~span
        ~checkpoint:t.checkpoint
    in
    let%map _ = require t reference in
    ()
  in
  { Alias.Lookup.resolve; observe }
;;

module Opened = struct
  type t =
    | Local of Inferred.typ
    | Configured of
        { scheme : Inferred.typ
        ; types : Alias.t
        ; reference : Reference.t
        ; members : Chatml_semantic_binding.Members.t option
        }

  let diagnostic t ~message =
    match t with
    | Local _ -> Diagnostic.create message
    | Configured { reference; _ } ->
      Diagnostic.create
        ~site:(Reference.site reference)
        ~related:
          [ Diagnostic.Related.create
              (Reference.binding_site reference)
              ~message:"Configured module binding"
          ]
        message
  ;;
end

let open_module t ~env ~name ~span =
  let open Result.Let_syntax in
  let binding = Scope.find env ~name in
  match Option.bind binding ~f:Scope.Binding.origin, binding with
  | None, Some binding -> Ok (Opened.Local (Scope.Binding.scheme binding))
  | origin, binding ->
    let%bind reference = reference t ?origin ~reason:Lookup.Reason.Open ~span name in
    let%map public = require t reference in
    let scheme =
      Option.value_map
        binding
        ~default:(Public_module.value_scheme public)
        ~f:Scope.Binding.scheme
    in
    let path = Origin.member_path (Reference.origin reference) in
    let types =
      Alias.nested_module (Public_module.types public) ~path ~checkpoint:t.checkpoint
    in
    let members =
      match Public_module.members public with
      | None -> None
      | Some root ->
        List.fold path ~init:(Some root) ~f:(fun members name ->
          t.checkpoint ();
          Option.bind members ~f:(fun members ->
            Chatml_semantic_binding.Members.find
              members
              ~namespace:Value
              ~name
              ~checkpoint:t.checkpoint
            |> Option.bind ~f:Chatml_semantic_binding.members))
    in
    (* A first-class module value has a value scheme without a declared static
       namespace. Explicit static modules were validated by export collection. *)
    Opened.Configured
      { scheme; types = Option.value types ~default:Alias.empty; reference; members }
;;
