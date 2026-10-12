open Core
module Captured = Chatml_source
module Prepared = Chatml_prepared_program
module Frozen = Chatml_typechecker.Type_snapshot
module Identity = Chatml_semantic_identity
module Query = Chatml_semantic_query

module Owner = struct
  type t = { seal : string }

  let create () = { seal = String.init 1 ~f:(fun _ -> '\000') }
end

module Symbol_id = struct
  type t =
    { owner : Owner.t
    ; index : int
    }

  let equal left right =
    phys_equal left.owner right.owner && Int.equal left.index right.index
  ;;

  let sexp_of_t t = [%sexp (t.index : int)]
end

module Namespace = struct
  type t =
    | Value
    | Type
    | Type_parameter
  [@@deriving equal, sexp_of]
end

module Unavailable = struct
  type t =
    | Not_checked
    | No_recorded_context
    | No_recorded_type
    | No_nominal_members
  [@@deriving sexp_of]
end

type 'a page =
  { items : 'a list
  ; truncated : bool
  }

type 'a fact =
  | Available of 'a
  | Unavailable of Unavailable.t

type 'a selection =
  | Selected of 'a
  | Missing of Unavailable.t
  | Ambiguous of 'a page

module Origin = struct
  type t =
    | Authored of Captured.Location.t
    | Implicit_specification_parameter of Captured.Location.t
    | Host of
        { contract : Captured.Digest.t
        ; member_path : string list
        }
end

module Symbol = struct
  type kind =
    | Value
    | Parameter
    | Module
    | Type_alias
    | Type_parameter

  type t =
    { id : Symbol_id.t
    ; name : string
    ; kind : kind
    ; origin : Origin.t
    ; scheme : Frozen.Id.t option
    ; specifications : Symbol_id.t list
    }

  let id t = t.id
  let name t = t.name
  let kind t = t.kind
  let origin t = t.origin
  let scheme t = t.scheme
  let specifications t = t.specifications
end

module Binding = struct
  type t =
    { symbol : Symbol_id.t
    ; name : string
    ; scheme : Frozen.Id.t option
    ; values : t list fact
    ; types : t list fact
    }

  let symbol t = t.symbol
  let scheme t = t.scheme
  let values t = t.values
  let types t = t.types
end

module Alias = struct
  type t =
    { symbol : Symbol_id.t
    ; parameters : Symbol_id.t list
    ; declaration : Chatml.Chatml_type_expr.Declaration.t
    ; template : Frozen.Id.t
    }

  let symbol t = t.symbol
  let parameters t = t.parameters
  let declaration t = t.declaration
  let template t = t.template
end

module Point = struct
  type t =
    { owner : Owner.t
    ; location : Captured.Location.t
    }

  let location t = t.location
end

module Context = struct
  type t =
    { owner : Owner.t
    ; index : int
    ; source : Captured.Id.t
    }

  let source t = t.source
end

module Reference = struct
  type target =
    | Symbol of Symbol_id.t
    | Configured_module of Prepared.Module_id.t
    | Structural_label
    | Unlocated_module_member
    | Language_type of string

  type t =
    { context : Context.t
    ; location : Captured.Location.t
    ; target : target
    ; inferred : Frozen.Id.t option
    }

  let selection t = t.location
  let target t = t.target
  let inferred_type t = t.inferred
end

module Query_limits = Query.Limits
module Error = Query.Error

module Exports = struct
  type t =
    { source : Captured.Id.t
    ; values : Binding.t list
    ; types : Binding.t list
    }

  let source t = t.source
  let values t = t.values
  let types t = t.types
end

module Configured_module = struct
  type t =
    { name : string
    ; module_id : Prepared.Module_id.t
    ; declared_at : Captured.Location.t
    ; exports : Exports.t fact
    ; shadowed_by : Symbol_id.t option
    }

  let name t = t.name
  let module_id t = t.module_id
  let declared_at t = t.declared_at
  let exports t = t.exports
  let shadowed_by t = t.shadowed_by
end

module Source_status = struct
  type t =
    | Checked_implementation
    | Checked_interface
    | Unchecked
    | Metadata
  [@@deriving equal, sexp_of]
end

module Host_requirement = struct
  type t =
    { name : string
    ; binding : Symbol_id.t
    ; required : Frozen.Id.t
    ; contract : Captured.Digest.t
    }

  let name t = t.name
  let binding t = t.binding
  let required t = t.required
  let contract t = t.contract
end

type transition =
  | Root
  | Enter of Context.t
  | Join of
      { values : Context.t option
      ; types : Context.t option
      }
  | Define of
      { parent : Context.t
      ; namespace : Namespace.t
      ; binding : Binding.t
      }

type context_row =
  { context : Context.t
  ; transition : transition
  }

type region =
  { context : Context.t
  ; location : Captured.Location.t
  ; inferred : Frozen.Id.t option
  }

type t =
  { owner : Owner.t
  ; identity : Identity.t
  ; prepared : Prepared.t
  ; types : Frozen.t
  ; graph : Chatml_dependency_graph.t
  ; runtime_order : Prepared.Module_id.t list
  ; symbols : Symbol.t array
  ; contexts : context_row array
  ; aliases : Alias.t list
  ; references : Reference.t list
  ; regions : region list
  ; exports : (Prepared.Module_id.t * Exports.t) list
  ; host_requirements : Host_requirement.t list
  ; checked_sources : (Captured.Id.t * Chatml_configured_lookup.Phase.t) list
  }

let identity t = t.identity
let sources t = Prepared.sources t.prepared
let scope t id = Prepared.find_scope t.prepared id
let types t = t.types
let graph t = t.graph
let runtime_order t = t.runtime_order
let diagnostics _ = []
let host_requirements t = t.host_requirements

let source_status t id =
  match Prepared.find_source t.prepared id with
  | None -> None
  | Some _ ->
    (match
       List.find t.checked_sources ~f:(fun (source, _) -> Captured.Id.equal source id)
     with
     | Some (_, Implementation) -> Some Source_status.Checked_implementation
     | Some (_, Interface) -> Some Source_status.Checked_interface
     | None -> Some (if Option.is_some (scope t id) then Unchecked else Metadata))
;;

let point t location =
  match Prepared.find_source t.prepared (Captured.Location.source location) with
  | None -> Error Error.Unknown_source
  | Some source ->
    let span = Captured.Location.span location in
    if
      (not
         (Captured.Digest.equal
            (Captured.digest source)
            (Captured.Location.source_digest location)))
      || span.left.offset < 0
      || span.right.offset > Captured.byte_length source
    then Error Error.Invalid_location
    else if not (Int.equal span.left.offset span.right.offset)
    then Error Error.Nonzero_point_range
    else Ok { Point.owner = t.owner; location }
;;

let symbol t id =
  if
    (not (phys_equal t.owner id.Symbol_id.owner))
    || id.index < 0
    || id.index >= Array.length t.symbols
  then Error Error.Foreign_symbol
  else Ok t.symbols.(id.index)
;;

let alias t id =
  Result.map (symbol t id) ~f:(fun _ ->
    List.find t.aliases ~f:(fun alias -> Symbol_id.equal alias.Alias.symbol id))
;;

let exports t id =
  if
    (not (Prepared.Module_id.equal id (Chatml_dependency_graph.entry t.graph)))
    && Option.is_none (Prepared.find_definition t.prepared id)
  then Error Error.Unknown_module
  else
    Ok
      (match List.Assoc.find t.exports id ~equal:Prepared.Module_id.equal with
       | Some exports -> Available exports
       | None -> Unavailable Not_checked)
;;

let check_point t point =
  if not (phys_equal t.owner point.Point.owner) then Query.reject Foreign_point
;;

let check_context t context =
  if not (phys_equal t.owner context.Context.owner) then Query.reject Foreign_context
;;

let point_offset point = (Captured.Location.span point.Point.location).left.offset

let same_location_source point location ~checkpoint =
  Query.equal_source
    (Captured.Location.source point.Point.location)
    (Captured.Location.source location)
    ~checkpoint
;;

let selected_at point location ~checkpoint =
  if not (same_location_source point location ~checkpoint)
  then false
  else (
    let span = Captured.Location.span location in
    span.left.offset <= point_offset point && point_offset point < span.right.offset)
;;

let reverse items ~checkpoint =
  List.fold items ~init:[] ~f:(fun reversed item ->
    checkpoint ();
    item :: reversed)
;;

let page limits items ~checkpoint =
  let rec take remaining items selected =
    checkpoint ();
    match items with
    | [] -> { items = reverse selected ~checkpoint; truncated = false }
    | _ :: _ when remaining <= 0 ->
      { items = reverse selected ~checkpoint; truncated = true }
    | item :: rest -> take (remaining - 1) rest (item :: selected)
  in
  take (Query_limits.max_results limits) items []
;;

let checked t point ~checkpoint =
  List.exists t.checked_sources ~f:(fun (id, _) ->
    Query.equal_source id (Captured.Location.source point.Point.location) ~checkpoint)
;;

let references_at t point ~limits ~checkpoint =
  Query.run limits ~checkpoint ~f:(fun ~checkpoint ->
    check_point t point;
    if not (checked t point ~checkpoint)
    then Unavailable Not_checked
    else (
      let items =
        List.filter t.references ~f:(fun reference ->
          checkpoint ();
          selected_at point reference.Reference.location ~checkpoint)
      in
      Available (page limits items ~checkpoint)))
;;

let definitions_at t point ~limits ~checkpoint =
  Query.run limits ~checkpoint ~f:(fun ~checkpoint ->
    check_point t point;
    if not (checked t point ~checkpoint)
    then Unavailable Not_checked
    else (
      let items =
        Array.fold t.symbols ~init:[] ~f:(fun items symbol ->
          checkpoint ();
          match symbol.Symbol.origin with
          | Host _ -> items
          | Authored location | Implicit_specification_parameter location ->
            if selected_at point location ~checkpoint then symbol :: items else items)
        |> reverse ~checkpoint
      in
      Available (page limits items ~checkpoint)))
;;

let selection limits values ~equal ~missing ~checkpoint =
  let values =
    List.fold values ~init:[] ~f:(fun unique value ->
      checkpoint ();
      if
        List.exists unique ~f:(fun old ->
          checkpoint ();
          equal old value)
      then unique
      else value :: unique)
    |> reverse ~checkpoint
  in
  match values with
  | [] -> Missing missing
  | [ value ] -> Selected value
  | _ :: _ :: _ -> Ambiguous (page limits values ~checkpoint)
;;

let type_at t point ~limits ~checkpoint =
  Query.run limits ~checkpoint ~f:(fun ~checkpoint ->
    check_point t point;
    if not (checked t point ~checkpoint)
    then Missing Not_checked
    else (
      let roots =
        List.filter_map t.references ~f:(fun reference ->
          checkpoint ();
          if selected_at point reference.Reference.location ~checkpoint
          then reference.inferred
          else None)
      in
      let roots =
        if not (List.is_empty roots)
        then roots
        else (
          let _, roots =
            List.fold t.regions ~init:(None, []) ~f:(fun (width, roots) region ->
              checkpoint ();
              if not (selected_at point region.location ~checkpoint)
              then width, roots
              else (
                match region.inferred with
                | None -> width, roots
                | Some root ->
                  let span = Captured.Location.span region.location in
                  let observed = span.right.offset - span.left.offset in
                  (match width with
                   | None -> Some observed, [ root ]
                   | Some width when observed < width -> Some observed, [ root ]
                   | Some width when observed = width -> Some width, root :: roots
                   | Some _ -> width, roots)))
          in
          reverse roots ~checkpoint)
      in
      selection limits roots ~equal:Frozen.Id.equal ~missing:No_recorded_type ~checkpoint))
;;

let context_at t point ~limits ~checkpoint =
  Query.run limits ~checkpoint ~f:(fun ~checkpoint ->
    check_point t point;
    if not (checked t point ~checkpoint)
    then Missing Not_checked
    else (
      let contexts =
        List.filter_map t.references ~f:(fun reference ->
          checkpoint ();
          if selected_at point reference.Reference.location ~checkpoint
          then Some reference.context
          else None)
      in
      let contexts =
        if not (List.is_empty contexts)
        then contexts
        else
          List.filter_map t.regions ~f:(fun region ->
            checkpoint ();
            let span = Captured.Location.span region.location in
            if
              Int.equal span.left.offset (point_offset point)
              && same_location_source point region.location ~checkpoint
            then Some region.context
            else None)
      in
      selection
        limits
        contexts
        ~equal:(fun left right -> Int.equal left.Context.index right.Context.index)
        ~missing:No_recorded_context
        ~checkpoint))
;;

let bindings t context (namespace : Namespace.t) ~checkpoint =
  let rec visit context selected =
    checkpoint ();
    match t.contexts.(context.Context.index).transition with
    | Root -> selected
    | Enter parent -> visit parent selected
    | Join { values; types } ->
      (match
         match namespace with
         | Value -> values
         | Type | Type_parameter -> types
       with
       | None -> selected
       | Some parent -> visit parent selected)
    | Define { parent; namespace = declared; binding } ->
      let selected =
        if
          Namespace.equal namespace declared
          && not
               (List.exists selected ~f:(fun old ->
                  Query.compare_string old.Binding.name binding.Binding.name ~checkpoint
                  = 0))
        then binding :: selected
        else selected
      in
      visit parent selected
  in
  visit context []
;;

let candidates t context ~namespace ~limits ~checkpoint =
  Query.run limits ~checkpoint ~f:(fun ~checkpoint ->
    check_context t context;
    let items =
      bindings t context namespace ~checkpoint
      |> List.sort ~compare:(fun left right ->
        Query.compare_string left.Binding.name right.Binding.name ~checkpoint)
    in
    page limits items ~checkpoint)
;;

let configured_modules t context ~limits ~checkpoint =
  Query.run limits ~checkpoint ~f:(fun ~checkpoint ->
    check_context t context;
    let lexical = bindings t context Value ~checkpoint in
    let items =
      match
        List.find (Prepared.scopes t.prepared) ~f:(fun scope ->
          checkpoint ();
          Query.equal_source (Prepared.Scope.source scope) context.source ~checkpoint)
      with
      | None -> []
      | Some scope ->
        List.map (Prepared.Scope.bindings scope) ~f:(fun binding ->
          checkpoint ();
          let shadowed_by =
            List.find_map lexical ~f:(fun lexical ->
              if Query.compare_string lexical.Binding.name binding.name ~checkpoint = 0
              then Some lexical.symbol
              else None)
          in
          let exports =
            match
              List.find_map t.exports ~f:(fun (id, exports) ->
                checkpoint ();
                if
                  Query.equal_source
                    (Prepared.Module_id.implementation id)
                    (Prepared.Module_id.implementation binding.module_id)
                    ~checkpoint
                then Some exports
                else None)
            with
            | Some exports -> Available exports
            | None -> Unavailable Not_checked
          in
          { Configured_module.name = binding.name
          ; module_id = binding.module_id
          ; declared_at = binding.declared_at
          ; exports
          ; shadowed_by
          })
    in
    let items =
      List.sort items ~compare:(fun left right ->
        Query.compare_string
          left.Configured_module.name
          right.Configured_module.name
          ~checkpoint)
    in
    page limits items ~checkpoint)
;;

module Private = struct
  module Owner = Owner

  let symbol_id owner index = { Symbol_id.owner; index }
  let context owner ~index ~source = { Context.owner; index; source }

  let symbol id ~name ~kind ~origin ~scheme ~specifications =
    { Symbol.id; name; kind; origin; scheme; specifications }
  ;;

  let binding symbol ~name ~scheme ~values ~types =
    { Binding.symbol; name; scheme; values; types }
  ;;

  let alias symbol ~parameters ~declaration ~template =
    { Alias.symbol; parameters; declaration; template }
  ;;

  let reference ~context ~location ~target ~inferred =
    { Reference.context; location; target; inferred }
  ;;

  let exports ~source ~values ~types = { Exports.source; values; types }

  let host_requirement ~name ~binding ~required ~contract =
    { Host_requirement.name; binding; required; contract }
  ;;

  type nonrec transition = transition =
    | Root
    | Enter of Context.t
    | Join of
        { values : Context.t option
        ; types : Context.t option
        }
    | Define of
        { parent : Context.t
        ; namespace : Namespace.t
        ; binding : Binding.t
        }

  type nonrec context_row = context_row =
    { context : Context.t
    ; transition : transition
    }

  type nonrec region = region =
    { context : Context.t
    ; location : Captured.Location.t
    ; inferred : Frozen.Id.t option
    }

  let create
        owner
        ~identity
        ~prepared
        ~types
        ~graph
        ~runtime_order
        ~symbols
        ~contexts
        ~aliases
        ~references
        ~regions
        ~exports
        ~host_requirements
        ~checked_sources
    =
    { owner
    ; identity
    ; prepared
    ; types
    ; graph
    ; runtime_order
    ; symbols
    ; contexts
    ; aliases
    ; references
    ; regions
    ; exports
    ; host_requirements
    ; checked_sources
    }
  ;;
end
