open Core
module Captured = Chatml_source
module Prepared = Chatml_prepared_program
module Diagnostic = Chatml_diagnostic

module Reason = struct
  type t =
    | Value_reference
    | Open
    | Qualified_type
  [@@deriving compare, equal, sexp_of]
end

module Phase = struct
  type t =
    | Implementation
    | Interface
  [@@deriving compare, equal, sexp_of]
end

type binding =
  { name : string
  ; dependency : Prepared.Module_id.t
  ; source : Captured.t
  ; location : Captured.Location.t
  }

type t =
  { owner : Prepared.Module_id.t
  ; source : Captured.t
  ; phase : Phase.t
  ; input_fingerprint : Captured.Digest.t
  ; bindings : binding String.Map.t
  ; selected_at : (Captured.t * Captured.Location.t) option
  }

let owner t = t.owner
let source t = t.source
let phase t = t.phase
let input_fingerprint t = t.input_fingerprint
let error message = Error (Diagnostic.create message)

let site source location ~checkpoint =
  Diagnostic.Site.create source ~span:(Captured.Location.span location) ~checkpoint
  |> Result.map_error ~f:(fun error -> Diagnostic.create (Error.to_string_hum error))
;;

let create input ~owner ~source:source_id ~checkpoint =
  checkpoint ();
  let phase =
    match Prepared.find_definition input owner with
    | Some definition ->
      if Captured.Id.equal source_id (Prepared.Definition.implementation definition)
      then Some Phase.Implementation
      else if
        Option.exists
          (Prepared.Definition.interface definition)
          ~f:(Captured.Id.equal source_id)
      then Some Phase.Interface
      else None
    | None ->
      if
        Captured.Id.equal source_id (Prepared.entry input)
        && Prepared.Module_id.equal owner (Prepared.Module_id.of_implementation source_id)
      then Some Phase.Implementation
      else None
  in
  match
    phase, Prepared.find_source input source_id, Prepared.find_scope input source_id
  with
  | Some phase, Some source, Some scope ->
    let open Result.Let_syntax in
    let capture location =
      match Prepared.find_source input (Captured.Location.source location) with
      | Some source -> Ok (source, location)
      | None -> error "Configured scope refers to an absent captured source"
    in
    let%bind bindings =
      List.fold_result
        (Prepared.Scope.bindings scope)
        ~init:String.Map.empty
        ~f:(fun bindings (binding : Prepared.Scope.binding) ->
          checkpoint ();
          let%map source, location = capture binding.declared_at in
          Map.set
            bindings
            ~key:binding.name
            ~data:
              { name = binding.name; dependency = binding.module_id; source; location })
    in
    let%map selected_at =
      match Prepared.Scope.selected_at scope with
      | None -> Ok None
      | Some location -> Result.map (capture location) ~f:Option.some
    in
    { owner
    ; source
    ; phase
    ; input_fingerprint = Prepared.fingerprint input
    ; bindings
    ; selected_at
    }
  | _ ->
    error
      "Configured lookup source is not this canonical module's admitted \
       implementation/interface"
;;

module Origin = struct
  type t =
    { owner : Prepared.Module_id.t
    ; source_id : Captured.Id.t
    ; source_digest : Captured.Digest.t
    ; phase : Phase.t
    ; input_fingerprint : Captured.Digest.t
    ; dependency : Prepared.Module_id.t
    ; name : string
    ; member_path : string list
    ; binding_site : Diagnostic.Site.t
    }

  let dependency t = t.dependency
  let member_path t = t.member_path
  let binding_site t = t.binding_site
  let member t ~name = { t with member_path = t.member_path @ [ name ] }
end

module Reference = struct
  type t =
    { origin : Origin.t
    ; reason : Reason.t
    ; site : Diagnostic.Site.t
    }

  let owner t = t.origin.owner
  let dependency t = t.origin.dependency
  let name t = t.origin.name
  let reason t = t.reason
  let phase t = t.origin.phase
  let input_fingerprint t = t.origin.input_fingerprint
  let site t = t.site
  let binding_site t = t.origin.binding_site
  let member_path t = t.origin.member_path
  let origin t = t.origin
end

let use_site t span ~checkpoint =
  Diagnostic.Site.create t.source ~span ~checkpoint
  |> Result.map_error ~f:(fun error -> Diagnostic.create (Error.to_string_hum error))
;;

let resolve t ~name ~reason ~span ~checkpoint =
  let open Result.Let_syntax in
  let%bind use = use_site t span ~checkpoint in
  match Map.find t.bindings name with
  | None ->
    let%bind related =
      match t.selected_at with
      | None -> Ok []
      | Some (source, location) ->
        let%map site = site source location ~checkpoint in
        [ Diagnostic.Related.create site ~message:"Configured scope selected here" ]
    in
    Error
      (Diagnostic.create ~site:use ~related ("Unknown configured module '" ^ name ^ "'"))
  | Some binding ->
    let%map binding_site = site binding.source binding.location ~checkpoint in
    let origin : Origin.t =
      { owner = t.owner
      ; source_id = Captured.id t.source
      ; source_digest = Captured.digest t.source
      ; phase = t.phase
      ; input_fingerprint = t.input_fingerprint
      ; dependency = binding.dependency
      ; name
      ; member_path = []
      ; binding_site
      }
    in
    { Reference.origin; reason; site = use }
;;

let at_use t (origin : Origin.t) ~reason ~span ~checkpoint =
  checkpoint ();
  if
    not
      (Prepared.Module_id.equal t.owner origin.owner
       && Captured.Id.equal (Captured.id t.source) origin.source_id
       && Captured.Digest.equal (Captured.digest t.source) origin.source_digest
       && Captured.Digest.equal t.input_fingerprint origin.input_fingerprint
       && Phase.equal t.phase origin.phase)
  then
    error "Configured binding belongs to another admitted input, source, owner or phase"
  else
    Result.map (use_site t span ~checkpoint) ~f:(fun site ->
      { Reference.origin; reason; site })
;;
