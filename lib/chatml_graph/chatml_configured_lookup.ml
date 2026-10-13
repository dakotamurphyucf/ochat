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

type repl_epoch =
  { witness : unit ref
  ; mutable current : unit ref option
  ; mutable admission : unit ref
  ; mutable closed : bool
  }

type t =
  { owner : Prepared.Module_id.t
  ; source : Captured.t
  ; phase : Phase.t
  ; input_fingerprint : Captured.Digest.t
  ; bindings : binding String.Map.t
  ; selected_at : (Captured.t * Captured.Location.t) option
  ; repl : (repl_epoch * unit ref) option
  }

let owner t = t.owner
let source t = t.source
let phase t = t.phase
let input_fingerprint t = t.input_fingerprint
let error message = Error (Diagnostic.create message)

let current t =
  match t.repl with
  | None -> true
  | Some (epoch, context) ->
    (not epoch.closed)
    && Option.exists epoch.current ~f:(fun current -> phys_equal current context)
;;

let current_or_error t =
  if current t then Ok () else error "REPL configured lookup is closed or stale"
;;

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
    ; repl = None
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
    ; repl_epoch : repl_epoch option
    }

  let dependency t = t.dependency
  let member_path t = t.member_path
  let binding_site t = t.binding_site
  let import_source t = t.source_id, t.source_digest
  let member t ~name = { t with member_path = t.member_path @ [ name ] }
end

module Reference = struct
  type t =
    { origin : Origin.t
    ; reason : Reason.t
    ; site : Diagnostic.Site.t
    ; use_owner : Prepared.Module_id.t
    ; use_source_id : Captured.Id.t
    ; use_source_digest : Captured.Digest.t
    ; use_phase : Phase.t
    ; use_input_fingerprint : Captured.Digest.t
    ; use_repl : (repl_epoch * unit ref) option
    }

  let owner t = t.use_owner
  let dependency t = t.origin.dependency
  let name t = t.origin.name
  let reason t = t.reason
  let phase t = t.use_phase
  let input_fingerprint t = t.use_input_fingerprint
  let site t = t.site
  let binding_site t = t.origin.binding_site
  let member_path t = t.origin.member_path
  let origin t = t.origin
  let use_source t = t.use_source_id, t.use_source_digest
end

let reference t origin ~reason ~site =
  { Reference.origin
  ; reason
  ; site
  ; use_owner = t.owner
  ; use_source_id = Captured.id t.source
  ; use_source_digest = Captured.digest t.source
  ; use_phase = t.phase
  ; use_input_fingerprint = t.input_fingerprint
  ; use_repl = t.repl
  }
;;

let use_site t span ~checkpoint =
  let open Result.Let_syntax in
  let%bind () = current_or_error t in
  Diagnostic.Site.create t.source ~span ~checkpoint
  |> Result.map_error ~f:(fun error -> Diagnostic.create (Error.to_string_hum error))
;;

let resolve t ~name ~reason ~span ~checkpoint =
  let open Result.Let_syntax in
  let%bind use = use_site t span ~checkpoint in
  let%bind () = current_or_error t in
  match Map.find t.bindings name with
  | None ->
    let%bind related =
      match t.selected_at with
      | None -> Ok []
      | Some (source, location) ->
        let%map site = site source location ~checkpoint in
        [ Diagnostic.Related.create site ~message:"Configured scope selected here" ]
    in
    let%bind () = current_or_error t in
    Error
      (Diagnostic.create ~site:use ~related ("Unknown configured module '" ^ name ^ "'"))
  | Some binding ->
    let%bind binding_site = site binding.source binding.location ~checkpoint in
    let%map () = current_or_error t in
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
      ; repl_epoch = Option.map t.repl ~f:fst
      }
    in
    reference t origin ~reason ~site:use
;;

let at_use t (origin : Origin.t) ~reason ~span ~checkpoint =
  checkpoint ();
  let same_input =
    Captured.Digest.equal t.input_fingerprint origin.input_fingerprint
    && Phase.equal t.phase origin.phase
  in
  let same_source =
    Prepared.Module_id.equal t.owner origin.owner
    && Captured.Id.equal (Captured.id t.source) origin.source_id
    && Captured.Digest.equal (Captured.digest t.source) origin.source_digest
  in
  let same_epoch =
    match t.repl, origin.repl_epoch with
    | Some (epoch, _), Some original -> phys_equal epoch.witness original.witness
    | _ -> false
  in
  let same_binding =
    match Map.find t.bindings origin.name with
    | Some binding -> Prepared.Module_id.equal binding.dependency origin.dependency
    | None -> false
  in
  if not (current t)
  then error "REPL configured lookup is closed or stale"
  else if
    not
      (same_input
       &&
       match t.repl, origin.repl_epoch with
       | None, None -> same_source
       | Some _, Some _ -> same_epoch && same_binding
       | _ -> false)
  then
    error "Configured binding belongs to another admitted input, source, owner or phase"
  else
    let open Result.Let_syntax in
    let%bind site = use_site t span ~checkpoint in
    let%map () = current_or_error t in
    reference t origin ~reason ~site
;;

let create_lookup = create

module Repl_scope = struct
  type context = t

  module Limits = struct
    type t =
      { max_snippets : int
      ; max_identity_bytes : int
      }

    let create ~max_snippets ~max_identity_bytes =
      if max_snippets <= 0 || max_identity_bytes <= 0
      then Or_error.error_string "REPL scope limits must be positive"
      else Ok { max_snippets; max_identity_bytes }
    ;;
  end

  module Error = struct
    type t =
      | Invalid_anchor of Diagnostic.t
      | Closed
      | Catalog_source_collision of Captured.Id.t
      | Reused_source of Captured.Id.t
      | Stale_source_revision of Captured.Id.t
      | Foreign_context
      | Stale_context
      | Foreign_reference
      | Snippet_limit
      | Identity_byte_limit
      | Accounting_overflow
    [@@deriving sexp_of]
  end

  type t =
    { prepared : Prepared.t
    ; anchor : context
    ; limits : Limits.t
    ; epoch : repl_epoch
    ; mutable seen :
        (Captured.Id.t, Captured.Digest.t, Captured.Id.comparator_witness) Map.t
    ; mutable identity_bytes : int
    }

  let input_fingerprint t = input_fingerprint t.anchor

  let create prepared ~anchor_owner ~anchor_implementation ~limits ~checkpoint =
    match
      create_lookup prepared ~owner:anchor_owner ~source:anchor_implementation ~checkpoint
    with
    | Error diagnostic -> Error (Error.Invalid_anchor diagnostic)
    | Ok anchor ->
      if not (Phase.equal anchor.phase Phase.Implementation)
      then
        Error
          (Error.Invalid_anchor
             (Diagnostic.create "REPL scope anchor must be an implementation"))
      else
        Ok
          { prepared
          ; anchor
          ; limits
          ; epoch =
              { witness = ref (); current = None; admission = ref (); closed = false }
          ; seen = Map.empty (module Captured.Id)
          ; identity_bytes = 0
          }
  ;;

  let for_submission t source ~checkpoint =
    let open Result.Let_syntax in
    let admitted_at = t.epoch.admission in
    checkpoint ();
    if t.epoch.closed
    then Error Error.Closed
    else if not (phys_equal t.epoch.admission admitted_at)
    then Error Error.Stale_context
    else (
      let id = Captured.id source in
      let namespace_bytes = String.length (Captured.Id.package_namespace id) in
      let path_bytes = String.length (Captured.Id.relative_path id) in
      let%bind bytes =
        if namespace_bytes > Int.max_value - path_bytes
        then Error Error.Accounting_overflow
        else Ok (namespace_bytes + path_bytes)
      in
      let%bind () =
        if bytes > t.limits.max_identity_bytes
        then Error Error.Identity_byte_limit
        else Ok ()
      in
      let%bind () =
        match Prepared.find_source t.prepared id with
        | Some _ -> Error (Error.Catalog_source_collision id)
        | None -> Ok ()
      in
      let%bind () =
        match Map.find t.seen id with
        | None -> Ok ()
        | Some digest ->
          if Captured.Digest.equal digest (Captured.digest source)
          then Error (Error.Reused_source id)
          else Error (Error.Stale_source_revision id)
      in
      let%bind () =
        if Map.length t.seen >= t.limits.max_snippets
        then Error Error.Snippet_limit
        else if t.identity_bytes > Int.max_value - bytes
        then Error Error.Accounting_overflow
        else if t.identity_bytes + bytes > t.limits.max_identity_bytes
        then Error Error.Identity_byte_limit
        else Ok ()
      in
      let seen = Map.set t.seen ~key:id ~data:(Captured.digest source) in
      let current = ref () in
      let context =
        { t.anchor with
          owner = Prepared.Module_id.of_implementation id
        ; source
        ; repl = Some (t.epoch, current)
        }
      in
      checkpoint ();
      if t.epoch.closed
      then Error Error.Closed
      else if not (phys_equal t.epoch.admission admitted_at)
      then Error Error.Stale_context
      else (
        t.seen <- seen;
        t.identity_bytes <- t.identity_bytes + bytes;
        t.epoch.current <- Some current;
        t.epoch.admission <- ref ();
        Ok context))
  ;;

  let validate_reference t ~context reference ~checkpoint =
    checkpoint ();
    if t.epoch.closed
    then Error Error.Closed
    else (
      match context.repl with
      | None -> Error Error.Foreign_context
      | Some (epoch, token) ->
        if not (phys_equal epoch.witness t.epoch.witness)
        then Error Error.Foreign_context
        else if not (current context)
        then Error Error.Stale_context
        else (
          let use_source, use_digest = Reference.use_source reference in
          let same_reference =
            match reference.use_repl with
            | None -> false
            | Some (owner_epoch, owner_token) ->
              phys_equal owner_epoch.witness t.epoch.witness
              && phys_equal owner_token token
              && Prepared.Module_id.equal (Reference.owner reference) context.owner
              && Captured.Id.equal use_source (Captured.id context.source)
              && Captured.Digest.equal use_digest (Captured.digest context.source)
              && Phase.equal (Reference.phase reference) context.phase
              && Captured.Digest.equal
                   (Reference.input_fingerprint reference)
                   context.input_fingerprint
              && Captured.Id.equal
                   (Captured.id (Diagnostic.Site.source reference.site))
                   (Captured.id context.source)
              && Captured.Digest.equal
                   (Captured.digest (Diagnostic.Site.source reference.site))
                   (Captured.digest context.source)
              && (match reference.origin.repl_epoch with
                  | Some origin_epoch -> phys_equal origin_epoch.witness t.epoch.witness
                  | None -> false)
              &&
                (match Map.find context.bindings (Reference.name reference) with
                | None -> false
                | Some binding ->
                  Prepared.Module_id.equal
                    binding.dependency
                    (Reference.dependency reference))
          in
          if not same_reference
          then Error Error.Foreign_reference
          else (
            checkpoint ();
            if t.epoch.closed
            then Error Error.Closed
            else if not (current context)
            then Error Error.Stale_context
            else Ok ())))
  ;;

  let revoke t =
    t.epoch.closed <- true;
    t.epoch.current <- None;
    t.epoch.admission <- ref ()
  ;;
end
