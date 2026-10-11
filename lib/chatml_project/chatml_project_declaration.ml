open Core

module Declared_at = struct
  type t =
    { source : Chatml_source.t
    ; location : Chatml_source.Location.t
    ; field_path : string list
    }

  let create source ~field_path ~checkpoint =
    let location =
      Chatml_source.locate
        source
        ~start_byte:0
        ~end_byte:(Chatml_source.byte_length source)
        ~checkpoint
      |> Or_error.ok_exn
    in
    { source; location; field_path }
  ;;

  let source t = t.source
  let with_field_path t ~field_path = { t with field_path }
  let location t = t.location
  let field_path t = t.field_path
end

module Error = struct
  type t =
    { source : Chatml_source.t
    ; details : Document_schema.Error.t
    ; field_path : string list
    }

  let create source ~details ~field_path = { source; details; field_path }
  let source t = t.source
  let details t = t.details
  let field_path t = t.field_path
end

module Git_pin = struct
  type object_format =
    | Sha1
    | Sha256
  [@@deriving equal, sexp_of]

  type t =
    { object_format : object_format
    ; commit : string
    }
  [@@deriving equal, sexp_of]

  let create ~object_format ~commit =
    let length =
      match object_format with
      | Sha1 -> 40
      | Sha256 -> 64
    in
    if
      String.length commit = length
      && String.for_all commit ~f:(function
        | '0' .. '9' | 'a' .. 'f' -> true
        | _ -> false)
    then Ok { object_format; commit }
    else
      Or_error.error_string
        "expected a full lowercase commit for the selected object format"
  ;;

  let object_format t = t.object_format
  let commit t = t.commit
end

module Dependency = struct
  type source =
    | Local of { directory : string }
    | Git of
        { repository : string
        ; pin : Git_pin.t
        }

  type t =
    { name : string
    ; source : source
    ; project : Source.Portable_path.t
    ; environment : string option
    ; namespace_override : string option
    ; declared_at : Declared_at.t
    }

  let is_valid_locator string ~checkpoint =
    checkpoint ();
    (not (String.is_empty string))
    && String.for_alli string ~f:(fun index char ->
      if index mod 4096 = 0 then checkpoint ();
      match char with
      | '\000' | '\r' | '\n' -> false
      | _ -> true)
  ;;

  let name t = t.name
  let source t = t.source
  let project t = t.project
  let environment t = t.environment
  let namespace_override t = t.namespace_override
  let declared_at t = t.declared_at
end

module Module_ref = struct
  type catalog =
    | Self
    | Dependency of string
  [@@deriving equal, sexp_of]

  type t =
    { catalog : catalog
    ; name : string
    }
  [@@deriving equal, sexp_of]

  let is_valid_name name ~checkpoint =
    checkpoint ();
    (not (String.is_empty name))
    && (match name.[0] with
        | 'A' .. 'Z' -> true
        | _ -> false)
    && String.for_alli name ~f:(fun index char ->
      if index mod 4096 = 0 then checkpoint ();
      match char with
      | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '_' -> true
      | _ -> false)
  ;;

  let catalog t = t.catalog
  let name t = t.name
end

module Source_dir = struct
  type directory =
    | Root
    | Relative of Source.Portable_path.t

  type t =
    { directory : directory
    ; declared_at : Declared_at.t
    }

  let directory t = t.directory
  let declared_at t = t.declared_at
end

module Module_override = struct
  type interface_selection =
    | Companion
    | Explicit of Source.Portable_path.t

  type t =
    { name : string
    ; implementation : Source.Portable_path.t
    ; interface_selection : interface_selection
    ; declared_at : Declared_at.t
    }

  let name t = t.name
  let implementation t = t.implementation
  let interface_selection t = t.interface_selection
  let declared_at t = t.declared_at
end

module Compiler_requirement = struct
  type t =
    { compiler_family : string option
    ; allowed_targets : string list option
    ; exact_contract : Chatml_source.Digest.t option
    ; declared_at : Declared_at.t option
    }

  let compiler_family t = t.compiler_family
  let allowed_targets t = t.allowed_targets
  let exact_contract t = t.exact_contract
  let declared_at t = t.declared_at

  let unconstrained =
    { compiler_family = None
    ; allowed_targets = None
    ; exact_contract = None
    ; declared_at = None
    }
  ;;
end

module Scope_spec = struct
  type binding =
    { name : string
    ; module_ref : Module_ref.t
    ; declared_at : Declared_at.t
    }

  type t =
    { name : string
    ; bindings : binding list option
    ; compiler_requirement : Compiler_requirement.t
    ; declared_at : Declared_at.t option
    }

  let name t = t.name
  let bindings t = t.bindings
  let compiler_requirement t = t.compiler_requirement
  let declared_at t = t.declared_at

  let implicit_default =
    { name = "default"
    ; bindings = None
    ; compiler_requirement = Compiler_requirement.unconstrained
    ; declared_at = None
    }
  ;;
end

module Entry = struct
  type t =
    { name : string
    ; source : Source.Portable_path.t
    ; target_name : string
    ; declared_at : Declared_at.t
    }

  let name t = t.name
  let source t = t.source
  let target_name t = t.target_name
  let declared_at t = t.declared_at
end

module Selection = struct
  type request =
    | Entry of
        { name : string
        ; environment : string option
        }
    | Library of { environment : string option }

  type t =
    { entry : Entry.t option
    ; environment : Scope_spec.t
    ; file_environments : Scope_spec.t String.Map.t
    }

  let entry t = t.entry
  let environment t = t.environment

  let scope_for t ~source =
    match Map.find t.file_environments (Source.Portable_path.to_string source) with
    | Some environment -> environment
    | None -> t.environment
  ;;
end

type t =
  { source : Chatml_source.t
  ; declared_at : Declared_at.t
  ; namespace : string
  ; source_dirs : Source_dir.t list
  ; exclusions : (Source.Portable_path.t * Declared_at.t) list
  ; module_overrides : Module_override.t list
  ; dependencies : Dependency.t list
  ; required_tools : (string * Declared_at.t) list
  ; environments : Scope_spec.t String.Map.t
  ; default_environment : string option
  ; file_environments : Scope_spec.t String.Map.t
  ; entries : (Entry.t * string option) String.Map.t
  }

let source t = t.source
let declared_at t = t.declared_at
let namespace t = t.namespace
let source_dirs t = t.source_dirs
let exclusions t = t.exclusions
let module_overrides t = t.module_overrides
let dependencies t = t.dependencies
let required_tools t = t.required_tools

let select t ~request ~checkpoint =
  checkpoint ();
  let fail field_path reason =
    Error
      (Error.create
         t.source
         ~details:(Document_schema.Error.Invalid_field { path = field_path; reason })
         ~field_path)
  in
  let open Result.Let_syntax in
  let%bind entry, requested_environment =
    match request with
    | Selection.Library { environment } -> Ok (None, environment)
    | Entry { name; environment } ->
      (match Map.find t.entries name with
       | None -> fail [ "entries" ] "selected entry does not exist"
       | Some (entry, declared_environment) ->
         Ok
           ( Some entry
           , match environment with
             | Some _ -> environment
             | None -> declared_environment ))
  in
  let environment =
    match requested_environment with
    | Some _ -> requested_environment
    | None -> t.default_environment
  in
  match environment with
  | None ->
    fail [ "environments" ] "project has no default environment; select one explicitly"
  | Some name ->
    (match Map.find t.environments name with
     | None -> fail [ "environments" ] "selected environment does not exist"
     | Some environment ->
       Ok { Selection.entry; environment; file_environments = t.file_environments })
;;
