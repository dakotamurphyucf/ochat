open Core
module Captured = Chatml_source
module Prepared = Chatml_prepared_program
module Build = Chatml_project_build
module Contract = Chatml.Chatml_compiler_contract
module Static = Chatml.Chatml_static_surface
module Limits = Fallback_model.Limits
module Error = Fallback_model.Error
module Work = Fallback_model.Work

module Name = struct
  type kind =
    | Global
    | Module

  type t =
    { text : string
    ; kind : kind
    }
end

module Selected_contract = struct
  type t =
    { prepared : Prepared.t
    ; names : Name.t list
    }

  let prepared t = t.prepared
  let names t = t.names

  exception Identity_limit

  let admit build ~selected_contract ~limits ~checkpoint =
    let work = Work.create ~max_steps:limits.Limits.max_contract_work_steps ~checkpoint in
    let poll () = Work.charge work 1 in
    try
      poll ();
      let contract =
        match Contract.kind selected_contract with
        | Contract.Kind.Standard target -> Contract.for_target target ~checkpoint:poll
        | Native -> selected_contract
      in
      let allocated = ref 0 in
      let allocate bytes =
        if bytes < 0 || bytes > limits.max_contract_identity_bytes - !allocated
        then raise Identity_limit;
        allocated := !allocated + bytes
      in
      let actual_bytes = Contract.input_bytes contract ~checkpoint:poll ~allocate in
      if actual_bytes > limits.max_contract_identity_bytes then raise Identity_limit;
      let fingerprint = Contract.fingerprint contract ~checkpoint:poll in
      let prepared = Build.prepared build in
      if
        (not (Captured.Digest.equal fingerprint (Prepared.compiler_contract prepared)))
        || not
             (Captured.Digest.equal
                fingerprint
                (Prepared.Environment.required_contract (Prepared.environment prepared)))
      then Error Error.Contract_mismatch
      else (
        let surface = Contract.static_surface contract in
        let names_rev = ref [] in
        List.iter (Static.globals surface) ~f:(fun (binding : Static.Binding.t) ->
          Work.charge_string work binding.name;
          Work.charge work 1;
          names_rev := Name.{ text = binding.name; kind = Global } :: !names_rev);
        List.iter (Static.modules surface) ~f:(fun (module_ : Static.Module.t) ->
          Work.charge_string work module_.name;
          Work.charge work 1;
          names_rev := Name.{ text = module_.name; kind = Module } :: !names_rev);
        Ok { prepared; names = List.rev !names_rev })
    with
    | Identity_limit -> Error Error.Contract_identity_limit
    | Work.Limit -> Error Error.Contract_work_limit
  ;;
end
