open! Core
module Ledger = Chatml_authoring_context
module Parsed = Prompt.Chat_markdown.Parsed
module Declaration = Prompt.Chat_markdown.Declaration
module Project_capture = Authoring_source_host.Document_context.Project_capture
module Build = Chatml_project_build
module Prepared = Chatml_prepared_program
module Contract = Chatml.Chatml_compiler_contract
module Digest = Chatml_source.Digest

module Limits = struct
  type t =
    { max_identity_bytes : int
    ; max_work_steps : int
    }

  let create ~max_identity_bytes ~max_work_steps =
    if
      max_identity_bytes <= 0
      || max_identity_bytes > 8 * 1024 * 1024
      || max_work_steps <= 0
      || max_work_steps > 1_000_000
    then Or_error.error_string "invalid selected-script contract admission limits"
    else Ok { max_identity_bytes; max_work_steps }
  ;;
end

module Error = struct
  type t =
    | Identity_limit
    | Work_limit
    | Contract_mismatch
  [@@deriving sexp_of]
end

exception Stop of Error.t

module Selected = struct
  type t =
    { build : Build.t
    ; contract : Contract.t
    ; prepared_fingerprint : Digest.t
    ; compiler_contract : Digest.t
    }

  let admit ~build ~contract ~limits ~checkpoint =
    let work = ref 0 in
    let allocation = ref 0 in
    let poll () =
      if !work >= limits.Limits.max_work_steps then raise (Stop Error.Work_limit);
      incr work;
      checkpoint ()
    in
    let allocate bytes =
      if bytes < 0 || bytes > limits.Limits.max_identity_bytes - !allocation
      then raise (Stop Error.Identity_limit);
      allocation := !allocation + bytes
    in
    try
      let authentic =
        match Contract.kind contract with
        | Contract.Kind.Standard target -> Contract.for_target target ~checkpoint:poll
        | Contract.Kind.Native -> contract
      in
      let length = Contract.input_bytes authentic ~checkpoint:poll ~allocate in
      if length > limits.max_identity_bytes then raise (Stop Error.Identity_limit);
      let compiler_contract = Contract.fingerprint authentic ~checkpoint:poll in
      let prepared = Build.prepared build in
      if
        (not (Digest.equal compiler_contract (Prepared.compiler_contract prepared)))
        || not
             (Digest.equal
                compiler_contract
                (Prepared.Environment.required_contract (Prepared.environment prepared)))
      then Error Error.Contract_mismatch
      else
        Ok
          { build
          ; contract = authentic
          ; prepared_fingerprint = Prepared.fingerprint prepared
          ; compiler_contract
          }
    with
    | Stop error -> Error error
  ;;

  let build t = t.build
  let contract t = t.contract
  let prepared_fingerprint t = t.prepared_fingerprint
  let compiler_contract t = t.compiler_contract

  let equal_identity left right =
    Digest.equal left.prepared_fingerprint right.prepared_fingerprint
    && Digest.equal left.compiler_contract right.compiler_contract
  ;;
end

module Policy = struct
  type t =
    { select_script :
        original_actor:Operator_authorization.t
        -> actor:Operator_authorization.t
        -> current:Ledger.Document.Inputs.t
        -> parsed:Parsed.t
        -> declaration:Declaration.t
        -> metadata:Project_capture.t option
        -> Selected.t option
    }

  let create ~select_script = { select_script }

  let select t ~original_actor ~actor ~current ~parsed ~declaration ~metadata =
    t.select_script ~original_actor ~actor ~current ~parsed ~declaration ~metadata
  ;;

  let same_now t ~original_actor ~actor ~current ~parsed ~declaration ~metadata ~selected =
    Option.exists
      (select t ~original_actor ~actor ~current ~parsed ~declaration ~metadata)
      ~f:(Selected.equal_identity selected)
  ;;
end
