open Core
module Contract = Chatml.Chatml_compiler_contract
module Target = Contract.Target

exception Limit_exceeded

module Entry = struct
  type t =
    { contract : Contract.t
    ; identity_bytes : int
    ; fingerprint : Chatml_source.Digest.t
    }

  let contract t = t.contract
  let identity_bytes t = t.identity_bytes
  let fingerprint t = t.fingerprint
end

type t =
  { contracts : (Target.t * Entry.t) list
  ; max_identity_bytes : int
  ; max_work_steps : int
  }

module Error = struct
  type t =
    | Invalid_limits
    | Contract_limit
  [@@deriving sexp_of]
end

let max_member_bytes = 16 * 1024 * 1024
let max_total_bytes = 40 * 1024 * 1024
let max_total_work = 1_000_000

let create ~targets ~max_identity_bytes ~max_work_steps ~checkpoint =
  if
    max_identity_bytes <= 0
    || max_identity_bytes > max_total_bytes
    || max_work_steps <= 0
    || max_work_steps > max_total_work
  then Error Error.Invalid_limits
  else (
    let remaining_bytes = ref max_identity_bytes in
    let work = ref 0 in
    let poll () =
      checkpoint ();
      if !work >= max_work_steps then raise Limit_exceeded;
      Int.incr work
    in
    let convert contracts target =
      poll ();
      if List.exists contracts ~f:(fun (old, _) -> Target.equal old target)
      then Ok contracts
      else (
        let selected_contract = Contract.for_target target ~checkpoint:poll in
        let member_bytes = ref max_member_bytes in
        let allocate bytes =
          if bytes < 0 || bytes > !member_bytes || bytes > !remaining_bytes
          then raise Limit_exceeded;
          member_bytes := !member_bytes - bytes;
          remaining_bytes := !remaining_bytes - bytes
        in
        let summary =
          Contract.summarize_identity selected_contract ~checkpoint:poll ~allocate
        in
        let encoded = Contract.Identity_summary.encoded_bytes summary in
        if encoded > max_member_bytes || encoded > max_identity_bytes
        then Error Error.Contract_limit
        else (
          let selected_fingerprint = Contract.Identity_summary.fingerprint summary in
          Ok
            (( target
             , Entry.
                 { contract = selected_contract
                 ; identity_bytes = encoded
                 ; fingerprint = selected_fingerprint
                 } )
             :: contracts)))
    in
    try
      List.fold_result targets ~init:[] ~f:convert
      |> Result.map ~f:(fun reversed ->
        { contracts = List.rev reversed; max_identity_bytes; max_work_steps })
    with
    | Limit_exceeded -> Error Error.Contract_limit)
;;

let find t target =
  List.find_map t.contracts ~f:(fun (key, entry) ->
    if Target.equal key target then Some entry else None)
;;

let max_identity_bytes t = t.max_identity_bytes
let max_work_steps t = t.max_work_steps
