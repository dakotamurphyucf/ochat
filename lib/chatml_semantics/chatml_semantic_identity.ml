module Captured = Chatml_source
module Prepared = Chatml_prepared_program

type t =
  { input_fingerprint : Captured.Digest.t
  ; required_contract : Captured.Digest.t
  ; selected_contract : Captured.Digest.t
  ; environment : Prepared.Environment.t
  ; entry : Captured.Id.t
  }

let create input ~selected_contract =
  { input_fingerprint = Prepared.fingerprint input
  ; required_contract = Prepared.compiler_contract input
  ; selected_contract
  ; environment = Prepared.environment input
  ; entry = Prepared.entry input
  }
;;

let input_fingerprint t = t.input_fingerprint
let required_contract t = t.required_contract
let selected_contract t = t.selected_contract
let environment t = t.environment
let entry t = t.entry
let semantic_format _ = "chatml-semantics.v1"
