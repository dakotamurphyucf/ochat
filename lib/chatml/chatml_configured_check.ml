open Core
module Lookup = Chatml_configured_lookup
module Reference = Lookup.Reference
module Parse = Chatml.Chatml_parse
module Checker = Chatml_typechecker
module Captured = Chatml_source
module Diagnostic = Chatml_diagnostic
module Public_module = Checker.Configured_resolution.Public_module

module Checked_unit = struct
  type t =
    { annotations : Checker.checked_program
    ; exports : Public_module.t
    }

  let annotations t = t.annotations
  let exports t = t.exports
end

type t =
  { machine : Checker.infer_state
  ; surface : Chatml.Chatml_builtin_surface.surface
  }

let create surface ~checkpoint =
  { machine = Checker.create_state ~checkpoint (); surface }
;;

let check_unit t ~source ~require unit =
  let captured = Parse.Unit.source unit in
  if
    not
      (Captured.Id.equal (Captured.id captured) (Captured.id (Lookup.source source))
       && Captured.Digest.equal
            (Captured.digest captured)
            (Captured.digest (Lookup.source source))
       && Lookup.Phase.equal (Lookup.phase source) Lookup.Phase.Implementation)
  then
    Error
      (Diagnostic.create
         "Configured parser unit does not match its admitted implementation context")
  else
    (* Real dependency checking is a borrowed compiler callback too. Tag only its
       raised exceptions, so an engine domain exception cannot become a diagnostic.
       Expected Result failures remain the canonical structured error path. *)
    let exception Callback_raised of exn * Stdlib.Printexc.raw_backtrace in
    let guarded_require ~source reference =
      try require ~source reference with
      | exn -> raise (Callback_raised (exn, Stdlib.Printexc.get_raw_backtrace ()))
    in
    try
      Chatml.Chatml_checkpoint.protect t.machine.checkpoint ~f:(fun checkpoint ->
        let state = Checker.for_source t.machine captured in
        let state =
          { state with
            checkpoint
          ; configured =
              Some
                (Checker.Configured_resolution.create
                   source
                   ~require:guarded_require
                   ~checkpoint)
          }
        in
        Checker.with_unit_level state ~f:(fun () ->
          try
            let annotations, exports =
              Checker.infer_configured_unit state t.surface (Parse.Unit.program unit)
            in
            checkpoint ();
            Ok { Checked_unit.annotations; exports }
          with
          | Checker.Type_diagnostic diagnostic -> Error diagnostic
          | Checker.Type_error message -> Error (Diagnostic.create message)
          | Checker.Type_error_with_loc (message, span) ->
            let diagnostic = Diagnostic.create ~span message in
            Error (Chatml.Chatml_owned_source.attach captured diagnostic ~checkpoint)))
    with
    | Callback_raised (exn, backtrace) -> Exn.raise_with_original_backtrace exn backtrace
;;
