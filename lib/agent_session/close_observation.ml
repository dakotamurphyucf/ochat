open! Core

type 'a exit =
  | Returned of 'a
  | Raised of exn * Stdlib.Printexc.raw_backtrace

type 'a t =
  { exit : 'a exit
  ; diagnostics : Cleanup_diagnostic.t list
  ; primary : Cleanup_diagnostic.t option
  }

let create exit ~diagnostics ?primary () =
  let diagnostics =
    List.fold diagnostics ~init:[] ~f:(fun reversed diagnostic ->
      if List.mem reversed diagnostic ~equal:Cleanup_diagnostic.equal
      then reversed
      else diagnostic :: reversed)
    |> List.rev
  in
  if
    Option.exists primary ~f:(fun primary ->
      not (List.mem diagnostics primary ~equal:Cleanup_diagnostic.equal))
  then invalid_arg "cleanup close primary is not an actual report group";
  { exit; diagnostics; primary }
;;

let exit t = t.exit
let diagnostics t = t.diagnostics
let primary t = t.primary

let resume t =
  match t.exit with
  | Returned result -> result
  | Raised (exn, backtrace) -> Exn.raise_with_original_backtrace exn backtrace
;;
