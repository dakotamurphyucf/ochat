open Core

type t = Chatml_source.t

let site source span ~checkpoint =
  Chatml_diagnostic.Site.create source ~span ~checkpoint |> Result.ok
;;

let attach source diagnostic ~checkpoint =
  match diagnostic.Chatml_diagnostic.site with
  | Some _ -> diagnostic
  | None ->
    let fallback =
      Option.bind diagnostic.span ~f:(fun span -> site source span ~checkpoint)
    in
    (match fallback with
     | Some _ -> Chatml_diagnostic.with_fallback diagnostic fallback
     | None ->
       (* A known captured owner cannot validate this span. Keep the message and
          related sites, but do not let a later legacy text adapter guess it. *)
       Chatml_diagnostic.create ~related:diagnostic.related diagnostic.message)
;;
