open! Core
module Linked = Chatml_linked_program

let%test_unit "real configured companion admits independent JSON callback result schemes" =
  let contract = Support.native_contract () in
  let make entry =
    Support.input
      contract
      ~interfaces:
        [ "decoder.chatml", "val decode : fun(json, fun(string) -> 'a) -> 'a", [] ]
      [ "entry.chatml", entry, [ "Decoder", "decoder.chatml" ]
      ; "decoder.chatml", "let decode value f = f(Json.tag(value))", []
      ]
  in
  ignore
    (Support.compiled
       contract
       (make
          {|let number = Decoder.decode(`Null, fun tag -> 7)
            let text = Decoder.decode(`Null, fun tag -> "checked")|})
     : Linked.t);
  let invalid = make {|let number = Decoder.decode(`Null, fun tag -> 7, true)|} in
  match
    Linked.compile invalid ~contract ~max_dependency_depth:16 ~checkpoint:(fun () -> ())
  with
  | Ok _ -> assert false
  | Error failure ->
    assert (
      String.equal (Support.owner (Linked.Failure.diagnostic failure)) "entry.chatml")
;;
