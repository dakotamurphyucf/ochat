open! Core

let example () =
  let src = Source.make "Hello world" in
  let point offset = Source.{ line = 1; column = offset; offset } in
  let first = Source.{ left = point 0; right = point 5 } in
  let second = Source.{ left = point 5; right = point 11 } in
  assert (Option.equal Char.equal (Source.at src 0) (Some 'H'));
  assert (Option.is_none (Source.at src 1_000_000));
  assert (String.equal (Source.read src first) "Hello");
  assert (String.equal (Source.read src (Source.merge second first)) "Hello world")
;;
