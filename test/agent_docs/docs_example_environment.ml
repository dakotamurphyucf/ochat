open! Core

let example () =
  let base = Environment.of_list [ "x", 1; "y", 2 ] in
  let extra = Environment.of_list [ "y", 0; "z", 3 ] in
  let merged = Environment.merge base extra in
  assert (Option.equal Int.equal (Environment.find_opt "x" merged) (Some 1));
  assert (Option.equal Int.equal (Environment.find_opt "y" merged) (Some 2));
  assert (Option.equal Int.equal (Environment.find_opt "z" merged) (Some 3))
;;
