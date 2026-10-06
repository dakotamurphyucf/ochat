(** Design evidence only: inspect the installed JSON PPX, not a provider codec. *)
open! Core

open! Jsonaf.Export

module Optional = struct
  type t = { field : string option [@jsonaf.option] } [@@deriving jsonaf]
end

module Nullable = struct
  type t = { field : string option } [@@deriving jsonaf]
end

module Required = struct
  type t = { field : string } [@@deriving jsonaf]
end

module Extra = struct
  type t = { field : string } [@@deriving jsonaf] [@@jsonaf.allow_extra_fields]
end

let cases = [ "{}"; {|{"field":null}|}; {|{"field":"value"}|} ]

let show ~decode ~encode input =
  let result =
    Result.try_with (fun () ->
      Jsonaf.of_string input |> decode |> encode |> Jsonaf.to_string)
  in
  match result with
  | Ok encoded -> printf "%s -> %s\n" input encoded
  | Error _ -> printf "%s -> decode_error\n" input
;;

let%expect_test "optional nonnullable generated field presence" =
  List.iter cases ~f:(show ~decode:Optional.t_of_jsonaf ~encode:Optional.jsonaf_of_t);
  [%expect
    {|
    {} -> {}
    {"field":null} -> decode_error
    {"field":"value"} -> {"field":"value"}
  |}]
;;

let%expect_test "required nullable generated field presence" =
  List.iter cases ~f:(show ~decode:Nullable.t_of_jsonaf ~encode:Nullable.jsonaf_of_t);
  [%expect
    {|
    {} -> decode_error
    {"field":null} -> {"field":null}
    {"field":"value"} -> {"field":"value"}
  |}]
;;

let%expect_test "required nonnullable generated field presence" =
  List.iter cases ~f:(show ~decode:Required.t_of_jsonaf ~encode:Required.jsonaf_of_t);
  [%expect
    {|
    {} -> decode_error
    {"field":null} -> decode_error
    {"field":"value"} -> {"field":"value"}
  |}]
;;

let%expect_test "allow_extra_fields is not extension preservation" =
  show
    ~decode:Extra.t_of_jsonaf
    ~encode:Extra.jsonaf_of_t
    {|{"field":"value","future":7}|};
  [%expect {| {"field":"value","future":7} -> {"field":"value"} |}]
;;

let%expect_test "decode state independently of encoder" =
  List.iter cases ~f:(fun input ->
    match Result.try_with (fun () -> Jsonaf.of_string input |> Optional.t_of_jsonaf) with
    | Ok { field } -> print_s [%sexp (field : string option)]
    | Error _ -> print_s [%sexp "decode_error"]);
  [%expect
    {|
    ()
    decode_error
    (value)
  |}]
;;

let%expect_test "exact encoding independently of decoder" =
  printf "%s\n" (Optional.jsonaf_of_t { field = None } |> Jsonaf.to_string);
  printf "%s\n" (Nullable.jsonaf_of_t { field = None } |> Jsonaf.to_string);
  printf "%s\n" (Required.jsonaf_of_t { field = "value" } |> Jsonaf.to_string);
  [%expect
    {|
    {}
    {"field":null}
    {"field":"value"}
  |}]
;;
