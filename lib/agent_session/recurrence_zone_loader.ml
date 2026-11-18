open Core
module Zone = Recurrence_rules.Zone
module Data = Recurrence_zone_data
module Local = Chatml_local_source

module Error = struct
  type t =
    | Invalid of Recurrence_rules.Timing_error.t
    | Filesystem of Local.Error.t
    | Data of Data.Error.t
  [@@deriving sexp_of]
end

let validate ~source ~name ~from ~until =
  let open Result.Let_syntax in
  let invalid error = Error.Invalid error in
  let%bind () = Zone.validate_name name |> Result.map_error ~f:invalid in
  (* Reuse the actual source-label contract. This validation-only value has a
     known-valid digest, is discarded, and is never returned as byte provenance. *)
  let%bind (_ : Zone.Provenance.t) =
    Zone.Provenance.create ~source ~sha256:(String.make 64 '0')
    |> Result.map_error ~f:invalid
  in
  if Time_ns.compare from until > 0
  then Error (Error.Invalid (Invalid "coverage endpoints must be ordered"))
  else
    Source.Portable_path.create name
    |> Result.map_error ~f:(fun _ ->
      Error.Invalid (Invalid "zone name is not a portable source member"))
;;

let poll checkpoint =
  Eio.Fiber.check ();
  checkpoint ();
  Eio.Fiber.check ()
;;

let decode bytes ~source ~name ~from ~until ~checkpoint =
  let open Result.Let_syntax in
  poll checkpoint;
  let%bind data =
    Data.of_string bytes ~source |> Result.map_error ~f:(fun e -> Error.Data e)
  in
  let%bind zone =
    Data.zone data ~name ~from ~until |> Result.map_error ~f:(fun e -> Error.Data e)
  in
  poll checkpoint;
  Ok zone
;;

let load_local owner ~source ~name ~from ~until ~checkpoint =
  let open Result.Let_syntax in
  let%bind member = validate ~source ~name ~from ~until in
  let%bind bytes =
    Local.read owner ~member ~max_bytes:Data.maximum_bytes ~checkpoint
    |> Result.map_error ~f:(fun error -> Error.Filesystem error)
  in
  decode bytes ~source ~name ~from ~until ~checkpoint
;;

let load root ~source ~name ~from ~until ~checkpoint =
  let open Result.Let_syntax in
  let%bind member = validate ~source ~name ~from ~until in
  let max_path_components = String.count name ~f:(Char.equal '/') + 1 in
  let%bind bytes =
    Local.with_root root ~max_path_components ~checkpoint ~f:(fun owner ->
      Local.read owner ~member ~max_bytes:Data.maximum_bytes ~checkpoint)
    |> Result.map_error ~f:(function `Filesystem error | `Callback error ->
        Error.Filesystem error)
  in
  (* The native bracket has joined and closed before pure data processing. *)
  decode bytes ~source ~name ~from ~until ~checkpoint
;;
