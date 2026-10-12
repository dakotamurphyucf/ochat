open Core
module B = Chatml_project_build
module S = Chatml_source
module R = Chatml_prepared_program

let restore ?(limits = Support.limits ()) ?(checkpoint = ignore) fixture source sources =
  B.restore source ~sources ~contract:(Fixtures.contract fixture) ~limits ~checkpoint
;;

let%test_unit "canonical and noncanonical lock restore retain exact owned bytes" =
  let fixture = Fixtures.make () in
  let original = Fixtures.compose fixture |> Support.ok in
  let sources = Support.without_lock original in
  let restored =
    restore fixture (B.lock_source original) (List.rev sources) |> Support.ok
  in
  Fixtures.assert_model fixture restored;
  Support.assert_same_source (B.lock_source original) (B.lock_source restored);
  assert (
    S.Digest.equal
      (R.fingerprint (B.prepared original))
      (R.fingerprint (B.prepared restored)));
  let wire = " \n" ^ S.contents (B.lock_source original) ^ "\n " in
  let noncanonical = Support.capture ~namespace:"root" "selected.lock.json" wire in
  let actual = restore fixture noncanonical sources |> Support.ok in
  Fixtures.assert_model fixture actual;
  Support.assert_same_source noncanonical (B.lock_source actual);
  Support.assert_same_source
    noncanonical
    (R.find_source (B.prepared actual) (S.id noncanonical) |> Option.value_exn);
  assert (
    not
      (S.Digest.equal
         (R.fingerprint (B.prepared original))
         (R.fingerprint (B.prepared actual))));
  let canonical lock =
    Chatml_project.Lock.to_string
      lock
      ~limits:Document_schema.Limits.default
      ~checkpoint:ignore
    |> function
    | Ok text -> text
    | Error _ -> failwith "valid receipt encoding rejected"
  in
  [%test_eq: string] (canonical (B.lock original)) (canonical (B.lock actual))
;;

let%test_unit "restoration rejects missing extra duplicate and changed actual captures" =
  let fixture = Fixtures.make () in
  let build = Fixtures.compose fixture |> Support.ok in
  let lock = B.lock_source build in
  let sources = Support.without_lock build in
  let util = Fixtures.util fixture in
  let without_util =
    List.filter sources ~f:(fun source -> not (S.Id.equal (S.id source) (S.id util)))
  in
  let missing = restore fixture lock without_util |> Support.reject in
  (match B.Error.problem missing with
   | Missing_capture id -> assert (S.Id.equal id (S.id util))
   | _ -> failwith "missing source classification");
  let duplicate = restore fixture lock (util :: sources) |> Support.reject in
  (match B.Error.problem duplicate with
   | Duplicate_capture id -> assert (S.Id.equal id (S.id util))
   | _ -> failwith "duplicate source classification");
  let changed = Support.capture ~namespace:"lib" "src/util.chatml" "let value = 9" in
  let supplied =
    List.map sources ~f:(fun source ->
      if S.Id.equal (S.id source) (S.id util) then changed else source)
  in
  let mismatch = restore fixture lock supplied |> Support.reject in
  (match B.Error.problem mismatch with
   | Lock_mismatch -> ()
   | _ -> failwith "changed byte classification");
  ignore
    (restore
       fixture
       lock
       (Support.capture ~namespace:"lib" "extra.chatml" "extra" :: sources)
     |> Support.reject
     : B.Error.t);
  let own_lock = restore fixture lock (lock :: sources) |> Support.reject in
  (match B.Error.problem own_lock with
   | Invalid_lock_identity -> ()
   | _ -> failwith "lock identity classification");
  let changed_receipt =
    match Jsonaf.of_string (S.contents lock) with
    | `Object fields ->
      `Object
        (List.map fields ~f:(fun (name, value) ->
           if String.equal name "required_tools" then name, `Array [] else name, value))
    | _ -> failwith "fixture lock is not object"
  in
  let tampered =
    Support.capture
      ~namespace:"root"
      "selected.lock.json"
      (Jsonaf.to_string changed_receipt)
  in
  ignore (restore fixture tampered sources |> Support.reject : B.Error.t);
  Fixtures.assert_model fixture (restore fixture lock sources |> Support.ok)
;;

let%test_unit "restoration of explicit host entry preserves its supplied identity" =
  let fixture = Fixtures.make ~library:true () in
  let host = Support.capture ~namespace:"host" "src/helper.chatml" "let main = 4" in
  let original = Fixtures.compose ~entry:(Captured host) fixture |> Support.ok in
  let restored =
    restore fixture (B.lock_source original) (Support.without_lock original) |> Support.ok
  in
  Support.assert_same_source
    host
    (R.find_source (B.prepared restored) (S.id host) |> Option.value_exn);
  [%test_eq: (string * string * string) list]
    (Fixtures.scope_targets (B.prepared original) (S.id host))
    (Fixtures.scope_targets (B.prepared restored) (S.id host));
  assert (
    S.Digest.equal
      (R.fingerprint (B.prepared original))
      (R.fingerprint (B.prepared restored)))
;;
