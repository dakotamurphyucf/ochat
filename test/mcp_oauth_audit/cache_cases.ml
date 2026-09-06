open! Core
module Manager = Oauth2_manager
module Token = Oauth2_types.Token

let credentials id secret scope = Manager.Client_secret { id; secret; scope }

let identities =
  [ credentials "one" "secret-a" (Some "read")
  ; credentials "two" "secret-a" (Some "read")
  ; credentials "one" "secret-b" (Some "read")
  ; credentials "one" "secret-a" (Some "write")
  ; credentials "one" "secret-a" None
  ; credentials "one" "secret-a" (Some "")
  ; Manager.Pkce { client_id = "one" }
  ; Manager.Pkce { client_id = "two" }
  ]
;;

let token name =
  { Token.access_token = name
  ; token_type = "Bearer"
  ; expires_in = 3600
  ; refresh_token = None
  ; scope = None
  ; obtained_at = Float.max_finite_value
  }
;;

let key_dimensions _env =
  let paths =
    List.concat_map [ "https://one"; "https://two" ] ~f:(fun issuer ->
      List.map identities ~f:(Manager.cache_file issuer))
  in
  Fixture.check
    (List.length (List.dedup_and_sort paths ~compare:String.compare) = 16)
    "cache identity collision";
  List.iter paths ~f:(fun path ->
    Fixture.check
      (not (String.is_substring path ~substring:"secret-a"))
      "credential exposed in filename")
;;

let storage_isolation env =
  Fixture.with_cache env (fun _ ->
    Eio.Switch.run (fun sw ->
      let issuer = "https://unused.invalid" in
      List.iteri identities ~f:(fun i creds ->
        Manager.store ~env issuer creds (token (Int.to_string i)));
      List.iteri identities ~f:(fun i creds ->
        let cached = Manager.get ~env ~sw ~issuer creds |> Result.ok_or_failwith in
        Fixture.check
          (String.equal cached.access_token (Int.to_string i))
          "another identity's token reused")))
;;

let acquisitions env =
  Fixture.with_cache env (fun _ ->
    let calls = ref 0 in
    Loopback.with_server
      env
      (fun _ ->
         incr calls;
         Loopback.json
           (sprintf
              {|{"access_token":"token-%d","token_type":"Bearer","expires_in":3600}|}
              !calls))
      (fun sw issuer ->
         let selected = List.take identities 6 in
         List.iter selected ~f:(fun creds ->
           ignore (Manager.get ~env ~sw ~issuer creds |> Result.ok_or_failwith : Token.t));
         Fixture.check (!calls = 6) "different credentials reused cached token";
         List.iteri selected ~f:(fun i creds ->
           let result = Manager.get ~env ~sw ~issuer creds |> Result.ok_or_failwith in
           Fixture.check
             (String.equal result.access_token (sprintf "token-%d" (i + 1)))
             "incorrect cached acquisition");
         Fixture.check (!calls = 6) "matching credentials not cached"))
;;

let old_cache_ignored env =
  Fixture.with_cache env (fun _ ->
    let creds = List.hd_exn identities in
    Loopback.with_server
      env
      (fun _ -> Loopback.json Oauth_cases.token_body)
      (fun sw issuer ->
         let fs = Eio.Stdenv.fs env in
         let root = Eio.Path.(fs / Manager.cache_dir ()) in
         Eio.Path.mkdirs ~exists_ok:true ~perm:0o700 root;
         let old = Eio.Path.(root / (Md5.to_hex (Md5.digest_string issuer) ^ ".json")) in
         Eio.Path.save
           ~create:(`Exclusive 0o600)
           old
           (Jsonaf.to_string (Token.jsonaf_of_t (token "wrong-old-identity")));
         let result = Manager.get ~env ~sw ~issuer creds |> Result.ok_or_failwith in
         Fixture.check
           (String.equal result.access_token "loopback-token")
           "unbound legacy token imported";
         Fixture.check (Eio.Path.is_file old) "old cache destructively removed"))
;;

let concurrent_publish env =
  Fixture.with_cache env (fun _ ->
    let issuer = "https://unused.invalid" in
    let creds = List.hd_exn identities in
    Eio.Fiber.List.iter
      (fun i -> Manager.store ~env issuer creds (token (Int.to_string i)))
      (List.init 20 ~f:Fn.id);
    let result = Manager.load ~env issuer creds |> Result.ok_or_failwith in
    Fixture.check (Option.is_some (Int.of_string_opt result.access_token)) "torn JSON";
    let root = Eio.Path.(Eio.Stdenv.fs env / Manager.cache_dir ()) in
    Fixture.check (List.length (Eio.Path.read_dir root) = 1) "temporary cache leak";
    let stat =
      Eio.Path.stat
        ~follow:true
        Eio.Path.(Eio.Stdenv.fs env / Manager.cache_file issuer creds)
    in
    Fixture.check (stat.perm land 0o077 = 0) "cache is not private")
;;

let cases =
  [ "oauth.cache-key-dimensions", key_dimensions
  ; "oauth.cache-storage-identity", storage_isolation
  ; "oauth.cache-distinct-acquisition", acquisitions
  ; "oauth.cache-legacy-ignored", old_cache_ignored
  ; "oauth.cache-concurrent-publication", concurrent_publish
  ]
;;
