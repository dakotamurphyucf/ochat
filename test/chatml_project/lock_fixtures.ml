(* Handwritten current wire receipt. Expected lengths/digests deliberately do not
   describe any captured files; pure receipt decoding must not pretend they do. *)
let canonical =
  {|{"schema_version":1,"root":{"namespace":"app","path":"chatml.project.json"},"entry":{"source":{"id":{"namespace":"app","path":"a.chatml"},"sha256":"0000000000000000000000000000000000000000000000000000000000000000","byte_length":0},"selection":{"kind":"declared","name":"main"}},"environment":"app-env","compiler":{"family":"intent-family","target":"intent-target","contract_sha256":"0000000000000000000000000000000000000000000000000000000000000000"},"required_tools":["alpha","zeta"],"catalogs":[{"namespace":"app","origin":{"kind":"local"},"declaration":{"namespace":"app","path":"chatml.project.json"},"selected_environment":"app-env","dependencies":[{"name":"shared","declaration":{"namespace":"shared","path":"chatml.project.json"}},{"name":"shared_alias","declaration":{"namespace":"shared","path":"chatml.project.json"}}],"sources":[{"id":{"namespace":"app","path":"a.chatml"},"sha256":"0000000000000000000000000000000000000000000000000000000000000000","byte_length":0},{"id":{"namespace":"app","path":"a.chatmli"},"sha256":"1111111111111111111111111111111111111111111111111111111111111111","byte_length":0},{"id":{"namespace":"app","path":"b.chatml"},"sha256":"0000000000000000000000000000000000000000000000000000000000000000","byte_length":0}],"modules":[{"implementation":{"namespace":"app","path":"a.chatml"},"interface":{"namespace":"app","path":"a.chatmli"}},{"implementation":{"namespace":"app","path":"b.chatml"}}],"scopes":[{"source":{"namespace":"app","path":"a.chatml"},"environment":"app-env","bindings":[{"name":"Alias","module":{"namespace":"shared","path":"remote.chatml"}},{"name":"Remote","module":{"namespace":"shared","path":"remote.chatml"}}]},{"source":{"namespace":"app","path":"b.chatml"},"environment":"none","bindings":[]}]},{"namespace":"shared","origin":{"kind":"local"},"declaration":{"namespace":"shared","path":"chatml.project.json"},"selected_environment":"lib-env","dependencies":[],"sources":[{"id":{"namespace":"shared","path":"remote.chatml"},"sha256":"0000000000000000000000000000000000000000000000000000000000000000","byte_length":0}],"modules":[{"implementation":{"namespace":"shared","path":"remote.chatml"}}],"scopes":[]}]}|}
;;

let minimal =
  {|{"schema_version":1,"root":{"namespace":"app","path":"chatml.project.json"},"entry":{"source":{"id":{"namespace":"app","path":"a.chatml"},"sha256":"0000000000000000000000000000000000000000000000000000000000000000","byte_length":0},"selection":{"kind":"declared","name":"main"}},"environment":"app-env","compiler":{"family":"intent-family","target":"intent-target","contract_sha256":"0000000000000000000000000000000000000000000000000000000000000000"},"required_tools":[],"catalogs":[]}|}
;;

(* Receipt-only Git package prefixes; source identities remain package-relative. *)
let git_root_origin =
  {|{"kind":"git","repository":"https://example.invalid/library.git","object_format":"sha1","commit":"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa","package_root":null}|}
;;

let git_package_origin =
  {|{"kind":"git","repository":"https://example.invalid/library.git","object_format":"sha1","commit":"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa","package_root":"packages/shared"}|}
;;
