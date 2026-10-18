open! Core
module P = Agent_protocol
module B = Chat_response.Background_request
module Child = Developer_child_selection
module Live = Developer_live_resources
module Root = Developer_root_owner

type t =
  { live : Live.live
  ; root : Root.run
  ; runtime_policy : Chat_response.Runtime_semantics.policy
  }

let invalid message = Error (P.Error.invalid_request message)
let ( let* ) result f = Result.bind result ~f

let create ~live ~root ~runtime_policy =
  let* () = Live.check_current live in
  let* _ = Root.current root in
  Ok { live; root; runtime_policy }
;;

let issued_by_root t root = phys_equal t.root root

let runtime_policy_identity ~base ~policy =
  [%sexp
    ("queued-selected-issuer.v3" : string)
  , (Chatml_source.Digest.to_hex base : string)
  , (policy : Chat_response.Runtime_semantics.policy)]
  |> Sexp.to_string_mach
  |> fun contents -> Chatml_source.Digest.of_contents contents ~checkpoint:Eio.Fiber.check
;;

let capture_child t ~child ~checkpoint =
  let* () =
    if Child.issued_by_root child t.root
    then Ok ()
    else invalid "queued child belongs to another physical Root"
  in
  let* comparison = Child.capture_issuer_source child ~live:t.live ~checkpoint in
  let runtime_policy =
    runtime_policy_identity
      ~base:(B.Issuer_source.runtime_policy comparison)
      ~policy:t.runtime_policy
  in
  let* after = Child.refresh child in
  let* () = Child.Admission.check_now after in
  Ok
    (B.Issuer_source.create
       ~artifact:(B.Issuer_source.artifact comparison)
       ~prompt:(B.Issuer_source.prompt comparison)
       ~workspace:(B.Issuer_source.workspace comparison)
       ~tool_contract:(B.Issuer_source.tool_contract comparison)
       ~source_policy:(B.Issuer_source.source_policy comparison)
       ~runtime_policy)
;;
