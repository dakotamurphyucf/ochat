open! Core
module P = Agent_protocol
module Request = Instruction_source_request

let denied () =
  Error
    (P.Error.create
       Permission_denied
       ~message:"instruction source use expired or denied"
       ~retryable:false
       ())
;;

module Backend = struct
  type t =
    { roots : Instruction_content.Root_id.t list
    ; check_now : Instruction_content.Root_id.t list -> (unit, P.Error.t) result
    ; refresh :
        Request.Selection.t
        -> checkpoint:(unit -> unit)
        -> (Request.Discovery.t, P.Error.t) result
    ; capture :
        Request.t
        -> checkpoint:(unit -> unit)
        -> (Instruction_capture.t, P.Error.t) result
    }

  let create ~roots ~check_now ~refresh ~capture = { roots; check_now; refresh; capture }
end

type t =
  { open_use :
      Operator_authorization.t
      -> checkpoint:(unit -> unit)
      -> (Backend.t, P.Error.t) result
  }

let create ~open_use = { open_use }

module Use = struct
  type t =
    { backend : Backend.t
    ; actor : Operator_authorization.t
    ; live : bool Atomic.t
    }

  let roots t = t.backend.roots

  let check_now t ~roots =
    if (not (Atomic.get t.live)) || not (Operator_authorization.is_current t.actor)
    then denied ()
    else if
      List.exists roots ~f:(fun root ->
        not (List.mem t.backend.roots root ~equal:Instruction_content.Root_id.equal))
    then denied ()
    else t.backend.check_now roots
  ;;
end

module Captured = struct
  type t =
    { use : Use.t
    ; contents : Instruction_capture.t
    ; roots : Instruction_content.Root_id.t list
    }

  let contents t = t.contents

  let check_now t ~use =
    if not (phys_equal t.use use) then denied () else Use.check_now use ~roots:t.roots
  ;;
end

let with_use t ~actor ~checkpoint f =
  let open Result.Let_syntax in
  if not (Operator_authorization.is_current actor)
  then denied ()
  else (
    let%bind backend = t.open_use actor ~checkpoint in
    let use = Use.{ backend; actor; live = Atomic.make true } in
    Exn.protect
      ~finally:(fun () -> Atomic.set use.live false)
      ~f:(fun () ->
        let%bind () = Use.check_now use ~roots:backend.roots in
        f use))
;;

let refresh use ~selection ~checkpoint =
  let open Result.Let_syntax in
  let%bind () = Request.Selection.validate selection in
  let roots = Request.Selection.roots selection in
  let%bind () = Use.check_now use ~roots in
  let result = use.Use.backend.refresh selection ~checkpoint in
  let%bind () = Use.check_now use ~roots in
  result
;;

let capture use ~request ~checkpoint =
  let open Result.Let_syntax in
  let roots = Request.Selection.roots (Request.selection request) in
  let%bind () = Use.check_now use ~roots in
  let result = use.Use.backend.capture request ~checkpoint in
  let%bind () = Use.check_now use ~roots in
  let%bind contents = result in
  if not (Request.matches request contents)
  then Error (P.Error.invalid_request "instruction source capture does not match request")
  else Ok { Captured.use; contents; roots }
;;
