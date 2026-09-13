open Core

let failed message =
  Agent_protocol.Error.create Invalid_state ~message ~retryable:false ()
;;

let decode = function
  | None -> Ok None
  | Some (`Object fields) ->
    (match List.Assoc.find fields "identity_snapshot_sexp" ~equal:String.equal with
     | Some (`String encoded) ->
       (try
          Ok
            (Some
               ([%of_sexp: Session.Moderator_state.Identity_snapshot.t]
                  (Sexp.of_string encoded)))
        with
        | exn -> Error (failed ("moderator snapshot decode failed: " ^ Exn.to_string exn)))
     | _ -> Error (failed "moderator snapshot is missing identity state"))
  | Some _ -> Error (failed "moderator snapshot must be an object")
;;

let observer snapshot =
  Result.map
    (decode snapshot)
    ~f:
      (Option.map ~f:(fun snapshot ->
         Agent_protocol.Invocation.
           { script_id = snapshot.Session.Moderator_state.Identity_snapshot.script_id
           ; source_sha256 = snapshot.script_source_hash
           }))
;;

let is_halted snapshot =
  Result.map
    (decode snapshot)
    ~f:
      (Option.value_map ~default:false ~f:(fun snapshot ->
         snapshot.Session.Moderator_state.Identity_snapshot.halted))
;;
