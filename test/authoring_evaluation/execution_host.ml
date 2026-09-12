open Core
module P = Agent_protocol
module Embedded = Agent_server.Embedded

exception Protocol_error of P.Error.t [@@deriving sexp]

let get = function
  | Ok value -> value
  | Error error -> raise (Protocol_error error)
;;

let call_events calls =
  List.concat_mapi calls ~f:(fun index (id, name, arguments) ->
    let open Openai.Responses.Response_stream in
    [ Output_item_added
        { item =
            Function_call
              { name
              ; arguments = ""
              ; call_id = id
              ; _type = "function_call"
              ; id = Some id
              ; status = None
              }
        ; output_index = index
        ; type_ = "response.output_item.added"
        }
    ; Function_call_arguments_done
        { arguments = Jsonaf.to_string arguments
        ; item_id = id
        ; output_index = index
        ; type_ = "response.function_call_arguments.done"
        }
    ])
  |> Stdlib.List.to_seq
;;

let request embedded command =
  Agent_client.Connection.request (Embedded.connection embedded) command |> get
;;

let snapshot embedded =
  match
    request
      embedded
      (Session_get { session_id = Embedded.session_id embedded; history = None })
  with
  | Session_get snapshot -> snapshot
  | _ -> failwith "evaluation host received another protocol response"
;;

(* Candidate sources execute in an actual embedded session. Only host-authored
   file names and root declarations are accepted here, never candidate ChatMD.
   Calls traverse provider serialization, invocation admission and tool dispatch.
   The scripted provider is local and never opens a model connection. *)
let run ~env ~sources ~workspace_files ~calls =
  Mirage_crypto_rng_unix.use_default ();
  let root = Agent_server_test_support.temporary_root env in
  Exn.protect
    ~finally:(fun () ->
      Eio.Path.rmtree ~missing_ok:true Eio.Path.(Eio.Stdenv.fs env / root))
    ~f:(fun () ->
      let workspace = Filename.concat root "workspace" in
      Eio.Path.mkdir ~perm:0o700 Eio.Path.(Eio.Stdenv.fs env / workspace);
      let save base (name, source) =
        Eio.Path.save
          ~create:(`Exclusive 0o600)
          Eio.Path.(Eio.Stdenv.fs env / base / name)
          source
      in
      List.iter sources ~f:(save root);
      List.iter workspace_files ~f:(save workspace);
      let requests = ref 0 in
      let post_stream ~sw:_ ~inputs:_ =
        incr requests;
        match !requests with
        | 1 -> call_events calls
        | _ -> Stdlib.Seq.empty
      in
      Eio.Switch.run (fun sw ->
        let embedded =
          Embedded.start
            ~sw
            ~env
            ~daemon_options:
              { Agent_server.Daemon.default_options with
                qualify_chatml_extensions = true
              ; model_post_stream = Some post_stream
              }
            { prompt_file = Filename.concat root "agent.chatmd"
            ; workspace
            ; tool_dir = root
            ; home = root
            ; data_root = None
            ; start_immediately = true
            ; permission_profile =
                { Embedded.default_permission_profile with
                  tool_default = Allow
                ; manifest_authorization = Assume_authorized
                }
            ; attachment_mode = Read_write
            ; event_capacity = 512
            }
          |> get
        in
        Exn.protect
          ~finally:(fun () -> Embedded.close embedded)
          ~f:(fun () ->
            ignore
              (request
                 embedded
                 (Session_send_message
                    { session_id = Embedded.session_id embedded
                    ; attachment_id = (Embedded.attachment embedded).id
                    ; content =
                        { kind = Plain_text
                        ; text = "Execute the evaluation calls."
                        ; attachments = []
                        }
                    ; idempotency_key =
                        P.Idempotency_key.of_string "evaluation:execute" |> get
                    })
               : P.Method_result.t);
            Eio.Time.with_timeout_exn (Eio.Stdenv.clock env) 10. (fun () ->
              let rec wait () =
                let current = snapshot embedded in
                Option.iter current.failure ~f:(fun error -> raise (Protocol_error error));
                match
                  !requests >= 2 && Option.is_none current.session.active_operation
                with
                | true ->
                  (match !requests = 2 && List.is_empty current.jobs with
                   | true -> current
                   | false ->
                     failwith "synchronous evaluation unexpectedly scheduled extra work")
                | false ->
                  Eio.Time.sleep (Eio.Stdenv.clock env) 0.01;
                  wait ()
              in
              wait ()))))
;;

let outcome (snapshot : P.Snapshot.t) call_id =
  List.find_map_exn snapshot.canonical_history.entries ~f:(fun entry ->
    match Agent_session.History_codec.of_protocol entry |> get |> History_entry.item with
    | Openai.Responses.Item.Function_call_output
        { call_id = actual; output = Text text; _ }
      when String.equal actual call_id ->
      Some (P.Invocation.outcome_of_json (Jsonaf.of_string text) |> get)
    | _ -> None)
;;
