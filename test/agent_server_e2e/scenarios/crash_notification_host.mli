(** Qualified daemon with a deterministic provider. [boundary] is [terminal], [pending],
    [committed], [accepted], [invocation-admitted], [invocation-resolved],
    [invocation-permission], [job-intent], or
    [recover]. Crash modes pause after the selected actual journal
    sync; the default recovery provider never requests a tool and records
    notification inputs. [provider] borrows a scenario's deterministic local
    provider for the same host lifetime without changing the journal boundary. *)
val run
  :  ?standalone:bool
  -> ?provider:Inference_fixture.post_stream
  -> Eio_unix.Stdenv.base
  -> config_path:string
  -> boundary:string
  -> unit

(** Deterministic watch invocation shared by notification and ingress crash hosts. *)
val call : Openai.Responses.Response_stream.t list
