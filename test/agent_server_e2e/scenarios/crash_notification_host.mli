(** Qualified daemon with a deterministic provider. [boundary] is [terminal], [pending],
    [committed], [accepted], [invocation-admitted], [invocation-resolved],
    [invocation-permission], [job-intent], or
    [recover]. Crash modes pause after the selected actual journal
    sync; recovery mode never requests a tool and records notification inputs. *)
val run
  :  ?standalone:bool
  -> Eio_unix.Stdenv.base
  -> config_path:string
  -> boundary:string
  -> unit

(** Deterministic watch invocation shared by notification and ingress crash hosts. *)
val call : Openai.Responses.Response_stream.t list
