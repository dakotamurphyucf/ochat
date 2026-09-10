(** Qualified daemon with a deterministic provider. [boundary] is [pending],
    [committed], [accepted], or [recover]. Crash modes pause after the selected actual journal
    sync; recovery mode never requests a tool and records notification inputs. *)
val run : Eio_unix.Stdenv.base -> config_path:string -> boundary:string -> unit

(** Deterministic watch invocation shared by notification and ingress crash hosts. *)
val call : Openai.Responses.Response_stream.t list
