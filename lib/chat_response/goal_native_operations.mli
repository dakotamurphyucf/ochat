(** Internal receipt operation injected by the actual joined native Tool.call
    bridge. Public ChatML syntax cannot turn an integer into Actor authority. *)
val name : string

val install
  :  current:(unit -> Goal_native_scope.t option)
  -> Chatml_host_runtime.runtime_config
  -> Chatml_host_runtime.runtime_config

val prepare
  :  Goal_native_scope.t option
  -> Chatml.Chatml_lang.eff list
  -> (Chatml.Chatml_lang.eff list, string) result

val deliver
  :  Goal_native_scope.t option
  -> receipts:int list
  -> perform:(Chatml.Chatml_lang.eff -> (unit, string) result)
  -> (unit, string) result
