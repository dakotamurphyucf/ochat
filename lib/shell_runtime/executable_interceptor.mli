open! Core

val before
  :  Hook_worker.t
  -> Shell_access.Context.t
  -> (Shell_access.Interceptor.before, Hook_worker.error) result

val after
  :  Hook_worker.t
  -> Shell_access.Interceptor.command_result
  -> (Shell_access.Interceptor.command_result, Hook_worker.error) result
