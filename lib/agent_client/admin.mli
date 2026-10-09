open! Core

(** Transport-neutral daemon administration reads. Mutations that require an
    attachment are exposed by {!Session_handle}. *)

(** [list_sessions connection] returns every session visible to the current
    principal. Enumerates at most 100000 sessions over 100 pages, returning an
    error on bound exhaustion or concurrent catalog change rather than truncating. *)
val list_sessions
  :  Connection.t
  -> (Agent_protocol.Session.t list, Agent_protocol.Error.t) result

(** [get_session connection session_id] returns the authoritative snapshot
    without attaching. *)
val get_session
  :  Connection.t
  -> Agent_protocol.Id.Session.t
  -> (Agent_protocol.Public.Snapshot.t, Agent_protocol.Error.t) result

(** Export retained content without attaching or selecting execution. Current
    method scopes and session visibility apply on every request. The resulting
    blob can be read with [Blob_download.download ~attachment_id:None]. *)
val export_session
  :  Connection.t
  -> session_id:Agent_protocol.Id.Session.t
  -> format:Agent_protocol.Session.Export_request.format
  -> revision:int64 option
  -> history:Agent_protocol.History.Window_request.t option
  -> (Agent_protocol.Method_result.Export.t, Agent_protocol.Error.t) result

val list_sessions_page
  :  Connection.t
  -> Agent_protocol.Session.List_request.t
  -> ( Agent_protocol.Session_catalog.t Agent_protocol.Page.t
       , Agent_protocol.Error.t )
       result

(** Explicitly bounded full enumeration. A nonempty initial cursor is invalid.
    Refresh conflicts are returned; callers choose whether to restart the query. *)
val enumerate_sessions
  :  Connection.t
  -> query:Agent_protocol.Session.List_request.t
  -> max_sessions:int
  -> max_pages:int
  -> (Agent_protocol.Session_catalog.t list, Agent_protocol.Error.t) result

(** Host-qualified logical organization. Scopes and owner/admin visibility are
    checked by the host on every page/mutation/receipt. No session execution. *)
val create_project
  :  Connection.t
  -> Agent_protocol.Organization_request.Create.t
  -> (Agent_protocol.Organization_group.Project.t, Agent_protocol.Error.t) result

val get_project
  :  Connection.t
  -> Agent_protocol.Organization_request.Project.Get.t
  -> (Agent_protocol.Organization_group.Project.t, Agent_protocol.Error.t) result

val list_projects_page
  :  Connection.t
  -> Agent_protocol.Organization_request.List.t
  -> ( Agent_protocol.Organization_group.Project.t Agent_protocol.Page.t
       , Agent_protocol.Error.t )
       result

val update_project
  :  Connection.t
  -> Agent_protocol.Organization_request.Project.Update.t
  -> (Agent_protocol.Organization_group.Project.t, Agent_protocol.Error.t) result

val delete_project
  :  Connection.t
  -> Agent_protocol.Organization_request.Project.Delete.t
  -> (Agent_protocol.Organization_result.Project_deleted.t, Agent_protocol.Error.t) result

val create_collection
  :  Connection.t
  -> Agent_protocol.Organization_request.Create.t
  -> (Agent_protocol.Organization_group.Collection.t, Agent_protocol.Error.t) result

val get_collection
  :  Connection.t
  -> Agent_protocol.Organization_request.Collection.Get.t
  -> (Agent_protocol.Organization_group.Collection.t, Agent_protocol.Error.t) result

val list_collections_page
  :  Connection.t
  -> Agent_protocol.Organization_request.List.t
  -> ( Agent_protocol.Organization_group.Collection.t Agent_protocol.Page.t
       , Agent_protocol.Error.t )
       result

val update_collection
  :  Connection.t
  -> Agent_protocol.Organization_request.Collection.Update.t
  -> (Agent_protocol.Organization_group.Collection.t, Agent_protocol.Error.t) result

val delete_collection
  :  Connection.t
  -> Agent_protocol.Organization_request.Collection.Delete.t
  -> ( Agent_protocol.Organization_result.Collection_deleted.t
       , Agent_protocol.Error.t )
       result

(** Completes all pages or fails at either explicit bound; never truncates or
    implicitly restarts a conflicted cursor. *)
val enumerate_projects
  :  Connection.t
  -> query:Agent_protocol.Organization_request.List.t
  -> max_groups:int
  -> max_pages:int
  -> (Agent_protocol.Organization_group.Project.t list, Agent_protocol.Error.t) result

(** Completes all pages or fails at either explicit bound; never truncates or
    implicitly restarts a conflicted cursor. *)
val enumerate_collections
  :  Connection.t
  -> query:Agent_protocol.Organization_request.List.t
  -> max_groups:int
  -> max_pages:int
  -> (Agent_protocol.Organization_group.Collection.t list, Agent_protocol.Error.t) result

(** Restore retained identity/content to Active while retaining the explicit
    resume gate. Does not construct a runtime or start retained work. *)
val restore_session
  :  Connection.t
  -> Agent_protocol.Session_lifecycle.Request.t
  -> (Agent_protocol.Session_lifecycle.Result.t, Agent_protocol.Error.t) result

(** Commit the explicit Active/Automatic execution gate. This is separate from
    actual runtime construction or session.start; the result reports the gate. *)
val resume_session
  :  Connection.t
  -> Agent_protocol.Session_lifecycle.Request.t
  -> (Agent_protocol.Session_lifecycle.Result.t, Agent_protocol.Error.t) result
