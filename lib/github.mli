(** Helpers for fetching raw files from GitHub.

    The module exposes {!download_files}, a convenience wrapper around
    the GitHub REST API that downloads every file stored directly under
    a given folder of a repository and writes them to the local
    filesystem using {!module:Io} helpers. *)

(** [download_files net dir folder owner repo] downloads all regular files
    located in [folder] of the GitHub repository [owner/repo] and saves
    them in the local directory [dir].

    The function performs the following steps:

    1. Calls [GET /repos/owner/repo/contents/folder] on the GitHub REST
       API (v3) to retrieve the directory listing.  The request is
       authenticated with the value of the environment variable
       [GITHUB_API_KEY] when it is defined.
    2. Parses the JSON response with {!module:Jsonaf} and extracts each
       element’s [name] and [download_url] fields.
    3. Performs a second HTTP GET for every [download_url] and stores
       the raw payload as [dir/name] (existing files are overwritten).

    The operation is synchronous with respect to the current fiber and
    returns when every file has been written.

    @param net   network capability obtained from [Eio.Stdenv.net].
    @param dir   output directory where the downloaded files will be
                 written.
    @param folder path inside the repository whose immediate files are
                  to be fetched (e.g. "assets" or "src/templates").
    @param owner GitHub account or organisation.
    @param repo  repository name.

    @raise Eio.Io          on network or filesystem errors.
    @raise Jsonaf.Parse_error if the API response cannot be decoded. *)
val download_files
  :  _ Eio.Net.t
  -> Eio.Fs.dir_ty Eio.Path.t
  -> string
  -> string
  -> string
  -> unit
