# `Github` – Download files from a GitHub repository

High-level overview
-------------------

`Github` is a lightweight helper that makes it trivial to **download every
file present in a specific folder of a GitHub repository**.  It relies on
Eio for concurrent I/O, Cohttp-eio for HTTPS access, and Jsonaf for
decoding the REST-API responses.

The public interface contains a single function:

```ocaml
val download_files :
  _ Eio.Net.t              (* network capability *)
  -> Eio.Fs.dir_ty Eio.Path.t  (* destination directory *)
  -> string                (* folder inside the repo *)
  -> string                (* repository owner *)
  -> string                (* repository name *)
  -> unit
```

Detailed API
------------

### `download_files net dir folder owner repo`

Downloads all *regular* files located **directly under** `folder` of the
GitHub repository `owner/repo` and writes them to `dir` using their
original filenames.

How it works:

1.  A `GET /repos/:owner/:repo/contents/:folder` request retrieves the
    directory listing.  When the `GITHUB_API_KEY` environment variable is
    present its value is sent as a *Bearer* token, allowing access to
    private repositories or higher rate-limits.
2.  The JSON payload is parsed with Jsonaf; each entry’s `name` and
    `download_url` fields are extracted.
3.  A second HTTPS request downloads every `download_url` and writes the
    body to `dir/name` (overwriting any existing file).

The function is **blocking with respect to the current Eio fiber**: it
returns only when the last file has been persisted to disk.

Parameters
~~~~~~~~~~

* `net` – network capability obtained from `Eio.Stdenv.net`.
* `dir` – destination directory where files will be written.
* `folder` – path inside the repository to fetch (e.g. `"assets"`).
* `owner` – GitHub account or organisation name.
* `repo` – repository name.

Exceptions
~~~~~~~~~~

* `Eio.Io` – on network or filesystem failures.
* `Jsonaf.Parse_error` – if the API response cannot be decoded.

Examples
--------

The snippet below fetches every file located in the `assets` folder of
`octocat/Hello-World` and stores them in the current working directory:

```ocaml
open Eio.Std

let () =
  Eio_main.run @@ fun env ->
  let net = Eio.Stdenv.net env in
  let dir = Eio.Stdenv.cwd env in
  Github.download_files net dir "assets" "octocat" "Hello-World"
```

If you need to access a **private repository**, export an access token
before running the program:

```bash
$ export GITHUB_API_KEY=ghp_xxxxxxxxxxxxxxxxxxxxx
```

Known limitations
-----------------

* **No recursion** – only files that live *directly* inside `folder` are
  downloaded.  Sub-directories are ignored.
* **Serial downloads** – files are fetched one after another.  For large
  directories using a task-pool to download files in parallel would be
  more efficient.

Future improvements could address both points.

