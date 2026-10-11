# Administrative project source host

The server's `project_sources` setting grants source-acquisition authority. It is
separate from authored `chatml.project.json` declarations, runtime tool grants and
script restoration policy. Omission selects local-only acquisition beneath each
prompt's actual retained original source root. It neither searches `PATH` for Git
nor grants arbitrary filesystem or network access.

An operator can add external local roots and exact Git repositories:

```scheme
(server
 ((project_sources
   ((external_roots (./shared-chatml /srv/ochat/shared))
    (git
     ((helper_directory /opt/ochat/git-2.56.0/helpers)
      (ca_bundle /opt/ochat/ca.pem)
      (memory_bytes_per_process 8589934592)
      (repositories
       ((https
         ((repository https://example.invalid/helpers.git)
          (authorization_header_file ./repository.header)))
        (local /srv/vendor/helpers.git)))))))))
```

These are server record fields inside the existing version-1 configuration. Local
administrative paths resolve against the configuration file's directory; existing
`~/` expansion remains available. Pure configuration validation bounds and
normalizes inputs without opening roots, helpers, CA files or credentials. The
actual startup owner performs those admissions. Git memory is required and
positive when enabled: the number above is an explicit example, not a default or
an assurance that it fits every deployment. The same supplied cap governs native
acquisition and executable qualification; insufficient caps fail without automatic
inherited-footprint widening.

Enabled Git admits the fixed Git 2.56.0 helper inventory and controlled command
profile. The operator supplies stable executable/helper/CA paths and exact HTTPS
or native local repository grants. Authorization files contain one explicit HTTP
header, for example `Authorization: Bearer ...`; live reads reject more than 4096
bytes, errors omit contents, and only authorized HTTPS fetches receive the header.
Absent CA configuration uses the qualified system trust profile. TLS verification
is never disabled. There is no ambient credential, helper, executable or repository
fallback. The private `project-git` cache below the actual daemon data root uses
existing lease, admission, atomic cache-publication and joined cleanup checks.
Host-selected native labels and stable ancestors are trusted deployment inputs;
they do not establish pathname-to-descriptor binding or an operating-system disk
quota. External source roots separately use retained descriptor-relative local
source confinement.

The finite source profile allows 256 sources, independent 8 MiB canonical-input
and physical-custody ceilings, 256 KiB per source and 262144-byte document metadata.
Its original catalog deadline is 120 seconds, independently of runtime's existing
compilation policy. Semantic/helper work is 10 million cooperative units; a native
bracket has 40 million and whole backend acquisition has 160 million. These units
are weighted admission estimates, not CPU or syscall measurements. With the
profile's 1 MiB pack and 16 KiB metadata caps, fetching reserves 8454144 payload
bytes and 33817600 native units before effects; a uniform 10-million native limit
would reject every fetch. Embedder policy overrides may reduce temporal/work
ceilings without changing grants or silently increasing limits.

`Project_source_host.with_host` borrows resources only for complete source
preparation. Actual source-preparation callers return immutable plans, exact
captured sources and selected locks, with no live native owner; the generic
borrow API itself does not constrain a callback's result type. Publication and
retirement use that portable preparation result without a continued borrow. Owner close revokes admission, cancels and joins borrowers
before releasing anchors, helpers and cache. Daemon shutdown must join reload and
session operations before owner release. Checkpoint and callback exceptions retain
their original identity; cancellation pending at resolved-result delivery still
cancels the caller. Existing prepared executions remain immutable.

Qualification uses real configured-source closures, fixed Git helpers, authorized
local TLS responses, both SHA1 and SHA256 pins, offline cache reads, exact header
bounds and deterministic owner/caller cancellation handshakes. The required Git
fixture cannot silently skip when its explicit qualification input is unavailable.
The supported Linux CI profile remains a separate native qualification gate.
