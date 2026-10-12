# Retained local package roots

Hosts map lexical absolute prefixes to already-open Eio directory grants. Sources
may request relative or absolute local directories; requests never grant authority.
Relative input uses the declaring package's cursor. Normalization removes `.`/`..`
lexically, then the longest component-prefix grant supplies descriptor-relative
nofollow traversal. No process cwd, realpath, path reopening or parent open occurs.

For example, a `/workspace` grant and declaring `/workspace/app` cursor resolve
`../shared` through workspace's `shared` component. A separately granted `/opt/lib`
can serve an absolute request. `/workspace-other` is outside `/workspace`. Cancelled
lexical components are not opened: `missing/../shared` chooses `shared`.

A grant label maps a retained object, even after rename; it is not proof that an OS
pathname still names that object. New attempts reacquire routes below supplied
anchors. Replacing an anchor itself requires a newly admitted host capability.
Each cursor preserves its own lexical base even when kernel identities are equal.
Directory object identity only informs sharing while owners stay alive. Never put
it in a durable lock/source closure or treat equal contents as equal origin.

## Resource ownership

One attempt owns a switch and all acquired root owners. Exact normalized routes
reuse a retained owner. The count charges actual descriptor owners, including two
owners with equal device/inode and a successfully acquired owner whose subsequent
identity/cursor admission failed. Such owners stay charged and switch-owned until
attempt close. No partially admitted cursor escapes; callers may abort the attempt
on an admission error. Reentrant/concurrent resolution returns Busy; native read/
fold operations retain their existing independent-handle callback behavior.

A native subroot clones its final acquired descriptor, closes intermediates, then
registers child cleanup before publication. It has an independent lifecycle and
may outlive its parent's native owner inside its own switch. Joined switch cleanup
rejects new operations, waits admitted operations, and closes exactly once. Native
allocation/errno/cancellation remain distinct; callbacks run in Eio fibers.

With retained-owner count R and serial traversal depth D, subroot traversal owns at
most D intermediate FDs plus one final duplicate before intermediates close. Anchor
borrowing adds a temporary Eio descriptor plus one duplicate. Thus acquisition
has at most R+D+2 directory descriptors (excluding caller's borrowed anchor FDs).
Source read/fold resources are separately bounded by the supplied native member
ceiling and existing local_source batch/read envelopes. Capturing concurrently
requires the caller to reserve concurrency resources separately; resolver itself
is Eio-fiber confined and sequential.

## Reservation model v1

Work units conservatively weight bounded stages; they are not measured CPU, syscall
counts or deadlines. Arithmetic checks before reservation/work. No checkpoints
implement ordinary admission failure.

- Admit anchor index i with canonical prefix byte length b: `(b+1)*(4*i+8)`
  units cover validation, set membership/insertion and route indexing. Native anchor
  duplication plus identity observation reserves16 units.
- Normalize a request with raw byte length l and base length b: `8*(l+b)+8`.
  Both raw and each intermediate normalized path must fit the locator caps.
- Select a grant and check route reuse: `4*sum(anchor_prefix_bytes)` plus
  `target_bytes*(4*height(route_count)+8)` plus `anchor_count+1`, where height is
  floor(log2(count))+1 for nonzero count. This covers all component-prefix visits
  and balanced route-map comparisons, not a promise about exact comparison count.
- Acquire a new route with d descendant components and target length b:
  `4*b+8*d+16` covers component validation, joined opens/final clone, identity
  observation and route insertion. Root slot is checked before acquisition.

Local capture's existing reservation model separately charges discovery, read,
source scans and Catalog validation. capture_from_owner omits the six legacy
root-bracket units; the outer owner accounts actual root acquisition. Bootstrap
source reidentification is O(1) and reuses immutable storage; decode against the
final identity again so declaration-owned locations are never forged/remapped.
