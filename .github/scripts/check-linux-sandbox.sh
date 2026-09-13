#!/usr/bin/env bash
set -euo pipefail

# Ubuntu's packaged profile permits bwrap to create namespaces while stripping
# capabilities from its payload. Loading it is runner setup, not an Ochat grant.
# Keep the host-wide AppArmor/userns policy and all runtime sandbox flags intact.
/usr/bin/bwrap --version
if [[ -r /sys/module/apparmor/parameters/enabled ]] &&
   [[ $(cat /sys/module/apparmor/parameters/enabled) == Y ]]; then
  profile=/etc/apparmor.d/bwrap-userns-restrict
  if [[ ! -f "$profile" ]]; then
    echo "Ubuntu's packaged Bubblewrap AppArmor profile is missing: $profile" >&2
    exit 1
  fi
  sudo apparmor_parser --replace "$profile"
fi

# Exercise the same empty environment and namespace setup used by private
# channels. This only checks host readiness; the normal integration tests prove
# restricted file/socket/descriptor access and exchange through the private pipes.
if ! /usr/bin/env -i /usr/bin/bwrap \
  --die-with-parent --new-session --unshare-all \
  --ro-bind / / --proc /proc --dev /dev --tmpfs /tmp -- /usr/bin/true; then
  echo "Bubblewrap namespace setup failed before framework tests." >&2
  cat /proc/self/attr/current >&2 || true
  sysctl kernel.apparmor_restrict_unprivileged_userns >&2 || true
  sudo journalctl --kernel --since '-2 minutes' --no-pager --grep='apparmor|bwrap' >&2 || true
  exit 1
fi
echo 'Bubblewrap namespace preflight passed.'
