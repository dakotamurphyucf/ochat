#!/usr/bin/env bash
set -euo pipefail

# CI provisioning only. This does not grant project fetch authority. Actual
# Git/native/TLS qualification runs in the repository's real integration tests.
# Source pin: https://www.kernel.org/pub/software/scm/git/sha256sums.asc
readonly version=2.56.0
readonly archive_sha256=26c56c296b38c0695b26fa95f475f1d01704d2d38e73465ca30b0b2f5dc789d3
readonly source_url="https://www.kernel.org/pub/software/scm/git/git-${version}.tar.xz"

if [[ $# != 0 || $(uname -s) != Linux ]]; then
  echo 'Git qualification provisioning supports native Linux CI only.' >&2
  exit 1
fi
case $(uname -m) in
  x86_64|aarch64) ;;
  *) echo 'Unsupported Git qualification architecture.' >&2; exit 1 ;;
esac
for variable in RUNNER_TEMP GITHUB_WORKSPACE GITHUB_ENV; do
  if [[ ${!variable:-} != /* ]]; then
    echo "Required absolute CI path missing: ${variable}" >&2
    exit 1
  fi
done

readonly evidence="${GITHUB_WORKSPACE}/.ci-evidence/git-host-build"
mkdir -p "$evidence"
# All output is nonsecret build/provisioning evidence, retained even on failure.
exec > >(tee "$evidence/provision.log") 2>&1
attempt=$(mktemp -d "${RUNNER_TEMP}/ochat-git-qualification-XXXXXXXX")
readonly attempt
chmod 0700 "$attempt"
readonly archive="${attempt}/git-${version}.tar.xz"
readonly source="${attempt}/git-${version}"
readonly helpers="${attempt}/helpers"
mkdir -m 0700 "$attempt/home" "$attempt/tmp" "$attempt/default-exec" "$helpers"

# Explicit system dependencies for both Ubuntu24.04 amd64 and arm64. Never
# install or replace Git, OCaml, an opam switch or a system helper inventory.
sudo apt-get update
sudo apt-get install --yes --no-install-recommends \
  build-essential ca-certificates curl xz-utils zlib1g-dev libcurl4-openssl-dev

/usr/bin/curl --fail --location --proto '=https' --proto-redir '=https' \
  --tlsv1.2 --connect-timeout 15 --max-time 180 --retry 2 \
  --max-filesize 33554432 --output "$archive" "$source_url"
printf '%s  %s\n' "$archive_sha256" "$archive" | /usr/bin/sha256sum --check --strict
/usr/bin/tar -xJf "$archive" -C "$attempt" --no-same-owner

# Match the maintained source profile qualified on macOS; system libcurl still
# provides HTTPS/TLS when Git's direct OpenSSL hash backend is disabled.
# Native compilation, no -march=native, cross compilation or global install.
# The compiled default helper path is owned and empty; tests explicitly select
# their sealed helper path. The source/build tree is never an exposed PATH.
readonly build_arguments=(
  -j2 -C "$source" git git-remote-https
  CC=/usr/bin/gcc CURL_CONFIG=/usr/bin/curl-config
  NO_GETTEXT=YesPlease NO_TCLTK=YesPlease NO_PERL=YesPlease
  NO_EXPAT=YesPlease NO_OPENSSL=YesPlease
  "prefix=${attempt}/uninstalled" "gitexecdir=${attempt}/default-exec"
)
/usr/bin/env -i PATH=/usr/bin:/bin LC_ALL=C \
  "HOME=${attempt}/home" "TMPDIR=${attempt}/tmp" \
  /usr/bin/make "${build_arguments[@]}"

# Only these admitted native entrypoints reach the tests. remote-http is a build
# intermediate; http-fetch, credential helpers and shell wrappers stay absent.
/usr/bin/install -m 0555 "$source/git" "$helpers/git"
/usr/bin/install -m 0555 "$source/git-remote-https" "$helpers/git-remote-https"
/usr/bin/ln -s git "$helpers/git-upload-pack"
inventory=$(/usr/bin/find "$helpers" -mindepth 1 -maxdepth 1 -printf '%f\n' | LC_ALL=C /usr/bin/sort)
readonly inventory
readonly expected_inventory=$'git\ngit-remote-https\ngit-upload-pack'
if [[ $inventory != "$expected_inventory" ]]; then
  echo 'Qualified helper inventory is incomplete or excessive.' >&2
  exit 1
fi
chmod 0555 "$helpers" "$attempt/default-exec"

{
  printf 'source=%s\nsource_sha256=%s\n' "$source_url" "$archive_sha256"
  printf 'runner_arch=%s\nimage=%s\nimage_version=%s\n' \
    "$(uname -m)" "${ImageOS:-missing}" "${ImageVersion:-missing}"
  printf 'build_arguments:'
  printf ' %q' "${build_arguments[@]}"
  printf '\n'
  /usr/bin/gcc -dumpmachine
  /usr/bin/gcc -dumpfullversion
  /usr/bin/curl-config --version
  /usr/bin/curl-config --features
  /usr/bin/curl-config --ssl-backends
  /usr/bin/curl-config --libs
  /usr/bin/dpkg-query --show --showformat='${binary:Package}\t${Version}\t${Architecture}\n' \
    build-essential ca-certificates curl xz-utils zlib1g-dev libcurl4-openssl-dev \
    libcurl4t64 libssl3t64 libc6 zlib1g
  /usr/bin/env -i PATH="$helpers" LC_ALL=C "$helpers/git" --version --build-options
  /usr/bin/sha256sum "$helpers/git" "$helpers/git-remote-https"
  /usr/bin/ldd "$helpers/git"
  /usr/bin/ldd "$helpers/git-remote-https"
} > "$evidence/profile.txt"

# This supplies a candidate; only passing real tests qualifies it. Preserve the
# installed system Git as checkout/opam utility and never write GITHUB_PATH.
printf 'OCHAT_TEST_GIT_2_56_DIRECTORY=%s\n' "$helpers" >> "$GITHUB_ENV"
echo 'Pinned Git candidate prepared; native/TLS qualification remains required.'
