# Lantern quickstart

Lantern is a small documentation project. Its checks report whether a tutorial
explains setup, links to the reference, and tells a reader how to verify success.

## Setup

Use a Unix shell with awk installed. From the sample-project directory, run:

```sh
sh scripts/check-docs.sh --check all
```

Read the [reference](reference.md) to understand each check.

This tutorial intentionally omits a verification section. The checker should
report that omission instead of treating a readable file as correct documentation.
