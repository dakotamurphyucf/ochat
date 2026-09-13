# Lantern check reference

- **setup:** the tutorial has a Setup heading and a check-docs.sh command.
- **links:** the tutorial links to reference.md and that file exists.
- **verification:** the tutorial has a Verification heading and an expected-result sentence.

The checker tests these conventions, not the accuracy of arbitrary prose. A
reviewer still needs to decide whether the instructions are useful and correct.
Exit status 0 means all selected checks passed; 1 means a check failed; 2 means
invalid arguments or a missing input. JSON is written to stdout in every normal
check run. With --write-report, the same JSON is also saved to reports/latest.json
beside sample-project. That output directory must already exist.
