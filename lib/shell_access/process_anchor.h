#ifndef OCHAT_PROCESS_ANCHOR_H
#define OCHAT_PROCESS_ANCHOR_H
#include <caml/mlvalues.h>

/* Forked-child only, no allocation/OCaml callbacks. Shared finite checked setup. */
const char *ochat_close_extra(int errors, int highest);
const char *ochat_apply_limits(value limits);
_Noreturn void ochat_anchor(int errors, value configuration, value actions);
#endif
