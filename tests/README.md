# Test execution modes

`just check` builds the package and runs `R CMD check` against the installed
package. `just test` and `just test-filter` use `testthat::test_local()` against
source. These modes do not necessarily run the same worker cases:

- A noninteractive check with `NOT_CRAN` unset treats `skip_on_cran()` as a skip.
- `test_local()` assumes it is outside CRAN and normally sets `NOT_CRAN=true`.
- Real worker integration guarded by `skip_ci_parallel_integration()` requires
  `RTEMIS_RUN_PARALLEL_TESTS=true` when `CI` or `CODEX_CI` is `true` or `1`.
- `RTEMIS_RUN_PARALLEL_TESTS=false` disables those cases in any environment.
  The opt-in never overrides a test's `skip_on_cran()` guard.

Sequential execution, seed handling and worker-policy assertions remain in
routine tests. CRAN examples and tests must stay within two simultaneous CPU
cores, including threads inside workers. Wider worker integration tests remain
outside CRAN checks. A successful check with skipped cases does not establish
that those worker cases passed.

```sh
just test                                     # source tests; automation gate applies
just check                                    # installed-package check
RTEMIS_RUN_PARALLEL_TESTS=false just test       # explicitly omit gated worker cases
just test-parallel /tmp/parallel-run-1 180      # opt-in, deadline, saved diagnostics
```

Repeated full-suite stalls have occurred in `parallel_outer` during automated
runs, while isolated runs can pass under the same nonterminal setup. The gate
contains that failure; it is not a diagnosed or repaired worker lifecycle bug.
Compare test mode, `NOT_CRAN`, test order, loaded packages, installed worker
library and backend versions before attributing a difference to the terminal.
The observed failed-fold test uses two workers: cancellation returns, then
cleanup enters `mirai::daemons()` without returning. Both passing and stalled
isolated runs have been observed; the cause below that call remains open.
Do not leave an opt-in diagnostic unbounded. Preserve a process timeout,
environment snapshot, exact test start/end events and worker operation events.

[testthat documents the NOT_CRAN convention](https://testthat.r-lib.org/reference/skip.html).
[CRAN documents its resource limit](https://cran.r-project.org/web/packages/policies.html).

`test-parallel` requires an empty output directory and never overwrites a prior
run. It records allowlisted environment values, library/backend versions,
per-test boundaries and worker-operation entry/exit events. `process.json`
records completion or timeout independently of the R result; a timeout exits
with status 124. The supervisor checks a wall-clock deadline between short waits and kills the
child process tree when overdue. A suspended host cannot execute the watchdog;
a late completion still reports a timeout after execution resumes.
The optional third argument selects a test-file filter for order comparisons.
Tracing changes timing, so a traced success does not rule out an intermittent
failure. Logs are local diagnostic artifacts, not package result schemas.
