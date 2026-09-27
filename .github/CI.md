# R continuous integration

## Shared convention

Routine CI uses Ubuntu and R release. R-lib workflows install binary packages
from Posit Package Manager where available and cache the installed R library.
The dependency action owns cache keys: OS, exact R version/architecture and the
resolved dependency lockfile. `cache: always` preserves dependencies even when
checks fail. Keep `cache-version` stable; change it only to invalidate a broken
cache. No commit SHA or test-shard name belongs in a package-library cache key.
Dependency resolution still runs on cache hits, and the package under test is
rebuilt from the checked-out source. Test results are never cached.

Hosted jobs get fresh VMs. System packages and R setup are not preserved by an
R-library cache; binary repositories reduce compilation work. Do not restore
`/usr`, apt databases, or compiler/system libraries from arbitrary caches.
Default-branch caches can seed PR runs; PR-created caches are scoped to that
PR and do not warm other branches or repositories. Main-branch checks keep the
shared baseline warm. Concurrent shards may share compatible cache keys.

Use one routine job unless a measured package requirement calls for more.
Keep existing required-check names stable. Errors and warnings fail checks;
review NOTES and require a clean final CRAN candidate. Stricter package gates
are documented below. Do not retry failed tests. Dependency-download retries
are acceptable when bounded. Preserve diagnostic artifacts on failures/timeouts.
Root-package workflows check all PR changes. Monorepo workflows filter on the
R package, their workflow/configuration, and external fixtures affecting R.
If a path-filtered workflow is made required for every PR, account for skipped
non-R changes in branch protection rather than leaving a permanently pending gate.

R-hub is manual (`rhub.yaml`) for platform/version checks before CRAN updates,
not a per-PR matrix. Configure R-hub credentials with its supported setup,
select the intended commit and supported platforms, and review each result.
R-hub defaults are not a declaration of the minimum supported R version.
Explicitly qualify the minimum R and dependency versions and a current Node
runtime where applicable. Supplement R-hub when a required environment is not
available. No workflow publishes packages or changes repository protection.
Remove the extra r-universe repository only when every needed version is on CRAN.

## Package requirements and exceptions

The general, supervised and fitting test groups run concurrently; the final R-CMD-check gate requires all three. Their library cache keys deliberately do not contain the test-group name. Only general runs examples. Notes remain failures here. Real parallel-worker integration is opt-in through run-parallel-tests; deterministic worker-policy/RNG tests remain routine. Keep tests/ci/test-groups.R and its partition-coverage test authoritative. Check diagnostics survive a step timeout. Manual R-hub runs execute the unsharded suite with real worker integration disabled unless explicitly configured.

## Verification and maintenance

Workflow syntax and conventions can be checked locally; cache speed and platform
behavior must be measured on hosted runs. After workflow changes, verify the
required-check list and compare dependency setup separately from check runtime.

References: [r-lib dependency caching](https://github.com/r-lib/actions/tree/v2/setup-r-dependencies),
[GitHub cache scope](https://docs.github.com/en/actions/reference/workflows-and-actions/dependency-caching),
and [R-hub workflows](https://github.com/r-hub/actions/tree/v1).
