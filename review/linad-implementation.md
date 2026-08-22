# LINAD: implementation review brief

For an independent reviewer. Written 2026-08-21 by the implementer, so treat
every judgment here as a claim to check rather than a fact to rely on. The
purpose of this document is to make it *easy to find defects*, not to argue the
code is correct.

Ten defects have been found in this code in the last week (Section 6). Several
were silent -- they degraded the fit without erroring, without failing a test,
and without showing on an accuracy benchmark. The review that matters is one
that finds the eleventh.

---

## 1. What the algorithm is

A **Linear Additive Tree**: a binary tree grown greedily, where every node
carries a *linear model* rather than a constant, and each node's model is fitted
to the negative gradient of the loss at that point -- so the tree is a stagewise
gradient-boosted additive model whose "weak learners" are the nodes of one tree.

A prediction for case `x` walks the root-to-leaf path and uses the leaf's
**accumulated** coefficient vector: each node's coefficients are its parent's
plus a shrunken update. So the model is a piecewise-linear function on a
recursive partition.

Four things make it not a standard model tree:

- **`gamma` soft weighting.** A case keeps full weight in the branch it belongs
  to and `gamma` times its weight in the other, so influence decays as
  `gamma^depth` rather than vanishing at the first split. Every node's model is
  therefore fitted on *every* case with surviving weight, not on the node's own
  cases. `gamma = 0` recovers a hard partition.
- **A line search.** Each update is scaled by a Newton step (`line_search`),
  estimated over all cases weighted by the soft membership weights.
- **Two split searches.** `stump` scores a candidate by the mean shift it
  produces (CART's criterion). `exhaustive` scores it by the loss after fitting
  **both child linear models**, which is the objective the tree is actually
  fitted to.
- **Selectable node models.** `constant`, `forward` (stepwise with an
  information-criterion stopping rule), `ridge`, `elasticnet`.

### Reductions it must satisfy

These are the strongest correctness statements available and are all tested:

| configuration | equals |
|---|---|
| `node_model = "constant"`, `gamma = 0`, `learning_rate = 1`, `line_search = "none"` | CART (`rpart`), predictions to 1e-10 |
| `max_leaves = 1` | the root model alone |
| `linad_stump()` on a residual | an `rpart` depth-1 stump, split value to 1e-8 |
| `linad_forward()` | `leaps::regsubsets` |
| stump vs exhaustive, constant leaves, `n_cuts` high | identical splits, bit for bit |

---

## 2. Where the code is

| file | lines | role |
|---|---|---|
| `R/linad.R` | 2645 | the engine: knows nothing about S7, preprocessing, or outcome types beyond `type` |
| `R/train_LINAD.R` | 288 | rtemis interface: `train_`, `predict_super`, `varimp_super` |
| `R/linad_forest.R` | 337 | bagged ensemble internals |
| `R/train_LINADForest.R` | 319 | forest interface, including infinitesimal-jackknife standard errors |
| `R/070_Hyperparameters.R` | -- | `LINADHyperparameters`, `setup_LINAD()`, and the gates |
| `tests/testthat/test_LINAD.R` | 1721 | 52 `test_that` blocks |

Every function in `R/linad.R` carries a `# %% name ----` section header, so
`grep -n "^# %% " R/linad.R` is the map.

### The call chain that matters

```
linad_fit()                    grow loop: frontier of nodes, each with a cached proposal
  linad_expand()               draws the mtry_split sample; retries over all features
    linad_propose()            gradient -> search -> route -> weights -> children -> line search
      linad_split_search()     the ONLY fork between the two searches
        linad_stump()          cumsum-based, mean criterion (+ slope term if split_criterion="linear")
        linad_sweep()          incremental sufficient statistics, child models per candidate
          score_split()        the ONLY place the exhaustive search decides anything
            linad_gram_solve() child model from a Gram
      linad_child()            the committed child's model
        linad_solve()          the node model proper
```

Two single-site invariants are deliberate and should be preserved by any change:
`linad_split_search()` is the only fork between searches, and `score_split()` is
the only scorer in the exhaustive path. Both exist because the search and the
commit diverging is the defect family that has bitten hardest (Section 6).

---

## 3. Invariants a reviewer should check

The engine's own structural checks live in `linad_check_tree()` (line 1877) and
run over twelve configurations in the suite. They cover tree shape, not
numerics.

**Not covered by any automated check, and worth attacking:**

1. **Search/commit agreement.** A candidate must be scored by the model the node
   will actually receive. This has failed three times: on the child-size floor,
   on `node_test`, and on `model_features` (not yet implemented, same hazard).
   `linad_min_child_cases()` (1303) is the shared floor; `linad_node_test()`
   (447) the shared model-choice rule.
2. **Coefficient accumulation.** A node's stored coefficients must reproduce its
   function value exactly; prediction must route to the leaf whose coefficients
   were fitted. Tested for both outcome types, but only at the defaults.
3. **`gamma`'s reach.** With `gamma > 0` the node model, the line search, and
   the split search all see different weight vectors. Whether each sees the
   *right* one is asserted in comments (`linad_steps()` at 1532 argues the line
   search must use all cases) and tested only indirectly.
4. **Scaling round trip.** The design is standardized (`linad_scaling()`, 2163)
   and coefficients are unscaled at the end (`linad_unscale()`, 2198). A
   reviewer should verify the round trip for factor dummies specifically.
5. **Classification.** The gradient/Hessian path (`linad_gradient()`, 707) and
   `constant_rule` differ from regression, and the probability orientation
   convention (rtemis expects the probability of the **second** factor level)
   is checked in exactly one test.

---

## 4. What is already verified, and how

- **External references.** `rpart` (twice), `leaps::regsubsets`. These pin only
  the degenerate corner: constant leaves, hard partition, no shrinkage, stump
  search. They say nothing about linear leaves, `gamma > 0`, or the exhaustive
  search.
- **Search against search.** The stump and exhaustive searches agree bit for bit
  on constant leaves once `n_cuts` is high enough -- two independent
  implementations, one answer. This is the strongest evidence for the exhaustive
  path and it still only covers constant leaves.
- **`linad_check_tree()`** -- structural invariants, twelve configurations.
- **Reductions and identities** -- see Section 1.

**The gap, stated plainly:** the configuration the paper actually advocates
(linear leaves, `gamma > 0`, exhaustive search) has *no external reference and
no independent implementation*. Its correctness rests on structural invariants
and on the reductions holding at the boundary of that configuration.

---

## 5. Where to attack first

Ranked by (likelihood of a defect) x (invisibility if present):

1. **`linad_sweep()` (2386), the factor branch.** Rewritten today. It enumerates
   all `2^(k-1)-1` partitions when that is within the `n_cuts` budget and falls
   back to a mean-residual ordering above it. Check: the mask enumeration covers
   each partition exactly once; per-level sufficient statistics sum correctly;
   the `gamma` mixture is right; the fallback path still matches its previous
   behavior.
2. **`linad_forward()` (170).** Rewritten today for speed (one triangular solve
   over all candidates instead of one each). Check the vectorized Schur
   complement against the per-candidate form, and the information-criterion
   stopping rule, which is the only thing making `nvmax` a ceiling.
3. **`linad_solve()` (489) and `linad_gram_solve()` (2289).** The same node model
   fitted two ways -- from raw data and from a Gram. **These must agree and
   nothing tests that they do.** This is the highest-value missing test in the
   codebase.
4. **`linad_ridge_edf()` (139).** Effective degrees of freedom for `node_test`.
   New today. Verify `tr((G+D)^-1 G)` against a direct computation.
5. **`linad_child()` (1161) and `linad_propose()` (1382).** Where weights,
   gradients, line-search steps and accumulation meet. The most state per line
   of any function here.
6. **Classification throughout.** Every defect found so far was found on
   regression data.

---

## 6. Defects found so far, as a taxonomy

The value of this list is predictive: each is a *kind* of error, and the kinds
recur.

**A. The search scores a model the commit will not build.** Three instances.
(i) The exhaustive search scored candidates by child models that violated the
minimum-cases floor, so three of six leaves came back identical to their
parents. (ii) `node_test` applied at the commit but not in the search. (iii)
Latent in the `model_features` design. *Mitigation adopted: one shared function,
not two agreeing implementations.*

**B. A theorem applied outside its hypothesis.** The exhaustive search ordered
factor levels by mean residual and cut contiguously -- optimal for the *mean*
(Fisher 1958), not for child linear models. On a three-level case it chose the
worst of six partitions, 6.8x the best. **Nothing about the code looked wrong.**
Every borrowed CART result deserves this audit: the greedy search, the surrogate
convention, impurity importance were all proved for constant leaves.

**C. A criterion charging the wrong number of parameters.** `node_test` counted
nonzero coefficients; ridge shrinks but never zeroes, so at 117 features it was
charged 117 and BIC demanded a 25x reduction in residual sum of squares. Every
node fell back to a constant, the root included, and the model silently became
CART. **The failure was shaped like a result.**

**D. A knob that changes something it does not name.** `node_test` also relaxed
the split floor, because that seemed principled. `min_cases_leaf` defaults to 1,
so enabling a model-selection rule dropped the split floor from 10 to 1 and
trees spent their leaf budget on singleton leaves. Test R2 0.545 -> 0.154 on
real data. **And every measurement taken of that feature before the fix compared
floor-against-floor rather than rule-against-rule**, so the published numbers
were all wrong.

**E. A degenerate case treated as an error.** `is_constant()` is TRUE of a single
value, so a one-case node was never fitted in any mode and inherited its parent.

**F. Silent length coercion.** `rep(penalty, d)` produced an over-long vector
whenever forward selection passed one penalty per active column;
`diag(x, n)` truncated it back to correct, so it worked by accident for months
and surfaced only when the surrounding code was rewritten.

**G. Framework-level.** An S7 setter called at construction with the class
prototype; a removed variable breaking two unrelated algorithms; a gate's error
message padding its allowed values.

### What the failure modes have in common

Almost none of them error. Almost none change a test result. **Several improve
or barely move an accuracy benchmark while being wrong** -- D moved RMSE against
the true function from 0.910 to 0.906 while the tree was spending half its leaf
budget on one-case leaves. Accuracy benchmarks filter wrong *answers*, not wrong
*structure*.

---

## 7. Running things

```sh
just install          # runs document, which runs format; read every  or ! line
just test             # ~9 min, currently FAIL 0 | WARN 38 | SKIP 3 | PASS 22225
Rscript data-raw/audit_props.R        # documented contracts vs property specs
just check-rd; just lint; just spell; just schemas-check
```

For interactive work, `devtools::load_all()` exposes every internal.

```r
mod <- train(dat, hyperparameters = setup_LINAD(), verbosity = 0L)
mod@model@frame          # one row per node: parent, children, n, split, loss
mod@model@coefficients   # nodes x design width, ACCUMULATED down each path
rtemis:::linad_check_tree(mod@model)   # character(0) when clean
draw_linad(mod@model)
```

Design and rationale live in `plan/linad.md` (dated Log, most recent last).
Findings and post-mortems live in `~/DevLog/rtemis/2026-08-*.md`. The paper's
framing of the algorithmic questions is in `~/Papers/2026/linad/`.

---

## 8. What would most improve confidence

Done since this brief was drafted, in `tests/testthat/test_LINAD_differential.R`
(109 assertions, 1.2s):

1. **The exhaustive search against a reference written from the definition** --
   explicit per-row weights for each side, a fit per side, the loss taken from
   the fitted values. No incremental sufficient statistics, no subtraction of
   node totals, no algebraic loss identity, and for constant leaves not even the
   solver. Six configurations on mixed numeric/factor data, three floor sizes,
   and twelve randomized problems varying shape, weights, membership and
   hyperparameters. **A reviewer should attack this reference first**: if it is
   wrong in the same direction as the implementation, it proves nothing.
2. **`linad_solve()` vs `linad_gram_solve()`**, ten randomized problems x three
   node models. They fit the same model by different routes -- one centers the
   design and carries the level separately, the other solves jointly with an
   intercept column -- and a divergence is a search optimizing a model that is
   never built.
3. **`linad_gram_loss()` and `linad_ridge_edf()` against the quantities they
   stand for**, and `linad_check_tree()` gained an optional `min_cases_child`,
   which is the invariant defect D violated.

Still open:

4. **Classification parity** -- every reduction and identity is currently tested
   on regression, and every defect found so far was found there. Note that
   `linad_solve()` and `linad_gram_solve()` are *expected* to disagree for
   classification, since the commit's constant is a Newton step and the search's
   is a weighted mean; whether that difference is intended is worth a reviewer's
   attention.
5. **A test that `gamma` changes what it should and nothing else.**
6. **Numeric invariants in `linad_check_tree()`** beyond the floor: that each
   leaf's coefficients reproduce its fitted values, and whether loss is
   non-increasing down a path (unverified -- do not assume it).
