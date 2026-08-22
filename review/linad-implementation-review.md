# LINAD implementation review

Reviewed 2026-08-22 at commit `e10f7ff9` (`HyperparametersSet`).

## Executive summary

I found one critical defect, four high-impact correctness defects, three
medium-impact defects, and several additional contract and validation gaps. The
most serious defect is
outside the numerical solver: factor encoding is not persisted with the fitted
model. An ordered factor is encoded with polynomial contrasts during training
but silently rebuilt with treatment contrasts during prediction. More generally,
changing R's global contrast option after fitting silently changes predictions.
On a three-level reproduction, predictions on the same rows changed by as much
as 15.01.

The main algorithmic defect is the open classification gap identified in the
brief: the exhaustive search and the commit do not fit the same child model.
This is observable in split choice, not just coefficients. On a fixed 80-case
problem, the exhaustive search selected a split whose committed logistic loss
was 51.89 although another admissible split achieved 47.62.

The other high-impact findings are that `node_selection = "global"` is
algebraically identical to `"local"`, and classification initialization jumps
from the logistic-loss minimizer at `root_learning_rate = 0` to the least-squares
mean of the `{-1, +1}` labels for every positive rate.

The focused regression-oriented checks are strong and passed. They do not cover
the configurations in which the high-impact defects occur.

## Scope and method

I reviewed:

- `R/linad.R`
- `R/train_LINAD.R`
- `R/linad_forest.R`
- `R/train_LINADForest.R`
- the LINAD properties in `R/070_Hyperparameters.R`
- the fitted classes in `R/200_Supervised.R`
- `tests/testthat/test_LINAD.R`
- `tests/testthat/test_LINAD_differential.R`
- `tests/testthat/test_LINADForest.R`
- relevant rationale in `plan/linad.md`

I traced the search/commit path, independently evaluated committed losses for
classification candidates, exercised factor design-matrix round trips, checked
selected-tree consumers, and tested scalar-shape boundaries in the forest.

No package code, documentation, tests, or configuration was edited. This report
is the only file I created.

## Findings

### 1. Critical: factor encoding is not persisted, causing silently wrong predictions

Locations: `R/linad.R:2161-2183`, `R/train_LINAD.R:96-120`,
`R/train_LINAD.R:197-202`, `R/linad_forest.R:124-149`, and
`R/200_Supervised.R:3134-3146`.

`linad_design_matrix()` records factor levels through `xlev`, but it does not
record or force the contrasts used by `model.matrix()`. The fitted class stores
levels, design assignments, and scales, but not a terms object, contrasts, or a
design-column contract.

There are two direct failure modes.

1. Ordered factors fail immediately. During training, an ordered factor uses
   R's default `contr.poly`. At prediction, the loop that reconstructs factors
   calls `factor(...)`, dropping the ordered class, so the same factor is encoded
   with `contr.treatment`. In a fixed three-level example the training columns
   were `(Intercept), g.L, g.Q, x`, the prediction columns were
   `(Intercept), gmid, ghigh, x`, and the maximum difference between the fitted
   function in the training basis and `predict()` was 10.50.
2. Ordinary factors depend on mutable process-global state. Fitting under
   `contr.treatment` and predicting the same rows after switching
   `options(contrasts = c("contr.sum", "contr.poly"))` changed the maximum
   prediction by 15.01 and the mean absolute prediction by 7.00. The stored
   columns were `(Intercept), gb, gc, x`; prediction rebuilt
   `(Intercept), g1, g2, x`. Multiplication is positional, so the mismatch is
   silent.

This affects LINAD and every tree in LINADForest. It also breaks save/load
reproducibility across sessions whose contrast options differ. The existing
saveRDS test uses the same session and contrast settings, so it cannot detect
the problem.

Recommended correction: make the encoding part of the model contract. Given the
documentation's explicit claim of reference coding, the simplest coherent
choice is to force treatment contrasts for every factor during both training and
prediction, including ordered factors. Alternatively store and replay the
training terms/contrasts object. In either case, compare rebuilt design column
names with the stored coefficient column names and abort on any mismatch rather
than multiplying by position.

Required tests:

- ordered-factor fitted values equal predictions on the training rows;
- predictions survive a change to global contrast options;
- custom factor contrasts either round-trip or are explicitly overridden;
- the same checks for LINADForest and after saveRDS/readRDS.

### 2. High: classification exhaustive search scores a different model from the commit

Locations: `R/linad.R:377-444`, `R/linad.R:489-630`,
`R/linad.R:2312-2385`, and `R/linad.R:2443-2511`.

For the default classification rule, the committed child uses
`linad_constant(..., rule = "closed_form")`, which is a Newton step based on the
gradient and Hessian. The exhaustive search has only `X'WX`, `X'Wr`, and
`sum(w r^2)` and gives the child the weighted least-squares intercept
`Xty[1] / G[1,1]`. Its slopes are consequently fitted around a different level.

The differential solver test at `tests/testthat/test_LINAD_differential.R:316`
uses regression only. Its comment says the two routes must fit the same node
model, but that invariant does not hold for the default classification path.

This is large enough to alter the chosen split. In a fixed reproduction with
`seed = 1`, 80 cases, two numeric predictors, ridge nodes, `lambda = 0.1`,
`gamma = 0.3`, `line_search = "none"`, and all numeric cuts available:

- `linad_sweep()` chose feature `a` at `-0.6576048`;
- the committed logistic loss of that split was 51.89203;
- feature `a` at `0.5848413` committed to a loss of 47.62273.

On the same problem, a direct committed ridge node and its Gram-scored version
differed by 0.166 in their coefficients; their constant/intercept quantities
were `-0.49231` and `-0.26021`, respectively.

The consequence is stronger than suboptimal ranking. `linad_propose()` commits
only the search winner. If that winner does not improve the actual loss, the
node can close without trying another candidate that would improve it.

Recommended correction: extend the exhaustive sufficient statistics so its
classification scorer can construct the same constant and slopes as
`linad_solve()`, then score the quantity the search contract names. At minimum,
the search and commit must share a classification-capable child-model builder.
If least-squares scoring is retained intentionally, rename and document it as a
surrogate rather than calling it the loss after fitting the committed models.

Required tests:

- classification `linad_solve()` versus the search-side child builder for both
  constant rules and every supported node model;
- every classification candidate scored by an explicit committed-model
  reference;
- randomized classification split rankings, including `gamma > 0`, node tests,
  and both line-search scopes.

### 3. High: `node_selection = "global"` is an inert hyperparameter

Location: `R/linad.R:1443-1470`.

The global branch starts with `after <- f`, replaces predictions only for
`index_left` and `index_right`, and subtracts the before/after loss over all
rows. Those two indexes partition exactly the current node. Every row outside
the node is unchanged and cancels from the subtraction. The expression is
therefore algebraically identical to the local branch:

```text
node loss - (left child loss + right child loss)
```

The code comment and `plan/linad.md:183-187` say the global criterion must
extrapolate the node's model to cases it does not contain. The current assignment
does not change those cases, so it does not implement that criterion.

A fixed seven-leaf classification fit produced identical frames and a maximum
coefficient difference of exactly zero between local and global selection. This
is not data-dependent; it follows from the cancellation above.

Recommended correction: define the intended global before/after predictions
explicitly over all cases, including how an out-of-node case chooses a proposed
child, and implement that definition. If no distinct global criterion is wanted,
remove the hyperparameter rather than expose an inert ablation.

Required test: construct at least two frontier nodes for which the intended local
and global rankings differ, and assert the selected node under each rule.

### 4. High: classification root initialization is discontinuous and is not the loss-minimizing constant

Locations: `R/linad.R:1662-1717`, `R/linad.R:342-374`, and
`R/200_Supervised.R:3114-3118`.

For every positive `root_learning_rate`, `linad_fit()` calls `linad_solve()` with
`type = "Regression"`. The root constant is therefore the weighted mean of the
`{-1, +1}` labels. At exactly zero, the fit is skipped and
`linad_baseline(..., "Classification")` returns half the log odds, the minimizer
of the classification loss.

For an 80% positive outcome with a constant node model:

| `root_learning_rate` | raw root | predicted positive probability |
|---:|---:|---:|
| 0 | 0.6931472 | 0.8000000 |
| 1e-12 | 0.6000000 | 0.7685248 |
| 0.5 | 0.6000000 | 0.7685248 |
| 1 | 0.6000000 | 0.7685248 |

This contradicts the fitted-class documentation that the root node value is the
constant minimizing the loss, and it contradicts `plan/linad.md:338-343`, which
defines the root as `c* + rate * (l(x) - c*)` with the classification `c*` equal
to half the log odds. It also makes an intercept-only classification model
miscalibrated at the root.

Recommended correction: compute the classification baseline independently of
the root slope fit and always retain it as the root constant; shrink only the
root slopes around that baseline, as the design rationale specifies.

Required tests:

- imbalanced classification at rates 0, a value close to zero, 0.5, and 1;
- intercept-only predicted probability equals weighted prevalence;
- root node value is invariant across rates for both outcome types.

### 5. High: LINADForest prediction and standard errors fail for one new case

Locations: `R/linad_forest.R:266-283`, `R/train_LINADForest.R:245-253`, and
`R/train_LINADForest.R:313-318`.

`linadforest_tree_predictions()` uses `vapply()` with
`numeric(NROW(newdata))`. When `NROW(newdata) == 1`, `vapply()` simplifies the
result to a numeric vector rather than a one-row matrix.

Observed on a three-tree forest:

- `predict(model, one_row)` errors with
  `'x' must be an array of at least two dimensions` in `rowMeans()`;
- `se(model, one_row)` errors with `argument is of length zero` because
  `ncol(predictions)` is `NULL`.

Recommended correction: construct the cases-by-trees result with a guaranteed
matrix shape, including the one-case and one-tree boundaries.

Required tests: regression and classification prediction and `se()` for exactly
one row, with one tree and with multiple trees.

### 6. Medium: split-gain importance includes descendants pruned from the selected tree

Location: `R/train_LINAD.R:241-279`.

The model retains the fully grown frame after validation selects a smaller tree.
`varimp_super()` correctly reads linear coefficients from the selected terminal
nodes. Its split-gain path, however, defines internal nodes as every row having a
left child except nodes that are themselves in the selected terminal set.
Internal descendants below a selected terminal are not terminal IDs, so they
are included even though the selected tree cannot reach them.

In a fixed seven-leaf fit, setting the selected size to one leaf should make
split gain exactly zero. `varimp_super()` instead reported total split gain
150.5518 from descendants below the root.

This affects ordinary LINAD models selected on validation data and is especially
likely in LINADForest, where each tree normally selects its size from OOB cases.
The documentation explicitly says both importance measures describe the selected
size.

Recommended correction: restrict split-gain nodes to the ancestors of the
selected terminal nodes, stopping traversal at those terminals. The analogous
logic already exists in `draw_linad()`.

Required tests: force a grown tree to each smaller recorded size and compare
split gain with a reference traversal of only the reachable selected tree.

### 7. Medium: equal-width cut placement ignores the feature maximum

Location: `R/linad.R:763-797`.

For width-based thinning, the code sets `values <- sorted[breaks]` and spans its
targets from the first to the last of those values. The last admissible break's
left value is not the feature maximum. On a feature with a sparse upper tail,
the range can therefore end before the tail begins.

For `sorted = c(0, 1, 2, 100)`, `breaks = 1:3`, and two requested cuts, the width
path returns only position 2, corresponding to a threshold of 1.5. A grid over
the documented feature range 0 to 100 places both nominal edges in the gap
between 2 and 100 and should collapse to the admissible split after position 3.

The current test checks only that width and frequency differ and that the chosen
width positions look more evenly spaced on one random sample. It does not pin
the endpoints of the width grid.

Recommended correction: define width targets over the full observed feature
range and map those targets to admissible thresholds, not to the left observed
value alone.

Required tests: hand-worked skewed vectors with a large final gap, repeated
values, and target edges that collapse into one admissible split.

### 8. Medium: numeric exhaustive search tries `n_cuts - 1` cuts

Locations: `R/linad.R:2548-2556` and `R/070_Hyperparameters.R:1763-1769`.

`n_cuts` is documented as the number of cut points tried per feature. The
numeric exhaustive path passes `state[["n_cuts"]] - 1L` to
`linad_cut_positions()`. The factor path uses `n_cuts` itself as its partition
budget. The rationale at `plan/linad.md:802-807` also says numeric features are
thinned to `n_cuts` candidates so numeric and factor features spend the same
budget.

The differential reference repeats `n_cuts - 1L`, so it proves agreement with
the implementation rather than the documented contract.

Recommended correction: pass `n_cuts` to the thinning function, or rename and
redocument the hyperparameter as a bin count. The current factor budget and plan
both support the former.

Required test: assert the exact number of retained numeric candidates when at
least `n_cuts` admissible positions exist.

## Additional design and hardening issues

### Elastic-net exhaustive search is a ridge surrogate, not an exhaustive search of the fitted model

Location: `R/linad.R:2312-2359`.

`linad_gram_solve()` sends every non-forward, non-constant model through the
ridge Cholesky solve. For `node_model = "elasticnet"`, candidate scoring ignores
`alpha` and does not fit the model that `linad_child()` will commit through
`glmnet`. The internal comment discloses this, but the public `split_search`
contract says exhaustive scoring fits both child models and gives no exception.

This is the same search/commit defect family as the classification finding,
although it is currently deliberate. It should either be implemented with the
actual elastic-net child fit or exposed and named as a surrogate search. Tests
should show how often the surrogate changes the winning split and whether
`alpha` can change the search ranking when it should.

### Search and commit use different active rows after deep soft weighting

Locations: `R/linad.R:1204-1217` and `R/linad.R:2451-2455`.

The commit drops rows below `LINAD_WEIGHT_TOLERANCE` relative to the largest
child weight. The exhaustive search forms its total and side Grams from all rows
without the same threshold. Once `gamma^depth` crosses the tolerance, the search
again scores a model the commit will not fit. Individual omitted weights are
small, but the discrepancy can aggregate on large data and is exactly the kind
of silent divergence the shared-scorer design is intended to prevent.

Apply the same active-row rule to search and commit, and add a differential test
whose weights straddle the tolerance.

### Resolved defaults are not carried by the fitted model

Location: `R/linad.R:2268-2308`.

Several operational defaults are resolved only in `linad_settings()` while the
hyperparameter object remains `NULL`; the plan itself calls the default
`n_cuts = 20` invisible to the schema. A serialized fitted run therefore does
not directly record all values it used. Reproducing it requires knowing the
package version's resolution rules. This is at odds with the stated auditable
and reproducible output goal.

Consider storing a resolved, read-only settings record on the fitted model or
the run result while retaining the user-facing gated hyperparameter object
unchanged.

### Case weights are normalized without LINAD-side validation

Locations: `R/train_LINAD.R:68-94` and `R/train_LINADForest.R:115-152`.

The reviewed methods do not validate weight length, finiteness, nonnegativity,
or a positive finite mean before dividing by `mean(weights)`. Some validation
may belong in the generic training layer because other algorithms share the
issue, but LINAD's numerical routines assume all of these properties. Invalid
weights should fail early with a corrective rtemis error rather than propagate
through weighted Grams and Cholesky failures.

### Fitted S7 classes do not validate relational invariants

Locations: `R/200_Supervised.R:3126-3148` and
`R/200_Supervised.R:3205-3223`.

`LinearAdditiveTree` and `LINADForest` declare primitive property classes but no
class validators. Construction does not check that frame rows match coefficient
rows, coefficient columns match the recorded design, steps contain valid node
IDs, bag-count columns match trees, or OOB prediction length matches bag-count
rows. `check_is_S7()` after construction verifies the class, not those
relationships, and `linad_check_tree()` is test-only and covers only some of
them.

Adding validators would turn future internal assembly defects into early,
corrective errors and align the output classes with the package's stated S7
policy.

### Printed node count and depth describe the fully grown tree, not the selected tree

Location: `R/200_Supervised.R:3152-3184`.

After validation selects a smaller tree, `print.LinearAdditiveTree()` reports
`n_leaves` from the selected tree but `nrow(frame)` and maximum depth from the
fully grown tree, including unreachable descendants. This does not change
predictions, but it makes the summary internally inconsistent. The displayed
node count and depth should be restricted to selected terminals and their
ancestors, as `draw_linad()` already does.

## Test results

The following non-mutating checks were run against the working tree:

- `test_LINAD_differential.R`: 113 passed, 0 failed.
- `test_LINAD.R`: 226 passed, 0 failed.
- `test_LINADForest.R`: 27 passed. Its one parallel-dispatch test could not run
  because the sandbox denied `mirai` permission to start a dispatcher; the
  sequential half passed. Running with `stop_on_failure = FALSE` confirmed the
  remaining forest assertions pass.

I also started the full `just test` recipe. It encountered the same sandbox
restriction in unrelated parallel tests and was stopped rather than treating
environmental failures as implementation evidence.

The passing differential tests provide good evidence for the regression Gram
algebra, factor partition enumeration within its budget, ridge effective degrees
of freedom, and the regression search/commit identity. They do not weaken the
findings above because the missing boundaries are explicit:

- classification is absent from the solver and exhaustive references;
- factor encoding is tested only under one contrast configuration with unordered
  factors;
- `node_selection = "global"` is checked only for structural soundness;
- root-rate tests use regression;
- forest tests predict batches, not one row;
- variable importance is not tested after selecting a smaller tree;
- width tests do not pin the full-range endpoints;
- the `n_cuts - 1` behavior is copied into the reference.

## Suggested correction order

1. Persist or force factor encoding and assert design-column identity at
   prediction.
2. Make classification exhaustive scoring build and evaluate the committed
   child model.
3. Repair or remove the inert global node-selection option.
4. Make the classification root constant the loss-minimizing baseline for every
   root learning rate.
5. Stabilize the forest cases-by-trees matrix shape for one-row prediction.
6. Restrict split gain and printed structure to the selected tree.
7. Correct width endpoints and the `n_cuts` off-by-one contract.
8. Resolve or explicitly expose the elastic-net surrogate and the deep-weight
   search/commit discrepancy.
9. Add resolved-settings records, weight checks, and fitted-class validators.
