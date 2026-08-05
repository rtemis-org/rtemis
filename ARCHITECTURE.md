# rtemis architecture

Orientation for contributors: the shape of the package, where things live, and
the invariants that are easy to break by accident. The code is the authority;
this file exists so a newcomer does not have to derive the layout from scratch.

## Shape

Two faces, deliberately different:

- **The user-facing API is functional.** `train()`, `cluster()` and `decomp()`
  take data plus configuration objects and return a result. Configuration is
  built by `setup_*()` functions — `setup_LightGBM()`, `setup_Resampler()`,
  `setup_Preprocessor()` — never by hand-constructing classes.
- **The internals are object-oriented.** Everything is an S7 class with
  declared, validated properties; behaviour is attached with `S7::method()`.

The seam between them is the `setup_*` function: it turns a user's partial,
convenient input into a complete, validated object. `read_config()` reconstructs
through `do.call(setup_*, x)` for the same reason, so a config from any source
arrives fully resolved.

## Dispatch: the `Hyperparameters` subclass names the algorithm

`setup_LightGBM()` returns a `LightGBMHyperparameters`, and `train()` dispatches
to `method(train_, LightGBMHyperparameters)`. There is no algorithm string to
keep in sync and no ambiguity about which method runs.

Consequently `train()`'s signature does not grow when algorithms are added: a
new algorithm is a new subclass plus a new method. `specs/supervised.md` is the
step-by-step contract for adding one.

Config families with variants (resamplers, decompositions, clusterers) follow the
same pattern — a base class, subclasses carrying a constant discriminator via
`prop_algorithm()`, and a dispatcher schema keyed on it.

## Properties are declared, not hand-validated

Every property is built by a `prop_*` factory (`R/010_Props.R`), which attaches a
machine-readable `PropertySpec`. One declaration produces the R validator, the
default, and the published JSON Schema fragment — constraints are data, not
closures, which is what makes them mechanically convertible.

Do not hand-write a validator for something a factory can express, and do not
introduce a bare `class_list` / `class_any` property: if a value has no wire
form it is marked `prop_r_only()` (as `Supervised@model` is, holding a fitted
backend object), and otherwise it gets a declared container and element type.

## Published artifacts

rtemis generates machine-readable contracts to `schema.rtemis.org` from the S7
classes. `data-raw/schema_registry.R` says which class backs which schema;
`data-raw/generate_schemas.R` and `data-raw/generate_defaults.R` emit them.

Three kinds, and they are **not** one schema at different strictness levels:

| Artifact | States |
|---|---|
| `<family>/v1/schema.json` | what a valid **config** looks like — a request, partial by nature |
| `<family>/v1/record.json` | what a valid **record** looks like — a complete statement of what ran |
| `defaults/v1/defaults.json` | what a user **gets when they supply nothing** |

The rule that keeps them apart, and the one most likely to be broken by a
well-meaning change:

> **A schema states what is true of the data. It never states what any interface
> chooses to fill in.**
>
> Test: if a default changes, which files change? Only `defaults.json`.

So a config schema requires nothing beyond the keys that carry the document's
shape — the discriminator, plus the payload key holding the variant's fields
where a family nests them — and never carries a `default` keyword.
`data-raw/schema_contract.R` asserts this on every config schema before it is
written, and `tests/testthat/test_SchemaContract.R` checks the same contract
from the class side.

Run state is the one thing both kinds declare: a property built with
`prop_state()` (GLMNET's `lambda.min`) appears in the config schema marked
`readOnly`, because only a run can produce it. What a record adds is that it
*requires* it.

**Regenerate, never hand-edit, anything under the schema repo.** A hand-edited
schema detaches from its source and the next regeneration silently reverts it.

## Repository layout

```
R/                  numbered files first (010_Props, 020_Props_read, 030_init, ...),
                    then unnumbered (train.R, train_*.R, draw_*.R, utils_*.R)
data-raw/           schema_registry.R + the two generators; audit_props.R
specs/              implementer contracts
plan/               work plans with dated Logs
tests/testthat/
inst/, man/, tools/
```

**Load order is filename order.** There is no `Collate` field in `DESCRIPTION`,
and S7 evaluates a property's class union *at definition time* — so a class can
only reference classes defined in an earlier-sorting file. The numeric prefixes
on `R/0*.R` and `R/1*.R` encode that dependency DAG. Renumbering those files is a
real change; check what references what first.

`R/030_init.R` defines the generics (`repr`, `describe`, `present`, `features`,
`outcome`, ...). New generics go there, not beside their first method.

## Logging and run state

Logging goes through `rtemis.core`: `msg()`, `info()`, and `abort()` with typed
rtemis condition classes. Every function that can print takes `verbosity`.

The run timeline is a tree: `node_enter()` / `node_exit()` bracket each stage and
nest, so a `train()` called inside a `train()` lands in the right place. The
result is a `SupervisedSession` stored on the fitted object, alongside a
`DataFingerprint` identifying the training data.

## Working on it

Use the `justfile`: `just format`, `just document`, `just test`, `just install`,
`just check-cran`.

Two things that bite:

- **Cross-repo work loads *installed* rtemis.** Run `just install` before testing
  dependent repos, or a green suite may be testing the previous version.
- **CRAN compliance is continuous**, not a release-time activity. See
  `AGENTS.md`.
