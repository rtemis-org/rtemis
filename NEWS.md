# rtemis news

## 1.0.0 First CRAN release

## 1.0.1

- Introduce `VariableImportance` S7 class to represent variable importance data, allowing for more than one measure of importance per model and update all relevant classes and methods.
- Calculate Partial_Effect_Variance as variable importance measure for GAM models
- Add `execution_config` argument to internal `train_` method and use it in LightRuleFit to propagate to LightGBM and GLMNET calls.