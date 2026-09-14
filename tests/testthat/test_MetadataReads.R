# test_MetadataReads.R
# ::rtemis::
# 2026- EDG rtemis.org

# %% Setup metadata ----
test_that("setup and wire metadata do not reconstruct property declarations", {
  Constant <- schema_class(
    "MetadataReadConstant",
    properties = list(fixed = prop_const("fixed"))
  )
  # Rebuilding a PropertySpec validates the complete declaration recursively.
  # Runtime reads only need the fields already validated by its factory.
  local_mocked_bindings(
    get_spec = function(...) {
      stop("Runtime metadata reconstructed a PropertySpec")
    }
  )
  expect_s7_class(setup_LightGBM(), LightGBMHyperparameters)
  expect_s7_class(setup_CART(), CARTHyperparameters)
  expect_s7_class(setup_Preprocessor(), PreprocessorConfig)
  expect_s7_class(setup_FutureExecution(n_workers = 1L), FutureExecutionConfig)
  cls <- LightGBMHyperparameters
  expect_identical(constant_spec_names(Constant), "fixed")
  expect_true("learning_rate" %in% tunable_spec_names(cls))
  expect_true("learning_rate" %in% spec_prop_names(cls))
  expect_false("learning_rate" %in% fixed_spec_names(cls))
  expect_identical(prop_role(cls@properties[["learning_rate"]]), "config")
  expect_true(prop_serialized(cls@properties[["learning_rate"]]))
  expect_false(prop_serialized(Constant@properties[["fixed"]]))
  expect_identical(
    wire_value(0.1, cls@properties[["learning_rate"]]),
    0.1
  )
  expect_error(setup_LightGBM(learning_rate = -1), "learning_rate")
})
