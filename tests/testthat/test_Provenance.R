# test_Provenance.R
# ::rtemis::
# 2026- EDG rtemis.org

test_that("provenance describes the captured implementation", {
  Observed <- S7::new_class(
    "ObservedImplementation",
    properties = list(session_info = S7::class_list)
  )
  captured <- list(
    R.version = list(version.string = "recorded runtime"),
    otherPkgs = list(rtemis = list(Version = "recorded package")),
    platform = "recorded platform"
  )
  observed <- provenance_of(Observed(session_info = captured))
  expect_identical(observed@implementation@name, "rtemis")
  expect_identical(observed@implementation@language, "r")
  expect_identical(observed@implementation@version, "recorded package")
  expect_identical(observed@implementation@language_version, "recorded runtime")
  expect_identical(observed@platform, "recorded platform")
  captured[["loadedOnly"]] <- captured[["otherPkgs"]]
  captured[["otherPkgs"]] <- NULL
  expect_identical(
    provenance_of(Observed(session_info = captured))@implementation@version,
    "recorded package"
  )
  current <- provenance_of(Observed(session_info = list()))@implementation
  expect_identical(
    current@version,
    as.character(utils::packageVersion("rtemis"))
  )
  expect_identical(current@language_version, R.version.string)
})


test_that("implementation identity is a portable typed component", {
  python <- Implementation(
    name = "rtemis-ml",
    version = "0.1.0",
    language = "python",
    language_version = "3.13"
  )
  wire <- S7_to_list(Provenance(implementation = python))
  expect_identical(wire[["implementation"]][["language"]], "python")
  expect_false(any(c("r_version", "rtemis_version") %in% names(wire)))
  expect_error(Implementation(name = 1))
  expect_error(Provenance(implementation = "python"))
  schema <- S7_to_JSONSchema(
    Implementation,
    id = "https://schema.rtemis.org/implementation/v1/schema.json"
  )
  restored <- JSONSchema_to_S7(schema, name = "RestoredImplementation")
  expect_identical(
    S7_to_list(do.call(restored, S7_to_list(python))),
    S7_to_list(python)
  )
})
