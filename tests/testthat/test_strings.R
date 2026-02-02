# test_strings.R
# ::rtemis::
# 2025 EDG rtemis.org

# repr_ls ----
x <- list(
  a = 1:5,
  b = letters[1:5],
  c = rnorm(5)
)
out <- repr_ls(x, title = "Test List")
test_that("repr_ls() works", {
  expect_true(is.character(out))
})

## Long list ----
x <- list(
  a = 1:100,
  b = letters[1:100],
  c = iris,
  d = sample(letters, 100, replace = TRUE),
  e = runif(100),
  f = setup_Preprocessor(),
  g = rpois(100, 2),
  h = rbinom(100, 10, 0.5),
  i = setup_PCA(),
  j = rnorm(100),
  k = rnorm(100),
  l = setup_LightCART()
)

test_that("repr_ls() handles long lists", {
  expect_true(is.character(repr_ls(x, limit = 5L)))
  expect_true(is.character(repr_ls(x, limit = -1L)))
})
