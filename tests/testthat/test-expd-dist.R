test_that("expd_dist basic properties hold", {
  skip_if_not_installed("stringdist")
  library(stringdist)
  cn <- c("A", "A1", "B")
  m <- expd_dist(cn, method = "lv")

  # symmetric and zero diagonal (ignore names)
  expect_equal(unname(diag(m)), rep(0, length(cn)))
  expect_equal(unname(m), unname(t(m)))

  # non-negative distances
  expect_true(all(m >= 0))
})
