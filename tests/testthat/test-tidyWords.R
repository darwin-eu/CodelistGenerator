test_that("trailing spaces", {
  expect_equal(tidyWords("abc"), tidyWords(" abc "))
})
test_that("hyphens", {
  expect_equal(tidyWords("ab c"), tidyWords("ab-c"))
})
test_that("capitalisation", {
  expect_equal(tidyWords("AbC"), tidyWords("abc"))
})
