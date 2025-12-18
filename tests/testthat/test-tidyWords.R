test_that("trailing spaces", {
  expect_equal(tidyWords("abc"), tidyWords(" abc "))
})
test_that("hyphens", {
  expect_equal(tidyWords("ab c"), tidyWords("ab-c"))
})
test_that("capitalisation", {
  expect_equal(tidyWords("AbC"), tidyWords("abc"))
})

test_that("non utf", {
  expect_equal(tidyWords("[ ¹⁸ F]AlF-NOTA-FAPI-04"),
               "falf nota fapi 04")
})

