test_that("trailing spaces", {
  expect_equal(tidy_words("abc"), tidy_words(" abc "))
})
test_that("hyphens", {
  expect_equal(tidy_words("ab c"), tidy_words("ab-c"))
})
test_that("capitalisation", {
  expect_equal(tidy_words("AbC"), tidy_words("abc"))
})
