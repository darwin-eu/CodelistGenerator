test_that("trailing spaces", {
  expect_equal(clean_words("abc"), clean_words(" abc "))
})
test_that("hyphens", {
  expect_equal(clean_words("abc"), clean_words("ab-c"))
})
