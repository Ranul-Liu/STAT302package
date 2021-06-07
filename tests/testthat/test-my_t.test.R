test_that("non-numeric input throws error", {
  expect_error(my_t.test("a", "greater", "b"))
})

test_that("not expected alternative input throws error", {
  expect_error(my_t.test(my_gapminder$lifeExp, "abc", 60))
})

test_that("my_t.test works mathematically", {
  result <- list(-1.68, 1703, "two.sided", 0.093)
  names(result) <- c("test_stat", "df", "alternative", "p_val")
  expect_equal(my_t.test(my_gapminder$lifeExp, "two.sided", 60), result)
  result <- list(1.516, 1703, "less", 0.935)
  names(result) <- c("test_stat", "df", "alternative", "p_val")
  expect_equal(my_t.test(my_gapminder$lifeExp, "less", 59), result)
  result <- list(1.516, 1703, "greater", 0.065)
  names(result) <- c("test_stat", "df", "alternative", "p_val")
  expect_equal(my_t.test(my_gapminder$lifeExp, "greater", 59), result)
})
