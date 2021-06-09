test_that("my_lm works", {
  estimate <- c(4.788852e+01, 0.00045, 13.59272, 8.65779,
                17.57234, 18.14604)
  names(estimate) <- c("(Intercept)", "gdpPercap", "continentAmericas",
                       "continentAsia", "continentEurope", "continentOceania")
  standarError <- c(0.33981, 0.00002, 0.60079, 0.55549,
                    0.62574, 1.78743 )
  names(standarError) <- c("(Intercept)", "gdpPercap", "continentAmericas",
                           "continentAsia", "continentEurope", "continentOceania")
  t <- c(140.92927, 18.94933, 22.62491, 15.58598, 28.08236, 10.15205)
  names(t) <- c("(Intercept)", "gdpPercap", "continentAmericas",
                "continentAsia", "continentEurope", "continentOceania")
  p <- c(0.00000, 0.00000, 0.00000, 0.00000,
         0.00000, 0.00000)
  names(p) <- c("(Intercept)", "gdpPercap", "continentAmericas",
                "continentAsia", "continentEurope", "continentOceania")
  result <- cbind(estimate, standarError, t, p)
  colnames(result) <- c("Estimate", "Std. Error", "t value", "Pr(>|t|)")
  expect_equal(my_lm(lifeExp ~ gdpPercap + continent, my_gapminder),
               as.table(result))
})
