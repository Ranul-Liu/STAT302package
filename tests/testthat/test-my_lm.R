test_that("my_lm works", {
  estimate <- c(4.788852e+01, 4.452704e-04, 1.359272e+01, 8.657793e+00,
                1.757234e+01, 1.814604e+01)
  names(estimate) <- c("(Intercept)", "gdpPercap", "continentAmericas",
                       "continentAsia", "continentEurope", "continentOceania")
  standarError <- c(3.398053e-01, 2.349795e-05, 6.007856e-01, 5.554859e-01,
                    6.257430e-01, 1.787426e+00 )
  names(standarError) <- c("(Intercept)", "gdpPercap", "continentAmericas",
                           "continentAsia", "continentEurope", "continentOceania")
  t <- c(140.92927, 18.94933, 22.62491, 15.58598, 28.08236, 10.15205)
  names(t) <- c("(Intercept)", "gdpPercap", "continentAmericas",
                "continentAsia", "continentEurope", "continentOceania")
  p <- c(0.000000e+00, 8.552893e-73, 2.822476e-99, 2.719424e-51,
         7.595526e-143, 1.502557e-23)
  names(p) <- c("(Intercept)", "gdpPercap", "continentAmericas",
                "continentAsia", "continentEurope", "continentOceania")
  result <- round(cbind(estimate, standarError, t, p), 3)
  colnames(result) <- c("Estimate", "Std. Error", "t value", "Pr(>|t|)")
  expect_equal(my_lm(lifeExp ~ gdpPercap + continent, my_gapminder),
               as.table(result))
})
