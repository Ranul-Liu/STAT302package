#' T-test function
#'
#' This function does one sample t-test.
#'
#' @param x Numeric vector of data to be tested.
#' @param alternative A string specifying the alternative hypothesis,
#'   only accept "two.sided", "less", or "greater".
#' @param mu The null hypothesis of the mean of \code{x}.
#' @keywords inference
#'
#' @return A list of t-test parameters, includes \code{test_stat},
#'   \code{df}, \code{alternative}, and \code{p_val}.
#'
#' @examples
#' my_t.test(my_gapminder$lifeExp, "two.sided", 60)
#' my_t.test(my_gapminder$lifeExp, "less", 60)
#' my_t.test(my_gapminder$lifeExp, "greater", 60)
#'
#' @export
my_t.test <- function(x, alternative, mu) {
  if (!is.numeric(x) | !is.numeric(mu)) {
    stop("Please give numeric input for x and mu")
  } else if (!(alternative %in% c("two.sided", "less", "greater"))) {
    stop("Alternative hypothesis should be two.sided, less or greater")
  } else {
    sample_mean <- mean(x)
    sample_sd <- sd(x)
    df <- length(x) - 1
    t <- (sample_mean - mu) / (sample_sd / sqrt(length(x)))
    p_less <- pt(t, df, lower.tail = TRUE)
    p_greater <- pt(t, df, lower.tail = FALSE)
    if (alternative == "less") {
      p <- p_less
    } else if (alternative == "greater") {
      p <- p_greater
    } else {
      p <- 2 * min(p_less, p_greater)
    }
    toReturn <- list(round(t, 3), df, alternative, round(p, 3))
    names(toReturn) <- c("test_stat", "df", "alternative", "p_val")
    return(toReturn)
  }
}
