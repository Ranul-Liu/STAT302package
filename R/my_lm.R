#' Linear regression function
#'
#' This function does linear regression according to formula and data.
#'
#' @param formula Formula object input used to fit \code{data}.
#' @param data Numeric data frame to be fitted by \code{formula}.
#' @keywords inference
#'
#' @return A table of linear model parameter of \code{formula} based on,
#'   \code{data}, includes \code{Estimate}, \code{Std. Error},
#'   \code{t value}, and \code{Pr(>|t|)}.
#'
#' @examples
#' my_lm(lifeExp ~ gdpPercap + continent, my_gapminder)
#'
#' @export
my_lm <- function(formula, data) {
  myModel <- model.frame(formula, data)
  x <- model.matrix(formula, data)
  y <- model.response(myModel)
  estimate <- solve(t(x) %*% x) %*% t(x) %*% y
  df <- length(y) - length(estimate)
  variance <- sum((y - x %*% estimate) ^ 2) / df
  standarError <- diag(sqrt(abs(variance * solve(t(x) %*% x))))
  t <- estimate / (standarError)
  p <- 2 * pt(abs(t), df, lower.tail = FALSE)
  toReturn <- round(cbind(estimate, standarError, t, p), 5)
  colnames(toReturn) <- c("Estimate", "Std. Error", "t value", "Pr(>|t|)")
  return(as.table(toReturn))
}
