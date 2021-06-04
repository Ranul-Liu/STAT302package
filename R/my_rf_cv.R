#' Random Forest Cross-Validation function
#' 
#' This function does Cross-Validation Random Forest prediction.
#' 
#' @param train A training data frame from my_penguins.
#' @param k_cv An int representing the number of folds in cv.
#' @keywords prediction
#' 
#' @return A number of the mean rate of cross-validation misclassification.
#' 
#' @examples
#' my_rf_cv(rf_train_df, 5)
#' 
#' @export
my_rf_cv <- function(train, k_cv) {
  n <- nrow(train)
  inds <- sample(rep(1: k_cv, length = n))
  train["fold"] <- inds
  MSEs <- 1: k_cv
  for (i in 1: k_cv) {
    data_train <- train %>% filter(fold != i) %>% select(!fold)
    data_test <- train %>% filter(fold == i) %>% select(!fold)
    model <- randomForest(body_mass_g ~ bill_length_mm + bill_depth_mm +
                            flipper_length_mm, data = data_train, ntree = 100)
    prediction <- predict(model, select(data_test, !body_mass_g))
    MSEs[i] <- sum((prediction - data_test$body_mass_g) ^ 2) / length(prediction)
  }
  return(mean(MSEs))
}