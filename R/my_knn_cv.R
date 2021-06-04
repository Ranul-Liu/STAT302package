#' k-Nearest Neighbors Cross-Validation function
#' 
#' This function does Cross-Validation k-Nearest Neighbors prediction.
#' 
#' @param train The training data frame.
#' @param cl A vector of true class value of \data{train}.
#' @param k_nn An int representing the number of neighbors in knn.
#' @param k_cv An int representing the number of folds in cv.
#' @keywords prediction
#' 
#' @return A list of predicted classes \code{class} and,
#'   the mean rate of cross-validation misclassification error \code{cv-err}.
#' 
#' @examples
#' my_knn_cv(train_df, result_df, 5, 5)
#' 
#' @export
my_knn_cv <- function(train, cl, k_nn, k_cv) {
  n <- nrow(train)
  inds <- sample(rep(1: k_cv, length = n))
  train["fold"] <- inds
  cl <- data.frame("cl" = cl, "fold" = inds)
  mis_rates <- 1: k_cv
  for (i in 1: k_cv) {
    data_train <- train %>% filter(fold != i) %>% select(!fold)
    data_test <- train %>% filter(fold == i) %>% select(!fold)
    cl_train <- cl %>% filter(fold != i) %>% select(!fold)
    cl_test <- cl %>% filter(fold == i) %>% select(!fold)
    prediction <- knn(data_train, data_test, cl_train$species, k = k_nn)
    mis_rates[i] <- 1 - sum(prediction == cl_test$species) / length(prediction)
  }
  return(list("class" = knn(train, train, cl$species, k = k_nn),
              "cv_err" = mean(mis_rates)))
}