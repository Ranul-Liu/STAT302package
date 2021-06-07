test_that("my_knn_cv works", {
  train <- na.omit(my_penguins) %>% dplyr::select(body_mass_g, bill_length_mm,
                                                  bill_depth_mm,
                                                  flipper_length_mm)
  cl <- na.omit(my_penguins) %>% dplyr::select(species)
  expect_type(my_knn_cv(train, cl, 5, 5), "list")
  expect_type(my_knn_cv(train, cl, 5, 5)$class, "integer")
  expect_type(my_knn_cv(train, cl, 5, 5)$cv_err, "double")
})
