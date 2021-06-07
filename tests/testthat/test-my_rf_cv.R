test_that("my_rf_cv works", {
  train <- na.omit(my_penguins) %>% dplyr::select(body_mass_g, bill_length_mm,
                                                  bill_depth_mm,
                                                  flipper_length_mm)
  expect_type(my_rf_cv(train, 5), "double")
})
