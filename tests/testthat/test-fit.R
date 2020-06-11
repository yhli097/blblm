test_that("fit function lm1 works by different methods", {
  data_list = split_data(mtcars, 3)
  data = data_list$`1`
  freq = rmultinom(1, nrow(mtcars), rep(1, nrow(data)))
  expect_equal(lm1(mpg ~ wt * hp, data, freq, "lm"), lm1(mpg ~ wt * hp, data, freq, "lmR"))
  expect_equal(lm1(mpg ~ wt * hp, data, freq, "lm"), lm1(mpg ~ wt * hp, data, freq, "lmC"))
})
