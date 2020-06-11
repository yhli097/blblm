test_that("check the length of coefficients", {

  #cl = makeCluster(2)
  fit = blblm(mpg ~ wt * hp, data = mtcars, m = 3, B = 100)

  expect_equal(length(coef(fit)), 4)
  #expect_equal(length(coef(fit, cluster = cl)), 4)

  #stopCluster(cl)
})
