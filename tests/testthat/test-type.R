test_that("test if the type of results is blblm", {
  fit1 = blblm(mpg ~ wt * hp, data = mtcars, m = 3, B = 100, method = "lmC")

  # file_names <- file.path("inst/extdata/files", list.files("inst/extdata/files"))
  # cl = makeCluster(2)
  # fit2 = blblm(y ~ x, files = file_names, B = 10, cluster = cl, method = "lmR")
  # stopCluster(cl)

  expect_s3_class(fit1, "blblm")
  #expect_s3_class(fit2, "blblm")
})
