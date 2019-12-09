context("mean_ci")

test_that("mean_ci calculates the true mean it should",{

  x <- rnorm(n = 10, mean = 150, sd = 10)
  expect_equal(mean_ci(x, bootstraps = 100)[['estimate']], mean(x))

})

