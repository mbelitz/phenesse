context("quantile_ci")

test_that("quantile_ci calculates the quantile quantile it should",{

  x <- rnorm(n = 10, mean = 150, sd = 10)
  expect_error(quantile_ci(observations = x,percentile = 0,
                           bootstraps = 100))

  expect_equal(quantile_ci(observations = x,percentile = 0,
                           bootstraps = 100, type = "perc")[['estimate']],
               quantile(x = x, probs = 0)[['0%']])

  expect_equal(quantile_ci(observations = x,percentile = 0.5,
                           bootstraps = 100)[['estimate']],
               quantile(x = x, probs = 0.5)[['50%']])

  expect_equal(quantile_ci(observations = x,percentile = 1,
                           bootstraps = 100)[['estimate']],
               quantile(x = x, probs = 1)[["100%"]])



})

