# Test file for sample_size_calc function

test_that("sample_size_calc works with basic inputs", {
  result <- sample_size_calc(power = 0.8, delta = 0.5, sd = 1.8)

  expect_s3_class(result, "study_sample_size")
  expect_true(result$n_per_group > 0)
  expect_equal(result$total_n, 2 * result$n_per_group)
  expect_equal(result$power, 0.8)
  expect_equal(result$delta, 0.5)
  expect_equal(result$sd_original, 1.8)
  expect_equal(result$R2, 0)
})

test_that("sample_size_calc benefits from covariate adjustment", {
  result_no_adj <- sample_size_calc(power = 0.8, delta = 0.5, sd = 1.8)
  result_with_adj <- sample_size_calc(power = 0.8, delta = 0.5, sd = 1.8, R2 = 0.15)

  # Should require fewer subjects with covariate adjustment
  expect_true(result_with_adj$n_per_group < result_no_adj$n_per_group)
  expect_true(result_with_adj$total_n < result_no_adj$total_n)

  # Adjusted SD should be lower
  expect_true(result_with_adj$sd_adjusted < result_with_adj$sd_original)
  expect_equal(result_with_adj$sd_adjusted, 1.8 * sqrt(1 - 0.15))
})

test_that("sample_size_calc handles different alternatives", {
  result_two_sided <- sample_size_calc(power = 0.8, delta = 0.5, sd = 1.8,
                                       alternative = "two.sided")
  result_one_sided <- sample_size_calc(power = 0.8, delta = 0.5, sd = 1.8,
                                       alternative = "one.sided")

  # One-sided test should require fewer subjects
  expect_true(result_one_sided$n_per_group < result_two_sided$n_per_group)
})

test_that("sample_size_calc input validation works", {
  # Test invalid power
  expect_error(sample_size_calc(power = 0, delta = 0.5, sd = 1.8),
               "power must be between 0 and 1 \\(exclusive\\)")
  expect_error(sample_size_calc(power = 1, delta = 0.5, sd = 1.8),
               "power must be between 0 and 1 \\(exclusive\\)")
  expect_error(sample_size_calc(power = 1.5, delta = 0.5, sd = 1.8),
               "power must be between 0 and 1 \\(exclusive\\)")

  # Test negative delta
  expect_error(sample_size_calc(power = 0.8, delta = -0.5, sd = 1.8),
               "delta must be a non-negative number")

  # Test invalid sd
  expect_error(sample_size_calc(power = 0.8, delta = 0.5, sd = 0),
               "sd must be a positive number")
  expect_error(sample_size_calc(power = 0.8, delta = 0.5, sd = -1.8),
               "sd must be a positive number")

  # Test invalid R2
  expect_error(sample_size_calc(power = 0.8, delta = 0.5, sd = 1.8, R2 = -0.1),
               "R2 must be between 0 and 1 \\(exclusive\\)")
  expect_error(sample_size_calc(power = 0.8, delta = 0.5, sd = 1.8, R2 = 1.0),
               "R2 must be between 0 and 1 \\(exclusive\\)")
})

test_that("sample_size_calc rounds up correctly", {
  # Should always return integer sample sizes
  result <- sample_size_calc(power = 0.8, delta = 0.5, sd = 1.8)
  expect_true(result$n_per_group == round(result$n_per_group))
  expect_true(result$total_n == round(result$total_n))
})

test_that("sample_size_calc verbose option works", {
  expect_silent(sample_size_calc(power = 0.8, delta = 0.5, sd = 1.8, verbose = FALSE))
})

test_that("print.study_sample_size method works", {
  result <- sample_size_calc(power = 0.8, delta = 0.5, sd = 1.8)

  expect_output(print(result), "Sample Size Calculation Results")
  expect_output(print(result), "Required sample size per group:")
  expect_output(print(result), "Total required sample size:")
  expect_output(print(result), "Target power:")
})
