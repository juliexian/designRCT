# Test file for power_analysis function

test_that("power_analysis works with basic inputs", {
  result <- power_analysis(n_per_group = 95, delta = 0.5, sd = 1.8)

  expect_s3_class(result, "study_power")
  expect_true(result$power > 0 && result$power < 1)
  expect_equal(result$total_n, 190)
  expect_equal(result$n_per_group, 95)
  expect_equal(result$delta, 0.5)
  expect_equal(result$sd_original, 1.8)
  expect_equal(result$R2, 0)
  expect_equal(result$alternative, "two.sided")
})

test_that("power_analysis works with covariate adjustment", {
  result_no_adj <- power_analysis(n_per_group = 95, delta = 0.5, sd = 1.8)
  result_with_adj <- power_analysis(n_per_group = 95, delta = 0.5, sd = 1.8, R2 = 0.15)

  # Power should be higher with covariate adjustment
  expect_true(result_with_adj$power > result_no_adj$power)

  # Adjusted SD should be lower
  expect_true(result_with_adj$sd_adjusted < result_with_adj$sd_original)
  expect_equal(result_with_adj$sd_adjusted, 1.8 * sqrt(1 - 0.15))

  # R2 should be recorded correctly
  expect_equal(result_with_adj$R2, 0.15)
})

test_that("power_analysis handles different alternatives correctly", {
  result_two_sided <- power_analysis(n_per_group = 95, delta = 0.5, sd = 1.8,
                                     alternative = "two.sided")
  result_one_sided <- power_analysis(n_per_group = 95, delta = 0.5, sd = 1.8,
                                     alternative = "one.sided")

  # One-sided test should have higher power
  expect_true(result_one_sided$power > result_two_sided$power)
  expect_equal(result_two_sided$alternative, "two.sided")
  expect_equal(result_one_sided$alternative, "one.sided")
})

test_that("power_analysis input validation works", {
  # Test negative n_per_group
  expect_error(power_analysis(n_per_group = -1, delta = 0.5, sd = 1.8),
               "n_per_group must be a positive number")

  # Test zero n_per_group
  expect_error(power_analysis(n_per_group = 0, delta = 0.5, sd = 1.8),
               "n_per_group must be a positive number")

  # Test negative delta
  expect_error(power_analysis(n_per_group = 95, delta = -0.5, sd = 1.8),
               "delta must be a non-negative number")

  # Test negative sd
  expect_error(power_analysis(n_per_group = 95, delta = 0.5, sd = -1.8),
               "sd must be a positive number")

  # Test zero sd
  expect_error(power_analysis(n_per_group = 95, delta = 0.5, sd = 0),
               "sd must be a positive number")

  # Test R2 out of bounds
  expect_error(power_analysis(n_per_group = 95, delta = 0.5, sd = 1.8, R2 = -0.1),
               "R2 must be between 0 and 1 \\(exclusive\\)")
  expect_error(power_analysis(n_per_group = 95, delta = 0.5, sd = 1.8, R2 = 1.0),
               "R2 must be between 0 and 1 \\(exclusive\\)")
  expect_error(power_analysis(n_per_group = 95, delta = 0.5, sd = 1.8, R2 = 1.5),
               "R2 must be between 0 and 1 \\(exclusive\\)")

  # Test invalid significance level
  expect_error(power_analysis(n_per_group = 95, delta = 0.5, sd = 1.8, sig.level = 0),
               "sig.level must be between 0 and 1 \\(exclusive\\)")
  expect_error(power_analysis(n_per_group = 95, delta = 0.5, sd = 1.8, sig.level = 1),
               "sig.level must be between 0 and 1 \\(exclusive\\)")

  # Test invalid alternative
  expect_error(power_analysis(n_per_group = 95, delta = 0.5, sd = 1.8, alternative = "invalid"),
               "alternative must be 'two.sided' or 'one.sided'")
})

test_that("power_analysis verbose option works", {
  expect_message(power_analysis(n_per_group = 95, delta = 0.5, sd = 1.8,
                                R2 = 0.15, verbose = TRUE),
                 "Original SD:")

  expect_silent(power_analysis(n_per_group = 95, delta = 0.5, sd = 1.8,
                               verbose = FALSE))
})

test_that("print.study_power method works", {
  result <- power_analysis(n_per_group = 95, delta = 0.5, sd = 1.8)

  expect_output(print(result), "Study Power Analysis Results")
  expect_output(print(result), "Power:")
  expect_output(print(result), "Sample size per group:")
  expect_output(print(result), "Total sample size:")
})
