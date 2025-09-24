# Test file for cost_analysis function

test_that("cost_analysis works with basic inputs", {
  result <- cost_analysis(n_per_group = 95, cost_per_subject = 1000)

  expect_s3_class(result, "study_costs")
  expect_equal(result$n_per_group, 95)
  expect_equal(result$total_enrolled, 190)
  expect_equal(result$total_completed, 190)
  expect_equal(result$cost_per_subject, 1000)
  expect_equal(result$variable_costs, 190000)
  expect_equal(result$total_costs, 190000)
  expect_equal(result$fixed_costs, 0)
})

test_that("cost_analysis handles fixed costs correctly", {
  result <- cost_analysis(n_per_group = 95, cost_per_subject = 1000, fixed_costs = 50000)

  expect_equal(result$fixed_costs, 50000)
  expect_equal(result$variable_costs, 190000)
  expect_equal(result$total_costs, 240000)
})

test_that("cost_analysis handles dropout rate correctly", {
  result_no_dropout <- cost_analysis(n_per_group = 95, cost_per_subject = 1000)
  result_with_dropout <- cost_analysis(n_per_group = 95, cost_per_subject = 1000,
                                       dropout_rate = 0.1)

  # Should need to enroll more subjects with dropout
  expect_true(result_with_dropout$total_enrolled > result_no_dropout$total_enrolled)
  expect_true(result_with_dropout$n_enrolled_per_group > result_no_dropout$n_enrolled_per_group)
  expect_true(result_with_dropout$total_costs > result_no_dropout$total_costs)

  # But completed subjects should be the same
  expect_equal(result_with_dropout$total_completed, result_no_dropout$total_completed)

  # Check the math
  expected_enrolled_per_group <- ceiling(95 / (1 - 0.1))
  expect_equal(result_with_dropout$n_enrolled_per_group, expected_enrolled_per_group)
})

test_that("cost_analysis calculates cost per completed subject correctly", {
  result <- cost_analysis(n_per_group = 95, cost_per_subject = 1000,
                          fixed_costs = 50000, dropout_rate = 0.1)

  expected_cost_per_completed <- result$total_costs / result$total_completed
  expect_equal(result$cost_per_completed, expected_cost_per_completed)
})

test_that("cost_analysis input validation works", {
  # Test negative n_per_group
  expect_error(cost_analysis(n_per_group = -1, cost_per_subject = 1000),
               "n_per_group must be a positive number")

  # Test zero n_per_group
  expect_error(cost_analysis(n_per_group = 0, cost_per_subject = 1000),
               "n_per_group must be a positive number")

  # Test negative cost_per_subject
  expect_error(cost_analysis(n_per_group = 95, cost_per_subject = -1000),
               "cost_per_subject must be non-negative")

  # Test negative fixed_costs
  expect_error(cost_analysis(n_per_group = 95, cost_per_subject = 1000, fixed_costs = -50000),
               "fixed_costs must be non-negative")

  # Test invalid dropout_rate
  expect_error(cost_analysis(n_per_group = 95, cost_per_subject = 1000, dropout_rate = -0.1),
               "dropout_rate must be between 0 and 1 \\(exclusive\\)")
  expect_error(cost_analysis(n_per_group = 95, cost_per_subject = 1000, dropout_rate = 1.0),
               "dropout_rate must be between 0 and 1 \\(exclusive\\)")
  expect_error(cost_analysis(n_per_group = 95, cost_per_subject = 1000, dropout_rate = 1.5),
               "dropout_rate must be between 0 and 1 \\(exclusive\\)")
})

test_that("cost_analysis handles edge cases", {
  # Zero cost per subject
  result <- cost_analysis(n_per_group = 95, cost_per_subject = 0, fixed_costs = 50000)
  expect_equal(result$variable_costs, 0)
  expect_equal(result$total_costs, 50000)

  # Zero fixed costs (already tested in basic case)
  result <- cost_analysis(n_per_group = 95, cost_per_subject = 1000, fixed_costs = 0)
  expect_equal(result$fixed_costs, 0)
})

test_that("cost_analysis verbose option works", {
  expect_silent(cost_analysis(n_per_group = 95, cost_per_subject = 1000, verbose = FALSE))
})

test_that("print.study_costs method works", {
  result <- cost_analysis(n_per_group = 95, cost_per_subject = 1000,
                          fixed_costs = 50000, dropout_rate = 0.1)

  expect_output(print(result), "Study Cost Analysis Results")
  expect_output(print(result), "Subjects needed per group:")
  expect_output(print(result), "Total enrollment:")
  expect_output(print(result), "Cost Breakdown:")
  expect_output(print(result), "Total costs:")
})
