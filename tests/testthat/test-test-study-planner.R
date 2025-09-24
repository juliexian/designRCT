# Test file for plan_study function

test_that("plan_study works for power scenario", {
  result <- plan_study("power", n_per_group = 95, delta = 0.5, sd = 1.8,
                       R2 = 0.15, verbose = FALSE)

  expect_s3_class(result, "comprehensive_study_plan")
  expect_equal(result$scenario, "power")
  expect_s3_class(result$analysis, "study_power")
  expect_null(result$costs)
})

test_that("plan_study works for sample_size scenario", {
  result <- plan_study("sample_size", power = 0.8, delta = 0.5, sd = 1.8,
                       R2 = 0.15, verbose = FALSE)

  expect_s3_class(result, "comprehensive_study_plan")
  expect_equal(result$scenario, "sample_size")
  expect_s3_class(result$analysis, "study_sample_size")
  expect_null(result$costs)
})

test_that("plan_study includes cost analysis when requested", {
  result <- plan_study("power", n_per_group = 95, delta = 0.5, sd = 1.8,
                       cost_per_subject = 1000, verbose = FALSE)

  expect_s3_class(result$costs, "study_costs")
  expect_equal(result$costs$cost_per_subject, 1000)
})

test_that("plan_study works with comprehensive parameters", {
  result <- plan_study("sample_size", power = 0.8, delta = 0.5, sd = 1.8,
                       R2 = 0.15, cost_per_subject = 1000,
                       fixed_costs = 50000, dropout_rate = 0.1, verbose = FALSE)

  expect_s3_class(result, "comprehensive_study_plan")
  expect_s3_class(result$analysis, "study_sample_size")
  expect_s3_class(result$costs, "study_costs")

  # Check that cost analysis used the calculated sample size
  expect_equal(result$costs$n_per_group, result$analysis$n_per_group)
})

test_that("plan_study input validation works", {
  # Invalid scenario
  expect_error(plan_study("invalid", n_per_group = 95, delta = 0.5, sd = 1.8),
               "scenario must be either 'power' or 'sample_size'")

  # Missing n_per_group for power scenario
  expect_error(plan_study("power", delta = 0.5, sd = 1.8),
               "n_per_group must be specified for power analysis")

  # Missing power for sample_size scenario
  expect_error(plan_study("sample_size", delta = 0.5, sd = 1.8),
               "power must be specified for sample size calculation")
})

test_that("plan_study passes parameters correctly to underlying functions", {
  # Test that alternative parameter is passed through
  result_two <- plan_study("power", n_per_group = 95, delta = 0.5, sd = 1.8,
                           alternative = "two.sided", verbose = FALSE)
  result_one <- plan_study("power", n_per_group = 95, delta = 0.5, sd = 1.8,
                           alternative = "one.sided", verbose = FALSE)

  expect_equal(result_two$analysis$alternative, "two.sided")
  expect_equal(result_one$analysis$alternative, "one.sided")
  expect_true(result_one$analysis$power > result_two$analysis$power)
})

test_that("plan_study verbose option works", {
  expect_silent(plan_study("power", n_per_group = 95, delta = 0.5, sd = 1.8,
                           verbose = FALSE))
})

test_that("print.comprehensive_study_plan method works", {
  result <- plan_study("power", n_per_group = 95, delta = 0.5, sd = 1.8,
                       cost_per_subject = 1000, verbose = FALSE)

  expect_output(print(result), "COMPREHENSIVE STUDY PLANNING RESULTS")
  expect_output(print(result), "Study Power Analysis Results")
  expect_output(print(result), "Study Cost Analysis Results")
})

test_that("plan_study integrates all components correctly", {
  # Test power scenario with all options
  result_power <- plan_study("power", n_per_group = 100, delta = 0.6, sd = 2.0,
                             R2 = 0.2, sig.level = 0.01, alternative = "one.sided",
                             cost_per_subject = 1500, fixed_costs = 75000,
                             dropout_rate = 0.15, verbose = FALSE)

  # Check analysis parameters
  expect_equal(result_power$analysis$n_per_group, 100)
  expect_equal(result_power$analysis$delta, 0.6)
  expect_equal(result_power$analysis$sd_original, 2.0)
  expect_equal(result_power$analysis$R2, 0.2)
  expect_equal(result_power$analysis$sig.level, 0.01)
  expect_equal(result_power$analysis$alternative, "one.sided")

  # Check cost parameters
  expect_equal(result_power$costs$n_per_group, 100)
  expect_equal(result_power$costs$cost_per_subject, 1500)
  expect_equal(result_power$costs$fixed_costs, 75000)
  expect_equal(result_power$costs$dropout_rate, 0.15)

  # Test sample_size scenario with all options
  result_sample <- plan_study("sample_size", power = 0.9, delta = 0.4, sd = 1.5,
                              R2 = 0.1, sig.level = 0.05, alternative = "two.sided",
                              cost_per_subject = 800, fixed_costs = 25000,
                              dropout_rate = 0.05, verbose = FALSE)

  # Check that cost analysis uses the calculated sample size
  expect_equal(result_sample$costs$n_per_group, result_sample$analysis$n_per_group)
})
