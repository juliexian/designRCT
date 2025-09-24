#' Calculate Power for Study Design with Covariate Adjustment
#'
#' @title Power Analysis with Covariate Adjustment
#' @description Calculates statistical power for two-sample t-tests with
#'   optional covariate adjustment to reduce residual variance.
#'
#' @param n_per_group Integer. Number of subjects per group
#' @param delta Numeric. Expected mean difference between treatment and control
#' @param sd Numeric. Pooled standard deviation without covariate adjustment
#' @param R2 Numeric. Proportion of variance explained by covariates (0-1).
#'   Default is 0 (no adjustment)
#' @param sig.level Numeric. Type I error rate (alpha). Default is 0.05
#' @param alternative Character. One of "two.sided", "one.sided". Default is "two.sided"
#' @param verbose Logical. If TRUE, prints detailed results. Default is FALSE
#'
#' @return A list containing power analysis results with components:
#' \itemize{
#'   \item power: Statistical power (1 - beta)
#'   \item n_per_group: Sample size per group
#'   \item total_n: Total sample size
#'   \item delta: Effect size (mean difference)
#'   \item sd_original: Original standard deviation
#'   \item sd_adjusted: Adjusted standard deviation after covariate adjustment
#'   \item R2: Variance explained by covariates
#'   \item sig.level: Significance level
#'   \item alternative: Test alternative
#' }
#'
#' @export
#' @importFrom stats power.t.test
#'
#' @examples
#' # Basic power analysis without covariate adjustment
#' power_analysis(n_per_group = 95, delta = 0.5, sd = 1.8)
#'
#' # Power analysis with covariate adjustment
#' power_analysis(n_per_group = 95, delta = 0.5, sd = 1.8, R2 = 0.15)
#'
#' # One-sided test with verbose output
#' power_analysis(n_per_group = 95, delta = 0.5, sd = 1.8,
#'                R2 = 0.15, alternative = "one.sided", verbose = TRUE)
power_analysis <- function(n_per_group, delta, sd, R2 = 0,
                           sig.level = 0.05, alternative = "two.sided",
                           verbose = FALSE) {

  # Input validation
  if (!is.numeric(n_per_group) || n_per_group <= 0) {
    stop("n_per_group must be a positive number")
  }
  if (!is.numeric(delta) || delta < 0) {
    stop("delta must be a non-negative number")
  }
  if (!is.numeric(sd) || sd <= 0) {
    stop("sd must be a positive number")
  }
  if (!is.numeric(R2) || R2 < 0 || R2 >= 1) {
    stop("R2 must be between 0 and 1 (exclusive)")
  }
  if (!is.numeric(sig.level) || sig.level <= 0 || sig.level >= 1) {
    stop("sig.level must be between 0 and 1 (exclusive)")
  }
  if (!alternative %in% c("two.sided", "one.sided")) {
    stop("alternative must be 'two.sided' or 'one.sided'")
  }

  # Adjust SD for covariates
  sd_adj <- sd * sqrt(1 - R2)

  if (verbose) {
    message("Original SD: ", sd)
    message("R-squared from covariates: ", R2)
    message("Adjusted SD: ", round(sd_adj, 4))
  }

  # Compute power with adjusted SD
  power_result <- power.t.test(
    n = n_per_group,
    delta = delta,
    sd = sd_adj,
    sig.level = sig.level,
    type = "two.sample",
    alternative = alternative
  )

  # Format results
  result <- list(
    power = power_result$power,
    n_per_group = n_per_group,
    total_n = 2 * n_per_group,
    delta = delta,
    sd_original = sd,
    sd_adjusted = sd_adj,
    R2 = R2,
    sig.level = sig.level,
    alternative = alternative
  )

  class(result) <- "study_power"

  if (verbose) {
    print(result)
  }

  return(result)
}

#' @export
print.study_power <- function(x, ...) {
  cat("Study Power Analysis Results\n")
  cat("============================\n")
  cat("Power:", round(x$power, 4), "\n")
  cat("Sample size per group:", x$n_per_group, "\n")
  cat("Total sample size:", x$total_n, "\n")
  cat("Effect size (delta):", x$delta, "\n")
  cat("Original SD:", x$sd_original, "\n")
  cat("Adjusted SD:", round(x$sd_adjusted, 4), "\n")
  cat("R-squared from covariates:", x$R2, "\n")
  cat("Significance level:", x$sig.level, "\n")
  cat("Alternative hypothesis:", x$alternative, "\n")
}

#' Calculate Required Sample Size for Given Power
#'
#' @title Sample Size Calculation with Covariate Adjustment
#' @description Calculates the required sample size per group to achieve
#'   a specified power level, with optional covariate adjustment.
#'
#' @param power Numeric. Desired statistical power (between 0 and 1)
#' @param delta Numeric. Expected mean difference between treatment and control
#' @param sd Numeric. Pooled standard deviation without covariate adjustment
#' @param R2 Numeric. Proportion of variance explained by covariates (0-1).
#'   Default is 0 (no adjustment)
#' @param sig.level Numeric. Type I error rate (alpha). Default is 0.05
#' @param alternative Character. One of "two.sided", "one.sided". Default is "two.sided"
#' @param verbose Logical. If TRUE, prints detailed results. Default is FALSE
#'
#' @return A list containing sample size calculation results
#'
#' @export
#' @importFrom stats power.t.test
#'
#' @examples
#' # Calculate sample size for 80% power
#' sample_size_calc(power = 0.8, delta = 0.5, sd = 1.8)
#'
#' # With covariate adjustment
#' sample_size_calc(power = 0.8, delta = 0.5, sd = 1.8, R2 = 0.15)
sample_size_calc <- function(power, delta, sd, R2 = 0,
                             sig.level = 0.05, alternative = "two.sided",
                             verbose = FALSE) {

  # Input validation
  if (!is.numeric(power) || power <= 0 || power >= 1) {
    stop("power must be between 0 and 1 (exclusive)")
  }
  if (!is.numeric(delta) || delta < 0) {
    stop("delta must be a non-negative number")
  }
  if (!is.numeric(sd) || sd <= 0) {
    stop("sd must be a positive number")
  }
  if (!is.numeric(R2) || R2 < 0 || R2 >= 1) {
    stop("R2 must be between 0 and 1 (exclusive)")
  }

  # Adjust SD for covariates
  sd_adj <- sd * sqrt(1 - R2)

  # Calculate required sample size
  n_result <- power.t.test(
    power = power,
    delta = delta,
    sd = sd_adj,
    sig.level = sig.level,
    type = "two.sample",
    alternative = alternative
  )

  result <- list(
    n_per_group = ceiling(n_result$n),
    total_n = 2 * ceiling(n_result$n),
    power = power,
    delta = delta,
    sd_original = sd,
    sd_adjusted = sd_adj,
    R2 = R2,
    sig.level = sig.level,
    alternative = alternative
  )

  class(result) <- "study_sample_size"

  if (verbose) {
    print(result)
  }

  return(result)
}

#' @export
print.study_sample_size <- function(x, ...) {
  cat("Sample Size Calculation Results\n")
  cat("===============================\n")
  cat("Required sample size per group:", x$n_per_group, "\n")
  cat("Total required sample size:", x$total_n, "\n")
  cat("Target power:", x$power, "\n")
  cat("Effect size (delta):", x$delta, "\n")
  cat("Original SD:", x$sd_original, "\n")
  cat("Adjusted SD:", round(x$sd_adjusted, 4), "\n")
  cat("R-squared from covariates:", x$R2, "\n")
  cat("Significance level:", x$sig.level, "\n")
  cat("Alternative hypothesis:", x$alternative, "\n")
}


#' Calculate Study Costs
#'
#' @title Study Cost Analysis
#' @description Calculates total study costs based on sample size and
#'   per-subject costs, with optional fixed costs.
#'
#' @param n_per_group Integer. Number of subjects per group
#' @param cost_per_subject Numeric. Cost per subject enrolled
#' @param fixed_costs Numeric. Fixed costs independent of sample size. Default is 0
#' @param dropout_rate Numeric. Expected dropout rate (0-1). Default is 0
#' @param verbose Logical. If TRUE, prints detailed cost breakdown. Default is FALSE
#'
#' @return A list containing cost analysis results
#'
#' @export
#'
#' @examples
#' # Basic cost calculation
#' cost_analysis(n_per_group = 95, cost_per_subject = 1000)
#'
#' # With fixed costs and dropout adjustment
#' cost_analysis(n_per_group = 95, cost_per_subject = 1000,
#'               fixed_costs = 50000, dropout_rate = 0.1)
cost_analysis <- function(n_per_group, cost_per_subject, fixed_costs = 0,
                          dropout_rate = 0, verbose = FALSE) {

  # Input validation
  if (!is.numeric(n_per_group) || n_per_group <= 0) {
    stop("n_per_group must be a positive number")
  }
  if (!is.numeric(cost_per_subject) || cost_per_subject < 0) {
    stop("cost_per_subject must be non-negative")
  }
  if (!is.numeric(fixed_costs) || fixed_costs < 0) {
    stop("fixed_costs must be non-negative")
  }
  if (!is.numeric(dropout_rate) || dropout_rate < 0 || dropout_rate >= 1) {
    stop("dropout_rate must be between 0 and 1 (exclusive)")
  }

  # Calculate enrollment needed to account for dropouts
  n_enrolled_per_group <- ceiling(n_per_group / (1 - dropout_rate))
  total_enrolled <- 2 * n_enrolled_per_group

  # Calculate costs
  variable_costs <- total_enrolled * cost_per_subject
  total_costs <- variable_costs + fixed_costs
  cost_per_completed <- total_costs / (2 * n_per_group)

  result <- list(
    n_per_group = n_per_group,
    n_enrolled_per_group = n_enrolled_per_group,
    total_enrolled = total_enrolled,
    total_completed = 2 * n_per_group,
    cost_per_subject = cost_per_subject,
    fixed_costs = fixed_costs,
    variable_costs = variable_costs,
    total_costs = total_costs,
    cost_per_completed = cost_per_completed,
    dropout_rate = dropout_rate
  )

  class(result) <- "study_costs"

  if (verbose) {
    print(result)
  }

  return(result)
}

#' @export
print.study_costs <- function(x, ...) {
  cat("Study Cost Analysis Results\n")
  cat("===========================\n")
  cat("Subjects needed per group:", x$n_per_group, "\n")
  cat("Subjects to enroll per group (with dropout):", x$n_enrolled_per_group, "\n")
  cat("Total enrollment:", x$total_enrolled, "\n")
  cat("Expected dropout rate:", paste0(x$dropout_rate * 100, "%"), "\n")
  cat("\nCost Breakdown:\n")
  cat("  Cost per subject: $", format(x$cost_per_subject, big.mark = ","), "\n")
  cat("  Variable costs: $", format(x$variable_costs, big.mark = ","), "\n")
  cat("  Fixed costs: $", format(x$fixed_costs, big.mark = ","), "\n")
  cat("  Total costs: $", format(x$total_costs, big.mark = ","), "\n")
  cat("  Cost per completed subject: $", format(round(x$cost_per_completed, 2), big.mark = ","), "\n")
}

#' Comprehensive Study Planning Analysis
#'
#' @title Complete Study Planning with Power, Sample Size, and Cost Analysis
#' @description Performs comprehensive study planning including power analysis,
#'   sample size calculation, and cost estimation.
#'
#' @param scenario Character. Either "power" (calculate power for given n) or
#'   "sample_size" (calculate n for given power)
#' @param n_per_group Integer. Number of subjects per group (for power calculation)
#' @param power Numeric. Desired power (for sample size calculation)
#' @param delta Numeric. Expected effect size (mean difference)
#' @param sd Numeric. Pooled standard deviation
#' @param R2 Numeric. Proportion of variance explained by covariates. Default is 0
#' @param sig.level Numeric. Significance level. Default is 0.05
#' @param alternative Character. "two.sided" or "one.sided". Default is "two.sided"
#' @param cost_per_subject Numeric. Cost per subject. Default is NULL (no cost analysis)
#' @param fixed_costs Numeric. Fixed study costs. Default is 0
#' @param dropout_rate Numeric. Expected dropout rate. Default is 0
#' @param verbose Logical. Print detailed results. Default is TRUE
#'
#' @return A list containing comprehensive study planning results
#'
#' @export
#'
#' @examples
#' # Power analysis scenario
#' plan_study("power", n_per_group = 95, delta = 0.5, sd = 1.8,
#'            R2 = 0.15, cost_per_subject = 1000)
#'
#' # Sample size calculation scenario
#' plan_study("sample_size", power = 0.8, delta = 0.5, sd = 1.8,
#'            R2 = 0.15, cost_per_subject = 1000, dropout_rate = 0.1)
plan_study <- function(scenario, n_per_group = NULL, power = NULL,
                       delta, sd, R2 = 0, sig.level = 0.05,
                       alternative = "two.sided", cost_per_subject = NULL,
                       fixed_costs = 0, dropout_rate = 0, verbose = TRUE) {

  if (!scenario %in% c("power", "sample_size")) {
    stop("scenario must be either 'power' or 'sample_size'")
  }

  # Perform power/sample size analysis
  if (scenario == "power") {
    if (is.null(n_per_group)) {
      stop("n_per_group must be specified for power analysis")
    }
    analysis <- power_analysis(n_per_group, delta, sd, R2, sig.level,
                               alternative, verbose = FALSE)
  } else {
    if (is.null(power)) {
      stop("power must be specified for sample size calculation")
    }
    analysis <- sample_size_calc(power, delta, sd, R2, sig.level,
                                 alternative, verbose = FALSE)
  }

  # Add cost analysis if requested
  costs <- NULL
  if (!is.null(cost_per_subject)) {
    n_for_costs <- if (scenario == "power") n_per_group else analysis$n_per_group
    costs <- cost_analysis(n_for_costs, cost_per_subject, fixed_costs,
                           dropout_rate, verbose = FALSE)
  }

  result <- list(
    scenario = scenario,
    analysis = analysis,
    costs = costs
  )

  class(result) <- "comprehensive_study_plan"

  if (verbose) {
    print(result)
  }

  return(result)
}

#' @export
print.comprehensive_study_plan <- function(x, ...) {
  cat("=================================\n")
  cat("COMPREHENSIVE STUDY PLANNING RESULTS\n")
  cat("=================================\n\n")

  # Print main analysis
  print(x$analysis)

  # Print cost analysis if available
  if (!is.null(x$costs)) {
    cat("\n")
    print(x$costs)
  }

  cat("\n=================================\n")
}
