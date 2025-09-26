
# designRCT

<!-- badges: start -->

[![R-CMD-check](https://github.com/juliexian/designRCT/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/juliexian/designRCT/actions/workflows/R-CMD-check.yaml)
[![Codecov test
coverage](https://codecov.io/gh/juliexian/designRCT/branch/main/graph/badge.svg)](https://codecov.io/gh/juliexian/designRCT?branch=main)
[![Codecov test
coverage](https://codecov.io/gh/juliexian/designRCT/graph/badge.svg)](https://app.codecov.io/gh/juliexian/designRCT)
<!-- badges: end -->

**designRCT** is an R package for comprehensive clinical trial design
and planning. It provides tools for power analysis, sample size
calculation, and cost estimation with support for covariate adjustment
to optimize study efficiency.

## Features

- **Power Analysis**: Calculate statistical power for given sample sizes
  with optional covariate adjustment
- **Sample Size Calculation**: Determine required sample size to achieve
  target power
- **Cost Analysis**: Estimate study costs including variable and fixed
  costs with dropout adjustments
- **Comprehensive Planning**: Integrated workflow combining power,
  sample size, and cost analyses
- **Flexible Design Options**: Support for one-sided and two-sided
  tests, various significance levels
- **Covariate Adjustment**: Reduce required sample sizes through R²
  variance explanation

## Installation

You can install the development version of designRCT from
[GitHub](https://github.com/) with:

``` r
# Install from GitHub
devtools::install_github("juliexian/designRCT")

# Or using remotes
remotes::install_github("juliexian/designRCT")
```

## Quick Start

``` r
library(designRCT)

# Power analysis with covariate adjustment
power_result <- power_analysis(
  n_per_group = 95, 
  delta = 0.5, 
  sd = 1.8, 
  R2 = 0.15,
  alternative = "one.sided"
)
power_result

# Sample size calculation for 80% power
sample_result <- sample_size_calc(
  power = 0.8, 
  delta = 0.5, 
  sd = 1.8, 
  R2 = 0.15
)
sample_result

# Cost analysis with dropout consideration
cost_result <- cost_analysis(
  n_per_group = 95,
  cost_per_subject = 1000,
  fixed_costs = 50000,
  dropout_rate = 0.1
)
cost_result

# Comprehensive study planning
complete_plan <- plan_study(
  scenario = "sample_size",
  power = 0.8,
  delta = 0.5,
  sd = 1.8,
  R2 = 0.15,
  cost_per_subject = 1000,
  fixed_costs = 50000,
  dropout_rate = 0.1
)
complete_plan
```

## Key Functions

### `power_analysis()`

Calculate statistical power for a given sample size:

``` r
power_analysis(n_per_group = 95, delta = 0.5, sd = 1.8, R2 = 0.15)
```

### `sample_size_calc()`

Determine required sample size for target power:

``` r
sample_size_calc(power = 0.8, delta = 0.5, sd = 1.8, R2 = 0.15)
```

### `cost_analysis()`

Estimate total study costs:

``` r
cost_analysis(n_per_group = 95, cost_per_subject = 1000, dropout_rate = 0.1)
```

### `plan_study()`

Comprehensive study planning combining all analyses:

``` r
plan_study("power", n_per_group = 95, delta = 0.5, sd = 1.8, 
           cost_per_subject = 1000)
```

## Parameters

- **n_per_group**: Number of subjects per group
- **delta**: Expected effect size (mean difference between groups)
- **sd**: Pooled standard deviation before covariate adjustment
- **R2**: Proportion of variance explained by covariates (0-1)
- **power**: Desired statistical power (0-1)
- **sig.level**: Type I error rate (default: 0.05)
- **alternative**: “two.sided” or “one.sided” (default: “two.sided”)
- **cost_per_subject**: Variable cost per enrolled subject
- **fixed_costs**: Fixed study costs independent of sample size
- **dropout_rate**: Expected dropout rate (0-1)

## Covariate Adjustment Benefits

Including baseline covariates in your analysis can significantly reduce
required sample sizes:

``` r
# Without covariate adjustment
sample_size_calc(power = 0.8, delta = 0.5, sd = 1.8, R2 = 0)

# With 15% variance explained by covariates
sample_size_calc(power = 0.8, delta = 0.5, sd = 1.8, R2 = 0.15)
```

The adjusted standard deviation is calculated as:
`sd_adjusted = sd * sqrt(1 - R2)`

## Building and Installing from Source

If you want to build the package locally:

``` bash
# Clone the repository
git clone https://github.com/juliexian/designRCT.git
cd designRCT

# In R:
devtools::document()    # Generate documentation
devtools::build()       # Build the package
devtools::install()     # Install locally
devtools::check()       # Run R CMD check
```

## Testing and Coverage

The package includes comprehensive unit tests:

``` r
# Run tests
devtools::test()

# Check code coverage
covr::package_coverage()
covr::report()
```

## Vignettes

For detailed examples and tutorials, see the package vignette:

``` r
# After installation
vignette("designRCT-introduction", package = "designRCT")
```

## Contributing

Contributions are welcome! Please feel free to submit a Pull Request.
For major changes, please open an issue first to discuss what you would
like to change.

## License

This project is licensed under the GPL (\>= 3) License - see the
[LICENSE](LICENSE) file for details.

## Citation

If you use designRCT in your research, please cite:

``` r
citation("designRCT")
```

## Contact

- **Author**: Julie Xian
- **Email**: <jxian008@gmail.com>
- **GitHub**: [@juliexian](https://github.com/juliexian)
- **Issues**: [Report bugs or request
  features](https://github.com/juliexian/designRCT/issues)

------------------------------------------------------------------------

**Note**: This package implements power analysis methods for clinical
trial design with emphasis on covariate adjustment for efficiency gains.
The statistical methods are based on standard t-test power calculations
with adjustments for reduced residual variance through baseline
covariate inclusion.
