
# FuzzyClass <img src="man/figures/logo.png" align="right" height="150" alt="FuzzyClass logo" />

<!-- badges: start -->

[![CRAN
version](https://www.r-pkg.org/badges/version/FuzzyClass)](https://cran.r-project.org/package=FuzzyClass)
[![CRAN
Download](https://cranlogs.r-pkg.org/badges/grand-total/FuzzyClass)](https://cran.r-project.org/package=FuzzyClass)
[![License:
MIT](https://img.shields.io/badge/License-MIT-blue.svg)](https://cran.r-project.org/package=FuzzyClass/LICENSE)
<!-- badges: end -->

**A comprehensive family of probability-based fuzzy and non-fuzzy
classifiers.**

Classification problems involve assigning categorical labels to data
instances based on observed features. In real-world applications,
classes often overlap, boundaries are imprecise, and measurements carry
uncertainty. Traditional Bayesian classifiers assume rigid
distributional parameters and crisp class assignments.

**`FuzzyClass`** addresses these challenges by unifying **Fuzzy Set
Theory** ($\mu \in [0, 1]$) with **Probabilistic Naive Bayes**
classification. By leveraging soft membership degrees, fuzzy parameters
(triangular and trapezoidal), and geometric distance weighting,
`FuzzyClass` delivers robust, interpretable, and high-performance
classification for complex and ambiguous data.

> A complete manual showcasing all classifiers and mathematical details
> is available at the [CRAN FuzzyClass Reference
> Manual](https://cran.r-project.org/package=FuzzyClass/FuzzyClass.pdf).

------------------------------------------------------------------------

## What’s New in Version 0.2.0

- 🐞 **Correct class labelling**: parameters and predicted labels now
  always follow the same class order (factor levels). Previous
  development code could mislabel predictions when the training data
  were not sorted by class.
- 📐 **Estimation aligned with the literature**: exact Gamma maximum
  likelihood, unbounded Beta shapes, Geometric pmf consistent with its
  estimator, Binomial `n >= max(x)`, class-wise sample sizes in the
  fuzzy-parameter confidence intervals and nested alpha-cuts in
  `PoiNBFuzzyParam`.
- ⚡ **Vectorized predictions**: prediction and fuzzy-number ranking
  (`metd = 1:5`) are vectorized over all observations, about 100× faster
  than version 0.1.7.
- 🪶 **Lighter dependencies**: `dplyr`, `purrr`, `tibble`, `tidyr`,
  `rlang`, `parallel`, `foreach` and `doParallel` are no longer required
  (`cores` is kept only for backward compatibility).

------------------------------------------------------------------------

## Available Classifiers (Taxonomy)

`FuzzyClass` provides **18 dedicated classifiers** categorized by
feature type and modeling paradigm:

| Classifier | Paradigm | Description |
|:---|:---|:---|
| [`FuzzyGaussianNaiveBayes`](#continuous-classifiers) | Continuous | Gaussian Naive Bayes weighted by fuzzy membership degrees. |
| [`GauNBFuzzyParam`](#continuous-classifiers) | Continuous | Gaussian Naive Bayes with fuzzy parameters ($\tilde{\mu}, \tilde{\sigma}^2$) across 5 estimation methods. |
| [`FuzzyExponentialNaiveBayes`](#continuous-classifiers) | Continuous | Exponential Naive Bayes with fuzzy memberships for positive continuous features. |
| [`ExpNBFuzzyParam`](#continuous-classifiers) | Continuous | Exponential Naive Bayes with fuzzy rate parameters. |
| [`FuzzyGammaNaiveBayes`](#continuous-classifiers) | Continuous | Gamma Naive Bayes with fuzzy memberships for skewed continuous data. |
| [`DWFuzzyGammaNaiveBayes`](#continuous-classifiers) | Continuous | Double-Weighted Fuzzy Gamma Naive Bayes. |
| [`FuzzyBetaNaiveBayes`](#continuous-classifiers) | Continuous | Beta Naive Bayes for continuous features bounded in $(0, 1)$ (rates/proportions). |
| [`FuzzyTriangNaiveBayes`](#continuous-classifiers) | Continuous | Naive Bayes with triangular membership functions. |
| [`FuzzyTrapeNaiveBayes`](#continuous-classifiers) | Continuous | Naive Bayes with trapezoidal membership functions. |
| [`FuzzyPoissonNaiveBayes`](#discrete-count-classifiers) | Discrete / Count | Poisson Naive Bayes with fuzzy memberships for integer count data. |
| [`PoiNBFuzzyParam`](#discrete-count-classifiers) | Discrete / Count | Poisson Naive Bayes with fuzzy rate parameters $\tilde{\lambda}$. |
| [`FuzzyBinomialNaiveBayes`](#discrete-count-classifiers) | Discrete / Count | Binomial Naive Bayes with fuzzy memberships for binary/trial counts. |
| [`FuzzyHipergeometricNaiveBayes`](#discrete-count-classifiers) | Discrete / Count | Hypergeometric Naive Bayes for sampling without replacement from finite populations. |
| [`DoubleWeightedFuzzyHipergeometricNaiveBayes`](#discrete-count-classifiers) | Discrete / Count | Double-Weighted Hypergeometric Naive Bayes. |
| [`FuzzyGeoNaiveBayes`](#geometric-rule-based-systems) | Geometric | Naive Bayes with distance-based geometric fuzzy memberships. |
| [`FuzzyNaiveBayes`](#geometric-rule-based-systems) | General | Generalized fuzzy Naive Bayes supporting continuous and categorical features. |
| [`FuzzyBayesRule`](#geometric-rule-based-systems) | Decision Rule | Fuzzy Bayes decision rule classifier. |
| [`FuzzyRuleBasedSystem`](#geometric-rule-based-systems) | Inference Rule | Fuzzy rule-based inference and classification system. |

------------------------------------------------------------------------

## Installation

### Stable Version from CRAN

You can install the released version of `FuzzyClass` from CRAN:

``` r
install.packages("FuzzyClass")
```

### Development Version from GitHub

To install the latest development version:

``` r
# install.packages("devtools")
devtools::install_github("leapigufpb/FuzzyClass")
```

------------------------------------------------------------------------

## Dependencies

`FuzzyClass` is designed with minimal, stable, and battle-tested
dependencies automatically installed by R:

- [caTools](https://cran.r-project.org/package=caTools)
- [e1071](https://cran.r-project.org/package=e1071)
- [EnvStats](https://cran.r-project.org/package=EnvStats)
- [MASS](https://cran.r-project.org/package=MASS)
- [maxLik](https://cran.r-project.org/package=maxLik)
- [mvtnorm](https://cran.r-project.org/package=mvtnorm)
- [Rdpack](https://cran.r-project.org/package=Rdpack)
- [rootSolve](https://cran.r-project.org/package=rootSolve)

------------------------------------------------------------------------

## Quick Start & Examples

### 1. Data Preparation

We demonstrate package functionality using the built-in
`VirtualRealityData` dataset:

``` r
library(FuzzyClass)

# Load dataset
data(VirtualRealityData)
df <- as.data.frame(VirtualRealityData)

# Split into Training (70%) and Testing (30%) sets
set.seed(123)
split <- caTools::sample.split(df[, 4], SplitRatio = 0.7)
Train <- subset(df, split == TRUE)
Test  <- subset(df, split == FALSE)

X_train <- Train[, -4]
y_train <- Train[, 4]
X_test  <- Test[, -4]
y_test  <- Test[, 4]
```

------------------------------------------------------------------------

### 2. Gaussian Naive Bayes with Fuzzy Parameters (`GauNBFuzzyParam`)

Fit a Gaussian model where mean and variance are represented as fuzzy
numbers:

``` r
# Fit model using fuzzy estimation method 2
fit_fgnb <- GauNBFuzzyParam(train = X_train, cl = y_train, metd = 2, cores = 1)

# Predict classes
pred_classes <- predict(fit_fgnb, X_test)

# Confusion Matrix
conf_mat <- table(Actual = y_test, Predicted = pred_classes)
conf_mat
#>       Predicted
#> Actual  1  2  3
#>      1 55  4  1
#>      2  8 34 18
#>      3  1  9 50

# Classification Accuracy
accuracy <- sum(diag(conf_mat)) / sum(conf_mat)
cat(sprintf("Test Accuracy: %.2f%%\n", accuracy * 100))
#> Test Accuracy: 77.22%
```

You can also extract the full posterior membership / probability matrix:

``` r
# Extract prediction probability matrix
prob_matrix <- predict(fit_fgnb, X_test, type = "matrix")
head(prob_matrix)
#>              1           2            3
#> [1,] 0.4991627 0.376053403 1.247839e-01
#> [2,] 0.9942344 0.005757106 8.478896e-06
#> [3,] 0.9818783 0.009466896 8.654851e-03
#> [4,] 0.9943634 0.005634324 2.298521e-06
#> [5,] 0.5559862 0.433612780 1.040104e-02
#> [6,] 0.9520142 0.013665813 3.432000e-02
```

------------------------------------------------------------------------

### 3. Continuous Fuzzy Gaussian Naive Bayes (`FuzzyGaussianNaiveBayes`)

Fit a Gaussian Naive Bayes classifier where observations have fuzzy
membership degrees:

``` r
# Fit standard fuzzy Gaussian Naive Bayes
fit_gnb <- FuzzyGaussianNaiveBayes(train = X_train, cl = y_train)

# Predict classes on test set
pred_gnb <- predict(fit_gnb, X_test)

# Confusion matrix
table(Actual = y_test, Predicted = pred_gnb)
#>       Predicted
#> Actual  1  2  3
#>      1 58  2  0
#>      2  7 50  3
#>      3  0 10 50
```

------------------------------------------------------------------------

## How to Contribute

Contributions, bug reports, and suggestions are very welcome!

1.  Fork the [FuzzyClass
    repository](https://github.com/leapigufpb/FuzzyClass).
2.  Create a feature branch: `git checkout -b feature/my-new-feature`.
3.  Commit your changes: `git commit -am 'Add new feature'`.
4.  Push to the branch: `git push origin feature/my-new-feature`.
5.  Submit a Pull Request.

------------------------------------------------------------------------

## Reporting Issues

If you encounter any issues or unexpected behavior, please submit an
issue on GitHub: 👉 <https://github.com/leapigufpb/FuzzyClass/issues>

Please provide a reproducible example (`reprex`) and session information
(`sessionInfo()`).
