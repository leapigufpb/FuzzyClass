
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

## What’s New in Version 0.3.0

- 🧱 **R6 classes**: every classifier is now an
  [R6](https://r6.r-lib.org) class (e.g. `FuzzyGaussianNaiveBayesModel`)
  with `$new()`, `$fit()`, `$predict()` and `$set_params()`. The
  functional interface (`FuzzyGaussianNaiveBayes(train, cl)` +
  `predict()`) still works and gives the same predictions.
- 💬 **Friendly training messages**: `$fit()` tells you which model is
  being trained, on how much data, and how long it took. Silence it with
  `verbose = FALSE` or `options(FuzzyClass.verbose = FALSE)`.
- 🔗 **tidymodels support**: the new parsnip model `fuzzy_naive_bayes()`
  exposes all classifiers as engines, ready for `workflows`, `recipes`,
  `rsample`, `tune` and `yardstick`, including tuning of `fuzzy`,
  `alpha_cut` and `ranking_method`.

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

`FuzzyClass` provides **17 classifiers** and a fuzzy rule-based system,
categorized by feature type and modeling paradigm. Each classifier `X`
has the functional interface `X(train, cl)`, the R6 class `XModel` and
an engine of `fuzzy_naive_bayes()`:

| Classifier | Paradigm | Description | tidymodels engine |
|:---|:---|:---|:---|
| [`FuzzyGaussianNaiveBayes`](#continuous-classifiers) | Continuous | Gaussian Naive Bayes weighted by fuzzy membership degrees. | `"gaussian"` |
| [`GauNBFuzzyParam`](#continuous-classifiers) | Continuous | Gaussian Naive Bayes with fuzzy parameters ($\tilde{\mu}, \tilde{\sigma}^2$) across 5 estimation methods. | `"gaussian_fuzzy_param"` |
| [`FuzzyExponentialNaiveBayes`](#continuous-classifiers) | Continuous | Exponential Naive Bayes with fuzzy memberships for positive continuous features. | `"exponential"` |
| [`ExpNBFuzzyParam`](#continuous-classifiers) | Continuous | Exponential Naive Bayes with fuzzy rate parameters. | `"exponential_fuzzy_param"` |
| [`FuzzyGammaNaiveBayes`](#continuous-classifiers) | Continuous | Gamma Naive Bayes with fuzzy memberships for skewed continuous data. | `"gamma"` |
| [`DWFuzzyGammaNaiveBayes`](#continuous-classifiers) | Continuous | Double-Weighted Fuzzy Gamma Naive Bayes. | `"dw_gamma"` |
| [`FuzzyBetaNaiveBayes`](#continuous-classifiers) | Continuous | Beta Naive Bayes for continuous features bounded in $(0, 1)$ (rates/proportions). | `"beta"` |
| [`FuzzyTriangularNaiveBayes`](#continuous-classifiers) | Continuous | Naive Bayes with triangular membership functions. | `"triangular"` |
| [`FuzzyTrapezoidalNaiveBayes`](#continuous-classifiers) | Continuous | Naive Bayes with trapezoidal membership functions. | `"trapezoidal"` |
| [`FuzzyPoissonNaiveBayes`](#discrete-count-classifiers) | Discrete / Count | Poisson Naive Bayes with fuzzy memberships for integer count data. | `"poisson"` |
| [`PoiNBFuzzyParam`](#discrete-count-classifiers) | Discrete / Count | Poisson Naive Bayes with fuzzy rate parameters $\tilde{\lambda}$. | `"poisson_fuzzy_param"` |
| [`FuzzyBinomialNaiveBayes`](#discrete-count-classifiers) | Discrete / Count | Binomial Naive Bayes with fuzzy memberships for binary/trial counts. | `"binomial"` |
| [`FuzzyHipergeometricNaiveBayes`](#discrete-count-classifiers) | Discrete / Count | Hypergeometric Naive Bayes for sampling without replacement from finite populations. | `"hypergeometric"` |
| [`DWFuzzyHipergeometricNaiveBayes`](#discrete-count-classifiers) | Discrete / Count | Double-Weighted Hypergeometric Naive Bayes. | `"dw_hypergeometric"` |
| [`FuzzyGeoNaiveBayes`](#geometric-rule-based-systems) | Geometric | Naive Bayes with distance-based geometric fuzzy memberships. | `"geometric"` |
| [`FuzzyNaiveBayes`](#geometric-rule-based-systems) | General | Generalized fuzzy Naive Bayes supporting continuous and categorical features. | `"naive"` |
| [`FuzzyBayesRule`](#geometric-rule-based-systems) | Decision Rule | Fuzzy Bayes decision rule classifier. | `"bayes_rule"` |
| [`FuzzyRuleBasedSystem`](#geometric-rule-based-systems) | Inference Rule | Fuzzy rule-based inference and classification system. | — |

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
- [cli](https://cran.r-project.org/package=cli)
- [e1071](https://cran.r-project.org/package=e1071)
- [EnvStats](https://cran.r-project.org/package=EnvStats)
- [MASS](https://cran.r-project.org/package=MASS)
- [maxLik](https://cran.r-project.org/package=maxLik)
- [mvtnorm](https://cran.r-project.org/package=mvtnorm)
- [R6](https://cran.r-project.org/package=R6)
- [Rdpack](https://cran.r-project.org/package=Rdpack)
- [rootSolve](https://cran.r-project.org/package=rootSolve)

The [tidymodels](https://www.tidymodels.org) packages (`parsnip`,
`dials`, `tune`, …) are optional: install them to use
`fuzzy_naive_bayes()`.

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
#> ℹ Gaussian Naive Bayes with Fuzzy Parameters: 420 observations, 3 predictors and 3 classes
#> ℹ Training the model: estimating its parameters
#> ✔ Model trained successfully [31ms]
#> 

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
#> ℹ Fuzzy Gaussian Naive Bayes (Zadeh-based): 420 observations, 3 predictors and 3 classes
#> ℹ Training the model: estimating its parameters
#> ✔ Model trained successfully [16ms]
#> 

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

### 4. The R6 Interface

Every classifier is an R6 class. Create the model, train it (a message
tells you what is happening) and predict:

``` r
model <- FuzzyGammaNaiveBayesModel$new(fuzzy = TRUE)
model$fit(train = X_train, cl = y_train)
#> ℹ Fuzzy Gamma Naive Bayes: 420 observations, 3 predictors and 3 classes
#> ℹ Training the model: estimating its parameters
#> ✔ Model trained successfully [6ms]
#> 
model
#> <FuzzyGammaNaiveBayes> Fuzzy Gamma Naive Bayes
#> ✔ Trained on 420 observations in 0.002s
#>   Hyperparameters: fuzzy = TRUE
#>   Predictors (3): V1, V2, V3
#>   Classes (3): 1, 2, 3

head(model$predict(X_test))
#> [1] 1 1 1 1 1 1
#> Levels: 1 2 3
head(model$predict(X_test, type = "prob"))
#>              1            2            3
#> [1,] 0.9084371 9.153798e-02 2.487652e-05
#> [2,] 0.9998998 1.002413e-04 6.140526e-11
#> [3,] 0.9942020 5.797331e-03 7.118546e-07
#> [4,] 0.9999186 8.141746e-05 4.390692e-11
#> [5,] 0.8022878 1.976974e-01 1.476847e-05
#> [6,] 0.9729373 2.706046e-02 2.218717e-06
```

Hyperparameters can be changed with `$set_params()`, and the calls can
be chained:

``` r
model$set_params(fuzzy = FALSE)$fit(X_train, y_train, verbose = FALSE)
mean(model$predict(X_test) == y_test)
#> [1] 0.8777778
```

------------------------------------------------------------------------

### 5. Using FuzzyClass with tidymodels

`fuzzy_naive_bayes()` is a [parsnip](https://parsnip.tidymodels.org)
model whose engines are the FuzzyClass classifiers, so it plugs into
workflows, recipes, resampling and tuning:

``` r
library(tidymodels)

vr <- VirtualRealityData |>
  as.data.frame() |>
  mutate(V4 = factor(V4))

set.seed(123)
vr_split <- initial_split(vr, prop = 0.7, strata = V4)

spec <- fuzzy_naive_bayes(alpha_cut = tune(), ranking_method = tune()) |>
  set_engine("gaussian_fuzzy_param")

wf <- workflow() |>
  add_formula(V4 ~ .) |>
  add_model(spec)

options(FuzzyClass.verbose = FALSE) # quiet while tuning
tuned <- tune_grid(wf, resamples = vfold_cv(training(vr_split), v = 5, strata = V4), grid = 5)
show_best(tuned, metric = "accuracy", n = 3)
#> # A tibble: 3 × 8
#>   alpha_cut ranking_method .metric  .estimator  mean     n std_err .config      
#>       <dbl>          <int> <chr>    <chr>      <dbl> <int>   <dbl> <chr>        
#> 1    0.0001              2 accuracy multiclass 0.788     5 0.0121  pre0_mod1_po…
#> 2    0.0501              1 accuracy multiclass 0.786     5 0.00842 pre0_mod3_po…
#> 3    0.0750              5 accuracy multiclass 0.786     5 0.00842 pre0_mod4_po…

final <- finalize_workflow(wf, select_best(tuned, metric = "accuracy")) |>
  last_fit(vr_split)
collect_metrics(final)
#> # A tibble: 3 × 4
#>   .metric     .estimator .estimate .config        
#>   <chr>       <chr>          <dbl> <chr>          
#> 1 accuracy    multiclass     0.783 pre0_mod0_post0
#> 2 roc_auc     hand_till      0.918 pre0_mod0_post0
#> 3 brier_class multiclass     0.156 pre0_mod0_post0
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
