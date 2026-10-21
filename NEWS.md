# FuzzyClass 0.3.0

## R6 classes

- All classifiers are now implemented as [R6](https://r6.r-lib.org) classes that inherit from the new base class `FuzzyClassifier`. Each classifier has a generator named after its function with the suffix `Model` (e.g. `FuzzyGaussianNaiveBayesModel`, `GauNBFuzzyParamModel`, `DWFuzzyGammaNaiveBayesModel`) and the methods `$new()`, `$fit(train, cl)`, `$predict(newdata, type)`, `$set_params(...)`, `$is_fitted()` and `$print()`. Calls can be chained: `FuzzyGammaNaiveBayesModel$new()$fit(x, y)$predict(x_new)`.
- Training now reports its progress with an informative message (classifier, number of observations, predictors and classes, and the training time). It can be silenced with `verbose = FALSE` or globally with `options(FuzzyClass.verbose = FALSE)` (see `fuzzy_verbose()`).
- The functional interface is unchanged: `FuzzyGaussianNaiveBayes(train, cl)` and the other functions train and return the R6 object, and `predict(fit, newdata)` / `predict(fit, newdata, type = "matrix")` keep working. `type = "prob"` is now accepted as a synonym of `"matrix"`. Predictions are identical to version 0.2.0.
- The estimated quantities are now stored in `fit$estimates` (they were the elements of the S3 list returned in previous versions), together with `fit$params`, `fit$predictors`, `fit$classes`, `fit$n_obs` and `fit$fit_time`.
- Informative error messages (powered by `cli`) for untrained models, `train`/`cl` with different sizes and missing required arguments (`wdelta`, `weta`).

## tidymodels

- New parsnip model `fuzzy_naive_bayes()`, so every classifier can be used with `parsnip`, `workflows`, `recipes`, `rsample`, `tune` and `yardstick`. The classifier is chosen by the engine (`"gaussian"`, `"gamma"`, `"beta"`, `"exponential"`, `"binomial"`, `"poisson"`, `"geometric"`, `"trapezoidal"`, `"triangular"`, `"hypergeometric"`, `"bayes_rule"`, `"naive"`, `"dw_gamma"`, `"dw_hypergeometric"`, `"gaussian_fuzzy_param"`, `"exponential_fuzzy_param"` and `"poisson_fuzzy_param"`).
- The main arguments `fuzzy`, `alpha_cut` and `ranking_method` (`metd`) can be tuned with `tune()`; their `dials` parameter objects are `fuzzy_membership()`, `alpha_cut()` and `ranking_method()`.
- `parsnip`, `dials` and the other tidymodels packages are only suggested: the model is registered when `parsnip` is loaded.

## Other changes

- Requires R >= 4.1.0. New imports: `R6` and `cli`.
- The tests now use the standard `testthat` structure (`tests/testthat/`), so they run with `R CMD check` and `devtools::test()`.

# FuzzyClass 0.2.0

- Added new method for fuzzy number ordering as `metd = 5` in functions `GauNBFuzzyParam`, `ExpNBFuzzyParam`, and `PoiNBFuzzyParam`.
- Significant performance optimization across all classifiers: vectorized predictions using native matrix operations and `max.col` for substantial speedups.
- Fixed discrete variables validation (`verifyNumbersFunction`) ensuring compatibility with R 4.2+ and resolving missing argument errors in `DWFuzzyHipergeometricNaiveBayes`, `FuzzyBinomialNaiveBayes`, `FuzzyHipergeometricNaiveBayes`, and `FuzzyPoissonNaiveBayes`.
- Fixed mathematical consistency in `FuzzyHipergeometricNaiveBayes` by applying joint membership once to the class score, consistent with other models.
- Fixed prior class probability multiplication in `FuzzyGeoNaiveBayes` applied once per class.
- Cleaned up package dependencies: removed unused dependencies (`dplyr`, `tibble`, `tidyr`, `rlang`, `purrr`) from `Imports`.
- Fixed class/label misalignment: parameters were estimated in the order the classes appear in the data while predictions were labelled with the sorted factor levels, so predictions were wrong whenever the training data were not sorted by class. All classifiers now use the factor levels (`get_classes()`) for estimation and prediction, and the columns of `type = "matrix"` follow the levels.
- `FuzzyGammaNaiveBayes` / `DWFuzzyGammaNaiveBayes`: exact Gamma maximum likelihood estimates (the previous optimisation was bounded by `max(x)`, which truncated the shape/scale).
- `FuzzyBetaNaiveBayes`: removed the artificial upper bound (10) on the Beta shapes; the method of moments is used as starting value.
- `FuzzyGeoNaiveBayes`: probability mass function consistent with the estimator (`dgeom`, support 0, 1, 2, ...); memberships use the same intervals in training and prediction, empty intervals (and values outside the training range) receive 0.001.
- `FuzzyBinomialNaiveBayes`: closed-form method of moments with `n >= max(x)`.
- `GauNBFuzzyParam` / `ExpNBFuzzyParam`: confidence intervals (alpha-cuts) use the class sample size instead of the total sample size. `ExpNBFuzzyParam` ranks densities (as the Gaussian and Poisson versions), since the ranking methods assume non-negative fuzzy numbers.
- `PoiNBFuzzyParam`: alpha-cuts are now nested (they shrink as alpha increases).
- Ranking methods (`metd = 1..5`) vectorized; the closed form of the Q distance replaces one eigen decomposition per observation. `doParallel`, `foreach` and `parallel` are no longer needed; `cores` is kept for backward compatibility.
- `FuzzyTriangularNaiveBayes` / `FuzzyTrapezoidalNaiveBayes`: densities outside the support are floored at 1e-10, avoiding ties decided by class order.
- `FuzzyNaiveBayes`: the arguments `m` and `Pi` are now passed to the estimators (they were ignored).


# FuzzyClass 0.1.7

- Add the "Fuzzy Hipergeometric Naive Bayes classifier" as `FuzzyHipergeometricNaiveBayes`
- Add the "Double Weighted Fuzzy Hipergeometric Naive Bayes classifier" as `DWFuzzyHipergeometricNaiveBayes`
- Fix: memberships functions. Now they work correctly with dataframes 

# FuzzyClass 0.1.6

- Add the "Fuzzy Rule-based System" with some functions
- Fix: `FuzzyNaiveBayes` to one column in the data
- Fix: "membership" to dataframe with one column in the data
- Fix: "Frequency" to discrete distributions


# FuzzyClass 0.1.5

- Add the "double weighted fuzzy gamma naive bayes classifier" as `DWFuzzyGammaNaiveBayes`
- Add the "geometric fuzzy gamma naive bayes classifier" as `GeometricFuzzyGammaNaiveBayes`
- Add the "Fuzzy Bayes Rule classifier" as `FuzzyBayesRule`
- Remove the ordering method for functions with fuzzy parameters `metd=4`.

# FuzzyClass 0.1.4

- Added a new ordering method for functions with fuzzy parameters: `GauNBFuzzyParam`, `ExpNBFuzzyParam`,`GamNBFuzzyParam` and `PoiNBFuzzyParam`. The form can be used considering `metd = 4`.

# FuzzyClass 0.1.3

- Update in parameters estimation in function `FuzzyTriangularNaiveBayes`.
- Update `predict` has been optimized to functions `FuzzyGammaNaiveBayes`.
- Update functions of `GamNBFuzzyParam` and `GauNBFuzzyParam`.
- `predict` function has been optimized to `FuzzyGammaNaiveBayes`, `FuzzyBetaNaiveBayes`, `FuzzyBinomialNaiveBayes`,`FuzzyExponencialNaiveBayes`,`FuzzyTriangularNaiveBayes`,`FuzzyPoissonNaiveBayes`

# FuzzyClass 0.1.2

## New features

- Changed `\()` by `function(.)` in function `FuzzyNaiveBayes`, line 262.
- Changed `description` in `FuzzyClass` Documentation



# FuzzyClass 0.1.1

## New features

- Estimation new form in function `FuzzyGammaNaiveBayes`.

