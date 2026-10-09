# Changelog

## FuzzyClass 0.2.0

- Added new method for fuzzy number ordering as `metd = 5` in functions
  `GauNBFuzzyParam`, `ExpNBFuzzyParam`, and `PoiNBFuzzyParam`.
- Significant performance optimization across all classifiers:
  vectorized predictions using native matrix operations and `max.col`
  for substantial speedups.
- Fixed discrete variables validation (`verifyNumbersFunction`) ensuring
  compatibility with R 4.2+ and resolving missing argument errors in
  `DWFuzzyHipergeometricNaiveBayes`, `FuzzyBinomialNaiveBayes`,
  `FuzzyHipergeometricNaiveBayes`, and `FuzzyPoissonNaiveBayes`.
- Fixed mathematical consistency in `FuzzyHipergeometricNaiveBayes` by
  applying joint membership once to the class score, consistent with
  other models.
- Fixed prior class probability multiplication in `FuzzyGeoNaiveBayes`
  applied once per class.
- Cleaned up package dependencies: removed unused dependencies (`dplyr`,
  `tibble`, `tidyr`, `rlang`, `purrr`) from `Imports`.
- Fixed class/label misalignment: parameters were estimated in the order
  the classes appear in the data while predictions were labelled with
  the sorted factor levels, so predictions were wrong whenever the
  training data were not sorted by class. All classifiers now use the
  factor levels (`get_classes()`) for estimation and prediction, and the
  columns of `type = "matrix"` follow the levels.
- `FuzzyGammaNaiveBayes` / `DWFuzzyGammaNaiveBayes`: exact Gamma maximum
  likelihood estimates (the previous optimisation was bounded by
  `max(x)`, which truncated the shape/scale).
- `FuzzyBetaNaiveBayes`: removed the artificial upper bound (10) on the
  Beta shapes; the method of moments is used as starting value.
- `FuzzyGeoNaiveBayes`: probability mass function consistent with the
  estimator (`dgeom`, support 0, 1, 2, …); memberships use the same
  intervals in training and prediction, empty intervals (and values
  outside the training range) receive 0.001.
- `FuzzyBinomialNaiveBayes`: closed-form method of moments with
  `n >= max(x)`.
- `GauNBFuzzyParam` / `ExpNBFuzzyParam`: confidence intervals
  (alpha-cuts) use the class sample size instead of the total sample
  size. `ExpNBFuzzyParam` ranks densities (as the Gaussian and Poisson
  versions), since the ranking methods assume non-negative fuzzy
  numbers.
- `PoiNBFuzzyParam`: alpha-cuts are now nested (they shrink as alpha
  increases).
- Ranking methods (`metd = 1..5`) vectorized; the closed form of the Q
  distance replaces one eigen decomposition per observation.
  `doParallel`, `foreach` and `parallel` are no longer needed; `cores`
  is kept for backward compatibility.
- `FuzzyTriangularNaiveBayes` / `FuzzyTrapezoidalNaiveBayes`: densities
  outside the support are floored at 1e-10, avoiding ties decided by
  class order.
- `FuzzyNaiveBayes`: the arguments `m` and `Pi` are now passed to the
  estimators (they were ignored).

## FuzzyClass 0.1.7

CRAN release: 2025-09-03

- Add the “Fuzzy Hipergeometric Naive Bayes classifier” as
  `FuzzyHipergeometricNaiveBayes`
- Add the “Double Weighted Fuzzy Hipergeometric Naive Bayes classifier”
  as `DWFuzzyHipergeometricNaiveBayes`
- Fix: memberships functions. Now they work correctly with dataframes

## FuzzyClass 0.1.6

CRAN release: 2024-02-14

- Add the “Fuzzy Rule-based System” with some functions
- Fix: `FuzzyNaiveBayes` to one column in the data
- Fix: “membership” to dataframe with one column in the data
- Fix: “Frequency” to discrete distributions

## FuzzyClass 0.1.5

CRAN release: 2023-08-17

- Add the “double weighted fuzzy gamma naive bayes classifier” as
  `DWFuzzyGammaNaiveBayes`
- Add the “geometric fuzzy gamma naive bayes classifier” as
  `GeometricFuzzyGammaNaiveBayes`
- Add the “Fuzzy Bayes Rule classifier” as `FuzzyBayesRule`
- Remove the ordering method for functions with fuzzy parameters
  `metd=4`.

## FuzzyClass 0.1.4

CRAN release: 2023-03-07

- Added a new ordering method for functions with fuzzy parameters:
  `GauNBFuzzyParam`, `ExpNBFuzzyParam`,`GamNBFuzzyParam` and
  `PoiNBFuzzyParam`. The form can be used considering `metd = 4`.

## FuzzyClass 0.1.3

CRAN release: 2022-11-16

- Update in parameters estimation in function
  `FuzzyTriangularNaiveBayes`.
- Update `predict` has been optimized to functions
  `FuzzyGammaNaiveBayes`.
- Update functions of `GamNBFuzzyParam` and `GauNBFuzzyParam`.
- `predict` function has been optimized to `FuzzyGammaNaiveBayes`,
  `FuzzyBetaNaiveBayes`,
  `FuzzyBinomialNaiveBayes`,`FuzzyExponencialNaiveBayes`,`FuzzyTriangularNaiveBayes`,`FuzzyPoissonNaiveBayes`

## FuzzyClass 0.1.2

CRAN release: 2022-08-25

### New features

- Changed `\()` by `function(.)` in function `FuzzyNaiveBayes`, line
  262.
- Changed `description` in `FuzzyClass` Documentation

## FuzzyClass 0.1.1

CRAN release: 2022-05-27

### New features

- Estimation new form in function `FuzzyGammaNaiveBayes`.
