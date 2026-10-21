# Verbosity of the FuzzyClass classifiers

By default, training a classifier prints a short progress message. Set
`options(FuzzyClass.verbose = FALSE)` to silence it globally (useful,
for example, when tuning many models with tidymodels), or use the
argument `verbose` of `$fit()` and of the functional interfaces.

## Usage

``` r
fuzzy_verbose()
```

## Value

The value of the option `FuzzyClass.verbose` (default `TRUE`).

## Examples

``` r
fuzzy_verbose()
#> [1] TRUE
old <- options(FuzzyClass.verbose = FALSE)
fit <- FuzzyGaussianNaiveBayes(iris[, -5], iris$Species) # silent
options(old)
```
