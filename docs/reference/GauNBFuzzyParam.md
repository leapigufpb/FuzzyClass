# Fuzzy Gaussian Naive Bayes Classifier with Fuzzy parameters

`GauNBFuzzyParam` Fuzzy Gaussian Naive Bayes Classifier with Fuzzy
parameters

## Usage

``` r
GauNBFuzzyParam(
  train,
  cl,
  alphacut = 1e-04,
  metd = 2,
  alp = c(0.35, 0.7, 0.86),
  w = c(0.1, 0.3, 0.6),
  cores = 2
)
```

## Arguments

- train:

  matrix or data frame of training set cases.

- cl:

  factor of true classifications of training set

- alphacut:

  value of the alpha-cut parameter, this value is between 0 and 1.

- metd:

  Method of transforming the triangle into scalar, It is the type of
  data entry for the test sample, use 'metd=1' if you want to use the
  Yager technique, 'metd=2' if you want to use the Q technique of the
  uniformity test (article: Directional Statistics and Shape analysis),
  'metd=3' if you want to use the Thorani technique, 'metd=4' if you
  want to use the alpha-order technique and 'metd=5' if you want to use
  the GTFN technique (article: A novel approach for arithmetic
  operations and ranking of generalized fuzzy numbers with application).

- alp:

  When metd for 4, it is necessary to have alp which are alpha-cut
  defined

- w:

  When metd for 4, it is necessary to have w which are alpha-cut weights
  defined

- cores:

  kept for backward compatibility; prediction is vectorized and does not
  start parallel workers (default = 2)

## Value

A vector of classifications

## References

Moraes RM, Ferreira JA, Machado LS (2021). “A New Bayesian Network Based
on Gaussian Naive Bayes with Fuzzy Parameters for Training Assessment in
Virtual Simulators.” *International Journal of Fuzzy Systems*,
**23**(3), 849–861.

## Examples

``` r

set.seed(1) # determining a seed
data(iris)

# Splitting into Training and Testing
split <- caTools::sample.split(t(iris[, 1]), SplitRatio = 0.7)
Train <- subset(iris, split == "TRUE")
Test <- subset(iris, split == "FALSE")
# ----------------
# matrix or data frame of test set cases.
# A vector will be interpreted as a row vector for a single case.
test <- Test[, -5]
fit_FGNB <- GauNBFuzzyParam(
  train = Train[, -5],
  cl = Train[, 5], metd = 1, cores = 2
)

pred_FGNB <- predict(fit_FGNB, test)

head(pred_FGNB)
#> [1] setosa setosa setosa setosa setosa setosa
#> Levels: setosa versicolor virginica
head(Test[, 5])
#> [1] setosa setosa setosa setosa setosa setosa
#> Levels: setosa versicolor virginica
```
