# Fuzzy Exponential Naive Bayes Classifier with Fuzzy parameters

`ExpNBFuzzyParam` Fuzzy Exponential Naive Bayes Classifier with Fuzzy
parameters

## Usage

``` r
ExpNBFuzzyParam(
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

  matrix or data frame of training set cases

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

Rodrigues AK, Batista TV, Moraes RM, Machado LS (2016). “A new
exponential naive bayes classifier with fuzzy parameters.” In *2016 IEEE
International Conference on Fuzzy Systems (FUZZ-IEEE)*, 1188–1194. IEEE.

## Examples

``` r

set.seed(1) # determining a seed
data(VirtualRealityData)

# Splitting into Training and Testing
split <- caTools::sample.split(t(VirtualRealityData[, 1]), SplitRatio = 0.7)
Train <- subset(VirtualRealityData, split == "TRUE")
Test <- subset(VirtualRealityData, split == "FALSE")

# ----------------
# matrix or data frame of test set cases.
# A vector will be interpreted as a row vector for a single case.
test <- Test[, -4]
fit_FENB <- ExpNBFuzzyParam(
  train = Train[, -4],
  cl = Train[, 4], metd = 1, cores = 2
)

pred_FENB <- predict(fit_FENB, test)

head(pred_FENB)
#> [1] 1 1 1 1 1 1
#> Levels: 1 2 3
head(Test[, 4])
#> [1] 1 1 1 1 1 1
```
