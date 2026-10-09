# Fuzzy Poisson Naive Bayes Classifier with Fuzzy parameters

`PoiNBFuzzyParam` Fuzzy Poisson Naive Bayes Classifier with Fuzzy
parameters

## Usage

``` r
PoiNBFuzzyParam(
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

Soares E, Machado L, Moraes R (2016). “Assessment of poisson naive bayes
classifier with fuzzy parameters using data from different statistical
distributions.” In *IV Bazilian Congress on Fuzzy Sistems (CBSF 2016)*,
volume 1, 57–68.

## Examples

``` r

set.seed(1) # determining a seed
class1 <- data.frame(vari1 = rpois(100,lambda = 2),
                     vari2 = rpois(100,lambda = 2),
                     vari3 = rpois(100,lambda = 2), class = 1)
class2 <- data.frame(vari1 = rpois(100,lambda = 1),
                     vari2 = rpois(100,lambda = 1),
                     vari3 = rpois(100,lambda = 1), class = 2)
class3 <- data.frame(vari1 = rpois(100,lambda = 5),
                     vari2 = rpois(100,lambda = 5),
                     vari3 = rpois(100,lambda = 5), class = 3)
data <- rbind(class1,class2,class3)

# Splitting into Training and Testing
split <- caTools::sample.split(t(data[, 1]), SplitRatio = 0.7)
Train <- subset(data, split == "TRUE")
Test <- subset(data, split == "FALSE")
# ----------------
# matrix or data frame of test set cases.
# A vector will be interpreted as a row vector for a single case.
test <- Test[, -4]
fit_FPoiNB <- PoiNBFuzzyParam(
  train = Train[, -4],
  cl = Train[, 4], metd = 1, cores = 2
)

pred_FPoiNB <- predict(fit_FPoiNB, test)

head(pred_FPoiNB)
#> [1] 2 2 2 1 2 2
#> Levels: 1 2 3
head(Test[, 4])
#> [1] 1 1 1 1 1 1
```
