# Fuzzy Gamma Naive Bayes

`FuzzyGammaNaiveBayes` Fuzzy Gamma Naive Bayes

## Usage

``` r
FuzzyGammaNaiveBayes(train, cl, cores = 2, fuzzy = TRUE)
```

## Arguments

- train:

  matrix or data frame of training set cases.

- cl:

  factor of true classifications of training set

- cores:

  kept for backward compatibility; prediction is vectorized and does not
  start parallel workers (default = 2)

- fuzzy:

  boolean variable to use the membership function

## Value

A vector of classifications

## References

Moraes RM, Soares EAMG, Machado LS (2018). “A Fuzzy Gamma Naive Bayes
Classifier.” In *Data Science and Knowledge Engineering for Sensing
Decision Support: Proceedings of the 13th International FLINS Conference
(FLINS 2018)*, 691–699. World Scientific.

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
fit_NBT <- FuzzyGammaNaiveBayes(
  train = Train[, -5],
  cl = Train[, 5], cores = 2
)

pred_NBT <- predict(fit_NBT, test)

head(pred_NBT)
#> [1] setosa setosa setosa setosa setosa setosa
#> Levels: setosa versicolor virginica
head(Test[, 5])
#> [1] setosa setosa setosa setosa setosa setosa
#> Levels: setosa versicolor virginica
```
