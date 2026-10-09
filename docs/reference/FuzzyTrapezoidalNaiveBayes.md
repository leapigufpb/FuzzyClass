# Fuzzy Naive Bayes Trapezoidal Classifier

`FuzzyTrapezoidalNaiveBayes` Fuzzy Naive Bayes Trapezoidal Classifier

## Usage

``` r
FuzzyTrapezoidalNaiveBayes(train, cl, cores = 2, fuzzy = T)
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

Lopes A, Ferreira J, Machado LS, Moraes RM (2022). “A New Fuzzy
Trapezoidal Naive Bayes Network as basis for Assessment in Training
based on Virtual Reality.” In *The 15th International FLINS Conference
on Machine learning, Multi agent and Cyber physical systems (FLINS
2022)*. Nankai University.

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
fit_NBT <- FuzzyTrapezoidalNaiveBayes(
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
