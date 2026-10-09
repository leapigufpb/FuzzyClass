# Fuzzy Gaussian Naive Bayes Classifier Zadeh-based

`FuzzyGaussianNaiveBayes` Fuzzy Gaussian Naive Bayes Classifier
Zadeh-based

## Usage

``` r
FuzzyGaussianNaiveBayes(train, cl, cores = 2, fuzzy = TRUE)
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

Moraes RM, Machado LS (2012). “Online Assessment in Medical Simulators
Based on Virtual Reality Using Fuzzy Gaussian Naive Bayes.” *Journal of
Multiple-Valued Logic & Soft Computing*, **18**.

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
fit_GNB <- FuzzyGaussianNaiveBayes(
  train = Train[, -5],
  cl = Train[, 5], cores = 2
)

pred_GNB <- predict(fit_GNB, test)

head(pred_GNB)
#> [1] setosa setosa setosa setosa setosa setosa
#> Levels: setosa versicolor virginica
head(Test[, 5])
#> [1] setosa setosa setosa setosa setosa setosa
#> Levels: setosa versicolor virginica
```
