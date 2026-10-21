# Fuzzy Gamma Naive Bayes

`FuzzyGammaNaiveBayes` Fuzzy Gamma Naive Bayes

## Usage

``` r
FuzzyGammaNaiveBayes(
  train,
  cl,
  cores = 2,
  fuzzy = TRUE,
  verbose = fuzzy_verbose()
)
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

- verbose:

  logical; if `TRUE` (default, see
  [`fuzzy_verbose()`](https://leapigufpb.github.io/FuzzyClass/reference/fuzzy_verbose.md))
  a message reports the training progress.

## Value

A trained
[FuzzyGammaNaiveBayesModel](https://leapigufpb.github.io/FuzzyClass/reference/FuzzyGammaNaiveBayesModel.md)
R6 object (which inherits from
[FuzzyClassifier](https://leapigufpb.github.io/FuzzyClass/reference/FuzzyClassifier.md)).
Use `predict(object, newdata, type = "class")` or
`object$predict(newdata)` to classify new observations; `type = "prob"`
(or `"matrix"`) returns the matrix of class probabilities.

## References

Moraes RM, Soares EAMG, Machado LS (2018). “A Fuzzy Gamma Naive Bayes
Classifier.” In *Data Science and Knowledge Engineering for Sensing
Decision Support: Proceedings of the 13th International FLINS Conference
(FLINS 2018)*, 691–699. World Scientific.

## See also

[FuzzyGammaNaiveBayesModel](https://leapigufpb.github.io/FuzzyClass/reference/FuzzyGammaNaiveBayesModel.md)
for the R6 interface and
[`fuzzy_naive_bayes()`](https://leapigufpb.github.io/FuzzyClass/reference/fuzzy_naive_bayes.md)
(engine `"gamma"`) for the tidymodels interface.

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
#> ℹ Fuzzy Gamma Naive Bayes: 105 observations, 4 predictors and 3 classes
#> ℹ Training the model: estimating its parameters
#> ✔ Model trained successfully [6ms]
#> 

pred_NBT <- predict(fit_NBT, test)

head(pred_NBT)
#> [1] setosa setosa setosa setosa setosa setosa
#> Levels: setosa versicolor virginica
head(Test[, 5])
#> [1] setosa setosa setosa setosa setosa setosa
#> Levels: setosa versicolor virginica
```
