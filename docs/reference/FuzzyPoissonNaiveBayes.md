# Fuzzy Poisson Naive Bayes

`FuzzyPoissonNaiveBayes` Fuzzy Poisson Naive Bayes

## Usage

``` r
FuzzyPoissonNaiveBayes(
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
[FuzzyPoissonNaiveBayesModel](https://leapigufpb.github.io/FuzzyClass/reference/FuzzyPoissonNaiveBayesModel.md)
R6 object (which inherits from
[FuzzyClassifier](https://leapigufpb.github.io/FuzzyClass/reference/FuzzyClassifier.md)).
Use `predict(object, newdata, type = "class")` or
`object$predict(newdata)` to classify new observations; `type = "prob"`
(or `"matrix"`) returns the matrix of class probabilities.

## References

Moraes RM, Machado LS (2015). “A fuzzy poisson naive bayes classifier
for epidemiological purposes.” In *2015 7th International Joint
Conference on Computational Intelligence (IJCCI)*, volume 2, 193–198.
IEEE.

## See also

[FuzzyPoissonNaiveBayesModel](https://leapigufpb.github.io/FuzzyClass/reference/FuzzyPoissonNaiveBayesModel.md)
for the R6 interface and
[`fuzzy_naive_bayes()`](https://leapigufpb.github.io/FuzzyClass/reference/fuzzy_naive_bayes.md)
(engine `"poisson"`) for the tidymodels interface.

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
fit_NBT <- FuzzyPoissonNaiveBayes(
  train = Train[, -4],
  cl = Train[, 4], cores = 2
)
#> ℹ Fuzzy Poisson Naive Bayes: 210 observations, 3 predictors and 3 classes
#> ℹ Training the model: estimating its parameters
#> ✔ Model trained successfully [5ms]
#> 

pred_NBT <- predict(fit_NBT, test)

head(pred_NBT)
#> [1] 2 2 2 1 1 1
#> Levels: 1 2 3
head(Test[, 4])
#> [1] 1 1 1 1 1 1
```
