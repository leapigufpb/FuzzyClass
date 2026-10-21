# Double Weighted Fuzzy Gamma Naive Bayes

`DWFuzzyGammaNaiveBayes` Double Weighted Fuzzy Gamma Naive Bayes

## Usage

``` r
DWFuzzyGammaNaiveBayes(
  train,
  cl,
  cores = 2,
  fuzzy = TRUE,
  wdelta,
  weta,
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

- wdelta:

  vector weight each class

- weta:

  vector weight each feature

- verbose:

  logical; if `TRUE` (default, see
  [`fuzzy_verbose()`](https://leapigufpb.github.io/FuzzyClass/reference/fuzzy_verbose.md))
  a message reports the training progress.

## Value

A trained
[DWFuzzyGammaNaiveBayesModel](https://leapigufpb.github.io/FuzzyClass/reference/DWFuzzyGammaNaiveBayesModel.md)
R6 object (which inherits from
[FuzzyClassifier](https://leapigufpb.github.io/FuzzyClass/reference/FuzzyClassifier.md)).
Use `predict(object, newdata, type = "class")` or
`object$predict(newdata)` to classify new observations; `type = "prob"`
(or `"matrix"`) returns the matrix of class probabilities.

## References

Moraes RM, Soares EAMG, Machado LS (2020). “A double weighted fuzzy
gamma naive bayes classifier.” *Journal of Intelligent & Fuzzy Systems*,
**38**(1), 577–588.

## See also

[DWFuzzyGammaNaiveBayesModel](https://leapigufpb.github.io/FuzzyClass/reference/DWFuzzyGammaNaiveBayesModel.md)
for the R6 interface and
[`fuzzy_naive_bayes()`](https://leapigufpb.github.io/FuzzyClass/reference/fuzzy_naive_bayes.md)
(engine `"dw_gamma"`) for the tidymodels interface.

## Examples

``` r

set.seed(1) # determining a seed
data(GamWeightData)

# Splitting into Training and Testing
split <- caTools::sample.split(t(GamWeightData[, 1]), SplitRatio = 0.7)
Train <- subset(GamWeightData, split == "TRUE")
Test <- subset(GamWeightData, split == "FALSE")
# ----------------
# matrix or data frame of test set cases.
# A vector will be interpreted as a row vector for a single case.
test <- Test[, -4]
fit_NBT <- DWFuzzyGammaNaiveBayes(
  train = Train[, -4],
  cl = Train[, 4], cores = 2,
  wdelta = c(2.002/6,1.998/6,2.000/6),
  weta = c(3/10,2/10, 5/10)
)
#> ℹ Double Weighted Fuzzy Gamma Naive Bayes: 420 observations, 3 predictors and 3 classes
#> ℹ Training the model: estimating its parameters
#> ✔ Model trained successfully [7ms]
#> 

pred_NBT <- predict(fit_NBT, test)

head(pred_NBT)
#> [1] 1 1 1 1 1 1
#> Levels: 1 2 3
head(Test[, 4])
#> [1] 1 1 1 1 1 1
#> Levels: 1 2 3
```
