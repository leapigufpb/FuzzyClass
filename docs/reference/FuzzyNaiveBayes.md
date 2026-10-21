# Fuzzy Naive Bayes

`FuzzyNaiveBayes` Fuzzy Naive Bayes

## Usage

``` r
FuzzyNaiveBayes(
  train,
  cl,
  fuzzy = TRUE,
  m = NULL,
  Pi = NULL,
  verbose = fuzzy_verbose()
)
```

## Arguments

- train:

  matrix or data frame of training set cases

- cl:

  factor of true classifications of training set

- fuzzy:

  boolean variable to use the membership function

- m:

  is M/N, where M is the number of classes and N is the number of train
  lines

- Pi:

  is 1/M, where M is the number of classes

- verbose:

  logical; if `TRUE` (default, see
  [`fuzzy_verbose()`](https://leapigufpb.github.io/FuzzyClass/reference/fuzzy_verbose.md))
  a message reports the training progress.

## Value

A trained
[FuzzyNaiveBayesModel](https://leapigufpb.github.io/FuzzyClass/reference/FuzzyNaiveBayesModel.md)
R6 object (which inherits from
[FuzzyClassifier](https://leapigufpb.github.io/FuzzyClass/reference/FuzzyClassifier.md)).
Use `predict(object, newdata, type = "class")` or
`object$predict(newdata)` to classify new observations; `type = "prob"`
(or `"matrix"`) returns the matrix of class probabilities.

## References

Moraes RM, Machado LS (2009). “Another approach for fuzzy naive bayes
applied on online training assessment in virtual reality simulators.” In
*Proceedings of Safety Health and Environmental World Congress*, 62–66.

## See also

[FuzzyNaiveBayesModel](https://leapigufpb.github.io/FuzzyClass/reference/FuzzyNaiveBayesModel.md)
for the R6 interface and
[`fuzzy_naive_bayes()`](https://leapigufpb.github.io/FuzzyClass/reference/fuzzy_naive_bayes.md)
(engine `"naive"`) for the tidymodels interface.

## Examples

``` r

set.seed(1) # determining a seed
data(HouseVotes84)

# Splitting into Training and Testing
split <- caTools::sample.split(t(HouseVotes84[, 1]), SplitRatio = 0.7)
Train <- subset(HouseVotes84, split == "TRUE")
Test <- subset(HouseVotes84, split == "FALSE")
# ----------------
# matrix or data frame of test set cases.
# A vector will be interpreted as a row vector for a single case.
test <- Test[, -1]
fit_FNB <- FuzzyNaiveBayes(
  train = Train[, -1],
  cl = Train[, 1]
)
#> ℹ Fuzzy Naive Bayes: 305 observations, 16 predictors and 2 classes
#> ℹ Training the model: estimating its parameters
#> ✔ Model trained successfully [14ms]
#> 

pred_FNB <- predict(fit_FNB, test)

head(pred_FNB)
#> [1] republican democrat   republican republican republican democrat  
#> Levels: democrat republican
head(Test[, 1])
#> [1] democrat   democrat   democrat   republican republican democrat  
#> Levels: democrat republican
```
