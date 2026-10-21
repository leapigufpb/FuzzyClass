# Fuzzy Hypergeometric Naive Bayes (R6 class)

R6 implementation of the Fuzzy Hypergeometric Naive Bayes classifier.
Create the model with `FuzzyHipergeometricNaiveBayesModel$new()`, train
it with `$fit(train, cl)` and classify new observations with
`$predict(newdata)`. The functional interface
[`FuzzyHipergeometricNaiveBayes()`](https://leapigufpb.github.io/FuzzyClass/reference/FuzzyHipergeometricNaiveBayes.md)
trains and returns an object of this class.

## Super class

[`FuzzyClassifier`](https://leapigufpb.github.io/FuzzyClass/reference/FuzzyClassifier.md)
-\> `FuzzyHipergeometricNaiveBayes`

## Methods

### Public methods

- [`FuzzyHipergeometricNaiveBayes$new()`](#method-FuzzyHipergeometricNaiveBayes-initialize)

- [`FuzzyHipergeometricNaiveBayes$clone()`](#method-FuzzyHipergeometricNaiveBayes-clone)

Inherited methods

- [`FuzzyClassifier$fit()`](https://leapigufpb.github.io/FuzzyClass/reference/FuzzyClassifier.html#method-fit)
- [`FuzzyClassifier$is_fitted()`](https://leapigufpb.github.io/FuzzyClass/reference/FuzzyClassifier.html#method-is_fitted)
- [`FuzzyClassifier$predict()`](https://leapigufpb.github.io/FuzzyClass/reference/FuzzyClassifier.html#method-predict)
- [`FuzzyClassifier$print()`](https://leapigufpb.github.io/FuzzyClass/reference/FuzzyClassifier.html#method-print)
- [`FuzzyClassifier$set_params()`](https://leapigufpb.github.io/FuzzyClass/reference/FuzzyClassifier.html#method-set_params)

------------------------------------------------------------------------

### `FuzzyHipergeometricNaiveBayes$new()`

Create a new (untrained) classifier.

#### Usage

    FuzzyHipergeometricNaiveBayes$new(fuzzy = TRUE)

#### Arguments

- `fuzzy`:

  See
  [`FuzzyHipergeometricNaiveBayes()`](https://leapigufpb.github.io/FuzzyClass/reference/FuzzyHipergeometricNaiveBayes.md).

------------------------------------------------------------------------

### `FuzzyHipergeometricNaiveBayes$clone()`

The objects of this class are cloneable with this method.

#### Usage

    FuzzyHipergeometricNaiveBayes$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.

## Examples

``` r
model <- FuzzyHipergeometricNaiveBayesModel$new()
x <- data.frame(s1 = c(1, 2, 1, 2), n1 = c(5, 5, 6, 6))
y <- factor(c("A", "A", "B", "B"))
model$fit(train = x, cl = y)
#> ℹ Fuzzy Hypergeometric Naive Bayes: 4 observations, 2 predictors and 2 classes
#> ℹ Training the model: estimating its parameters
#> ✔ Model trained successfully [10ms]
#> 
model$predict(x)
#> [1] A B A B
#> Levels: A B
model
#> <FuzzyHipergeometricNaiveBayes> Fuzzy Hypergeometric Naive Bayes
#> ✔ Trained on 4 observations in 0.001s
#>   Hyperparameters: fuzzy = TRUE
#>   Predictors (2): s1, n1
#>   Classes (2): A, B
```
