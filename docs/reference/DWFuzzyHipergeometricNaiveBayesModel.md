# Double Weighted Fuzzy Hypergeometric Naive Bayes (R6 class)

R6 implementation of the Double Weighted Fuzzy Hypergeometric Naive
Bayes classifier. Create the model with
`DWFuzzyHipergeometricNaiveBayesModel$new()`, train it with
`$fit(train, cl)` and classify new observations with
`$predict(newdata)`. The functional interface
[`DWFuzzyHipergeometricNaiveBayes()`](https://leapigufpb.github.io/FuzzyClass/reference/DWFuzzyHipergeometricNaiveBayes.md)
trains and returns an object of this class.

## Super class

[`FuzzyClassifier`](https://leapigufpb.github.io/FuzzyClass/reference/FuzzyClassifier.md)
-\> `DWFuzzyHipergeometricNaiveBayes`

## Methods

### Public methods

- [`DWFuzzyHipergeometricNaiveBayes$new()`](#method-DWFuzzyHipergeometricNaiveBayes-initialize)

- [`DWFuzzyHipergeometricNaiveBayes$clone()`](#method-DWFuzzyHipergeometricNaiveBayes-clone)

Inherited methods

- [`FuzzyClassifier$fit()`](https://leapigufpb.github.io/FuzzyClass/reference/FuzzyClassifier.html#method-fit)
- [`FuzzyClassifier$is_fitted()`](https://leapigufpb.github.io/FuzzyClass/reference/FuzzyClassifier.html#method-is_fitted)
- [`FuzzyClassifier$predict()`](https://leapigufpb.github.io/FuzzyClass/reference/FuzzyClassifier.html#method-predict)
- [`FuzzyClassifier$print()`](https://leapigufpb.github.io/FuzzyClass/reference/FuzzyClassifier.html#method-print)
- [`FuzzyClassifier$set_params()`](https://leapigufpb.github.io/FuzzyClass/reference/FuzzyClassifier.html#method-set_params)

------------------------------------------------------------------------

### `DWFuzzyHipergeometricNaiveBayes$new()`

Create a new (untrained) classifier.

#### Usage

    DWFuzzyHipergeometricNaiveBayes$new(fuzzy = TRUE, wdelta, weta)

#### Arguments

- `fuzzy`:

  See
  [`DWFuzzyHipergeometricNaiveBayes()`](https://leapigufpb.github.io/FuzzyClass/reference/DWFuzzyHipergeometricNaiveBayes.md).

- `wdelta`:

  See
  [`DWFuzzyHipergeometricNaiveBayes()`](https://leapigufpb.github.io/FuzzyClass/reference/DWFuzzyHipergeometricNaiveBayes.md).

- `weta`:

  See
  [`DWFuzzyHipergeometricNaiveBayes()`](https://leapigufpb.github.io/FuzzyClass/reference/DWFuzzyHipergeometricNaiveBayes.md).

------------------------------------------------------------------------

### `DWFuzzyHipergeometricNaiveBayes$clone()`

The objects of this class are cloneable with this method.

#### Usage

    DWFuzzyHipergeometricNaiveBayes$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.

## Examples

``` r
model <- DWFuzzyHipergeometricNaiveBayesModel$new(wdelta = c(0.5, 0.5), weta = 1)
x <- data.frame(s1 = c(1, 2, 1, 2), n1 = c(5, 5, 6, 6))
y <- factor(c("A", "A", "B", "B"))
model$fit(train = x, cl = y)
#> ℹ Double Weighted Fuzzy Hypergeometric Naive Bayes: 4 observations, 2 predictors and 2 classes
#> ℹ Training the model: estimating its parameters
#> ✔ Model trained successfully [4ms]
#> 
model$predict(x)
#> [1] A B A B
#> Levels: A B
model
#> <DWFuzzyHipergeometricNaiveBayes> Double Weighted Fuzzy Hypergeometric Naive Bayes
#> ✔ Trained on 4 observations in 0.001s
#>   Hyperparameters: fuzzy = TRUE, wdelta = c(0.5, 0.5), weta = 1
#>   Predictors (2): s1, n1
#>   Classes (2): A, B
```
