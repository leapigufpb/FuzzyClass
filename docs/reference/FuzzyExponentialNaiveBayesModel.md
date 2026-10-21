# Fuzzy Exponential Naive Bayes (R6 class)

R6 implementation of the Fuzzy Exponential Naive Bayes classifier.
Create the model with `FuzzyExponentialNaiveBayesModel$new()`, train it
with `$fit(train, cl)` and classify new observations with
`$predict(newdata)`. The functional interface
[`FuzzyExponentialNaiveBayes()`](https://leapigufpb.github.io/FuzzyClass/reference/FuzzyExponentialNaiveBayes.md)
trains and returns an object of this class.

## Super class

[`FuzzyClassifier`](https://leapigufpb.github.io/FuzzyClass/reference/FuzzyClassifier.md)
-\> `FuzzyExponentialNaiveBayes`

## Methods

### Public methods

- [`FuzzyExponentialNaiveBayes$new()`](#method-FuzzyExponentialNaiveBayes-initialize)

- [`FuzzyExponentialNaiveBayes$clone()`](#method-FuzzyExponentialNaiveBayes-clone)

Inherited methods

- [`FuzzyClassifier$fit()`](https://leapigufpb.github.io/FuzzyClass/reference/FuzzyClassifier.html#method-fit)
- [`FuzzyClassifier$is_fitted()`](https://leapigufpb.github.io/FuzzyClass/reference/FuzzyClassifier.html#method-is_fitted)
- [`FuzzyClassifier$predict()`](https://leapigufpb.github.io/FuzzyClass/reference/FuzzyClassifier.html#method-predict)
- [`FuzzyClassifier$print()`](https://leapigufpb.github.io/FuzzyClass/reference/FuzzyClassifier.html#method-print)
- [`FuzzyClassifier$set_params()`](https://leapigufpb.github.io/FuzzyClass/reference/FuzzyClassifier.html#method-set_params)

------------------------------------------------------------------------

### `FuzzyExponentialNaiveBayes$new()`

Create a new (untrained) classifier.

#### Usage

    FuzzyExponentialNaiveBayes$new(fuzzy = TRUE)

#### Arguments

- `fuzzy`:

  See
  [`FuzzyExponentialNaiveBayes()`](https://leapigufpb.github.io/FuzzyClass/reference/FuzzyExponentialNaiveBayes.md).

------------------------------------------------------------------------

### `FuzzyExponentialNaiveBayes$clone()`

The objects of this class are cloneable with this method.

#### Usage

    FuzzyExponentialNaiveBayes$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.

## Examples

``` r
model <- FuzzyExponentialNaiveBayesModel$new()
model$fit(train = iris[, -5], cl = iris$Species)
#> ℹ Fuzzy Exponential Naive Bayes: 150 observations, 4 predictors and 3 classes
#> ℹ Training the model: estimating its parameters
#> ✔ Model trained successfully [5ms]
#> 
head(model$predict(iris[, -5]))
#> [1] setosa setosa setosa setosa setosa setosa
#> Levels: setosa versicolor virginica
head(model$predict(iris[, -5], type = "prob"))
#>         setosa  versicolor    virginica
#> [1,] 0.9997578 0.000230420 1.180106e-05
#> [2,] 0.9983755 0.001346715 2.777607e-04
#> [3,] 0.9973642 0.002194846 4.409301e-04
#> [4,] 0.9974935 0.002076859 4.296132e-04
#> [5,] 0.9997543 0.000228113 1.753854e-05
#> [6,] 0.9238276 0.057099756 1.907260e-02
model
#> <FuzzyExponentialNaiveBayes> Fuzzy Exponential Naive Bayes
#> ✔ Trained on 150 observations in 0.002s
#>   Hyperparameters: fuzzy = TRUE
#>   Predictors (4): Sepal.Length, Sepal.Width, Petal.Length, Petal.Width
#>   Classes (3): setosa, versicolor, virginica
```
