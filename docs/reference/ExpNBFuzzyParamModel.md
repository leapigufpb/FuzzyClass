# Exponential Naive Bayes with Fuzzy Parameters (R6 class)

R6 implementation of the Exponential Naive Bayes with Fuzzy Parameters
classifier. Create the model with `ExpNBFuzzyParamModel$new()`, train it
with `$fit(train, cl)` and classify new observations with
`$predict(newdata)`. The functional interface
[`ExpNBFuzzyParam()`](https://leapigufpb.github.io/FuzzyClass/reference/ExpNBFuzzyParam.md)
trains and returns an object of this class.

## Super class

[`FuzzyClassifier`](https://leapigufpb.github.io/FuzzyClass/reference/FuzzyClassifier.md)
-\> `ExpNBFuzzyParam`

## Methods

### Public methods

- [`ExpNBFuzzyParam$new()`](#method-ExpNBFuzzyParam-initialize)

- [`ExpNBFuzzyParam$clone()`](#method-ExpNBFuzzyParam-clone)

Inherited methods

- [`FuzzyClassifier$fit()`](https://leapigufpb.github.io/FuzzyClass/reference/FuzzyClassifier.html#method-fit)
- [`FuzzyClassifier$is_fitted()`](https://leapigufpb.github.io/FuzzyClass/reference/FuzzyClassifier.html#method-is_fitted)
- [`FuzzyClassifier$predict()`](https://leapigufpb.github.io/FuzzyClass/reference/FuzzyClassifier.html#method-predict)
- [`FuzzyClassifier$print()`](https://leapigufpb.github.io/FuzzyClass/reference/FuzzyClassifier.html#method-print)
- [`FuzzyClassifier$set_params()`](https://leapigufpb.github.io/FuzzyClass/reference/FuzzyClassifier.html#method-set_params)

------------------------------------------------------------------------

### `ExpNBFuzzyParam$new()`

Create a new (untrained) classifier.

#### Usage

    ExpNBFuzzyParam$new(
      alphacut = 1e-04,
      metd = 2,
      alp = c(0.35, 0.7, 0.86),
      w = c(0.1, 0.3, 0.6)
    )

#### Arguments

- `alphacut`:

  See
  [`ExpNBFuzzyParam()`](https://leapigufpb.github.io/FuzzyClass/reference/ExpNBFuzzyParam.md).

- `metd`:

  See
  [`ExpNBFuzzyParam()`](https://leapigufpb.github.io/FuzzyClass/reference/ExpNBFuzzyParam.md).

- `alp`:

  See
  [`ExpNBFuzzyParam()`](https://leapigufpb.github.io/FuzzyClass/reference/ExpNBFuzzyParam.md).

- `w`:

  See
  [`ExpNBFuzzyParam()`](https://leapigufpb.github.io/FuzzyClass/reference/ExpNBFuzzyParam.md).

------------------------------------------------------------------------

### `ExpNBFuzzyParam$clone()`

The objects of this class are cloneable with this method.

#### Usage

    ExpNBFuzzyParam$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.

## Examples

``` r
model <- ExpNBFuzzyParamModel$new()
model$fit(train = iris[, -5], cl = iris$Species)
#> ℹ Exponential Naive Bayes with Fuzzy Parameters: 150 observations, 4 predictors and 3 classes
#> ℹ Training the model: estimating its parameters
#> ✔ Model trained successfully [5ms]
#> 
head(model$predict(iris[, -5]))
#> [1] setosa setosa setosa setosa setosa setosa
#> Levels: setosa versicolor virginica
head(model$predict(iris[, -5], type = "prob"))
#>         setosa versicolor  virginica
#> [1,] 0.9276884 0.05351419 0.01879743
#> [2,] 0.9246951 0.05533441 0.01997048
#> [3,] 0.9267533 0.05399649 0.01925021
#> [4,] 0.9239140 0.05588354 0.02020246
#> [5,] 0.9278839 0.05339115 0.01872497
#> [6,] 0.6432215 0.24967286 0.10710568
model
#> <ExpNBFuzzyParam> Exponential Naive Bayes with Fuzzy Parameters
#> ✔ Trained on 150 observations in 0.001s
#>   Hyperparameters: alphacut = 1e-04, metd = 2, alp = c(0.35, 0.7, 0.86), w = c(0.1, 0.3, 0.6)
#>   Predictors (4): Sepal.Length, Sepal.Width, Petal.Length, Petal.Width
#>   Classes (3): setosa, versicolor, virginica
```
