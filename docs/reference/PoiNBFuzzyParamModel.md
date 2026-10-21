# Poisson Naive Bayes with Fuzzy Parameters (R6 class)

R6 implementation of the Poisson Naive Bayes with Fuzzy Parameters
classifier. Create the model with `PoiNBFuzzyParamModel$new()`, train it
with `$fit(train, cl)` and classify new observations with
`$predict(newdata)`. The functional interface
[`PoiNBFuzzyParam()`](https://leapigufpb.github.io/FuzzyClass/reference/PoiNBFuzzyParam.md)
trains and returns an object of this class.

## Super class

[`FuzzyClassifier`](https://leapigufpb.github.io/FuzzyClass/reference/FuzzyClassifier.md)
-\> `PoiNBFuzzyParam`

## Methods

### Public methods

- [`PoiNBFuzzyParam$new()`](#method-PoiNBFuzzyParam-initialize)

- [`PoiNBFuzzyParam$clone()`](#method-PoiNBFuzzyParam-clone)

Inherited methods

- [`FuzzyClassifier$fit()`](https://leapigufpb.github.io/FuzzyClass/reference/FuzzyClassifier.html#method-fit)
- [`FuzzyClassifier$is_fitted()`](https://leapigufpb.github.io/FuzzyClass/reference/FuzzyClassifier.html#method-is_fitted)
- [`FuzzyClassifier$predict()`](https://leapigufpb.github.io/FuzzyClass/reference/FuzzyClassifier.html#method-predict)
- [`FuzzyClassifier$print()`](https://leapigufpb.github.io/FuzzyClass/reference/FuzzyClassifier.html#method-print)
- [`FuzzyClassifier$set_params()`](https://leapigufpb.github.io/FuzzyClass/reference/FuzzyClassifier.html#method-set_params)

------------------------------------------------------------------------

### `PoiNBFuzzyParam$new()`

Create a new (untrained) classifier.

#### Usage

    PoiNBFuzzyParam$new(
      alphacut = 1e-04,
      metd = 2,
      alp = c(0.35, 0.7, 0.86),
      w = c(0.1, 0.3, 0.6)
    )

#### Arguments

- `alphacut`:

  See
  [`PoiNBFuzzyParam()`](https://leapigufpb.github.io/FuzzyClass/reference/PoiNBFuzzyParam.md).

- `metd`:

  See
  [`PoiNBFuzzyParam()`](https://leapigufpb.github.io/FuzzyClass/reference/PoiNBFuzzyParam.md).

- `alp`:

  See
  [`PoiNBFuzzyParam()`](https://leapigufpb.github.io/FuzzyClass/reference/PoiNBFuzzyParam.md).

- `w`:

  See
  [`PoiNBFuzzyParam()`](https://leapigufpb.github.io/FuzzyClass/reference/PoiNBFuzzyParam.md).

------------------------------------------------------------------------

### `PoiNBFuzzyParam$clone()`

The objects of this class are cloneable with this method.

#### Usage

    PoiNBFuzzyParam$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.

## Examples

``` r
model <- PoiNBFuzzyParamModel$new()
x <- round(iris[, -5])
model$fit(train = x, cl = iris$Species)
#> ℹ Poisson Naive Bayes with Fuzzy Parameters: 150 observations, 4 predictors and 3 classes
#> ℹ Training the model: estimating its parameters
#> ✔ Model trained successfully [5ms]
#> 
head(model$predict(x))
#> [1] setosa setosa setosa setosa setosa setosa
#> Levels: setosa versicolor virginica
head(model$predict(x, type = "prob"))
#>         setosa versicolor   virginica
#> [1,] 0.9714948 0.02235177 0.006153474
#> [2,] 0.9606296 0.03048538 0.008885010
#> [3,] 0.9606296 0.03048538 0.008885010
#> [4,] 0.9343234 0.05187582 0.013800742
#> [5,] 0.9714948 0.02235177 0.006153474
#> [6,] 0.9502309 0.03980497 0.009964100
model
#> <PoiNBFuzzyParam> Poisson Naive Bayes with Fuzzy Parameters
#> ✔ Trained on 150 observations in 0.002s
#>   Hyperparameters: alphacut = 1e-04, metd = 2, alp = c(0.35, 0.7, 0.86), w = c(0.1, 0.3, 0.6)
#>   Predictors (4): Sepal.Length, Sepal.Width, Petal.Length, Petal.Width
#>   Classes (3): setosa, versicolor, virginica
```
