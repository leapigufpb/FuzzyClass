# Gaussian Naive Bayes with Fuzzy Parameters (R6 class)

R6 implementation of the Gaussian Naive Bayes with Fuzzy Parameters
classifier. Create the model with `GauNBFuzzyParamModel$new()`, train it
with `$fit(train, cl)` and classify new observations with
`$predict(newdata)`. The functional interface
[`GauNBFuzzyParam()`](https://leapigufpb.github.io/FuzzyClass/reference/GauNBFuzzyParam.md)
trains and returns an object of this class.

## Super class

[`FuzzyClassifier`](https://leapigufpb.github.io/FuzzyClass/reference/FuzzyClassifier.md)
-\> `GauNBFuzzyParam`

## Methods

### Public methods

- [`GauNBFuzzyParam$new()`](#method-GauNBFuzzyParam-initialize)

- [`GauNBFuzzyParam$clone()`](#method-GauNBFuzzyParam-clone)

Inherited methods

- [`FuzzyClassifier$fit()`](https://leapigufpb.github.io/FuzzyClass/reference/FuzzyClassifier.html#method-fit)
- [`FuzzyClassifier$is_fitted()`](https://leapigufpb.github.io/FuzzyClass/reference/FuzzyClassifier.html#method-is_fitted)
- [`FuzzyClassifier$predict()`](https://leapigufpb.github.io/FuzzyClass/reference/FuzzyClassifier.html#method-predict)
- [`FuzzyClassifier$print()`](https://leapigufpb.github.io/FuzzyClass/reference/FuzzyClassifier.html#method-print)
- [`FuzzyClassifier$set_params()`](https://leapigufpb.github.io/FuzzyClass/reference/FuzzyClassifier.html#method-set_params)

------------------------------------------------------------------------

### `GauNBFuzzyParam$new()`

Create a new (untrained) classifier.

#### Usage

    GauNBFuzzyParam$new(
      alphacut = 1e-04,
      metd = 2,
      alp = c(0.35, 0.7, 0.86),
      w = c(0.1, 0.3, 0.6)
    )

#### Arguments

- `alphacut`:

  See
  [`GauNBFuzzyParam()`](https://leapigufpb.github.io/FuzzyClass/reference/GauNBFuzzyParam.md).

- `metd`:

  See
  [`GauNBFuzzyParam()`](https://leapigufpb.github.io/FuzzyClass/reference/GauNBFuzzyParam.md).

- `alp`:

  See
  [`GauNBFuzzyParam()`](https://leapigufpb.github.io/FuzzyClass/reference/GauNBFuzzyParam.md).

- `w`:

  See
  [`GauNBFuzzyParam()`](https://leapigufpb.github.io/FuzzyClass/reference/GauNBFuzzyParam.md).

------------------------------------------------------------------------

### `GauNBFuzzyParam$clone()`

The objects of this class are cloneable with this method.

#### Usage

    GauNBFuzzyParam$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.

## Examples

``` r
model <- GauNBFuzzyParamModel$new()
model$fit(train = iris[, -5], cl = iris$Species)
#> ℹ Gaussian Naive Bayes with Fuzzy Parameters: 150 observations, 4 predictors and 3 classes
#> ℹ Training the model: estimating its parameters
#> ✔ Model trained successfully [6ms]
#> 
head(model$predict(iris[, -5]))
#> [1] setosa setosa setosa setosa setosa setosa
#> Levels: setosa versicolor virginica
head(model$predict(iris[, -5], type = "prob"))
#>         setosa   versicolor    virginica
#> [1,] 0.9999635 1.809295e-05 1.838935e-05
#> [2,] 0.9991539 3.173806e-04 5.286910e-04
#> [3,] 0.9997328 5.620449e-05 2.110275e-04
#> [4,] 0.9994605 1.264808e-04 4.130517e-04
#> [5,] 0.9999905 4.140095e-06 5.332975e-06
#> [6,] 0.9993921 5.885719e-04 1.929828e-05
model
#> <GauNBFuzzyParam> Gaussian Naive Bayes with Fuzzy Parameters
#> ✔ Trained on 150 observations in 0.002s
#>   Hyperparameters: alphacut = 1e-04, metd = 2, alp = c(0.35, 0.7, 0.86), w = c(0.1, 0.3, 0.6)
#>   Predictors (4): Sepal.Length, Sepal.Width, Petal.Length, Petal.Width
#>   Classes (3): setosa, versicolor, virginica
```
