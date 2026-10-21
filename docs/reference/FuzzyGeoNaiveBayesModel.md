# Fuzzy Geometric Naive Bayes (R6 class)

R6 implementation of the Fuzzy Geometric Naive Bayes classifier. Create
the model with `FuzzyGeoNaiveBayesModel$new()`, train it with
`$fit(train, cl)` and classify new observations with
`$predict(newdata)`. The functional interface
[`FuzzyGeoNaiveBayes()`](https://leapigufpb.github.io/FuzzyClass/reference/FuzzyGeoNaiveBayes.md)
trains and returns an object of this class.

## Super class

[`FuzzyClassifier`](https://leapigufpb.github.io/FuzzyClass/reference/FuzzyClassifier.md)
-\> `FuzzyGeoNaiveBayes`

## Methods

### Public methods

- [`FuzzyGeoNaiveBayes$new()`](#method-FuzzyGeoNaiveBayes-initialize)

- [`FuzzyGeoNaiveBayes$clone()`](#method-FuzzyGeoNaiveBayes-clone)

Inherited methods

- [`FuzzyClassifier$fit()`](https://leapigufpb.github.io/FuzzyClass/reference/FuzzyClassifier.html#method-fit)
- [`FuzzyClassifier$is_fitted()`](https://leapigufpb.github.io/FuzzyClass/reference/FuzzyClassifier.html#method-is_fitted)
- [`FuzzyClassifier$predict()`](https://leapigufpb.github.io/FuzzyClass/reference/FuzzyClassifier.html#method-predict)
- [`FuzzyClassifier$print()`](https://leapigufpb.github.io/FuzzyClass/reference/FuzzyClassifier.html#method-print)
- [`FuzzyClassifier$set_params()`](https://leapigufpb.github.io/FuzzyClass/reference/FuzzyClassifier.html#method-set_params)

------------------------------------------------------------------------

### `FuzzyGeoNaiveBayes$new()`

Create a new (untrained) classifier.

#### Usage

    FuzzyGeoNaiveBayes$new(fuzzy = T)

#### Arguments

- `fuzzy`:

  See
  [`FuzzyGeoNaiveBayes()`](https://leapigufpb.github.io/FuzzyClass/reference/FuzzyGeoNaiveBayes.md).

------------------------------------------------------------------------

### `FuzzyGeoNaiveBayes$clone()`

The objects of this class are cloneable with this method.

#### Usage

    FuzzyGeoNaiveBayes$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.

## Examples

``` r
model <- FuzzyGeoNaiveBayesModel$new()
x <- round(iris[, -5])
model$fit(train = x, cl = iris$Species)
#> ℹ Fuzzy Geometric Naive Bayes: 150 observations, 4 predictors and 3 classes
#> ℹ Training the model: estimating its parameters
#> ✔ Model trained successfully [5ms]
#> 
head(model$predict(x))
#> [1] setosa setosa setosa setosa setosa setosa
#> Levels: setosa versicolor virginica
head(model$predict(x, type = "prob"))
#>         setosa   versicolor    virginica
#> [1,] 1.0000000 5.954024e-11 6.095216e-10
#> [2,] 0.9999999 5.123826e-08 7.197528e-09
#> [3,] 0.9999999 5.123826e-08 7.197528e-09
#> [4,] 0.9999999 6.339152e-08 9.350967e-09
#> [5,] 1.0000000 5.954024e-11 6.095216e-10
#> [6,] 1.0000000 7.366266e-11 7.918853e-10
model
#> <FuzzyGeoNaiveBayes> Fuzzy Geometric Naive Bayes
#> ✔ Trained on 150 observations in 0.001s
#>   Hyperparameters: fuzzy = TRUE
#>   Predictors (4): Sepal.Length, Sepal.Width, Petal.Length, Petal.Width
#>   Classes (3): setosa, versicolor, virginica
```
