# Fuzzy Gamma Naive Bayes (R6 class)

R6 implementation of the Fuzzy Gamma Naive Bayes classifier. Create the
model with `FuzzyGammaNaiveBayesModel$new()`, train it with
`$fit(train, cl)` and classify new observations with
`$predict(newdata)`. The functional interface
[`FuzzyGammaNaiveBayes()`](https://leapigufpb.github.io/FuzzyClass/reference/FuzzyGammaNaiveBayes.md)
trains and returns an object of this class.

## Super class

[`FuzzyClassifier`](https://leapigufpb.github.io/FuzzyClass/reference/FuzzyClassifier.md)
-\> `FuzzyGammaNaiveBayes`

## Methods

### Public methods

- [`FuzzyGammaNaiveBayes$new()`](#method-FuzzyGammaNaiveBayes-initialize)

- [`FuzzyGammaNaiveBayes$clone()`](#method-FuzzyGammaNaiveBayes-clone)

Inherited methods

- [`FuzzyClassifier$fit()`](https://leapigufpb.github.io/FuzzyClass/reference/FuzzyClassifier.html#method-fit)
- [`FuzzyClassifier$is_fitted()`](https://leapigufpb.github.io/FuzzyClass/reference/FuzzyClassifier.html#method-is_fitted)
- [`FuzzyClassifier$predict()`](https://leapigufpb.github.io/FuzzyClass/reference/FuzzyClassifier.html#method-predict)
- [`FuzzyClassifier$print()`](https://leapigufpb.github.io/FuzzyClass/reference/FuzzyClassifier.html#method-print)
- [`FuzzyClassifier$set_params()`](https://leapigufpb.github.io/FuzzyClass/reference/FuzzyClassifier.html#method-set_params)

------------------------------------------------------------------------

### `FuzzyGammaNaiveBayes$new()`

Create a new (untrained) classifier.

#### Usage

    FuzzyGammaNaiveBayes$new(fuzzy = TRUE)

#### Arguments

- `fuzzy`:

  See
  [`FuzzyGammaNaiveBayes()`](https://leapigufpb.github.io/FuzzyClass/reference/FuzzyGammaNaiveBayes.md).

------------------------------------------------------------------------

### `FuzzyGammaNaiveBayes$clone()`

The objects of this class are cloneable with this method.

#### Usage

    FuzzyGammaNaiveBayes$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.

## Examples

``` r
model <- FuzzyGammaNaiveBayesModel$new()
model$fit(train = iris[, -5], cl = iris$Species)
#> ℹ Fuzzy Gamma Naive Bayes: 150 observations, 4 predictors and 3 classes
#> ℹ Training the model: estimating its parameters
#> ✔ Model trained successfully [6ms]
#> 
head(model$predict(iris[, -5]))
#> [1] setosa setosa setosa setosa setosa setosa
#> Levels: setosa versicolor virginica
head(model$predict(iris[, -5], type = "prob"))
#>      setosa   versicolor    virginica
#> [1,]      1 2.937007e-40 3.451249e-68
#> [2,]      1 1.063598e-38 1.899100e-66
#> [3,]      1 8.288584e-41 2.528645e-69
#> [4,]      1 1.147633e-37 1.187635e-64
#> [5,]      1 1.127658e-40 2.033227e-68
#> [6,]      1 5.370364e-23 3.248167e-44
model
#> <FuzzyGammaNaiveBayes> Fuzzy Gamma Naive Bayes
#> ✔ Trained on 150 observations in 0.003s
#>   Hyperparameters: fuzzy = TRUE
#>   Predictors (4): Sepal.Length, Sepal.Width, Petal.Length, Petal.Width
#>   Classes (3): setosa, versicolor, virginica
```
