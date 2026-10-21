# Fuzzy Poisson Naive Bayes (R6 class)

R6 implementation of the Fuzzy Poisson Naive Bayes classifier. Create
the model with `FuzzyPoissonNaiveBayesModel$new()`, train it with
`$fit(train, cl)` and classify new observations with
`$predict(newdata)`. The functional interface
[`FuzzyPoissonNaiveBayes()`](https://leapigufpb.github.io/FuzzyClass/reference/FuzzyPoissonNaiveBayes.md)
trains and returns an object of this class.

## Super class

[`FuzzyClassifier`](https://leapigufpb.github.io/FuzzyClass/reference/FuzzyClassifier.md)
-\> `FuzzyPoissonNaiveBayes`

## Methods

### Public methods

- [`FuzzyPoissonNaiveBayes$new()`](#method-FuzzyPoissonNaiveBayes-initialize)

- [`FuzzyPoissonNaiveBayes$clone()`](#method-FuzzyPoissonNaiveBayes-clone)

Inherited methods

- [`FuzzyClassifier$fit()`](https://leapigufpb.github.io/FuzzyClass/reference/FuzzyClassifier.html#method-fit)
- [`FuzzyClassifier$is_fitted()`](https://leapigufpb.github.io/FuzzyClass/reference/FuzzyClassifier.html#method-is_fitted)
- [`FuzzyClassifier$predict()`](https://leapigufpb.github.io/FuzzyClass/reference/FuzzyClassifier.html#method-predict)
- [`FuzzyClassifier$print()`](https://leapigufpb.github.io/FuzzyClass/reference/FuzzyClassifier.html#method-print)
- [`FuzzyClassifier$set_params()`](https://leapigufpb.github.io/FuzzyClass/reference/FuzzyClassifier.html#method-set_params)

------------------------------------------------------------------------

### `FuzzyPoissonNaiveBayes$new()`

Create a new (untrained) classifier.

#### Usage

    FuzzyPoissonNaiveBayes$new(fuzzy = TRUE)

#### Arguments

- `fuzzy`:

  See
  [`FuzzyPoissonNaiveBayes()`](https://leapigufpb.github.io/FuzzyClass/reference/FuzzyPoissonNaiveBayes.md).

------------------------------------------------------------------------

### `FuzzyPoissonNaiveBayes$clone()`

The objects of this class are cloneable with this method.

#### Usage

    FuzzyPoissonNaiveBayes$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.

## Examples

``` r
model <- FuzzyPoissonNaiveBayesModel$new()
x <- round(iris[, -5])
model$fit(train = x, cl = iris$Species)
#> ℹ Fuzzy Poisson Naive Bayes: 150 observations, 4 predictors and 3 classes
#> ℹ Training the model: estimating its parameters
#> ✔ Model trained successfully [5ms]
#> 
head(model$predict(x))
#> [1] setosa setosa setosa setosa setosa setosa
#> Levels: setosa versicolor virginica
head(model$predict(x, type = "prob"))
#>         setosa   versicolor    virginica
#> [1,] 1.0000000 1.167572e-08 1.825419e-08
#> [2,] 0.9991218 8.779474e-04 2.403822e-07
#> [3,] 0.9991218 8.779474e-04 2.403822e-07
#> [4,] 0.9977523 2.246878e-03 8.192892e-07
#> [5,] 1.0000000 1.167572e-08 1.825419e-08
#> [6,] 0.9999999 2.992200e-08 6.230073e-08
model
#> <FuzzyPoissonNaiveBayes> Fuzzy Poisson Naive Bayes
#> ✔ Trained on 150 observations in 0.001s
#>   Hyperparameters: fuzzy = TRUE
#>   Predictors (4): Sepal.Length, Sepal.Width, Petal.Length, Petal.Width
#>   Classes (3): setosa, versicolor, virginica
```
