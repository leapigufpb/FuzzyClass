# Fuzzy Beta Naive Bayes (R6 class)

R6 implementation of the Fuzzy Beta Naive Bayes classifier. Create the
model with `FuzzyBetaNaiveBayesModel$new()`, train it with
`$fit(train, cl)` and classify new observations with
`$predict(newdata)`. The functional interface
[`FuzzyBetaNaiveBayes()`](https://leapigufpb.github.io/FuzzyClass/reference/FuzzyBetaNaiveBayes.md)
trains and returns an object of this class.

## Super class

[`FuzzyClassifier`](https://leapigufpb.github.io/FuzzyClass/reference/FuzzyClassifier.md)
-\> `FuzzyBetaNaiveBayes`

## Methods

### Public methods

- [`FuzzyBetaNaiveBayes$new()`](#method-FuzzyBetaNaiveBayes-initialize)

- [`FuzzyBetaNaiveBayes$clone()`](#method-FuzzyBetaNaiveBayes-clone)

Inherited methods

- [`FuzzyClassifier$fit()`](https://leapigufpb.github.io/FuzzyClass/reference/FuzzyClassifier.html#method-fit)
- [`FuzzyClassifier$is_fitted()`](https://leapigufpb.github.io/FuzzyClass/reference/FuzzyClassifier.html#method-is_fitted)
- [`FuzzyClassifier$predict()`](https://leapigufpb.github.io/FuzzyClass/reference/FuzzyClassifier.html#method-predict)
- [`FuzzyClassifier$print()`](https://leapigufpb.github.io/FuzzyClass/reference/FuzzyClassifier.html#method-print)
- [`FuzzyClassifier$set_params()`](https://leapigufpb.github.io/FuzzyClass/reference/FuzzyClassifier.html#method-set_params)

------------------------------------------------------------------------

### `FuzzyBetaNaiveBayes$new()`

Create a new (untrained) classifier.

#### Usage

    FuzzyBetaNaiveBayes$new(fuzzy = TRUE)

#### Arguments

- `fuzzy`:

  See
  [`FuzzyBetaNaiveBayes()`](https://leapigufpb.github.io/FuzzyClass/reference/FuzzyBetaNaiveBayes.md).

------------------------------------------------------------------------

### `FuzzyBetaNaiveBayes$clone()`

The objects of this class are cloneable with this method.

#### Usage

    FuzzyBetaNaiveBayes$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.

## Examples

``` r
model <- FuzzyBetaNaiveBayesModel$new()
model$fit(train = iris[, -5] / 10, cl = iris$Species)
#> ℹ Fuzzy Beta Naive Bayes: 150 observations, 4 predictors and 3 classes
#> ℹ Training the model: estimating its parameters
#> ✔ Model trained successfully [12ms]
#> 
head(model$predict(iris[, -5] / 10))
#> [1] setosa setosa setosa setosa setosa setosa
#> Levels: setosa versicolor virginica
head(model$predict(iris[, -5] / 10, type = "prob"))
#>      setosa   versicolor    virginica
#> [1,]      1 6.379672e-27 3.805856e-22
#> [2,]      1 1.006187e-24 6.258096e-20
#> [3,]      1 5.427808e-26 2.081452e-20
#> [4,]      1 3.865326e-24 1.801236e-19
#> [5,]      1 5.910458e-27 9.044085e-22
#> [6,]      1 4.222896e-18 1.204009e-15
model
#> <FuzzyBetaNaiveBayes> Fuzzy Beta Naive Bayes
#> ✔ Trained on 150 observations in 0.009s
#>   Hyperparameters: fuzzy = TRUE
#>   Predictors (4): Sepal.Length, Sepal.Width, Petal.Length, Petal.Width
#>   Classes (3): setosa, versicolor, virginica
```
