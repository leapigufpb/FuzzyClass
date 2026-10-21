# Fuzzy Trapezoidal Naive Bayes (R6 class)

R6 implementation of the Fuzzy Trapezoidal Naive Bayes classifier.
Create the model with `FuzzyTrapezoidalNaiveBayesModel$new()`, train it
with `$fit(train, cl)` and classify new observations with
`$predict(newdata)`. The functional interface
[`FuzzyTrapezoidalNaiveBayes()`](https://leapigufpb.github.io/FuzzyClass/reference/FuzzyTrapezoidalNaiveBayes.md)
trains and returns an object of this class.

## Super class

[`FuzzyClassifier`](https://leapigufpb.github.io/FuzzyClass/reference/FuzzyClassifier.md)
-\> `FuzzyTrapezoidalNaiveBayes`

## Methods

### Public methods

- [`FuzzyTrapezoidalNaiveBayes$new()`](#method-FuzzyTrapezoidalNaiveBayes-initialize)

- [`FuzzyTrapezoidalNaiveBayes$clone()`](#method-FuzzyTrapezoidalNaiveBayes-clone)

Inherited methods

- [`FuzzyClassifier$fit()`](https://leapigufpb.github.io/FuzzyClass/reference/FuzzyClassifier.html#method-fit)
- [`FuzzyClassifier$is_fitted()`](https://leapigufpb.github.io/FuzzyClass/reference/FuzzyClassifier.html#method-is_fitted)
- [`FuzzyClassifier$predict()`](https://leapigufpb.github.io/FuzzyClass/reference/FuzzyClassifier.html#method-predict)
- [`FuzzyClassifier$print()`](https://leapigufpb.github.io/FuzzyClass/reference/FuzzyClassifier.html#method-print)
- [`FuzzyClassifier$set_params()`](https://leapigufpb.github.io/FuzzyClass/reference/FuzzyClassifier.html#method-set_params)

------------------------------------------------------------------------

### `FuzzyTrapezoidalNaiveBayes$new()`

Create a new (untrained) classifier.

#### Usage

    FuzzyTrapezoidalNaiveBayes$new(fuzzy = T)

#### Arguments

- `fuzzy`:

  See
  [`FuzzyTrapezoidalNaiveBayes()`](https://leapigufpb.github.io/FuzzyClass/reference/FuzzyTrapezoidalNaiveBayes.md).

------------------------------------------------------------------------

### `FuzzyTrapezoidalNaiveBayes$clone()`

The objects of this class are cloneable with this method.

#### Usage

    FuzzyTrapezoidalNaiveBayes$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.

## Examples

``` r
model <- FuzzyTrapezoidalNaiveBayesModel$new()
model$fit(train = iris[, -5], cl = iris$Species)
#> ℹ Fuzzy Trapezoidal Naive Bayes: 150 observations, 4 predictors and 3 classes
#> ℹ Training the model: estimating its parameters
#> ✔ Model trained successfully [9ms]
#> 
head(model$predict(iris[, -5]))
#> [1] setosa setosa setosa setosa setosa setosa
#> Levels: setosa versicolor virginica
head(model$predict(iris[, -5], type = "prob"))
#>      setosa   versicolor    virginica
#> [1,]      1 5.110629e-34 4.063011e-26
#> [2,]      1 5.258433e-33 1.760757e-33
#> [3,]      1 1.220708e-32 3.996640e-33
#> [4,]      1 6.310119e-33 2.112909e-33
#> [5,]      1 2.870785e-34 2.319120e-26
#> [6,]      1 8.909196e-32 1.637972e-32
model
#> <FuzzyTrapezoidalNaiveBayes> Fuzzy Trapezoidal Naive Bayes
#> ✔ Trained on 150 observations in 0.005s
#>   Hyperparameters: fuzzy = TRUE
#>   Predictors (4): Sepal.Length, Sepal.Width, Petal.Length, Petal.Width
#>   Classes (3): setosa, versicolor, virginica
```
