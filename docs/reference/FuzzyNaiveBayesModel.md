# Fuzzy Naive Bayes (R6 class)

R6 implementation of the Fuzzy Naive Bayes classifier. Create the model
with `FuzzyNaiveBayesModel$new()`, train it with `$fit(train, cl)` and
classify new observations with `$predict(newdata)`. The functional
interface
[`FuzzyNaiveBayes()`](https://leapigufpb.github.io/FuzzyClass/reference/FuzzyNaiveBayes.md)
trains and returns an object of this class.

## Super class

[`FuzzyClassifier`](https://leapigufpb.github.io/FuzzyClass/reference/FuzzyClassifier.md)
-\> `FuzzyNaiveBayes`

## Methods

### Public methods

- [`FuzzyNaiveBayes$new()`](#method-FuzzyNaiveBayes-initialize)

- [`FuzzyNaiveBayes$clone()`](#method-FuzzyNaiveBayes-clone)

Inherited methods

- [`FuzzyClassifier$fit()`](https://leapigufpb.github.io/FuzzyClass/reference/FuzzyClassifier.html#method-fit)
- [`FuzzyClassifier$is_fitted()`](https://leapigufpb.github.io/FuzzyClass/reference/FuzzyClassifier.html#method-is_fitted)
- [`FuzzyClassifier$predict()`](https://leapigufpb.github.io/FuzzyClass/reference/FuzzyClassifier.html#method-predict)
- [`FuzzyClassifier$print()`](https://leapigufpb.github.io/FuzzyClass/reference/FuzzyClassifier.html#method-print)
- [`FuzzyClassifier$set_params()`](https://leapigufpb.github.io/FuzzyClass/reference/FuzzyClassifier.html#method-set_params)

------------------------------------------------------------------------

### `FuzzyNaiveBayes$new()`

Create a new (untrained) classifier.

#### Usage

    FuzzyNaiveBayes$new(fuzzy = TRUE, m = NULL, Pi = NULL)

#### Arguments

- `fuzzy`:

  See
  [`FuzzyNaiveBayes()`](https://leapigufpb.github.io/FuzzyClass/reference/FuzzyNaiveBayes.md).

- `m`:

  See
  [`FuzzyNaiveBayes()`](https://leapigufpb.github.io/FuzzyClass/reference/FuzzyNaiveBayes.md).

- `Pi`:

  See
  [`FuzzyNaiveBayes()`](https://leapigufpb.github.io/FuzzyClass/reference/FuzzyNaiveBayes.md).

------------------------------------------------------------------------

### `FuzzyNaiveBayes$clone()`

The objects of this class are cloneable with this method.

#### Usage

    FuzzyNaiveBayes$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.

## Examples

``` r
model <- FuzzyNaiveBayesModel$new()
model$fit(train = iris[, -5], cl = iris$Species)
#> ℹ Fuzzy Naive Bayes: 150 observations, 4 predictors and 3 classes
#> ℹ Training the model: estimating its parameters
#> ✔ Model trained successfully [6ms]
#> 
head(model$predict(iris[, -5]))
#> [1] setosa setosa setosa setosa setosa setosa
#> Levels: setosa versicolor virginica
head(model$predict(iris[, -5], type = "prob"))
#>         setosa   versicolor    virginica
#> [1,] 0.9999970 2.921316e-06 3.824268e-08
#> [2,] 0.9999064 8.004112e-05 1.357392e-05
#> [3,] 0.9997257 2.370988e-04 3.724606e-05
#> [4,] 0.9997994 1.714983e-04 2.908385e-05
#> [5,] 0.9999970 2.921315e-06 7.648535e-08
#> [6,] 0.9616918 3.035953e-02 7.948677e-03
model
#> <FuzzyNaiveBayes> Fuzzy Naive Bayes
#> ✔ Trained on 150 observations in 0.002s
#>   Hyperparameters: fuzzy = TRUE, m = NULL, Pi = NULL
#>   Predictors (4): Sepal.Length, Sepal.Width, Petal.Length, Petal.Width
#>   Classes (3): setosa, versicolor, virginica
```
