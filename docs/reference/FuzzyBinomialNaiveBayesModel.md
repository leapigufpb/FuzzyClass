# Fuzzy Binomial Naive Bayes (R6 class)

R6 implementation of the Fuzzy Binomial Naive Bayes classifier. Create
the model with `FuzzyBinomialNaiveBayesModel$new()`, train it with
`$fit(train, cl)` and classify new observations with
`$predict(newdata)`. The functional interface
[`FuzzyBinomialNaiveBayes()`](https://leapigufpb.github.io/FuzzyClass/reference/FuzzyBinomialNaiveBayes.md)
trains and returns an object of this class.

## Super class

[`FuzzyClassifier`](https://leapigufpb.github.io/FuzzyClass/reference/FuzzyClassifier.md)
-\> `FuzzyBinomialNaiveBayes`

## Methods

### Public methods

- [`FuzzyBinomialNaiveBayes$new()`](#method-FuzzyBinomialNaiveBayes-initialize)

- [`FuzzyBinomialNaiveBayes$clone()`](#method-FuzzyBinomialNaiveBayes-clone)

Inherited methods

- [`FuzzyClassifier$fit()`](https://leapigufpb.github.io/FuzzyClass/reference/FuzzyClassifier.html#method-fit)
- [`FuzzyClassifier$is_fitted()`](https://leapigufpb.github.io/FuzzyClass/reference/FuzzyClassifier.html#method-is_fitted)
- [`FuzzyClassifier$predict()`](https://leapigufpb.github.io/FuzzyClass/reference/FuzzyClassifier.html#method-predict)
- [`FuzzyClassifier$print()`](https://leapigufpb.github.io/FuzzyClass/reference/FuzzyClassifier.html#method-print)
- [`FuzzyClassifier$set_params()`](https://leapigufpb.github.io/FuzzyClass/reference/FuzzyClassifier.html#method-set_params)

------------------------------------------------------------------------

### `FuzzyBinomialNaiveBayes$new()`

Create a new (untrained) classifier.

#### Usage

    FuzzyBinomialNaiveBayes$new(fuzzy = TRUE)

#### Arguments

- `fuzzy`:

  See
  [`FuzzyBinomialNaiveBayes()`](https://leapigufpb.github.io/FuzzyClass/reference/FuzzyBinomialNaiveBayes.md).

------------------------------------------------------------------------

### `FuzzyBinomialNaiveBayes$clone()`

The objects of this class are cloneable with this method.

#### Usage

    FuzzyBinomialNaiveBayes$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.

## Examples

``` r
model <- FuzzyBinomialNaiveBayesModel$new()
x <- round(iris[, -5])
model$fit(train = x, cl = iris$Species)
#> ℹ Fuzzy Binomial Naive Bayes: 150 observations, 4 predictors and 3 classes
#> ℹ Training the model: estimating its parameters
#> ✔ Model trained successfully [5ms]
#> 
head(model$predict(x))
#> [1] setosa setosa setosa setosa setosa setosa
#> Levels: setosa versicolor virginica
head(model$predict(x, type = "prob"))
#>         setosa   versicolor    virginica
#> [1,] 1.0000000 0.000000e+00 4.642555e-14
#> [2,] 0.9999845 1.552739e-05 1.097178e-12
#> [3,] 0.9999845 1.552739e-05 1.097178e-12
#> [4,] 0.9999021 9.794360e-05 7.814212e-12
#> [5,] 1.0000000 0.000000e+00 4.642555e-14
#> [6,] 1.0000000 0.000000e+00 3.306748e-13
model
#> <FuzzyBinomialNaiveBayes> Fuzzy Binomial Naive Bayes
#> ✔ Trained on 150 observations in 0.002s
#>   Hyperparameters: fuzzy = TRUE
#>   Predictors (4): Sepal.Length, Sepal.Width, Petal.Length, Petal.Width
#>   Classes (3): setosa, versicolor, virginica
```
