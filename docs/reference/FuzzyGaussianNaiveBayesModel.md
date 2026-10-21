# Fuzzy Gaussian Naive Bayes (Zadeh-based) (R6 class)

R6 implementation of the Fuzzy Gaussian Naive Bayes (Zadeh-based)
classifier. Create the model with `FuzzyGaussianNaiveBayesModel$new()`,
train it with `$fit(train, cl)` and classify new observations with
`$predict(newdata)`. The functional interface
[`FuzzyGaussianNaiveBayes()`](https://leapigufpb.github.io/FuzzyClass/reference/FuzzyGaussianNaiveBayes.md)
trains and returns an object of this class.

## Super class

[`FuzzyClassifier`](https://leapigufpb.github.io/FuzzyClass/reference/FuzzyClassifier.md)
-\> `FuzzyGaussianNaiveBayes`

## Methods

### Public methods

- [`FuzzyGaussianNaiveBayes$new()`](#method-FuzzyGaussianNaiveBayes-initialize)

- [`FuzzyGaussianNaiveBayes$clone()`](#method-FuzzyGaussianNaiveBayes-clone)

Inherited methods

- [`FuzzyClassifier$fit()`](https://leapigufpb.github.io/FuzzyClass/reference/FuzzyClassifier.html#method-fit)
- [`FuzzyClassifier$is_fitted()`](https://leapigufpb.github.io/FuzzyClass/reference/FuzzyClassifier.html#method-is_fitted)
- [`FuzzyClassifier$predict()`](https://leapigufpb.github.io/FuzzyClass/reference/FuzzyClassifier.html#method-predict)
- [`FuzzyClassifier$print()`](https://leapigufpb.github.io/FuzzyClass/reference/FuzzyClassifier.html#method-print)
- [`FuzzyClassifier$set_params()`](https://leapigufpb.github.io/FuzzyClass/reference/FuzzyClassifier.html#method-set_params)

------------------------------------------------------------------------

### `FuzzyGaussianNaiveBayes$new()`

Create a new (untrained) classifier.

#### Usage

    FuzzyGaussianNaiveBayes$new(fuzzy = TRUE)

#### Arguments

- `fuzzy`:

  See
  [`FuzzyGaussianNaiveBayes()`](https://leapigufpb.github.io/FuzzyClass/reference/FuzzyGaussianNaiveBayes.md).

------------------------------------------------------------------------

### `FuzzyGaussianNaiveBayes$clone()`

The objects of this class are cloneable with this method.

#### Usage

    FuzzyGaussianNaiveBayes$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.

## Examples

``` r
model <- FuzzyGaussianNaiveBayesModel$new()
model$fit(train = iris[, -5], cl = iris$Species)
#> ℹ Fuzzy Gaussian Naive Bayes (Zadeh-based): 150 observations, 4 predictors and 3 classes
#> ℹ Training the model: estimating its parameters
#> ✔ Model trained successfully [6ms]
#> 
head(model$predict(iris[, -5]))
#> [1] setosa setosa setosa setosa setosa setosa
#> Levels: setosa versicolor virginica
head(model$predict(iris[, -5], type = "prob"))
#>      setosa   versicolor    virginica
#> [1,]      1 3.089056e-21 2.007145e-29
#> [2,]      1 1.868147e-19 1.505721e-27
#> [3,]      1 2.429303e-20 2.675230e-28
#> [4,]      1 2.714064e-19 2.829119e-27
#> [5,]      1 1.054105e-21 1.242936e-29
#> [6,]      1 3.466495e-15 2.992237e-22
model
#> <FuzzyGaussianNaiveBayes> Fuzzy Gaussian Naive Bayes (Zadeh-based)
#> ✔ Trained on 150 observations in 0.002s
#>   Hyperparameters: fuzzy = TRUE
#>   Predictors (4): Sepal.Length, Sepal.Width, Petal.Length, Petal.Width
#>   Classes (3): setosa, versicolor, virginica
```
