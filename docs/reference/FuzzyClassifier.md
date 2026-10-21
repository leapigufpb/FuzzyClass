# Base R6 class of the FuzzyClass classifiers

`FuzzyClassifier` is the (abstract) R6 class shared by every classifier
of the package. It implements the common workflow:

- `$new(...)` creates an untrained model holding its hyperparameters;

- `$fit(train, cl)` estimates the model and reports the progress;

- `$predict(newdata, type)` classifies new observations;

- `$set_params(...)` changes hyperparameters (the model must be
  re-trained).

Do not instantiate it directly: use one of the concrete classes, e.g.
[FuzzyGaussianNaiveBayesModel](https://leapigufpb.github.io/FuzzyClass/reference/FuzzyGaussianNaiveBayesModel.md),
[FuzzyGammaNaiveBayesModel](https://leapigufpb.github.io/FuzzyClass/reference/FuzzyGammaNaiveBayesModel.md)
or
[GauNBFuzzyParamModel](https://leapigufpb.github.io/FuzzyClass/reference/GauNBFuzzyParamModel.md).
Each of them also has a functional interface (e.g.
[`FuzzyGaussianNaiveBayes()`](https://leapigufpb.github.io/FuzzyClass/reference/FuzzyGaussianNaiveBayes.md))
that trains and returns the R6 object, and every classifier is available
to tidymodels through
[`fuzzy_naive_bayes()`](https://leapigufpb.github.io/FuzzyClass/reference/fuzzy_naive_bayes.md).

Since `$fit()` and `$set_params()` return the object invisibly, calls
can be chained:
`FuzzyGammaNaiveBayesModel$new()$fit(x, y)$predict(x_new)`.

## Public fields

- `params`:

  Named list with the hyperparameters of the classifier.

- `estimates`:

  List with the quantities estimated by `$fit()` (`NULL` while the model
  has not been trained).

- `predictors`:

  Names of the predictors seen during training.

- `classes`:

  Classes (levels of the response) seen during training.

- `n_obs`:

  Number of training observations.

- `fit_time`:

  Training time, in seconds.

## Active bindings

- `label`:

  Name of the classifier (read-only); it depends on whether the
  membership functions are used (`fuzzy`).

## Methods

### Public methods

- [`FuzzyClassifier$new()`](#method-FuzzyClassifier-initialize)

- [`FuzzyClassifier$fit()`](#method-FuzzyClassifier-fit)

- [`FuzzyClassifier$predict()`](#method-FuzzyClassifier-predict)

- [`FuzzyClassifier$set_params()`](#method-FuzzyClassifier-set_params)

- [`FuzzyClassifier$is_fitted()`](#method-FuzzyClassifier-is_fitted)

- [`FuzzyClassifier$print()`](#method-FuzzyClassifier-print)

- [`FuzzyClassifier$clone()`](#method-FuzzyClassifier-clone)

------------------------------------------------------------------------

### `FuzzyClassifier$new()`

Create a new (untrained) classifier.

#### Usage

    FuzzyClassifier$new(...)

#### Arguments

- `...`:

  Named hyperparameters of the classifier.

------------------------------------------------------------------------

### `FuzzyClassifier$fit()`

Estimate the classifier from training data.

#### Usage

    FuzzyClassifier$fit(train, cl, verbose = fuzzy_verbose())

#### Arguments

- `train`:

  Matrix or data frame of training set cases.

- `cl`:

  Factor (or vector) of true classifications of the training set.

- `verbose`:

  Logical; if `TRUE` (default, see
  [`fuzzy_verbose()`](https://leapigufpb.github.io/FuzzyClass/reference/fuzzy_verbose.md))
  a message reports the training progress.

#### Returns

The trained object, invisibly.

------------------------------------------------------------------------

### `FuzzyClassifier$predict()`

Classify new observations.

#### Usage

    FuzzyClassifier$predict(newdata, type = c("class", "prob", "matrix", "raw"))

#### Arguments

- `newdata`:

  Matrix or data frame of test set cases, with the predictors in the
  same order used in training.

- `type`:

  `"class"` (default) for a factor of predicted classes, or `"prob"` for
  a matrix of class probabilities (one column per class). `"matrix"` and
  `"raw"` are accepted as synonyms of `"prob"`.

#### Returns

A factor (`type = "class"`) or a numeric matrix.

------------------------------------------------------------------------

### `FuzzyClassifier$set_params()`

Update hyperparameters. The model is reset and must be trained again
with `$fit()`.

#### Usage

    FuzzyClassifier$set_params(...)

#### Arguments

- `...`:

  Named hyperparameters, as in `$new()`.

#### Returns

The object, invisibly.

------------------------------------------------------------------------

### `FuzzyClassifier$is_fitted()`

Has the model been trained?

#### Usage

    FuzzyClassifier$is_fitted()

#### Returns

`TRUE` or `FALSE`.

------------------------------------------------------------------------

### `FuzzyClassifier$print()`

Print a summary of the classifier.

#### Usage

    FuzzyClassifier$print(...)

#### Arguments

- `...`:

  Ignored.

------------------------------------------------------------------------

### `FuzzyClassifier$clone()`

The objects of this class are cloneable with this method.

#### Usage

    FuzzyClassifier$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.

## Examples

``` r
model <- FuzzyGaussianNaiveBayesModel$new(fuzzy = TRUE)
model$fit(train = iris[, -5], cl = iris$Species)
#> ℹ Fuzzy Gaussian Naive Bayes (Zadeh-based): 150 observations, 4 predictors and 3 classes
#> ℹ Training the model: estimating its parameters
#> ✔ Model trained successfully [7ms]
#> 
model
#> <FuzzyGaussianNaiveBayes> Fuzzy Gaussian Naive Bayes (Zadeh-based)
#> ✔ Trained on 150 observations in 0.003s
#>   Hyperparameters: fuzzy = TRUE
#>   Predictors (4): Sepal.Length, Sepal.Width, Petal.Length, Petal.Width
#>   Classes (3): setosa, versicolor, virginica

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

# S3 generics also work
head(predict(model, iris[, -5]))
#> [1] setosa setosa setosa setosa setosa setosa
#> Levels: setosa versicolor virginica

# Change a hyperparameter and re-train, silently
model$set_params(fuzzy = FALSE)$fit(iris[, -5], iris$Species, verbose = FALSE)
model$label
#> [1] "Gaussian Naive Bayes"
```
