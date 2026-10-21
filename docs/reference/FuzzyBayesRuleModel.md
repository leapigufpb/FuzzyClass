# Fuzzy Bayes Rule (R6 class)

R6 implementation of the Fuzzy Bayes Rule classifier. Create the model
with `FuzzyBayesRuleModel$new()`, train it with `$fit(train, cl)` and
classify new observations with `$predict(newdata)`. The functional
interface
[`FuzzyBayesRule()`](https://leapigufpb.github.io/FuzzyClass/reference/FuzzyBayesRule.md)
trains and returns an object of this class.

## Super class

[`FuzzyClassifier`](https://leapigufpb.github.io/FuzzyClass/reference/FuzzyClassifier.md)
-\> `FuzzyBayesRule`

## Methods

### Public methods

- [`FuzzyBayesRule$new()`](#method-FuzzyBayesRule-initialize)

- [`FuzzyBayesRule$clone()`](#method-FuzzyBayesRule-clone)

Inherited methods

- [`FuzzyClassifier$fit()`](https://leapigufpb.github.io/FuzzyClass/reference/FuzzyClassifier.html#method-fit)
- [`FuzzyClassifier$is_fitted()`](https://leapigufpb.github.io/FuzzyClass/reference/FuzzyClassifier.html#method-is_fitted)
- [`FuzzyClassifier$predict()`](https://leapigufpb.github.io/FuzzyClass/reference/FuzzyClassifier.html#method-predict)
- [`FuzzyClassifier$print()`](https://leapigufpb.github.io/FuzzyClass/reference/FuzzyClassifier.html#method-print)
- [`FuzzyClassifier$set_params()`](https://leapigufpb.github.io/FuzzyClass/reference/FuzzyClassifier.html#method-set_params)

------------------------------------------------------------------------

### `FuzzyBayesRule$new()`

Create a new (untrained) classifier.

#### Usage

    FuzzyBayesRule$new(fuzzy = TRUE)

#### Arguments

- `fuzzy`:

  See
  [`FuzzyBayesRule()`](https://leapigufpb.github.io/FuzzyClass/reference/FuzzyBayesRule.md).

------------------------------------------------------------------------

### `FuzzyBayesRule$clone()`

The objects of this class are cloneable with this method.

#### Usage

    FuzzyBayesRule$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.

## Examples

``` r
model <- FuzzyBayesRuleModel$new()
model$fit(train = iris[, -5], cl = iris$Species)
#> ℹ Fuzzy Bayes Rule: 150 observations, 4 predictors and 3 classes
#> ℹ Training the model: estimating its parameters
#> ✔ Model trained successfully [5ms]
#> 
head(model$predict(iris[, -5]))
#> [1] setosa setosa setosa setosa setosa setosa
#> Levels: setosa versicolor virginica
head(model$predict(iris[, -5], type = "prob"))
#>      setosa   versicolor    virginica
#> [1,]      1 5.096275e-29 2.780366e-45
#> [2,]      1 4.512707e-21 2.845258e-37
#> [3,]      1 1.593062e-23 1.248931e-38
#> [4,]      1 7.338988e-21 2.780687e-34
#> [5,]      1 3.487249e-30 2.811773e-45
#> [6,]      1 1.878231e-27 8.760727e-42
model
#> <FuzzyBayesRule> Fuzzy Bayes Rule
#> ✔ Trained on 150 observations in 0.002s
#>   Hyperparameters: fuzzy = TRUE
#>   Predictors (4): Sepal.Length, Sepal.Width, Petal.Length, Petal.Width
#>   Classes (3): setosa, versicolor, virginica
```
