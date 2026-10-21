# Fuzzy Triangular Naive Bayes (R6 class)

R6 implementation of the Fuzzy Triangular Naive Bayes classifier. Create
the model with `FuzzyTriangularNaiveBayesModel$new()`, train it with
`$fit(train, cl)` and classify new observations with
`$predict(newdata)`. The functional interface
[`FuzzyTriangularNaiveBayes()`](https://leapigufpb.github.io/FuzzyClass/reference/FuzzyTriangularNaiveBayes.md)
trains and returns an object of this class.

## Super class

[`FuzzyClassifier`](https://leapigufpb.github.io/FuzzyClass/reference/FuzzyClassifier.md)
-\> `FuzzyTriangularNaiveBayes`

## Methods

### Public methods

- [`FuzzyTriangularNaiveBayes$new()`](#method-FuzzyTriangularNaiveBayes-initialize)

- [`FuzzyTriangularNaiveBayes$clone()`](#method-FuzzyTriangularNaiveBayes-clone)

Inherited methods

- [`FuzzyClassifier$fit()`](https://leapigufpb.github.io/FuzzyClass/reference/FuzzyClassifier.html#method-fit)
- [`FuzzyClassifier$is_fitted()`](https://leapigufpb.github.io/FuzzyClass/reference/FuzzyClassifier.html#method-is_fitted)
- [`FuzzyClassifier$predict()`](https://leapigufpb.github.io/FuzzyClass/reference/FuzzyClassifier.html#method-predict)
- [`FuzzyClassifier$print()`](https://leapigufpb.github.io/FuzzyClass/reference/FuzzyClassifier.html#method-print)
- [`FuzzyClassifier$set_params()`](https://leapigufpb.github.io/FuzzyClass/reference/FuzzyClassifier.html#method-set_params)

------------------------------------------------------------------------

### `FuzzyTriangularNaiveBayes$new()`

Create a new (untrained) classifier.

#### Usage

    FuzzyTriangularNaiveBayes$new(fuzzy = TRUE)

#### Arguments

- `fuzzy`:

  See
  [`FuzzyTriangularNaiveBayes()`](https://leapigufpb.github.io/FuzzyClass/reference/FuzzyTriangularNaiveBayes.md).

------------------------------------------------------------------------

### `FuzzyTriangularNaiveBayes$clone()`

The objects of this class are cloneable with this method.

#### Usage

    FuzzyTriangularNaiveBayes$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.

## Examples

``` r
model <- FuzzyTriangularNaiveBayesModel$new()
model$fit(train = iris[, -5], cl = iris$Species)
#> ℹ Fuzzy Triangular Naive Bayes: 150 observations, 4 predictors and 3 classes
#> ℹ Training the model: estimating its parameters
#> ✔ Model trained successfully [6ms]
#> 
head(model$predict(iris[, -5]))
#> [1] setosa setosa setosa setosa setosa setosa
#> Levels: setosa versicolor virginica
head(model$predict(iris[, -5], type = "prob"))
#>      setosa   versicolor    virginica
#> [1,]      1 4.772065e-35 5.627793e-36
#> [2,]      1 3.239345e-24 9.240629e-34
#> [3,]      1 7.466441e-25 1.292511e-33
#> [4,]      1 1.895370e-33 1.248279e-33
#> [5,]      1 4.763587e-35 2.120271e-36
#> [6,]      1 7.612816e-32 2.268199e-33
model
#> <FuzzyTriangularNaiveBayes> Fuzzy Triangular Naive Bayes
#> ✔ Trained on 150 observations in 0.003s
#>   Hyperparameters: fuzzy = TRUE
#>   Predictors (4): Sepal.Length, Sepal.Width, Petal.Length, Petal.Width
#>   Classes (3): setosa, versicolor, virginica
```
