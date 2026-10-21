# Double Weighted Fuzzy Gamma Naive Bayes (R6 class)

R6 implementation of the Double Weighted Fuzzy Gamma Naive Bayes
classifier. Create the model with `DWFuzzyGammaNaiveBayesModel$new()`,
train it with `$fit(train, cl)` and classify new observations with
`$predict(newdata)`. The functional interface
[`DWFuzzyGammaNaiveBayes()`](https://leapigufpb.github.io/FuzzyClass/reference/DWFuzzyGammaNaiveBayes.md)
trains and returns an object of this class.

## Super class

[`FuzzyClassifier`](https://leapigufpb.github.io/FuzzyClass/reference/FuzzyClassifier.md)
-\> `DWFuzzyGammaNaiveBayes`

## Methods

### Public methods

- [`DWFuzzyGammaNaiveBayes$new()`](#method-DWFuzzyGammaNaiveBayes-initialize)

- [`DWFuzzyGammaNaiveBayes$clone()`](#method-DWFuzzyGammaNaiveBayes-clone)

Inherited methods

- [`FuzzyClassifier$fit()`](https://leapigufpb.github.io/FuzzyClass/reference/FuzzyClassifier.html#method-fit)
- [`FuzzyClassifier$is_fitted()`](https://leapigufpb.github.io/FuzzyClass/reference/FuzzyClassifier.html#method-is_fitted)
- [`FuzzyClassifier$predict()`](https://leapigufpb.github.io/FuzzyClass/reference/FuzzyClassifier.html#method-predict)
- [`FuzzyClassifier$print()`](https://leapigufpb.github.io/FuzzyClass/reference/FuzzyClassifier.html#method-print)
- [`FuzzyClassifier$set_params()`](https://leapigufpb.github.io/FuzzyClass/reference/FuzzyClassifier.html#method-set_params)

------------------------------------------------------------------------

### `DWFuzzyGammaNaiveBayes$new()`

Create a new (untrained) classifier.

#### Usage

    DWFuzzyGammaNaiveBayes$new(fuzzy = TRUE, wdelta, weta)

#### Arguments

- `fuzzy`:

  See
  [`DWFuzzyGammaNaiveBayes()`](https://leapigufpb.github.io/FuzzyClass/reference/DWFuzzyGammaNaiveBayes.md).

- `wdelta`:

  See
  [`DWFuzzyGammaNaiveBayes()`](https://leapigufpb.github.io/FuzzyClass/reference/DWFuzzyGammaNaiveBayes.md).

- `weta`:

  See
  [`DWFuzzyGammaNaiveBayes()`](https://leapigufpb.github.io/FuzzyClass/reference/DWFuzzyGammaNaiveBayes.md).

------------------------------------------------------------------------

### `DWFuzzyGammaNaiveBayes$clone()`

The objects of this class are cloneable with this method.

#### Usage

    DWFuzzyGammaNaiveBayes$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.

## Examples

``` r
model <- DWFuzzyGammaNaiveBayesModel$new(wdelta = rep(1/3, 3), weta = c(0.3, 0.2, 0.5))
data(GamWeightData)
model$fit(train = GamWeightData[, -4], cl = GamWeightData[, 4])
#> ℹ Double Weighted Fuzzy Gamma Naive Bayes: 600 observations, 3 predictors and 3 classes
#> ℹ Training the model: estimating its parameters
#> ✔ Model trained successfully [5ms]
#> 
head(model$predict(GamWeightData[, -4]))
#> [1] 1 1 1 1 1 1
#> Levels: 1 2 3
model
#> <DWFuzzyGammaNaiveBayes> Double Weighted Fuzzy Gamma Naive Bayes
#> ✔ Trained on 600 observations in 0.003s
#>   Hyperparameters: fuzzy = TRUE, wdelta = c(0.333333333333333, 0.333333333333333, 0.333333333333333), weta = c(0.3, 0.2, 0.5)
#>   Predictors (3): Var1, Var2, Var3
#>   Classes (3): 1, 2, 3
```
