# R6 Classes and tidymodels

Since version 0.3.0, every classifier of `FuzzyClass` is an
[R6](https://r6.r-lib.org) class, and all of them can be used within the
[tidymodels](https://www.tidymodels.org) framework through the parsnip
model
[`fuzzy_naive_bayes()`](https://leapigufpb.github.io/FuzzyClass/reference/fuzzy_naive_bayes.md).
This vignette shows both interfaces.

``` r

library(FuzzyClass)

set.seed(123)
train_index <- sample(nrow(iris), nrow(iris) * 0.7)
train_data <- iris[train_index, ]
test_data <- iris[-train_index, ]
```

## The R6 interface

Each classifier `X` has an R6 generator `XModel`, which inherits from
the base class `FuzzyClassifier`. A model is created with `$new()`,
which receives the hyperparameters of the classifier:

``` r

model <- GauNBFuzzyParamModel$new(metd = 2)
model
#> <GauNBFuzzyParam> Gaussian Naive Bayes with Fuzzy Parameters
#> ◯ Not trained yet: call $fit(train, cl)
#>   Hyperparameters: alphacut = 1e-04, metd = 2, alp = c(0.35, 0.7, 0.86), w = c(0.1, 0.3, 0.6)
```

`$fit()` estimates the model. While training, a message reports which
classifier is being estimated, the size of the data and the training
time:

``` r

model$fit(train = train_data[, -5], cl = train_data$Species)
#> ℹ Gaussian Naive Bayes with Fuzzy Parameters: 105 observations, 4 predictors and 3 classes
#> ℹ Training the model: estimating its parameters
#> ✔ Model trained successfully [18ms]
#> 
model
#> <GauNBFuzzyParam> Gaussian Naive Bayes with Fuzzy Parameters
#> ✔ Trained on 105 observations in 0.012s
#>   Hyperparameters: alphacut = 1e-04, metd = 2, alp = c(0.35, 0.7, 0.86), w = c(0.1, 0.3, 0.6)
#>   Predictors (4): Sepal.Length, Sepal.Width, Petal.Length, Petal.Width
#>   Classes (3): setosa, versicolor, virginica
```

`$predict()` classifies new observations, returning the predicted
classes or, with `type = "prob"`, the matrix of class probabilities:

``` r

pred <- model$predict(test_data[, -5])
head(pred)
#> [1] setosa setosa setosa setosa setosa setosa
#> Levels: setosa versicolor virginica
mean(pred == test_data$Species)
#> [1] 0.9777778

head(model$predict(test_data[, -5], type = "prob"))
#>         setosa   versicolor    virginica
#> [1,] 0.9999232 4.009250e-05 3.665857e-05
#> [2,] 0.9992695 3.065529e-04 4.239664e-04
#> [3,] 0.9997697 6.845832e-05 1.618868e-04
#> [4,] 0.9999763 1.008667e-05 1.362083e-05
#> [5,] 0.9997869 1.780041e-04 3.508405e-05
#> [6,] 0.9998933 5.571614e-05 5.093610e-05
```

Hyperparameters are changed with `$set_params()`, which resets the model
so that it must be trained again. Since `$fit()` and `$set_params()`
return the object, the calls can be chained; `verbose = FALSE` trains
silently:

``` r

model$set_params(metd = 5)$fit(train_data[, -5], train_data$Species, verbose = FALSE)
mean(model$predict(test_data[, -5]) == test_data$Species)
#> [1] 0.9777778
```

The training message can also be disabled for the whole session with
`options(FuzzyClass.verbose = FALSE)`.

### The functional interface

The functions of the previous versions are still available. They train
the model and return the R6 object, and the S3 method
[`predict()`](https://rdrr.io/r/stats/predict.html) is supported:

``` r

fit <- FuzzyGaussianNaiveBayes(train = train_data[, -5], cl = train_data$Species)
#> ℹ Fuzzy Gaussian Naive Bayes (Zadeh-based): 105 observations, 4 predictors and 3 classes
#> ℹ Training the model: estimating its parameters
#> ✔ Model trained successfully [15ms]
#> 
class(fit)
#> [1] "FuzzyGaussianNaiveBayes" "FuzzyClassifier"        
#> [3] "R6"
head(predict(fit, test_data[, -5]))
#> [1] setosa setosa setosa setosa setosa setosa
#> Levels: setosa versicolor virginica
head(predict(fit, test_data[, -5], type = "matrix"))
#>      setosa   versicolor    virginica
#> [1,]      1 3.814667e-22 5.829880e-26
#> [2,]      1 1.796064e-21 4.262538e-25
#> [3,]      1 1.609334e-22 2.497921e-25
#> [4,]      1 4.662083e-23 1.468580e-26
#> [5,]      1 5.191471e-21 5.592203e-24
#> [6,]      1 2.501888e-20 2.733399e-24
```

## Using FuzzyClass with tidymodels

[`fuzzy_naive_bayes()`](https://leapigufpb.github.io/FuzzyClass/reference/fuzzy_naive_bayes.md)
defines a parsnip model specification. The classifier is chosen through
the engine (see
[`?fuzzy_naive_bayes`](https://leapigufpb.github.io/FuzzyClass/reference/fuzzy_naive_bayes.md)
for the full list):

``` r

library(parsnip)
library(workflows)
library(recipes)
library(rsample)
library(tune)
library(yardstick)

spec <- fuzzy_naive_bayes(fuzzy = TRUE) |>
  set_engine("gaussian")
spec
#> Fuzzy Naive Bayes Model Specification (classification)
#> 
#> Main Arguments:
#>   fuzzy = TRUE
#> 
#> Computational engine: gaussian

iris_fit <- fit(spec, Species ~ ., data = train_data)
predict(iris_fit, test_data[1:5, ])
#> # A tibble: 5 × 1
#>   .pred_class
#>   <fct>      
#> 1 setosa     
#> 2 setosa     
#> 3 setosa     
#> 4 setosa     
#> 5 setosa
predict(iris_fit, test_data[1:5, ], type = "prob")
#> # A tibble: 5 × 3
#>   .pred_setosa .pred_versicolor .pred_virginica
#>          <dbl>            <dbl>           <dbl>
#> 1            1         3.81e-22        5.83e-26
#> 2            1         1.80e-21        4.26e-25
#> 3            1         1.61e-22        2.50e-25
#> 4            1         4.66e-23        1.47e-26
#> 5            1         5.19e-21        5.59e-24
```

Arguments specific to a classifier are passed to
[`set_engine()`](https://parsnip.tidymodels.org/reference/set_engine.html):

``` r

fuzzy_naive_bayes() |>
  set_engine("dw_gamma", wdelta = rep(1/3, 3), weta = rep(1/4, 4), verbose = FALSE) |>
  fit(Species ~ ., data = train_data) |>
  augment(new_data = test_data) |>
  yardstick::accuracy(truth = Species, estimate = .pred_class)
#> # A tibble: 1 × 3
#>   .metric  .estimator .estimate
#>   <chr>    <chr>          <dbl>
#> 1 accuracy multiclass     0.956
```

### Tuning

The main arguments `fuzzy`, `alpha_cut` and `ranking_method` can be
tuned. Here we tune the fuzzy-parameter Gaussian classifier inside a
workflow with a recipe, using cross-validation. The training messages
are disabled while tuning:

``` r

options(FuzzyClass.verbose = FALSE)

split <- initial_split(iris, prop = 0.7, strata = Species)
folds <- vfold_cv(training(split), v = 5, strata = Species)

wf <- workflow() |>
  add_recipe(recipe(Species ~ ., data = training(split))) |>
  add_model(
    fuzzy_naive_bayes(alpha_cut = tune(), ranking_method = tune()) |>
      set_engine("gaussian_fuzzy_param")
  )

set.seed(1)
tuned <- tune_grid(wf, resamples = folds, grid = 6,
                   metrics = metric_set(yardstick::accuracy, yardstick::roc_auc))
show_best(tuned, metric = "accuracy", n = 3)
#> # A tibble: 3 × 8
#>   alpha_cut ranking_method .metric  .estimator  mean     n std_err .config      
#>       <dbl>          <int> <chr>    <chr>      <dbl> <int>   <dbl> <chr>        
#> 1    0.0201              5 accuracy multiclass 0.943     5  0.0178 pre0_mod2_po…
#> 2    0.0401              1 accuracy multiclass 0.943     5  0.0178 pre0_mod3_po…
#> 3    0.0800              1 accuracy multiclass 0.943     5  0.0178 pre0_mod5_po…

final <- finalize_workflow(wf, select_best(tuned, metric = "accuracy")) |>
  last_fit(split, metrics = metric_set(yardstick::accuracy, yardstick::roc_auc))
collect_metrics(final)
#> # A tibble: 2 × 4
#>   .metric  .estimator .estimate .config        
#>   <chr>    <chr>          <dbl> <chr>          
#> 1 accuracy multiclass     0.978 pre0_mod0_post0
#> 2 roc_auc  hand_till      0.996 pre0_mod0_post0

options(FuzzyClass.verbose = TRUE)
```
