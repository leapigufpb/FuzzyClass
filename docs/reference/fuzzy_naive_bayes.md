# Fuzzy naive Bayes models for tidymodels

`fuzzy_naive_bayes()` defines a classification model, estimated by one
of the FuzzyClass classifiers, that can be used with the tidymodels
ecosystem (`parsnip`, `workflows`, `recipes`, `rsample`, `tune`,
`yardstick`, ...).

The classifier is chosen through the engine:

|  |  |  |
|----|----|----|
| Engine | Classifier | Main arguments |
| `"gaussian"` (default) | [`FuzzyGaussianNaiveBayes()`](https://leapigufpb.github.io/FuzzyClass/reference/FuzzyGaussianNaiveBayes.md) | `fuzzy` |
| `"gamma"` | [`FuzzyGammaNaiveBayes()`](https://leapigufpb.github.io/FuzzyClass/reference/FuzzyGammaNaiveBayes.md) | `fuzzy` |
| `"beta"` | [`FuzzyBetaNaiveBayes()`](https://leapigufpb.github.io/FuzzyClass/reference/FuzzyBetaNaiveBayes.md) | `fuzzy` |
| `"exponential"` | [`FuzzyExponentialNaiveBayes()`](https://leapigufpb.github.io/FuzzyClass/reference/FuzzyExponentialNaiveBayes.md) | `fuzzy` |
| `"binomial"` | [`FuzzyBinomialNaiveBayes()`](https://leapigufpb.github.io/FuzzyClass/reference/FuzzyBinomialNaiveBayes.md) | `fuzzy` |
| `"poisson"` | [`FuzzyPoissonNaiveBayes()`](https://leapigufpb.github.io/FuzzyClass/reference/FuzzyPoissonNaiveBayes.md) | `fuzzy` |
| `"geometric"` | [`FuzzyGeoNaiveBayes()`](https://leapigufpb.github.io/FuzzyClass/reference/FuzzyGeoNaiveBayes.md) | `fuzzy` |
| `"trapezoidal"` | [`FuzzyTrapezoidalNaiveBayes()`](https://leapigufpb.github.io/FuzzyClass/reference/FuzzyTrapezoidalNaiveBayes.md) | `fuzzy` |
| `"triangular"` | [`FuzzyTriangularNaiveBayes()`](https://leapigufpb.github.io/FuzzyClass/reference/FuzzyTriangularNaiveBayes.md) | `fuzzy` |
| `"hypergeometric"` | [`FuzzyHipergeometricNaiveBayes()`](https://leapigufpb.github.io/FuzzyClass/reference/FuzzyHipergeometricNaiveBayes.md) | `fuzzy` |
| `"bayes_rule"` | [`FuzzyBayesRule()`](https://leapigufpb.github.io/FuzzyClass/reference/FuzzyBayesRule.md) | `fuzzy` |
| `"naive"` | [`FuzzyNaiveBayes()`](https://leapigufpb.github.io/FuzzyClass/reference/FuzzyNaiveBayes.md) | `fuzzy` |
| `"dw_gamma"` | [`DWFuzzyGammaNaiveBayes()`](https://leapigufpb.github.io/FuzzyClass/reference/DWFuzzyGammaNaiveBayes.md) | `fuzzy` |
| `"dw_hypergeometric"` | [`DWFuzzyHipergeometricNaiveBayes()`](https://leapigufpb.github.io/FuzzyClass/reference/DWFuzzyHipergeometricNaiveBayes.md) | `fuzzy` |
| `"gaussian_fuzzy_param"` | [`GauNBFuzzyParam()`](https://leapigufpb.github.io/FuzzyClass/reference/GauNBFuzzyParam.md) | `alpha_cut`, `ranking_method` |
| `"exponential_fuzzy_param"` | [`ExpNBFuzzyParam()`](https://leapigufpb.github.io/FuzzyClass/reference/ExpNBFuzzyParam.md) | `alpha_cut`, `ranking_method` |
| `"poisson_fuzzy_param"` | [`PoiNBFuzzyParam()`](https://leapigufpb.github.io/FuzzyClass/reference/PoiNBFuzzyParam.md) | `alpha_cut`, `ranking_method` |

Main arguments that do not apply to the chosen engine are ignored. Any
other argument of the classifier (e.g. `wdelta` and `weta` for the
double weighted classifiers, `alp` and `w` for `ranking_method = 4`, or
`verbose`) can be passed through
[`parsnip::set_engine()`](https://parsnip.tidymodels.org/reference/set_engine.html).

The predictors are passed to the classifier as they are (no dummy
variables are created), so use a recipe to prepare them when needed.
Each fit prints a training message; set
`options(FuzzyClass.verbose = FALSE)` (see
[`fuzzy_verbose()`](https://leapigufpb.github.io/FuzzyClass/reference/fuzzy_verbose.md))
or `set_engine(..., verbose = FALSE)` to silence it, for example when
tuning.

This function requires the `parsnip` package.

## Usage

``` r
fuzzy_naive_bayes(
  mode = "classification",
  engine = "gaussian",
  fuzzy = NULL,
  alpha_cut = NULL,
  ranking_method = NULL
)

# S3 method for class 'fuzzy_naive_bayes'
update(
  object,
  parameters = NULL,
  fuzzy = NULL,
  alpha_cut = NULL,
  ranking_method = NULL,
  fresh = FALSE,
  ...
)
```

## Arguments

- mode:

  A single character string for the type of model. The only possible
  value is `"classification"`.

- engine:

  A single character string specifying the classifier (see the table
  above).

- fuzzy:

  Logical; whether the membership functions are used (argument `fuzzy`
  of the classifiers). Tunable with
  [`fuzzy_membership()`](https://leapigufpb.github.io/FuzzyClass/reference/fuzzy_membership.md).

- alpha_cut:

  Smallest alpha-cut of the fuzzy parameters (argument `alphacut`).
  Tunable with
  [`alpha_cut()`](https://leapigufpb.github.io/FuzzyClass/reference/fuzzy_membership.md).

- ranking_method:

  Integer from 1 to 5: method used to rank the fuzzy numbers (argument
  `metd`). Tunable with
  [`ranking_method()`](https://leapigufpb.github.io/FuzzyClass/reference/fuzzy_membership.md).

- object:

  A `fuzzy_naive_bayes` model specification.

- parameters:

  A 1-row data frame (or `NULL`) with the new values of the main
  arguments, as used by
  [`tune::finalize_model()`](https://tune.tidymodels.org/reference/finalize_model.html).

- fresh:

  Logical; replace all the arguments (`TRUE`) or only update the ones
  given (`FALSE`)?

- ...:

  Engine-specific arguments to update.

## Value

A model specification object with classes `fuzzy_naive_bayes` and
`model_spec`.

## Examples

``` r
library(parsnip)

spec <- fuzzy_naive_bayes(fuzzy = TRUE) |>
  set_engine("gaussian")
spec
#> Fuzzy Naive Bayes Model Specification (classification)
#> 
#> Main Arguments:
#>   fuzzy = TRUE
#> 
#> Computational engine: gaussian 
#> 

model_fit <- fit(spec, Species ~ ., data = iris)
#> ℹ Fuzzy Gaussian Naive Bayes (Zadeh-based): 150 observations, 4 predictors and 3 classes
#> ℹ Training the model: estimating its parameters
#> ✔ Model trained successfully [6ms]
#> 
predict(model_fit, iris[c(1, 51, 101), ])
#> # A tibble: 3 × 1
#>   .pred_class
#>   <fct>      
#> 1 setosa     
#> 2 versicolor 
#> 3 virginica  
predict(model_fit, iris[c(1, 51, 101), ], type = "prob")
#> # A tibble: 3 × 3
#>   .pred_setosa .pred_versicolor .pred_virginica
#>          <dbl>            <dbl>           <dbl>
#> 1    1   e+  0         3.09e-21       2.01 e-29
#> 2    1.76e-108         9.47e- 1       5.35 e- 2
#> 3    6.71e-251         9.75e-12       1.000e+ 0

# Fuzzy parameters with the GTFN ranking method
fuzzy_naive_bayes(ranking_method = 5) |>
  set_engine("gaussian_fuzzy_param") |>
  fit(Species ~ ., data = iris)
#> ℹ Gaussian Naive Bayes with Fuzzy Parameters: 150 observations, 4 predictors and 3 classes
#> ℹ Training the model: estimating its parameters
#> ✔ Model trained successfully [5ms]
#> 
#> parsnip model object
#> 
#> <GauNBFuzzyParam> Gaussian Naive Bayes with Fuzzy Parameters
#> ✔ Trained on 150 observations in 0.002s
#>   Hyperparameters: alphacut = 1e-04, metd = 5, alp = c(0.35, 0.7, 0.86), w = c(0.1, 0.3, 0.6)
#>   Predictors (4): Sepal.Length, Sepal.Width, Petal.Length, Petal.Width
#>   Classes (3): setosa, versicolor, virginica

# Engine-specific arguments
data(GamWeightData)
fuzzy_naive_bayes() |>
  set_engine("dw_gamma", wdelta = rep(1/3, 3), weta = c(0.3, 0.2, 0.5)) |>
  fit(class ~ ., data = GamWeightData)
#> ℹ Double Weighted Fuzzy Gamma Naive Bayes: 600 observations, 3 predictors and 3 classes
#> ℹ Training the model: estimating its parameters
#> ✔ Model trained successfully [5ms]
#> 
#> parsnip model object
#> 
#> <DWFuzzyGammaNaiveBayes> Double Weighted Fuzzy Gamma Naive Bayes
#> ✔ Trained on 600 observations in 0.002s
#>   Hyperparameters: fuzzy = TRUE, wdelta = c(0.333333333333333, 0.333333333333333, 0.333333333333333), weta = c(0.3, 0.2, 0.5)
#>   Predictors (3): Var1, Var2, Var3
#>   Classes (3): 1, 2, 3
```
