# Tuning parameters of the fuzzy naive Bayes models

Parameter objects, for the `dials` package, of the main arguments of
[`fuzzy_naive_bayes()`](https://leapigufpb.github.io/FuzzyClass/reference/fuzzy_naive_bayes.md).
They are used automatically by `tune` when an argument is marked with
`tune()`.

## Usage

``` r
fuzzy_membership(values = c(TRUE, FALSE))

alpha_cut(range = c(1e-04, 0.1), trans = NULL)

ranking_method(range = c(1L, 5L), trans = NULL)
```

## Arguments

- values:

  A logical vector of possible values.

- range:

  A two-element vector with the bounds of the parameter.

- trans:

  A `trans` object from the `scales` package, or `NULL`.

## Value

A `param` object of the `dials` package.

## Details

- `fuzzy_membership()`: whether the membership functions are used
  (argument `fuzzy`).

- `alpha_cut()`: smallest alpha-cut of the fuzzy parameters (argument
  `alphacut`).

- `ranking_method()`: method used to rank the fuzzy numbers (argument
  `metd`): 1 = Yager, 2 = Q distance, 3 = Thorani, 4 = alpha-order, 5 =
  GTFN.

These functions require the `dials` package.

## Examples

``` r
fuzzy_membership()
#> Fuzzy Membership Function (qualitative)
#> 2 possible values include:
#> TRUE and FALSE
alpha_cut()
#> Smallest Alpha-Cut (quantitative)
#> Range: [1e-04, 0.1]
ranking_method()
#> Fuzzy Number Ranking Method (quantitative)
#> Range: [1, 5]
```
