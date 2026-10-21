# Predict method for FuzzyClass classifiers

S3 interface to the `$predict()` method of the
[FuzzyClassifier](https://leapigufpb.github.io/FuzzyClass/reference/FuzzyClassifier.md)
R6 objects, so that `predict(model, newdata)` keeps working as in
previous versions.

## Usage

``` r
# S3 method for class 'FuzzyClassifier'
predict(object, newdata, type = "class", ...)
```

## Arguments

- object:

  A trained
  [FuzzyClassifier](https://leapigufpb.github.io/FuzzyClass/reference/FuzzyClassifier.md)
  object.

- newdata:

  Matrix or data frame of test set cases.

- type:

  `"class"` (default) for predicted classes, or `"prob"` (synonyms
  `"matrix"` and `"raw"`) for the matrix of class probabilities.

- ...:

  Ignored.

## Value

A factor (`type = "class"`) or a numeric matrix.
