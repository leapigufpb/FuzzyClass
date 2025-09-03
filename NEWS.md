# FuzzyClass (development version)

# FuzzyClass 0.1.7

- Add the "Fuzzy Hipergeometric Naive Bayes classifier" as `FuzzyHipergeometricNaiveBayes`
- Add the "Double Weighted Fuzzy Hipergeometric Naive Bayes classifier" as `DWFuzzyHipergeometricNaiveBayes`
- Fix: memberships functions. Now they work correctly with dataframes 

# FuzzyClass 0.1.6

- Add the "Fuzzy Rule-based System" with some functions
- Fix: `FuzzyNaiveBayes` to one column in the data
- Fix: "membership" to dataframe with one column in the data
- Fix: "Frequency" to discrete distributions


# FuzzyClass 0.1.5

- Add the "double weighted fuzzy gamma naive bayes classifier" as `DWFuzzyGammaNaiveBayes`
- Add the "geometric fuzzy gamma naive bayes classifier" as `GeometricFuzzyGammaNaiveBayes`
- Add the "Fuzzy Bayes Rule classifier" as `FuzzyBayesRule`
- Remove the ordering method for functions with fuzzy parameters `metd=4`.

# FuzzyClass 0.1.4

- Added a new ordering method for functions with fuzzy parameters: `GauNBFuzzyParam`, `ExpNBFuzzyParam`,`GamNBFuzzyParam` and `PoiNBFuzzyParam`. The form can be used considering `metd = 4`.

# FuzzyClass 0.1.3

- Update in parameters estimation in function `FuzzyTriangularNaiveBayes`.
- Update `predict` has been optimized to functions `FuzzyGammaNaiveBayes`.
- Update functions of `GamNBFuzzyParam` and `GauNBFuzzyParam`.
- `predict` function has been optimized to `FuzzyGammaNaiveBayes`, `FuzzyBetaNaiveBayes`, `FuzzyBinomialNaiveBayes`,`FuzzyExponencialNaiveBayes`,`FuzzyTriangularNaiveBayes`,`FuzzyPoissonNaiveBayes`

# FuzzyClass 0.1.2

## New features

- Changed `\()` by `function(.)` in function `FuzzyNaiveBayes`, line 262.
- Changed `description` in `FuzzyClass` Documentation



# FuzzyClass 0.1.1

## New features

- Estimation new form in function `FuzzyGammaNaiveBayes`.

