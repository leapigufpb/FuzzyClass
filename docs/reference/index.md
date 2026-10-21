# Package index

## R6 Interface

All classifiers are R6 classes that inherit from `FuzzyClassifier`:
create them with `$new()`, train with `$fit()` and classify with
`$predict()`.

- [`FuzzyClassifier`](https://leapigufpb.github.io/FuzzyClass/reference/FuzzyClassifier.md)
  : Base R6 class of the FuzzyClass classifiers
- [`DWFuzzyGammaNaiveBayesModel`](https://leapigufpb.github.io/FuzzyClass/reference/DWFuzzyGammaNaiveBayesModel.md)
  : Double Weighted Fuzzy Gamma Naive Bayes (R6 class)
- [`DWFuzzyHipergeometricNaiveBayesModel`](https://leapigufpb.github.io/FuzzyClass/reference/DWFuzzyHipergeometricNaiveBayesModel.md)
  : Double Weighted Fuzzy Hypergeometric Naive Bayes (R6 class)
- [`ExpNBFuzzyParamModel`](https://leapigufpb.github.io/FuzzyClass/reference/ExpNBFuzzyParamModel.md)
  : Exponential Naive Bayes with Fuzzy Parameters (R6 class)
- [`FuzzyBayesRuleModel`](https://leapigufpb.github.io/FuzzyClass/reference/FuzzyBayesRuleModel.md)
  : Fuzzy Bayes Rule (R6 class)
- [`FuzzyBetaNaiveBayesModel`](https://leapigufpb.github.io/FuzzyClass/reference/FuzzyBetaNaiveBayesModel.md)
  : Fuzzy Beta Naive Bayes (R6 class)
- [`FuzzyBinomialNaiveBayesModel`](https://leapigufpb.github.io/FuzzyClass/reference/FuzzyBinomialNaiveBayesModel.md)
  : Fuzzy Binomial Naive Bayes (R6 class)
- [`FuzzyExponentialNaiveBayesModel`](https://leapigufpb.github.io/FuzzyClass/reference/FuzzyExponentialNaiveBayesModel.md)
  : Fuzzy Exponential Naive Bayes (R6 class)
- [`FuzzyGammaNaiveBayesModel`](https://leapigufpb.github.io/FuzzyClass/reference/FuzzyGammaNaiveBayesModel.md)
  : Fuzzy Gamma Naive Bayes (R6 class)
- [`FuzzyGaussianNaiveBayesModel`](https://leapigufpb.github.io/FuzzyClass/reference/FuzzyGaussianNaiveBayesModel.md)
  : Fuzzy Gaussian Naive Bayes (Zadeh-based) (R6 class)
- [`FuzzyGeoNaiveBayesModel`](https://leapigufpb.github.io/FuzzyClass/reference/FuzzyGeoNaiveBayesModel.md)
  : Fuzzy Geometric Naive Bayes (R6 class)
- [`FuzzyHipergeometricNaiveBayesModel`](https://leapigufpb.github.io/FuzzyClass/reference/FuzzyHipergeometricNaiveBayesModel.md)
  : Fuzzy Hypergeometric Naive Bayes (R6 class)
- [`FuzzyNaiveBayesModel`](https://leapigufpb.github.io/FuzzyClass/reference/FuzzyNaiveBayesModel.md)
  : Fuzzy Naive Bayes (R6 class)
- [`FuzzyPoissonNaiveBayesModel`](https://leapigufpb.github.io/FuzzyClass/reference/FuzzyPoissonNaiveBayesModel.md)
  : Fuzzy Poisson Naive Bayes (R6 class)
- [`FuzzyTrapezoidalNaiveBayesModel`](https://leapigufpb.github.io/FuzzyClass/reference/FuzzyTrapezoidalNaiveBayesModel.md)
  : Fuzzy Trapezoidal Naive Bayes (R6 class)
- [`FuzzyTriangularNaiveBayesModel`](https://leapigufpb.github.io/FuzzyClass/reference/FuzzyTriangularNaiveBayesModel.md)
  : Fuzzy Triangular Naive Bayes (R6 class)
- [`GauNBFuzzyParamModel`](https://leapigufpb.github.io/FuzzyClass/reference/GauNBFuzzyParamModel.md)
  : Gaussian Naive Bayes with Fuzzy Parameters (R6 class)
- [`PoiNBFuzzyParamModel`](https://leapigufpb.github.io/FuzzyClass/reference/PoiNBFuzzyParamModel.md)
  : Poisson Naive Bayes with Fuzzy Parameters (R6 class)
- [`predict(`*`<FuzzyClassifier>`*`)`](https://leapigufpb.github.io/FuzzyClass/reference/predict.FuzzyClassifier.md)
  : Predict method for FuzzyClass classifiers
- [`fuzzy_verbose()`](https://leapigufpb.github.io/FuzzyClass/reference/fuzzy_verbose.md)
  : Verbosity of the FuzzyClass classifiers

## tidymodels Interface

Use any classifier with parsnip, workflows, recipes and tune.

- [`fuzzy_naive_bayes()`](https://leapigufpb.github.io/FuzzyClass/reference/fuzzy_naive_bayes.md)
  [`update(`*`<fuzzy_naive_bayes>`*`)`](https://leapigufpb.github.io/FuzzyClass/reference/fuzzy_naive_bayes.md)
  : Fuzzy naive Bayes models for tidymodels
- [`fuzzy_membership()`](https://leapigufpb.github.io/FuzzyClass/reference/fuzzy_membership.md)
  [`alpha_cut()`](https://leapigufpb.github.io/FuzzyClass/reference/fuzzy_membership.md)
  [`ranking_method()`](https://leapigufpb.github.io/FuzzyClass/reference/fuzzy_membership.md)
  : Tuning parameters of the fuzzy naive Bayes models

## Continuous Classifiers

Probabilistic classifiers for continuous features using fuzzy sets and
fuzzy parameters.

- [`FuzzyGaussianNaiveBayes()`](https://leapigufpb.github.io/FuzzyClass/reference/FuzzyGaussianNaiveBayes.md)
  : Fuzzy Gaussian Naive Bayes Classifier Zadeh-based
- [`GauNBFuzzyParam()`](https://leapigufpb.github.io/FuzzyClass/reference/GauNBFuzzyParam.md)
  : Fuzzy Gaussian Naive Bayes Classifier with Fuzzy parameters
- [`FuzzyExponentialNaiveBayes()`](https://leapigufpb.github.io/FuzzyClass/reference/FuzzyExponentialNaiveBayes.md)
  : Fuzzy Exponential Naive Bayes
- [`ExpNBFuzzyParam()`](https://leapigufpb.github.io/FuzzyClass/reference/ExpNBFuzzyParam.md)
  : Fuzzy Exponential Naive Bayes Classifier with Fuzzy parameters
- [`FuzzyGammaNaiveBayes()`](https://leapigufpb.github.io/FuzzyClass/reference/FuzzyGammaNaiveBayes.md)
  : Fuzzy Gamma Naive Bayes
- [`DWFuzzyGammaNaiveBayes()`](https://leapigufpb.github.io/FuzzyClass/reference/DWFuzzyGammaNaiveBayes.md)
  : Double Weighted Fuzzy Gamma Naive Bayes
- [`FuzzyBetaNaiveBayes()`](https://leapigufpb.github.io/FuzzyClass/reference/FuzzyBetaNaiveBayes.md)
  : Fuzzy Beta Naive Bayes
- [`FuzzyTriangularNaiveBayes()`](https://leapigufpb.github.io/FuzzyClass/reference/FuzzyTriangularNaiveBayes.md)
  : Fuzzy Naive Bayes Triangular Classifier
- [`FuzzyTrapezoidalNaiveBayes()`](https://leapigufpb.github.io/FuzzyClass/reference/FuzzyTrapezoidalNaiveBayes.md)
  : Fuzzy Naive Bayes Trapezoidal Classifier

## Discrete and Count Classifiers

Classifiers for discrete and count data with fuzzy memberships.

- [`FuzzyPoissonNaiveBayes()`](https://leapigufpb.github.io/FuzzyClass/reference/FuzzyPoissonNaiveBayes.md)
  : Fuzzy Poisson Naive Bayes
- [`PoiNBFuzzyParam()`](https://leapigufpb.github.io/FuzzyClass/reference/PoiNBFuzzyParam.md)
  : Fuzzy Poisson Naive Bayes Classifier with Fuzzy parameters
- [`FuzzyBinomialNaiveBayes()`](https://leapigufpb.github.io/FuzzyClass/reference/FuzzyBinomialNaiveBayes.md)
  : Fuzzy Binomial Naive Bayes
- [`FuzzyHipergeometricNaiveBayes()`](https://leapigufpb.github.io/FuzzyClass/reference/FuzzyHipergeometricNaiveBayes.md)
  : Fuzzy Hipergeometric Naive Bayes
- [`DWFuzzyHipergeometricNaiveBayes()`](https://leapigufpb.github.io/FuzzyClass/reference/DWFuzzyHipergeometricNaiveBayes.md)
  : Double Weighted Fuzzy Hipergeometric Naive Bayes

## Geometric and Decision Rule Classifiers

Classifiers using geometric distance memberships and fuzzy rule systems.

- [`FuzzyGeoNaiveBayes()`](https://leapigufpb.github.io/FuzzyClass/reference/FuzzyGeoNaiveBayes.md)
  : Fuzzy Naive Bayes Geometric Classifier
- [`FuzzyNaiveBayes()`](https://leapigufpb.github.io/FuzzyClass/reference/FuzzyNaiveBayes.md)
  : Fuzzy Naive Bayes
- [`FuzzyBayesRule()`](https://leapigufpb.github.io/FuzzyClass/reference/FuzzyBayesRule.md)
  : Fuzzy Bayes Rule
- [`FuzzyClass`](https://leapigufpb.github.io/FuzzyClass/reference/FuzzyClass-package.md)
  [`FuzzyClass-package`](https://leapigufpb.github.io/FuzzyClass/reference/FuzzyClass-package.md)
  : FuzzyClass: Fuzzy and Non-Fuzzy Classifiers

## Datasets

Example datasets included with FuzzyClass.

- [`VirtualRealityData`](https://leapigufpb.github.io/FuzzyClass/reference/VirtualRealityData.md)
  : Virtual Reality Simulator Data
- [`SimulatedData`](https://leapigufpb.github.io/FuzzyClass/reference/SimulatedData.md)
  : Simulated Data
- [`HouseVotes84`](https://leapigufpb.github.io/FuzzyClass/reference/HouseVotes84.md)
  : United States Congressional Voting Records 1984
- [`GamWeightData`](https://leapigufpb.github.io/FuzzyClass/reference/GamWeightData.md)
  : Gamma Weighted Data
