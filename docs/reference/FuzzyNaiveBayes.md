# Fuzzy Naive Bayes

`FuzzyNaiveBayes` Fuzzy Naive Bayes

## Usage

``` r
FuzzyNaiveBayes(train, cl, fuzzy = TRUE, m = NULL, Pi = NULL)
```

## Arguments

- train:

  matrix or data frame of training set cases

- cl:

  factor of true classifications of training set

- fuzzy:

  boolean variable to use the membership function

- m:

  is M/N, where M is the number of classes and N is the number of train
  lines

- Pi:

  is 1/M, where M is the number of classes

## Value

A vector of classifications

## References

Moraes RM, Machado LS (2009). “Another approach for fuzzy naive bayes
applied on online training assessment in virtual reality simulators.” In
*Proceedings of Safety Health and Environmental World Congress*, 62–66.

## Examples

``` r

set.seed(1) # determining a seed
data(HouseVotes84)

# Splitting into Training and Testing
split <- caTools::sample.split(t(HouseVotes84[, 1]), SplitRatio = 0.7)
Train <- subset(HouseVotes84, split == "TRUE")
Test <- subset(HouseVotes84, split == "FALSE")
# ----------------
# matrix or data frame of test set cases.
# A vector will be interpreted as a row vector for a single case.
test <- Test[, -1]
fit_FNB <- FuzzyNaiveBayes(
  train = Train[, -1],
  cl = Train[, 1]
)

pred_FNB <- predict(fit_FNB, test)

head(pred_FNB)
#> [1] republican democrat   republican republican republican democrat  
#> Levels: democrat republican
head(Test[, 1])
#> [1] democrat   democrat   democrat   republican republican democrat  
#> Levels: democrat republican
```
