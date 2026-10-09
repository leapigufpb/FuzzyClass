# Fuzzy Naive Bayes Geometric Classifier

`FuzzyGeoNaiveBayes` Naive Bayes Geometric Classifier

## Usage

``` r
FuzzyGeoNaiveBayes(train, cl, cores = 2, fuzzy = T)
```

## Arguments

- train:

  matrix or data frame of training set cases.

- cl:

  factor of true classifications of training set

- cores:

  kept for backward compatibility; prediction is vectorized and does not
  start parallel workers (default = 2)

- fuzzy:

  boolean variable to use the membership function

## Value

A vector of classifications

## References

Ferreira J, Lopes A, Machado L, Moraes R (2023). “A Novel Fuzzy
Geometric Naive Bayes Network for Online Skills Assessment in Training
Based on Virtual Reality.” *In Proceedings of the 15th International
Joint Conference on Computational Intelligence*, 395–401.

## Examples

``` r

set.seed(1) # determining a seed
class1 <- data.frame(vari1 = rgeom(100,prob = 0.2),
                     vari2 = rgeom(100,prob = 0.2),
                     vari3 = rgeom(100,prob = 0.2), class = 1)
class2 <- data.frame(vari1 = rgeom(100,prob = 0.5),
                     vari2 = rgeom(100,prob = 0.5),
                     vari3 = rgeom(100,prob = 0.5), class = 2)
class3 <- data.frame(vari1 = rgeom(100,prob = 0.9),
                     vari2 = rgeom(100,prob = 0.9),
                     vari3 = rgeom(100,prob = 0.9), class = 3)
data <- rbind(class1,class2,class3)

# Splitting into Training and Testing
split <- caTools::sample.split(t(data[, 1]), SplitRatio = 0.7)
Train <- subset(data, split == "TRUE")
Test <- subset(data, split == "FALSE")
# ----------------
# matrix or data frame of test set cases.
# A vector will be interpreted as a row vector for a single case.
test <- Test[, -4]
fit_NBT <- FuzzyGeoNaiveBayes(
  train = Train[, -4],
  cl = Train[, 4], cores = 2
)

pred_NBT <- predict(fit_NBT, test)

head(pred_NBT)
#> [1] 1 1 1 1 1 1
#> Levels: 1 2 3
head(Test[, 4])
#> [1] 1 1 1 1 1 1
```
