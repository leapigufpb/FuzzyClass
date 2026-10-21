# Double Weighted Fuzzy Hipergeometric Naive Bayes

`DWFuzzyHipergeometricNaiveBayes` Double Weighted Fuzzy Hipergeometric
Naive Bayes

## Usage

``` r
DWFuzzyHipergeometricNaiveBayes(
  train,
  cl,
  cores = 2,
  fuzzy = TRUE,
  wdelta,
  weta,
  verbose = fuzzy_verbose()
)
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

- wdelta:

  vector weight each class

- weta:

  vector weight each feature

- verbose:

  logical; if `TRUE` (default, see
  [`fuzzy_verbose()`](https://leapigufpb.github.io/FuzzyClass/reference/fuzzy_verbose.md))
  a message reports the training progress.

## Value

A trained
[DWFuzzyHipergeometricNaiveBayesModel](https://leapigufpb.github.io/FuzzyClass/reference/DWFuzzyHipergeometricNaiveBayesModel.md)
R6 object (which inherits from
[FuzzyClassifier](https://leapigufpb.github.io/FuzzyClass/reference/FuzzyClassifier.md)).
Use `predict(object, newdata, type = "class")` or
`object$predict(newdata)` to classify new observations; `type = "prob"`
(or `"matrix"`) returns the matrix of class probabilities.

## References

Ferreira J, Machado LS, de Moraes RM (2025). “A New Double Weighted
Fuzzy Hypergeometric Naive Bayes Network and its Application for User’s
Assessment in Virtual Reality Simulators.” *International Journal of
Computational Intelligence Systems*, **18**(1), 1–17.

## See also

[DWFuzzyHipergeometricNaiveBayesModel](https://leapigufpb.github.io/FuzzyClass/reference/DWFuzzyHipergeometricNaiveBayesModel.md)
for the R6 interface and
[`fuzzy_naive_bayes()`](https://leapigufpb.github.io/FuzzyClass/reference/fuzzy_naive_bayes.md)
(engine `"dw_hypergeometric"`) for the tidymodels interface.

## Examples

``` r

set.seed(1)

substituir_zero <- function(x) {
  if (x == 0) {
    while(1){
     new_valor <- rhyper(1,3,30,10)
      if(new_valor != 0){
        return(new_valor)
      }
    }
  } else {
    return(x)
  }
}

#Building dataframe
class1 <- data.frame(rhyper(72,3,30,10), 
                     sample(seq(1,5),72,replace = TRUE), class = "Bem treinado")
colnames(class1)[1] <- "sucessos"
class1$sucessos <- sapply(class1$sucessos, substituir_zero)
colnames(class1)[2] <- "tentativas"
colnames(class1)[3] <- "avaliação"
class2 <- data.frame(rhyper(72,3,17,12), 
                     sample(seq(4,7),72,replace = TRUE), class = "Mediano")
colnames(class2)[1] <- "sucessos"
#class2$sucessos <- ifelse(class2$sucessos == 0, 2, class2$sucessos)
colnames(class2)[2] <- "tentativas"
colnames(class2)[3] <- "avaliação"
class3 <- data.frame(rhyper(72,3,9,11),
                     sample(seq(7,10),72,replace = TRUE), class = "Precisa treinar")
colnames(class3)[1] <- "sucessos"
#class3$sucessos <- ifelse(class3$sucessos == 0, 3, class3$sucessos)
colnames(class3)[2] <- "tentativas"
colnames(class3)[3] <- "avaliação"
data <- rbind(class1,class2,class3)

# Weights
weta1 <- c(0.22, 0.33, 0.45)
wdelta1 <- c(0.33,0.33,0.33) 

#spliting dataframe
split <- caTools::sample.split(t(data[, 1]), SplitRatio = 0.7)
Train <- subset(data, split == "TRUE")
Test <- subset(data, split == "FALSE")

test <- Test[, -3]
fit_NHT <- DWFuzzyHipergeometricNaiveBayes(
 train = Train[, -3],
 cl = Train[, 3], cores = 2,
 fuzzy = TRUE,
 wdelta = wdelta1,
 weta = weta1)
#> ℹ Double Weighted Fuzzy Hypergeometric Naive Bayes: 151 observations, 2 predictors and 3 classes
#> ℹ Training the model: estimating its parameters
#> ✔ Model trained successfully [5ms]
#> 

pred_NHT <- predict(fit_NHT, test)


```
