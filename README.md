
# FuzzyClass <img src="man/figures/logo.png" style="float: right" height="139"/>

<!-- badges: start -->

[![CRAN
version](https://www.r-pkg.org/badges/version/FuzzyClass)](https://cran.r-project.org/package=FuzzyClass)
[![CRAN
Download](https://cranlogs.r-pkg.org/badges/grand-total/FuzzyClass)](https://cran.r-project.org/package=FuzzyClass)
<!-- badges: end -->

Last update: 05-03-2023

## A family of probabilities-based classifiers fuzzy and non-fuzzy

### Installation

``` r
# Installation
install.packages("devtools")
devtools::install_github("leapigufpb/FuzzyClass")
```

### Usage

``` r
# package import
library(FuzzyClass)
```

### Data reading and preparation for use

``` r

library(FuzzyClass)
library(caret)

#' ---------------------------------------------
#' The following shows how the functions are used:
#' --------------
#' Reading a database:
#'
#' Actual training data:
data(VirtualRealityData)

VirtualRealityData <- as.data.frame(VirtualRealityData)

# Splitting into Training and Testing
split <- caTools::sample.split(t(VirtualRealityData[,1]), SplitRatio = 0.7)
Train <- subset(VirtualRealityData, split == "TRUE")
Test <- subset(VirtualRealityData, split == "FALSE")
# ----------------

test = Test[,-4]
```

#### Fuzzy Gaussian Naive Bayes with Fuzzy Parameters

``` r
# --------------------------------------------------
# Fuzzy Gaussian Naive Bayes with Fuzzy Parameters


fit_FGNB <- GauNBFuzzyParam(train =  Train[,-4],
                                    cl = Train[,4], metd = 2, cores = 1)


print(fit_FGNB)
#> 
#> Fuzzy Gaussian Naive Bayes Classifier for Discrete Predictors
#> 
#> Variables:
#> [1] "V1" "V2" "V3"
#> Class:
#> [1] "1" "2" "3"
saida <- predict(fit_FGNB, test)
confusionMatrix(factor(Test[,4]), saida)
#> Confusion Matrix and Statistics
#> 
#>           Reference
#> Prediction  1  2  3
#>          1 62  6  0
#>          2  6 38 13
#>          3  0  7 48
#> 
#> Overall Statistics
#>                                           
#>                Accuracy : 0.8222          
#>                  95% CI : (0.7584, 0.8751)
#>     No Information Rate : 0.3778          
#>     P-Value [Acc > NIR] : < 2.2e-16       
#>                                           
#>                   Kappa : 0.7323          
#>                                           
#>  Mcnemar's Test P-Value : NA              
#> 
#> Statistics by Class:
#> 
#>                      Class: 1 Class: 2 Class: 3
#> Sensitivity            0.9118   0.7451   0.7869
#> Specificity            0.9464   0.8527   0.9412
#> Pos Pred Value         0.9118   0.6667   0.8727
#> Neg Pred Value         0.9464   0.8943   0.8960
#> Prevalence             0.3778   0.2833   0.3389
#> Detection Rate         0.3444   0.2111   0.2667
#> Detection Prevalence   0.3778   0.3167   0.3056
#> Balanced Accuracy      0.9291   0.7989   0.8640

saidaMatrix <- predict(fit_FGNB, test, type = "matrix")
```

``` r
# --------------------------------------------------
# head view

saida |> head()
#> [1] 1 1 1 1 1 1
#> Levels: 1 2 3

saidaMatrix |> head()
#>              1           2            3
#> [1,] 0.5190477 0.381592934 9.935940e-02
#> [2,] 0.9938309 0.006160007 9.063041e-06
#> [3,] 0.9361335 0.063651768 2.147316e-04
#> [4,] 0.9938859 0.006110688 3.403995e-06
#> [5,] 0.8330550 0.156143228 1.080176e-02
#> [6,] 0.4868158 0.481737673 3.144651e-02
```
