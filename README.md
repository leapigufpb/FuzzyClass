
# FuzzyClass <img src="man/figures/logo.png" style="float: right" height="139"/>

<!-- badges: start -->

[![CRAN
version](https://www.r-pkg.org/badges/version/FuzzyClass)](https://cran.r-project.org/package=FuzzyClass)
[![CRAN
Download](https://cranlogs.r-pkg.org/badges/grand-total/FuzzyClass)](https://cran.r-project.org/package=FuzzyClass)
<!-- badges: end -->

Last update: 05-11-2022

## A family of probabilities-based classifiers Fuzzy and Non-Fuzzy

### Installation

``` r
# Installation
install.packages("devtools")
devtools::install_github("Jodavid/FuzzyClass")
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
#>          1 55  8  1
#>          2  5 42 12
#>          3  1  9 47
#> 
#> Overall Statistics
#>                                          
#>                Accuracy : 0.8            
#>                  95% CI : (0.734, 0.8558)
#>     No Information Rate : 0.3389         
#>     P-Value [Acc > NIR] : <2e-16         
#>                                          
#>                   Kappa : 0.6999         
#>                                          
#>  Mcnemar's Test P-Value : 0.772          
#> 
#> Statistics by Class:
#> 
#>                      Class: 1 Class: 2 Class: 3
#> Sensitivity            0.9016   0.7119   0.7833
#> Specificity            0.9244   0.8595   0.9167
#> Pos Pred Value         0.8594   0.7119   0.8246
#> Neg Pred Value         0.9483   0.8595   0.8943
#> Prevalence             0.3389   0.3278   0.3333
#> Detection Rate         0.3056   0.2333   0.2611
#> Detection Prevalence   0.3556   0.3278   0.3167
#> Balanced Accuracy      0.9130   0.7857   0.8500

saida <- predict(fit_FGNB, test, type = "matrix")
```

<!--

#### Fuzzy Gaussian Naive Bayes based in Zadeh


```r
fit_FGNB <- FuzzyGaussianNaiveBayes(train =  Train[,-4],
                                    cl = Train[,4], cores = 1,
                                    fuzzy = T)
print(fit_FGNB)
#> 
#> Fuzzy Gaussian Naive Bayes Classifier for Discrete Predictors Zadeh-based
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
#>          1 56  4  4
#>          2  2 52  5
#>          3  0  8 49
#> 
#> Overall Statistics
#>                                           
#>                Accuracy : 0.8722          
#>                  95% CI : (0.8145, 0.9172)
#>     No Information Rate : 0.3556          
#>     P-Value [Acc > NIR] : <2e-16          
#>                                           
#>                   Kappa : 0.8084          
#>                                           
#>  Mcnemar's Test P-Value : 0.1473          
#> 
#> Statistics by Class:
#> 
#>                      Class: 1 Class: 2 Class: 3
#> Sensitivity            0.9655   0.8125   0.8448
#> Specificity            0.9344   0.9397   0.9344
#> Pos Pred Value         0.8750   0.8814   0.8596
#> Neg Pred Value         0.9828   0.9008   0.9268
#> Prevalence             0.3222   0.3556   0.3222
#> Detection Rate         0.3111   0.2889   0.2722
#> Detection Prevalence   0.3556   0.3278   0.3167
#> Balanced Accuracy      0.9500   0.8761   0.8896

# -----
fit_GNB <- FuzzyGaussianNaiveBayes(train =  Train[,-4],
                               cl = Train[,4], cores = 2,
                               fuzzy = F)
print(fit_GNB)
#> 
#> Gaussian Naive Bayes Classifier for Discrete Predictors
#> 
#> Variables:
#> [1] "V1" "V2" "V3"
#> Class:
#> [1] "1" "2" "3"

saida <- predict(fit_GNB, test)
confusionMatrix(factor(Test[,4]), saida)
#> Confusion Matrix and Statistics
#> 
#>           Reference
#> Prediction  1  2  3
#>          1 60  4  0
#>          2  3 51  5
#>          3  0  8 49
#> 
#> Overall Statistics
#>                                           
#>                Accuracy : 0.8889          
#>                  95% CI : (0.8336, 0.9308)
#>     No Information Rate : 0.35            
#>     P-Value [Acc > NIR] : < 2.2e-16       
#>                                           
#>                   Kappa : 0.8331          
#>                                           
#>  Mcnemar's Test P-Value : NA              
#> 
#> Statistics by Class:
#> 
#>                      Class: 1 Class: 2 Class: 3
#> Sensitivity            0.9524   0.8095   0.9074
#> Specificity            0.9658   0.9316   0.9365
#> Pos Pred Value         0.9375   0.8644   0.8596
#> Neg Pred Value         0.9741   0.9008   0.9593
#> Prevalence             0.3500   0.3500   0.3000
#> Detection Rate         0.3333   0.2833   0.2722
#> Detection Prevalence   0.3556   0.3278   0.3167
#> Balanced Accuracy      0.9591   0.8706   0.9220

saida <- predict(fit_GNB, test, type = "matrix")
```

#### Fuzzy Naive Bayes Triangular


```r
fit_FNBT <- FuzzyTriangularNaiveBayes(train =  Train[,-4],
                                  cl = Train[,4], cores = 2,
                                  fuzzy = T)

print(fit_FNBT)
#> 
#> Fuzzy Naive Bayes Triangular Classifier for Discrete Predictors
#> 
#> Class:
#> [1] "1" "2" "3"

saida <- predict(fit_FNBT, test)
confusionMatrix(factor(Test[,4]), saida)
#> Confusion Matrix and Statistics
#> 
#>           Reference
#> Prediction  1  2  3
#>          1 57  7  0
#>          2  2 55  2
#>          3  0 19 38
#> 
#> Overall Statistics
#>                                           
#>                Accuracy : 0.8333          
#>                  95% CI : (0.7707, 0.8846)
#>     No Information Rate : 0.45            
#>     P-Value [Acc > NIR] : < 2.2e-16       
#>                                           
#>                   Kappa : 0.7496          
#>                                           
#>  Mcnemar's Test P-Value : NA              
#> 
#> Statistics by Class:
#> 
#>                      Class: 1 Class: 2 Class: 3
#> Sensitivity            0.9661   0.6790   0.9500
#> Specificity            0.9421   0.9596   0.8643
#> Pos Pred Value         0.8906   0.9322   0.6667
#> Neg Pred Value         0.9828   0.7851   0.9837
#> Prevalence             0.3278   0.4500   0.2222
#> Detection Rate         0.3167   0.3056   0.2111
#> Detection Prevalence   0.3556   0.3278   0.3167
#> Balanced Accuracy      0.9541   0.8193   0.9071

saida <- predict(fit_FNBT, test, type = "matrix")

# ----------------

fit_NBT <- FuzzyTriangularNaiveBayes(train =  Train[,-4],
                                 cl = Train[,4], cores = 2,
                                 fuzzy = F)
print(fit_NBT)
#> 
#> Naive Bayes Triangular Classifier for Discrete Predictors
#> 
#> Class:
#> [1] "1" "2" "3"

saida <- predict(fit_NBT, test)
confusionMatrix(factor(Test[,4]), saida)
#> Confusion Matrix and Statistics
#> 
#>           Reference
#> Prediction  1  2  3
#>          1 58  6  0
#>          2  2 52  5
#>          3  0 11 46
#> 
#> Overall Statistics
#>                                           
#>                Accuracy : 0.8667          
#>                  95% CI : (0.8081, 0.9127)
#>     No Information Rate : 0.3833          
#>     P-Value [Acc > NIR] : < 2.2e-16       
#>                                           
#>                   Kappa : 0.7998          
#>                                           
#>  Mcnemar's Test P-Value : NA              
#> 
#> Statistics by Class:
#> 
#>                      Class: 1 Class: 2 Class: 3
#> Sensitivity            0.9667   0.7536   0.9020
#> Specificity            0.9500   0.9369   0.9147
#> Pos Pred Value         0.9062   0.8814   0.8070
#> Neg Pred Value         0.9828   0.8595   0.9593
#> Prevalence             0.3333   0.3833   0.2833
#> Detection Rate         0.3222   0.2889   0.2556
#> Detection Prevalence   0.3556   0.3278   0.3167
#> Balanced Accuracy      0.9583   0.8453   0.9083

saida <- predict(fit_NBT, test, type = "matrix")
```

#### Fuzzy Exponential Naive Bayes Classifier


```r

fit_FENB <- ExpNBFuzzyParam(train =  Train[,-4],
                                    cl = Train[,4], metd = 1, cores = 2)
                              
print(fit_FENB)
#> 
#> Fuzzy Exponential Naive Bayes Classifier for Discrete Predictors
#> 
#> Variables:
#> [1] "V1" "V2" "V3"
#> Class:
#> [1] "1" "2" "3"

saida <- predict(fit_FENB, test)
confusionMatrix(factor(Test[,4]), saida)
#> Confusion Matrix and Statistics
#> 
#>           Reference
#> Prediction  1  2  3
#>          1 52 12  0
#>          2  3 48  8
#>          3  0  8 49
#> 
#> Overall Statistics
#>                                           
#>                Accuracy : 0.8278          
#>                  95% CI : (0.7645, 0.8799)
#>     No Information Rate : 0.3778          
#>     P-Value [Acc > NIR] : < 2.2e-16       
#>                                           
#>                   Kappa : 0.7419          
#>                                           
#>  Mcnemar's Test P-Value : NA              
#> 
#> Statistics by Class:
#> 
#>                      Class: 1 Class: 2 Class: 3
#> Sensitivity            0.9455   0.7059   0.8596
#> Specificity            0.9040   0.9018   0.9350
#> Pos Pred Value         0.8125   0.8136   0.8596
#> Neg Pred Value         0.9741   0.8347   0.9350
#> Prevalence             0.3056   0.3778   0.3167
#> Detection Rate         0.2889   0.2667   0.2722
#> Detection Prevalence   0.3556   0.3278   0.3167
#> Balanced Accuracy      0.9247   0.8038   0.8973

saida <- predict(fit_FENB, test, type = "matrix")
head(saida)
#>              1         2         3
#> [1,] 0.2960105 0.3362679 0.3677216
#> [2,] 0.3200898 0.3304015 0.3495087
#> [3,] 0.2979288 0.3357966 0.3662746
#> [4,] 0.3152053 0.3350115 0.3497833
#> [5,] 0.3322442 0.3222843 0.3454714
#> [6,] 0.3248209 0.3252478 0.3499313
```

#### Fuzzy Gamma Naive Bayes Classifier


```r

fit_NBT <- FuzzyGammaNaiveBayes(train =  Train[,-4],
                                    cl = Train[,4], cores = 2)
                              
print(fit_NBT)
#> 
#> Fuzzy Gamma Naive Bayes Classifier for Discrete Predictors
#> 
#> Class:
#> [1] "1" "2" "3"

saida <- predict(fit_NBT, test)
saida <- factor(saida,levels = unique(Test[,4]))
confusionMatrix(factor(Test[,4]), saida)
#> Confusion Matrix and Statistics
#> 
#>           Reference
#> Prediction  1  2  3
#>          1 59  5  0
#>          2  3 45 11
#>          3  0  7 50
#> 
#> Overall Statistics
#>                                           
#>                Accuracy : 0.8556          
#>                  95% CI : (0.7956, 0.9034)
#>     No Information Rate : 0.3444          
#>     P-Value [Acc > NIR] : < 2.2e-16       
#>                                           
#>                   Kappa : 0.7833          
#>                                           
#>  Mcnemar's Test P-Value : NA              
#> 
#> Statistics by Class:
#> 
#>                      Class: 1 Class: 2 Class: 3
#> Sensitivity            0.9516   0.7895   0.8197
#> Specificity            0.9576   0.8862   0.9412
#> Pos Pred Value         0.9219   0.7627   0.8772
#> Neg Pred Value         0.9741   0.9008   0.9106
#> Prevalence             0.3444   0.3167   0.3389
#> Detection Rate         0.3278   0.2500   0.2778
#> Detection Prevalence   0.3556   0.3278   0.3167
#> Balanced Accuracy      0.9546   0.8378   0.8804

saida <- predict(fit_NBT, test, type = "matrix")
head(saida)
#>              1            2            3
#> [1,] 0.9999997 2.694066e-07 8.586751e-24
#> [2,] 0.9880355 1.196449e-02 2.157128e-16
#> [3,] 0.9999996 4.436154e-07 1.962562e-24
#> [4,] 0.9993203 6.796611e-04 5.151856e-19
#> [5,] 0.5603109 4.396891e-01 8.807597e-10
#> [6,] 0.8913566 1.086434e-01 4.994418e-12
```

#### Fuzzy Exponential Naive Bayes Classifier


```r

fit_NBE <- FuzzyExponentialNaiveBayes(train =  Train[,-4],
                                    cl = Train[,4], cores = 2)
                              
print(fit_NBE)
#> 
#> Fuzzy Exponential Naive Bayes Classifier for Discrete Predictors
#> 
#> Class:
#> [1] "1" "2" "3"

saida <- predict(fit_NBE, test)
saida <- factor(saida,levels = unique(Test[,4]))
confusionMatrix(factor(Test[,4]), saida)
#> Confusion Matrix and Statistics
#> 
#>           Reference
#> Prediction  1  2  3
#>          1  6 55  3
#>          2  0 48 11
#>          3  0 56  1
#> 
#> Overall Statistics
#>                                           
#>                Accuracy : 0.3056          
#>                  95% CI : (0.2392, 0.3784)
#>     No Information Rate : 0.8833          
#>     P-Value [Acc > NIR] : 1               
#>                                           
#>                   Kappa : -0.0331         
#>                                           
#>  Mcnemar's Test P-Value : <2e-16          
#> 
#> Statistics by Class:
#> 
#>                      Class: 1 Class: 2 Class: 3
#> Sensitivity           1.00000  0.30189 0.066667
#> Specificity           0.66667  0.47619 0.660606
#> Pos Pred Value        0.09375  0.81356 0.017544
#> Neg Pred Value        1.00000  0.08264 0.886179
#> Prevalence            0.03333  0.88333 0.083333
#> Detection Rate        0.03333  0.26667 0.005556
#> Detection Prevalence  0.35556  0.32778 0.316667
#> Balanced Accuracy     0.83333  0.38904 0.363636

saida <- predict(fit_NBT, test, type = "matrix")
head(saida)
#>              1            2            3
#> [1,] 0.9999997 2.694066e-07 8.586751e-24
#> [2,] 0.9880355 1.196449e-02 2.157128e-16
#> [3,] 0.9999996 4.436154e-07 1.962562e-24
#> [4,] 0.9993203 6.796611e-04 5.151856e-19
#> [5,] 0.5603109 4.396891e-01 8.807597e-10
#> [6,] 0.8913566 1.086434e-01 4.994418e-12
```
-->
