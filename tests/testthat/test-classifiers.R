
data <- iris

# Test for ExpNBFuzzyParam
test_that("Test for ExpNBFuzzyParam", {
  fit <- ExpNBFuzzyParam(train = iris[,-5], cl = iris[,5])
  pred <- predict(fit,iris[,-5])
  expect_equal(TRUE, is.factor(pred))
})

# Test for FuzzyBetaNaiveBayes
test_that("Test for FuzzyBetaNaiveBayes", {
  fit <- FuzzyBetaNaiveBayes(train = iris[,-5], cl = iris[,5])
  pred <- predict(fit,iris[,-5])
  expect_equal(TRUE, is.factor(pred))
})

# Test for FuzzyBinomialNaiveBayes
test_that("Test for FuzzyBinomialNaiveBayes", {
  fit <- FuzzyBinomialNaiveBayes(train = round(iris[,-5]), cl = iris[,5])
  pred <- predict(fit,round(iris[,-5]))
  expect_equal(TRUE, is.factor(pred))
})

test_that("Test for FuzzyBinomialNaiveBayes - one column", {
  fit <- FuzzyBinomialNaiveBayes(train = round(iris[,1]), cl = iris[,5])
  pred <- predict(fit,round(iris[,1]))
  expect_equal(TRUE, is.factor(pred))
})

# Test for FuzzyExponentialNaiveBayes
test_that("Test for FuzzyExponentialNaiveBayes", {
  fit <- FuzzyExponentialNaiveBayes(train = iris[,-5], cl = iris[,5])
  pred <- predict(fit,iris[,-5])
  expect_equal(TRUE, is.factor(pred))
})

# Test for FuzzyGammaNaiveBayes
test_that("Test for FuzzyGammaNaiveBayes", {
  fit <- FuzzyGammaNaiveBayes(train = iris[,-5], cl = iris[,5])
  pred <- predict(fit,iris[,-5])
  expect_equal(TRUE, is.factor(pred))
})

# Test for FuzzyGaussianNaiveBayes
test_that("Test for FuzzyGaussianNaiveBayes", {
  fit <- FuzzyGaussianNaiveBayes(train = iris[,-5], cl = iris[,5])
  pred <- predict(fit,iris[,-5])
  expect_equal(TRUE, is.factor(pred))
})

# Test for FuzzyNaiveBayes
test_that("Test for FuzzyNaiveBayes", {
  fit <- FuzzyNaiveBayes(train = iris[,-5], cl = iris[,5])
  pred <- predict(fit,iris[,-5])
  expect_equal(TRUE, is.factor(pred))
})

# Test for FuzzyPoissonNaiveBayes
test_that("Test for FuzzyPoissonNaiveBayes", {
  fit <- FuzzyPoissonNaiveBayes(train = round(iris[,-5]), cl = iris[,5])
  pred <- predict(fit,round(iris[,-5]))
  expect_equal(TRUE, is.factor(pred))
})

test_that("Test for FuzzyPoissonNaiveBayes - one column", {
  fit <- FuzzyPoissonNaiveBayes(train = round(iris[,1]), cl = iris[,5])
  pred <- predict(fit,round(iris[,1]))
  expect_equal(TRUE, is.factor(pred))
})

# Test for FuzzyTrapezoidalNaiveBayes
test_that("Test for FuzzyTrapezoidalNaiveBayes", {
  fit <- FuzzyTrapezoidalNaiveBayes(train = iris[,-5], cl = iris[,5])
  pred <- predict(fit,round(iris[,-5]))
  expect_equal(TRUE, is.factor(pred))
})

# Test for FuzzyTriangularNaiveBayes
test_that("Test for FuzzyTriangularNaiveBayes", {
  fit <- FuzzyTriangularNaiveBayes(train = iris[,-5], cl = iris[,5])
  pred <- predict(fit,round(iris[,-5]))
  expect_equal(TRUE, is.factor(pred))
})

# Test for GauNBFuzzyParam
test_that("Test for GauNBFuzzyParam", {
  fit <- GauNBFuzzyParam(train = iris[,-5], cl = iris[,5])
  pred <- predict(fit,round(iris[,-5]))
  expect_equal(TRUE, is.factor(pred))
})

# Test for PoiNBFuzzyParam
test_that("Test for PoiNBFuzzyParam", {
  fit <- PoiNBFuzzyParam(train = round(iris[,-5]), cl = iris[,5], metd = 1)
  pred <- predict(fit, round(iris[,-5]))
  expect_equal(TRUE, is.factor(pred))
})

# Test for DWFuzzyGammaNaiveBayes
test_that("Test for DWFuzzyGammaNaiveBayes", {
  data(GamWeightData)
  train_x <- GamWeightData[1:60, -4]
  train_y <- GamWeightData[1:60, 4]
  fit <- DWFuzzyGammaNaiveBayes(train = train_x, cl = train_y,
                               wdelta = c(1/3, 1/3, 1/3), weta = c(1/3, 1/3, 1/3))
  pred <- predict(fit, train_x)
  expect_equal(TRUE, is.factor(pred))
})

# Test for FuzzyBayesRule
test_that("Test for FuzzyBayesRule", {
  fit <- FuzzyBayesRule(train = iris[,-5], cl = iris[,5])
  pred <- predict(fit, iris[,-5])
  expect_equal(TRUE, is.factor(pred))
})

# Test for FuzzyGeoNaiveBayes
test_that("Test for FuzzyGeoNaiveBayes", {
  fit <- FuzzyGeoNaiveBayes(train = round(iris[,-5]) + 1, cl = iris[,5])
  pred <- predict(fit, round(iris[,-5]) + 1)
  expect_equal(TRUE, is.factor(pred))
})

# Test for FuzzyHipergeometricNaiveBayes
test_that("Test for FuzzyHipergeometricNaiveBayes", {
  hyp_data <- data.frame(s1 = c(1, 2, 1, 2), n1 = c(5, 5, 6, 6))
  cl_hyp <- factor(c("A", "A", "B", "B"))
  fit <- FuzzyHipergeometricNaiveBayes(train = hyp_data, cl = cl_hyp)
  pred <- predict(fit, hyp_data)
  expect_equal(TRUE, is.factor(pred))
})

# Test for DWFuzzyHipergeometricNaiveBayes
test_that("Test for DWFuzzyHipergeometricNaiveBayes", {
  hyp_data <- data.frame(s1 = c(1, 2, 1, 2), n1 = c(5, 5, 6, 6))
  cl_hyp <- factor(c("A", "A", "B", "B"))
  fit <- DWFuzzyHipergeometricNaiveBayes(train = hyp_data, cl = cl_hyp,
                                        wdelta = c(0.5, 0.5), weta = c(1))
  pred <- predict(fit, hyp_data)
  expect_equal(TRUE, is.factor(pred))
})



# Regression test: class labels must stay aligned with the class parameters
# when the training data are not sorted by class.
test_that("Class labels are aligned with unsorted training data", {
  set.seed(1)
  idx <- sample(150, 100)
  X <- iris[idx, -5]; y <- iris[idx, 5]
  Xt <- iris[-idx, -5]; yt <- iris[-idx, 5]
  Xd <- round(iris[, -5])
  acc <- function(pred, truth) mean(as.character(pred) == as.character(truth))
  expect_gt(acc(predict(FuzzyGammaNaiveBayes(X, y), Xt), yt), 0.8)
  expect_gt(acc(predict(FuzzyGaussianNaiveBayes(X, y), Xt), yt), 0.8)
  expect_gt(acc(predict(FuzzyExponentialNaiveBayes(X, y, fuzzy = FALSE), Xt), yt), 0.8)
  expect_gt(acc(predict(FuzzyPoissonNaiveBayes(Xd[idx, ], y), Xd[-idx, ]), yt), 0.8)
  expect_gt(acc(predict(GauNBFuzzyParam(X, y), Xt), yt), 0.8)
  expect_gt(acc(predict(ExpNBFuzzyParam(X, y, metd = 4), Xt), yt), 0.8)
  probs <- predict(FuzzyGammaNaiveBayes(X, y), Xt, type = "matrix")
  expect_equal(colnames(probs)[max.col(probs)],
               as.character(predict(FuzzyGammaNaiveBayes(X, y), Xt)))
})
