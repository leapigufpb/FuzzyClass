library(testthat)
library(FuzzyClass)

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

