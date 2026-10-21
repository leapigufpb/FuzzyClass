test_that("R6 classifiers follow the new/fit/predict workflow", {
  model <- FuzzyGaussianNaiveBayesModel$new()
  expect_s3_class(model, c("FuzzyGaussianNaiveBayes", "FuzzyClassifier", "R6"))
  expect_false(model$is_fitted())

  expect_identical(model$fit(iris[, -5], iris$Species), model)
  expect_true(model$is_fitted())
  expect_equal(model$predictors, names(iris)[1:4])
  expect_equal(model$classes, levels(iris$Species))
  expect_equal(model$n_obs, 150)

  pred <- model$predict(iris[, -5])
  expect_s3_class(pred, "factor")
  expect_length(pred, 150)

  prob <- model$predict(iris[, -5], type = "prob")
  expect_equal(dim(prob), c(150, 3))
  expect_equal(colnames(prob), levels(iris$Species))
  expect_equal(unname(rowSums(prob)), rep(1, 150))
  expect_equal(model$predict(iris[, -5], type = "matrix"), prob)
})

test_that("the functional interface returns the R6 object and keeps predict() working", {
  fit <- FuzzyGammaNaiveBayes(iris[, -5], iris$Species)
  expect_s3_class(fit, "FuzzyClassifier")
  expect_equal(predict(fit, iris[, -5]), fit$predict(iris[, -5]))
  expect_equal(predict(fit, iris[, -5], type = "matrix"), fit$predict(iris[, -5], type = "prob"))

  r6 <- FuzzyGammaNaiveBayesModel$new()$fit(iris[, -5], iris$Species)
  expect_equal(predict(r6, iris[, -5]), predict(fit, iris[, -5]))
})

test_that("fit() reports the training progress in English", {
  model <- FuzzyGaussianNaiveBayesModel$new()
  msgs <- character()
  withCallingHandlers(
    model$fit(iris[, -5], iris$Species, verbose = TRUE),
    message = function(m) {
      msgs <<- c(msgs, conditionMessage(m))
      invokeRestart("muffleMessage")
    }
  )
  msgs <- paste(msgs, collapse = "")
  expect_match(msgs, "Fuzzy Gaussian Naive Bayes")
  expect_match(msgs, "150 observations, 4 predictors and 3 classes")
  expect_match(msgs, "Model trained successfully")

  expect_silent(model$fit(iris[, -5], iris$Species, verbose = FALSE))
})

test_that("hyperparameters can be changed with set_params()", {
  model <- FuzzyGaussianNaiveBayesModel$new()$fit(iris[, -5], iris$Species)
  expect_equal(model$label, "Fuzzy Gaussian Naive Bayes (Zadeh-based)")
  model$set_params(fuzzy = FALSE)
  expect_false(model$is_fitted())
  expect_equal(model$label, "Gaussian Naive Bayes")
  expect_equal(
    model$fit(iris[, -5], iris$Species)$predict(iris[, -5]),
    predict(FuzzyGaussianNaiveBayes(iris[, -5], iris$Species, fuzzy = FALSE), iris[, -5])
  )
  expect_error(model$set_params(unknown = 1), "hyperparameters")
})

test_that("informative errors", {
  expect_error(FuzzyGaussianNaiveBayesModel$new()$predict(iris[, -5]), "not been trained")
  expect_error(FuzzyGaussianNaiveBayesModel$new()$fit(iris[, -5], iris$Species[1:10]), "same number")
  expect_error(DWFuzzyGammaNaiveBayesModel$new(), "wdelta")
  expect_error(FuzzyClassifier$new()$fit(iris[, -5], iris$Species), "abstract")
})

test_that("print() describes the model", {
  model <- FuzzyGaussianNaiveBayesModel$new()
  expect_output(print(model), "Not trained")
  model$fit(iris[, -5], iris$Species)
  expect_output(print(model), "Trained on 150 observations")
  expect_output(print(model), "setosa, versicolor, virginica")
})

test_that("all classifiers have an R6 class", {
  Xd <- round(iris[, -5])
  hyp <- data.frame(s1 = c(1, 2, 1, 2), n1 = c(5, 5, 6, 6))
  clh <- factor(c("A", "A", "B", "B"))
  models <- list(
    list(FuzzyBetaNaiveBayesModel$new(), iris[, -5] / 10, iris$Species),
    list(FuzzyExponentialNaiveBayesModel$new(), iris[, -5], iris$Species),
    list(FuzzyBinomialNaiveBayesModel$new(), Xd, iris$Species),
    list(FuzzyPoissonNaiveBayesModel$new(), Xd, iris$Species),
    list(FuzzyGeoNaiveBayesModel$new(), Xd + 1, iris$Species),
    list(FuzzyTrapezoidalNaiveBayesModel$new(), iris[, -5], iris$Species),
    list(FuzzyTriangularNaiveBayesModel$new(), iris[, -5], iris$Species),
    list(FuzzyHipergeometricNaiveBayesModel$new(), hyp, clh),
    list(FuzzyBayesRuleModel$new(), iris[, -5], iris$Species),
    list(FuzzyNaiveBayesModel$new(), iris[, -5], iris$Species),
    list(FuzzyNaiveBayesModel$new(fuzzy = FALSE), iris[, -5], iris$Species),
    list(DWFuzzyGammaNaiveBayesModel$new(wdelta = rep(1/3, 3), weta = rep(1/3, 4)), iris[, -5], iris$Species),
    list(DWFuzzyHipergeometricNaiveBayesModel$new(wdelta = c(0.5, 0.5), weta = 1), hyp, clh),
    list(GauNBFuzzyParamModel$new(metd = 5), iris[, -5], iris$Species),
    list(ExpNBFuzzyParamModel$new(metd = 4), iris[, -5], iris$Species),
    list(PoiNBFuzzyParamModel$new(metd = 1), Xd, iris$Species)
  )
  for (m in models) {
    model <- m[[1]]$fit(m[[2]], m[[3]])
    pred <- model$predict(m[[2]])
    expect_s3_class(pred, "factor")
    expect_length(pred, nrow(m[[2]]))
    prob <- model$predict(m[[2]], type = "prob")
    expect_equal(nrow(prob), nrow(m[[2]]))
  }
})

test_that("R6 objects survive serialization", {
  model <- FuzzyGaussianNaiveBayesModel$new()$fit(iris[, -5], iris$Species)
  path <- tempfile(fileext = ".rds")
  saveRDS(model, path)
  restored <- readRDS(path)
  expect_equal(restored$predict(iris[, -5]), model$predict(iris[, -5]))
})
