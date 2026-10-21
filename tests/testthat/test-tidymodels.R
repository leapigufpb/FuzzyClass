skip_if_not_installed("parsnip")

test_that("fuzzy_naive_bayes() is registered in parsnip with all engines", {
  spec <- fuzzy_naive_bayes()
  expect_s3_class(spec, c("fuzzy_naive_bayes", "model_spec"))
  expect_equal(spec$engine, "gaussian")
  engines <- parsnip::show_engines("fuzzy_naive_bayes")
  expect_setequal(engines$engine, names(fuzzy_engines()))
  expect_true(all(engines$mode == "classification"))
  expect_output(print(spec), "Fuzzy Naive Bayes Model Specification")
})

test_that("parsnip fit and predict return tidy predictions", {
  fit <- fuzzy_naive_bayes(fuzzy = TRUE) |>
    parsnip::set_engine("gaussian") |>
    parsnip::fit(Species ~ ., data = iris)
  expect_s3_class(fit$fit, "FuzzyGaussianNaiveBayes")

  pred <- predict(fit, iris)
  expect_named(pred, ".pred_class")
  expect_equal(pred$.pred_class, predict(FuzzyGaussianNaiveBayes(iris[, -5], iris$Species), iris[, -5]))

  prob <- predict(fit, iris, type = "prob")
  expect_named(prob, paste0(".pred_", levels(iris$Species)))
  expect_equal(unname(rowSums(prob)), rep(1, 150))
})

test_that("main and engine arguments reach the classifier", {
  fit <- fuzzy_naive_bayes(alpha_cut = 0.01, ranking_method = 5) |>
    parsnip::set_engine("gaussian_fuzzy_param") |>
    parsnip::fit(Species ~ ., data = iris)
  expect_equal(fit$fit$params$alphacut, 0.01)
  expect_equal(fit$fit$params$metd, 5)

  fit <- fuzzy_naive_bayes(fuzzy = FALSE) |>
    parsnip::set_engine("dw_gamma", wdelta = rep(1/3, 3), weta = rep(1/4, 4)) |>
    parsnip::fit(Species ~ ., data = iris)
  expect_false(fit$fit$params$fuzzy)
  expect_equal(fit$fit$params$weta, rep(1/4, 4))

  spec <- update(fuzzy_naive_bayes(fuzzy = TRUE), fuzzy = FALSE)
  expect_false(rlang::eval_tidy(spec$args$fuzzy))
})

test_that("categorical predictors are passed as they are", {
  data(HouseVotes84)
  votes <- na.omit(HouseVotes84)
  fit <- fuzzy_naive_bayes() |>
    parsnip::set_engine("naive") |>
    parsnip::fit(Class ~ ., data = votes)
  expect_s3_class(predict(fit, votes)$.pred_class, "factor")
})

test_that("works with workflows, recipes and tune", {
  skip_if_not_installed("workflows")
  skip_if_not_installed("recipes")
  skip_if_not_installed("rsample")
  skip_if_not_installed("tune")
  skip_if_not_installed("yardstick")
  skip_if_not_installed("dials")

  set.seed(1)
  folds <- rsample::vfold_cv(iris, v = 3, strata = Species)
  rec <- recipes::recipe(Species ~ ., data = iris)
  wf <- workflows::workflow() |>
    workflows::add_recipe(rec) |>
    workflows::add_model(
      fuzzy_naive_bayes(alpha_cut = tune::tune(), ranking_method = tune::tune()) |>
        parsnip::set_engine("gaussian_fuzzy_param")
    )

  params <- hardhat::extract_parameter_set_dials(wf)
  expect_setequal(params$id, c("alpha_cut", "ranking_method"))

  res <- tune::tune_grid(wf, resamples = folds, grid = 3,
                         metrics = yardstick::metric_set(yardstick::accuracy))
  best <- tune::select_best(res, metric = "accuracy")
  final <- tune::finalize_workflow(wf, best) |> parsnip::fit(iris)
  acc <- mean(predict(final, iris)$.pred_class == iris$Species)
  expect_gt(acc, 0.8)

  wf_fuzzy <- workflows::update_model(wf, fuzzy_naive_bayes(fuzzy = tune::tune()))
  res <- tune::tune_grid(wf_fuzzy, resamples = folds, grid = 2)
  expect_equal(nrow(tune::collect_metrics(res)), 2 * 3)
})

test_that("tuning parameter objects", {
  skip_if_not_installed("dials")
  expect_s3_class(fuzzy_membership(), "qual_param")
  expect_s3_class(alpha_cut(), "quant_param")
  expect_equal(dials::range_get(ranking_method()), list(lower = 1L, upper = 5L))
})
