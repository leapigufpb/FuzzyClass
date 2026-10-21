#' Base R6 class of the FuzzyClass classifiers
#'
#' @description
#' `FuzzyClassifier` is the (abstract) R6 class shared by every classifier of
#' the package. It implements the common workflow:
#'
#' * `$new(...)` creates an untrained model holding its hyperparameters;
#' * `$fit(train, cl)` estimates the model and reports the progress;
#' * `$predict(newdata, type)` classifies new observations;
#' * `$set_params(...)` changes hyperparameters (the model must be re-trained).
#'
#' Do not instantiate it directly: use one of the concrete classes, e.g.
#' [FuzzyGaussianNaiveBayesModel], [FuzzyGammaNaiveBayesModel] or
#' [GauNBFuzzyParamModel]. Each of them also has a functional interface (e.g.
#' [FuzzyGaussianNaiveBayes()]) that trains and returns the R6 object, and every
#' classifier is available to tidymodels through [fuzzy_naive_bayes()].
#'
#' Since `$fit()` and `$set_params()` return the object invisibly, calls can be
#' chained: `FuzzyGammaNaiveBayesModel$new()$fit(x, y)$predict(x_new)`.
#'
#' @examples
#' model <- FuzzyGaussianNaiveBayesModel$new(fuzzy = TRUE)
#' model$fit(train = iris[, -5], cl = iris$Species)
#' model
#'
#' head(model$predict(iris[, -5]))
#' head(model$predict(iris[, -5], type = "prob"))
#'
#' # S3 generics also work
#' head(predict(model, iris[, -5]))
#'
#' # Change a hyperparameter and re-train, silently
#' model$set_params(fuzzy = FALSE)$fit(iris[, -5], iris$Species, verbose = FALSE)
#' model$label
#' @export
FuzzyClassifier <- R6::R6Class(
  classname = "FuzzyClassifier",
  public = list(
    #' @field params Named list with the hyperparameters of the classifier.
    params = NULL,
    #' @field estimates List with the quantities estimated by `$fit()`
    #'   (`NULL` while the model has not been trained).
    estimates = NULL,
    #' @field predictors Names of the predictors seen during training.
    predictors = NULL,
    #' @field classes Classes (levels of the response) seen during training.
    classes = NULL,
    #' @field n_obs Number of training observations.
    n_obs = NULL,
    #' @field fit_time Training time, in seconds.
    fit_time = NULL,

    #' @description Create a new (untrained) classifier.
    #' @param ... Named hyperparameters of the classifier.
    initialize = function(...) {
      self$params <- list(...)
      invisible(self)
    },

    #' @description Estimate the classifier from training data.
    #' @param train Matrix or data frame of training set cases.
    #' @param cl Factor (or vector) of true classifications of the training set.
    #' @param verbose Logical; if `TRUE` (default, see [fuzzy_verbose()]) a
    #'   message reports the training progress.
    #' @return The trained object, invisibly.
    fit = function(train, cl, verbose = fuzzy_verbose()) {
      cl_vec <- c(unlist(cl))
      n <- NROW(train)
      if (length(cl_vec) != n) {
        cli::cli_abort(c(
          "{.arg train} and {.arg cl} must have the same number of observations.",
          x = "{.arg train} has {n} row{?s} but {.arg cl} has {length(cl_vec)} element{?s}."
        ))
      }
      if (anyNA(cl_vec)) {
        cli::cli_abort("{.arg cl} must not contain missing values.")
      }
      train_df <- as.data.frame(train)
      p <- ncol(train_df)
      k <- length(unique(cl_vec))
      label <- self$label

      if (isTRUE(verbose)) {
        cli::cli_alert_info("{.strong {label}}: {n} observation{?s}, {p} predictor{?s} and {k} class{?es}")
        cli::cli_progress_step(
          msg = "Training the model: estimating its parameters",
          msg_done = "Model trained successfully",
          msg_failed = "Could not train the {label} model"
        )
      }
      start <- proc.time()[["elapsed"]]
      estimates <- private$estimate(train, cl)

      self$estimates <- estimates
      self$predictors <- colnames(train_df)
      self$classes <- if (is.factor(cl_vec)) levels(droplevels(cl_vec)) else as.character(sort(unique(cl_vec)))
      self$n_obs <- n
      self$fit_time <- proc.time()[["elapsed"]] - start
      invisible(self)
    },

    #' @description Classify new observations.
    #' @param newdata Matrix or data frame of test set cases, with the
    #'   predictors in the same order used in training.
    #' @param type `"class"` (default) for a factor of predicted classes, or
    #'   `"prob"` for a matrix of class probabilities (one column per class).
    #'   `"matrix"` and `"raw"` are accepted as synonyms of `"prob"`.
    #' @return A factor (`type = "class"`) or a numeric matrix.
    predict = function(newdata, type = c("class", "prob", "matrix", "raw")) {
      if (!self$is_fitted()) {
        cli::cli_abort(c(
          "This {.cls {class(self)[1]}} model has not been trained yet.",
          i = "Call {.code $fit(train, cl)} before predicting."
        ))
      }
      type <- match.arg(type)
      private$classify(newdata, type = if (type == "class") "class" else "matrix")
    },

    #' @description Update hyperparameters. The model is reset and must be
    #'   trained again with `$fit()`.
    #' @param ... Named hyperparameters, as in `$new()`.
    #' @return The object, invisibly.
    set_params = function(...) {
      new <- list(...)
      unknown <- setdiff(names(new), names(self$params))
      if (is.null(names(new)) || any(names(new) == "") || length(unknown) > 0) {
        cli::cli_abort(c(
          "All hyperparameters must be named and belong to this classifier.",
          i = "Valid hyperparameters: {.arg {names(self$params)}}."
        ))
      }
      self$params[names(new)] <- new
      private$reset()
      invisible(self)
    },

    #' @description Has the model been trained?
    #' @return `TRUE` or `FALSE`.
    is_fitted = function() {
      !is.null(self$estimates)
    },

    #' @description Print a summary of the classifier.
    #' @param ... Ignored.
    print = function(...) {
      cat(cli::style_bold(paste0("<", class(self)[1], "> ", self$label)), "\n", sep = "")
      if (self$is_fitted()) {
        cat(cli::col_green(cli::symbol$tick), " Trained on ", self$n_obs, " observations in ",
            format(round(self$fit_time, 3), nsmall = 3), "s\n", sep = "")
      } else {
        cat(cli::col_yellow(cli::symbol$circle), " Not trained yet: call $fit(train, cl)\n", sep = "")
      }
      if (length(self$params) > 0) {
        values <- vapply(self$params, function(v) paste(deparse(v), collapse = ""), "")
        cat("  Hyperparameters: ", paste(names(values), values, sep = " = ", collapse = ", "), "\n", sep = "")
      }
      if (self$is_fitted()) {
        cat("  Predictors (", length(self$predictors), "): ", paste(self$predictors, collapse = ", "), "\n", sep = "")
        cat("  Classes (", length(self$classes), "): ", paste(self$classes, collapse = ", "), "\n", sep = "")
      }
      invisible(self)
    }
  ),
  active = list(
    #' @field label Name of the classifier (read-only); it depends on whether
    #'   the membership functions are used (`fuzzy`).
    label = function(value) {
      if (!missing(value)) cli::cli_abort("{.field label} is read-only.")
      if (isFALSE(self$params$fuzzy) && !is.null(private$label_crisp)) private$label_crisp else private$label_fuzzy
    }
  ),
  private = list(
    label_fuzzy = "Fuzzy classifier",
    label_crisp = NULL,
    estimate = function(train, cl) {
      cli::cli_abort("{.cls FuzzyClassifier} is abstract: use one of the classifiers of the package.")
    },
    classify = function(newdata, type) {
      cli::cli_abort("{.cls FuzzyClassifier} is abstract: use one of the classifiers of the package.")
    },
    reset = function() {
      self$estimates <- NULL
      self$predictors <- NULL
      self$classes <- NULL
      self$n_obs <- NULL
      self$fit_time <- NULL
    }
  )
)

#' Predict method for FuzzyClass classifiers
#'
#' S3 interface to the `$predict()` method of the [FuzzyClassifier] R6 objects,
#' so that `predict(model, newdata)` keeps working as in previous versions.
#'
#' @param object A trained [FuzzyClassifier] object.
#' @param newdata Matrix or data frame of test set cases.
#' @param type `"class"` (default) for predicted classes, or `"prob"` (synonyms
#'   `"matrix"` and `"raw"`) for the matrix of class probabilities.
#' @param ... Ignored.
#' @return A factor (`type = "class"`) or a numeric matrix.
#' @export
predict.FuzzyClassifier <- function(object, newdata, type = "class", ...) {
  object$predict(newdata, type = type)
}

#' Verbosity of the FuzzyClass classifiers
#'
#' By default, training a classifier prints a short progress message. Set
#' `options(FuzzyClass.verbose = FALSE)` to silence it globally (useful, for
#' example, when tuning many models with tidymodels), or use the argument
#' `verbose` of `$fit()` and of the functional interfaces.
#'
#' @return The value of the option `FuzzyClass.verbose` (default `TRUE`).
#' @examples
#' fuzzy_verbose()
#' old <- options(FuzzyClass.verbose = FALSE)
#' fit <- FuzzyGaussianNaiveBayes(iris[, -5], iris$Species) # silent
#' options(old)
#' @export
fuzzy_verbose <- function() {
  isTRUE(getOption("FuzzyClass.verbose", TRUE))
}

#' @noRd
check_required_params <- function(..., names) {
  miss <- names[c(...)]
  if (length(miss) > 0) {
    cli::cli_abort("Argument{?s} {.arg {miss}} {?is/are} required.", call = parent.frame())
  }
}
