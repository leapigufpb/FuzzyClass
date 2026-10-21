#' Fuzzy naive Bayes models for tidymodels
#'
#' @description
#' `fuzzy_naive_bayes()` defines a classification model, estimated by one of the
#' FuzzyClass classifiers, that can be used with the tidymodels ecosystem
#' (`parsnip`, `workflows`, `recipes`, `rsample`, `tune`, `yardstick`, ...).
#'
#' The classifier is chosen through the engine:
#'
#' | Engine                      | Classifier                              | Main arguments                |
#' |-----------------------------|-----------------------------------------|-------------------------------|
#' | `"gaussian"` (default)      | [FuzzyGaussianNaiveBayes()]             | `fuzzy`                       |
#' | `"gamma"`                   | [FuzzyGammaNaiveBayes()]                | `fuzzy`                       |
#' | `"beta"`                    | [FuzzyBetaNaiveBayes()]                 | `fuzzy`                       |
#' | `"exponential"`             | [FuzzyExponentialNaiveBayes()]          | `fuzzy`                       |
#' | `"binomial"`                | [FuzzyBinomialNaiveBayes()]             | `fuzzy`                       |
#' | `"poisson"`                 | [FuzzyPoissonNaiveBayes()]              | `fuzzy`                       |
#' | `"geometric"`               | [FuzzyGeoNaiveBayes()]                  | `fuzzy`                       |
#' | `"trapezoidal"`             | [FuzzyTrapezoidalNaiveBayes()]          | `fuzzy`                       |
#' | `"triangular"`              | [FuzzyTriangularNaiveBayes()]           | `fuzzy`                       |
#' | `"hypergeometric"`          | [FuzzyHipergeometricNaiveBayes()]       | `fuzzy`                       |
#' | `"bayes_rule"`              | [FuzzyBayesRule()]                      | `fuzzy`                       |
#' | `"naive"`                   | [FuzzyNaiveBayes()]                     | `fuzzy`                       |
#' | `"dw_gamma"`                | [DWFuzzyGammaNaiveBayes()]              | `fuzzy`                       |
#' | `"dw_hypergeometric"`       | [DWFuzzyHipergeometricNaiveBayes()]     | `fuzzy`                       |
#' | `"gaussian_fuzzy_param"`    | [GauNBFuzzyParam()]                     | `alpha_cut`, `ranking_method` |
#' | `"exponential_fuzzy_param"` | [ExpNBFuzzyParam()]                     | `alpha_cut`, `ranking_method` |
#' | `"poisson_fuzzy_param"`     | [PoiNBFuzzyParam()]                     | `alpha_cut`, `ranking_method` |
#'
#' Main arguments that do not apply to the chosen engine are ignored. Any other
#' argument of the classifier (e.g. `wdelta` and `weta` for the double weighted
#' classifiers, `alp` and `w` for `ranking_method = 4`, or `verbose`) can be
#' passed through [parsnip::set_engine()].
#'
#' The predictors are passed to the classifier as they are (no dummy
#' variables are created), so use a recipe to prepare them when needed. Each
#' fit prints a training message; set `options(FuzzyClass.verbose = FALSE)`
#' (see [fuzzy_verbose()]) or `set_engine(..., verbose = FALSE)` to silence it,
#' for example when tuning.
#'
#' This function requires the `parsnip` package.
#'
#' @param mode A single character string for the type of model. The only
#'   possible value is `"classification"`.
#' @param engine A single character string specifying the classifier (see the
#'   table above).
#' @param fuzzy Logical; whether the membership functions are used (argument
#'   `fuzzy` of the classifiers). Tunable with [fuzzy_membership()].
#' @param alpha_cut Smallest alpha-cut of the fuzzy parameters (argument
#'   `alphacut`). Tunable with [alpha_cut()].
#' @param ranking_method Integer from 1 to 5: method used to rank the fuzzy
#'   numbers (argument `metd`). Tunable with [ranking_method()].
#'
#' @return A model specification object with classes `fuzzy_naive_bayes` and
#'   `model_spec`.
#'
#' @examplesIf requireNamespace("parsnip", quietly = TRUE)
#' library(parsnip)
#'
#' spec <- fuzzy_naive_bayes(fuzzy = TRUE) |>
#'   set_engine("gaussian")
#' spec
#'
#' model_fit <- fit(spec, Species ~ ., data = iris)
#' predict(model_fit, iris[c(1, 51, 101), ])
#' predict(model_fit, iris[c(1, 51, 101), ], type = "prob")
#'
#' # Fuzzy parameters with the GTFN ranking method
#' fuzzy_naive_bayes(ranking_method = 5) |>
#'   set_engine("gaussian_fuzzy_param") |>
#'   fit(Species ~ ., data = iris)
#'
#' # Engine-specific arguments
#' data(GamWeightData)
#' fuzzy_naive_bayes() |>
#'   set_engine("dw_gamma", wdelta = rep(1/3, 3), weta = c(0.3, 0.2, 0.5)) |>
#'   fit(class ~ ., data = GamWeightData)
#' @export
fuzzy_naive_bayes <- function(mode = "classification",
                              engine = "gaussian",
                              fuzzy = NULL,
                              alpha_cut = NULL,
                              ranking_method = NULL) {
  check_parsnip()
  args <- list(
    fuzzy = rlang::enquo(fuzzy),
    alpha_cut = rlang::enquo(alpha_cut),
    ranking_method = rlang::enquo(ranking_method)
  )
  parsnip::new_model_spec(
    "fuzzy_naive_bayes",
    args = args,
    eng_args = NULL,
    mode = mode,
    user_specified_mode = !missing(mode),
    method = NULL,
    engine = engine,
    user_specified_engine = !missing(engine)
  )
}

#' @export
print.fuzzy_naive_bayes <- function(x, ...) {
  parsnip::print_model_spec(x, desc = "Fuzzy Naive Bayes", ...)
  invisible(x)
}

#' @param object A `fuzzy_naive_bayes` model specification.
#' @param parameters A 1-row data frame (or `NULL`) with the new values of the
#'   main arguments, as used by `tune::finalize_model()`.
#' @param fresh Logical; replace all the arguments (`TRUE`) or only update
#'   the ones given (`FALSE`)?
#' @param ... Engine-specific arguments to update.
#' @rdname fuzzy_naive_bayes
#' @importFrom stats update
#' @export
update.fuzzy_naive_bayes <- function(object,
                                     parameters = NULL,
                                     fuzzy = NULL,
                                     alpha_cut = NULL,
                                     ranking_method = NULL,
                                     fresh = FALSE,
                                     ...) {
  check_parsnip()
  args <- list(
    fuzzy = rlang::enquo(fuzzy),
    alpha_cut = rlang::enquo(alpha_cut),
    ranking_method = rlang::enquo(ranking_method)
  )
  parsnip::update_spec(
    object = object,
    parameters = parameters,
    args_enquo_list = args,
    fresh = fresh,
    cls = "fuzzy_naive_bayes",
    ...
  )
}

#' Tuning parameters of the fuzzy naive Bayes models
#'
#' Parameter objects, for the `dials` package, of the main arguments of
#' [fuzzy_naive_bayes()]. They are used automatically by `tune` when an
#' argument is marked with `tune()`.
#'
#' * `fuzzy_membership()`: whether the membership functions are used
#'   (argument `fuzzy`).
#' * `alpha_cut()`: smallest alpha-cut of the fuzzy parameters (argument
#'   `alphacut`).
#' * `ranking_method()`: method used to rank the fuzzy numbers (argument
#'   `metd`): 1 = Yager, 2 = Q distance, 3 = Thorani, 4 = alpha-order,
#'   5 = GTFN.
#'
#' These functions require the `dials` package.
#'
#' @param values A logical vector of possible values.
#' @param range A two-element vector with the bounds of the parameter.
#' @param trans A `trans` object from the `scales` package, or `NULL`.
#'
#' @return A `param` object of the `dials` package.
#' @examplesIf requireNamespace("dials", quietly = TRUE)
#' fuzzy_membership()
#' alpha_cut()
#' ranking_method()
#' @export
fuzzy_membership <- function(values = c(TRUE, FALSE)) {
  check_installed_pkg("dials")
  dials::new_qual_param(
    type = "logical",
    values = values,
    label = c(fuzzy_membership = "Fuzzy Membership Function"),
    finalize = NULL
  )
}

#' @rdname fuzzy_membership
#' @export
alpha_cut <- function(range = c(1e-4, 0.1), trans = NULL) {
  check_installed_pkg("dials")
  dials::new_quant_param(
    type = "double",
    range = range,
    inclusive = c(TRUE, TRUE),
    trans = trans,
    label = c(alpha_cut = "Smallest Alpha-Cut"),
    finalize = NULL
  )
}

#' @rdname fuzzy_membership
#' @export
ranking_method <- function(range = c(1L, 5L), trans = NULL) {
  check_installed_pkg("dials")
  dials::new_quant_param(
    type = "integer",
    range = range,
    inclusive = c(TRUE, TRUE),
    trans = trans,
    label = c(ranking_method = "Fuzzy Number Ranking Method"),
    finalize = NULL
  )
}

# --------------------------------------------------------------------------
# Registration of the model in parsnip
# --------------------------------------------------------------------------

# Engines: classifier (functional interface) and main arguments it supports.
#' @noRd
fuzzy_engines <- function() {
  with_fuzzy <- c(fuzzy = "fuzzy")
  fuzzy_param <- c(alpha_cut = "alphacut", ranking_method = "metd")
  list(
    gaussian = list(fun = "FuzzyGaussianNaiveBayes", args = with_fuzzy),
    gamma = list(fun = "FuzzyGammaNaiveBayes", args = with_fuzzy),
    beta = list(fun = "FuzzyBetaNaiveBayes", args = with_fuzzy),
    exponential = list(fun = "FuzzyExponentialNaiveBayes", args = with_fuzzy),
    binomial = list(fun = "FuzzyBinomialNaiveBayes", args = with_fuzzy),
    poisson = list(fun = "FuzzyPoissonNaiveBayes", args = with_fuzzy),
    geometric = list(fun = "FuzzyGeoNaiveBayes", args = with_fuzzy),
    trapezoidal = list(fun = "FuzzyTrapezoidalNaiveBayes", args = with_fuzzy),
    triangular = list(fun = "FuzzyTriangularNaiveBayes", args = with_fuzzy),
    hypergeometric = list(fun = "FuzzyHipergeometricNaiveBayes", args = with_fuzzy),
    bayes_rule = list(fun = "FuzzyBayesRule", args = with_fuzzy),
    naive = list(fun = "FuzzyNaiveBayes", args = with_fuzzy),
    dw_gamma = list(fun = "DWFuzzyGammaNaiveBayes", args = with_fuzzy),
    dw_hypergeometric = list(fun = "DWFuzzyHipergeometricNaiveBayes", args = with_fuzzy),
    gaussian_fuzzy_param = list(fun = "GauNBFuzzyParam", args = fuzzy_param),
    exponential_fuzzy_param = list(fun = "ExpNBFuzzyParam", args = fuzzy_param),
    poisson_fuzzy_param = list(fun = "PoiNBFuzzyParam", args = fuzzy_param)
  )
}

#' @noRd
fuzzy_tuning_params <- c(
  fuzzy = "fuzzy_membership",
  alpha_cut = "alpha_cut",
  ranking_method = "ranking_method"
)

# Registers `fuzzy_naive_bayes` and its engines in the parsnip model
# environment. Called when FuzzyClass is loaded (if parsnip is already loaded)
# and when parsnip is loaded afterwards; it does nothing if already registered.
#' @noRd
register_fuzzy_naive_bayes <- function() {
  if (!requireNamespace("parsnip", quietly = TRUE)) {
    return(invisible(FALSE))
  }
  model <- "fuzzy_naive_bayes"
  if (model %in% parsnip::get_model_env()$models) {
    return(invisible(TRUE))
  }
  mode <- "classification"
  parsnip::set_new_model(model)
  parsnip::set_model_mode(model, mode)

  engines <- fuzzy_engines()
  for (eng in names(engines)) {
    parsnip::set_model_engine(model, mode = mode, eng = eng)
    parsnip::set_dependency(model, eng = eng, pkg = "FuzzyClass", mode = mode)

    main_args <- engines[[eng]]$args
    for (arg in names(main_args)) {
      parsnip::set_model_arg(
        model = model,
        eng = eng,
        parsnip = arg,
        original = main_args[[arg]],
        func = list(pkg = "FuzzyClass", fun = fuzzy_tuning_params[[arg]]),
        has_submodel = FALSE
      )
    }

    parsnip::set_fit(
      model = model,
      eng = eng,
      mode = mode,
      value = list(
        interface = "data.frame",
        data = c(x = "train", y = "cl"),
        protect = c("train", "cl"),
        func = c(pkg = "FuzzyClass", fun = engines[[eng]]$fun),
        defaults = list()
      )
    )

    parsnip::set_encoding(
      model = model,
      eng = eng,
      mode = mode,
      options = list(
        predictor_indicators = "none",
        compute_intercept = FALSE,
        remove_intercept = FALSE,
        allow_sparse_x = FALSE
      )
    )

    parsnip::set_pred(
      model = model,
      eng = eng,
      mode = mode,
      type = "class",
      value = list(
        pre = NULL,
        post = NULL,
        func = c(fun = "predict"),
        args = list(object = quote(object$fit), newdata = quote(new_data), type = "class")
      )
    )

    parsnip::set_pred(
      model = model,
      eng = eng,
      mode = mode,
      type = "prob",
      value = list(
        pre = NULL,
        post = function(x, object) as.data.frame(x),
        func = c(fun = "predict"),
        args = list(object = quote(object$fit), newdata = quote(new_data), type = "prob")
      )
    )
  }
  invisible(TRUE)
}

#' @noRd
check_installed_pkg <- function(pkg) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    cli::cli_abort(c(
      "The package {.pkg {pkg}} is required for this function.",
      i = "Install it with {.code install.packages(\"{pkg}\")}."
    ), call = parent.frame())
  }
}

#' @noRd
check_parsnip <- function() {
  check_installed_pkg("parsnip")
  register_fuzzy_naive_bayes()
}
