#' Fuzzy Exponential Naive Bayes Classifier with Fuzzy parameters
#'
#' \code{ExpNBFuzzyParam} Fuzzy Exponential Naive Bayes Classifier with Fuzzy parameters
#'
#'
#' @param train matrix or data frame of training set cases
#' @param cl factor of true classifications of training set
#' @param alphacut value of the alpha-cut parameter, this value is between 0 and 1.
#' @param metd Method of transforming the triangle into scalar, It is the type of data entry for the test sample, use 'metd=1' if you want to use the Yager technique, 'metd=2' if you want to use the Q technique of the uniformity test (article: Directional Statistics and Shape analysis), 'metd=3' if you want to use the Thorani technique, 'metd=4' if you want to use the alpha-order technique and 'metd=5' if you want to use the GTFN technique (article: A novel approach for arithmetic operations and ranking of generalized fuzzy numbers with application).
#' @param alp When metd for 4, it is necessary to have alp which are alpha-cut defined
#' @param w When metd for 4, it is necessary to have w which are alpha-cut weights defined
#' @param cores kept for backward compatibility; prediction is vectorized and does not start parallel workers (default = 2)
#'
#' @return A vector of classifications
#'
#' @references
#' \insertRef{rodrigues2016new}{FuzzyClass}
#'
#'
#'
#' @examples
#'
#' set.seed(1) # determining a seed
#' data(VirtualRealityData)
#'
#' # Splitting into Training and Testing
#' split <- caTools::sample.split(t(VirtualRealityData[, 1]), SplitRatio = 0.7)
#' Train <- subset(VirtualRealityData, split == "TRUE")
#' Test <- subset(VirtualRealityData, split == "FALSE")
#'
#' # ----------------
#' # matrix or data frame of test set cases.
#' # A vector will be interpreted as a row vector for a single case.
#' test <- Test[, -4]
#' fit_FENB <- ExpNBFuzzyParam(
#'   train = Train[, -4],
#'   cl = Train[, 4], metd = 1, cores = 2
#' )
#'
#' pred_FENB <- predict(fit_FENB, test)
#'
#' head(pred_FENB)
#' head(Test[, 4])
#' @importFrom stats dexp
#' @importFrom Rdpack reprompt
#'
#' @export
ExpNBFuzzyParam <- function(train, cl,  alphacut = 0.0001, metd = 2, alp = c(0.35, 0.7, 0.86), w = c(0.1,0.3,0.6), cores = 2) {
  UseMethod("ExpNBFuzzyParam")
}

#' @export
ExpNBFuzzyParam.default <- function(train, cl,  alphacut = 0.0001, metd = 2, alp = c(0.35, 0.7, 0.86), w = c(0.1,0.3,0.6), cores = 2) {

  # --------------------------------------------------------
  # Estimating class parameters
  p_data <- predata(train,cl)
  # --
  train <-  p_data$train
  cols <- p_data$cols
  dados <- p_data$dados
  M <- p_data$M
  # --------------------------------------------------------
  # --------------------------------------------------------
  # Finding lambdas for each class
  lambdas <- lapply(1:length(get_classes(M)), function(i) colMeans(subset(dados, M == get_classes(M)[i])))
  # --------------------------------------------------------
  # --------------------------------------------------------
  # Estimating Triangular Parameters
  alpha = alp
  if (metd != 4) {
    alpha <- seq(alphacut, 1.1, 0.1)
    alpha <- ifelse(alpha > 1, 1, alpha)
  }
  # -------------------------------
  # Sample size of each class (the confidence intervals are built per class)
  n_class <- as.numeric(table(factor(M, levels = get_classes(M))))
  # -------------------------------
  #  Lambda Parameters
  # ------------------
  Parameters_lambda <- lapply(1:length(lambdas), function(i) { # Pra grupos
    N <- n_class[i]
    lapply(1:length(lambdas[[1]]), function(k) { # Para dimensao
      round(
        t(sapply(1:length(alpha), function(j) {
          c(
            (sqrt(N) * (lambdas[[i]][k])) / (sqrt(N) + qnorm(1 - alpha[j] / 2)),
            (sqrt(N) * (lambdas[[i]][k])) / max(sqrt(N) - qnorm(1 - alpha[j] / 2), 0) # Inf when n is too small
          )
        })),
        3
      )
    })
  })
  # -------------------------------------------------------
  structure(list(
    Parameters_lambda = Parameters_lambda,
    lambdas = lambdas,
    cols = cols,
    M = M,
    alpha = alpha,
    metd = metd,
    cores = cores,
    w = w
  ),
  class = "ExpNBFuzzyParam"
  )
}
# -------------------------

#' @export
print.ExpNBFuzzyParam <- function(x, ...) {
  # -----------------
  cat("\nFuzzy Exponential Naive Bayes Classifier for Discrete Predictors\n\n")
  # -----------------
  cat("Variables:\n")
  print(names(x$lambdas[[1]]))
  cat("Class:\n")
  print(levels(x$M))
  # -----------------
}


#' @export
predict.ExpNBFuzzyParam <- function(object,
                                    newdata,
                                    type = "class",
                                    ...) {
  # --------------------------------------------------------
  test <- as.data.frame(newdata)
  # --------------------------------------------------------
  Parameters_lambda <- object$Parameters_lambda
  lambdas <- object$lambdas
  cols <- object$cols
  M <- object$M
  alpha <- object$alpha
  metd <- object$metd
  cores <- object$cores
  w <- object$w
  # --------------------------------------------------------

  # --------------------------------------------------------
  # Calculation of triangles for each test observation
  # sum of Logs and calculation of Barycenter
  # --------------
  N_test <- nrow(test)
  test_mat <- as.matrix(test[, 1:cols, drop = FALSE])
  n_classes <- length(lambdas)
  classes <- get_classes(M)

  # Densities (not log-densities), as in GauNBFuzzyParam and PoiNBFuzzyParam:
  # the ranking methods assume non-negative fuzzy numbers.
  dens <- function(x, i, k, j) {
    list(dexp(x, rate = 1 / as.numeric(Parameters_lambda[[i]][[k]][j, 1])),
         dexp(x, rate = 1 / as.numeric(Parameters_lambda[[i]][[k]][j, 2])))
  }
  R_M_obs <- fuzzy_param_scores(test_mat, n_classes, cols, alpha, metd, w, dens)

  scores_to_output(R_M_obs, classes, type)
}
