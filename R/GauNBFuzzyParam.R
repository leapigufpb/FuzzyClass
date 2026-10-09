#' Fuzzy Gaussian Naive Bayes Classifier with Fuzzy parameters
#'
#' \code{GauNBFuzzyParam} Fuzzy Gaussian Naive Bayes Classifier with Fuzzy parameters
#'
#'
#' @param train matrix or data frame of training set cases.
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
#' \insertRef{moraes2021new}{FuzzyClass}
#'
#' @examples
#'
#' set.seed(1) # determining a seed
#' data(iris)
#'
#' # Splitting into Training and Testing
#' split <- caTools::sample.split(t(iris[, 1]), SplitRatio = 0.7)
#' Train <- subset(iris, split == "TRUE")
#' Test <- subset(iris, split == "FALSE")
#' # ----------------
#' # matrix or data frame of test set cases.
#' # A vector will be interpreted as a row vector for a single case.
#' test <- Test[, -5]
#' fit_FGNB <- GauNBFuzzyParam(
#'   train = Train[, -5],
#'   cl = Train[, 5], metd = 1, cores = 2
#' )
#'
#' pred_FGNB <- predict(fit_FGNB, test)
#'
#' head(pred_FGNB)
#' head(Test[, 5])
#' @importFrom stats cov dnorm qchisq qnorm
#' @importFrom Rdpack reprompt
#'
#' @export
GauNBFuzzyParam <- function(train, cl, alphacut = 0.0001, metd = 2, alp = c(0.35, 0.7, 0.86), w = c(0.1,0.3,0.6), cores = 2) {
  UseMethod("GauNBFuzzyParam")
}

#' @export
GauNBFuzzyParam.default <- function(train, cl, alphacut = 0.0001, metd = 2, alp = c(0.35, 0.7, 0.86), w = c(0.1,0.3,0.6), cores = 2) {

  # --------------------------------------------------------
  # Estimating class parameters
  p_data <- predata(train,cl)
  # --
  train <-  p_data$train
  cols <- p_data$cols
  dados <- p_data$dados
  M <- p_data$M
  intervalos <- p_data$intervalos
  #--------------------------------------------------------
  # --------------------------------------------------------
  # Finding Mu and Sigma for each class
  medias <- lapply(1:length(get_classes(M)), function(i) colMeans(subset(dados, M == get_classes(M)[i])))
  varian <- lapply(1:length(get_classes(M)), function(i) diag(diag(cov(subset(dados, M == get_classes(M)[i]))), (cols), (cols)))
  # --------------------------------------------------------
  # --------------------------------------------------------
  # Estimating Triangular Parameters
  alpha = alp
  if (metd != 4) {
    alpha <- seq(alphacut, 1.1, 0.1)
    alpha <- ifelse(alpha > 1, 1, alpha)
  }
  # -------------------------------
  # Sample size of each class: the confidence intervals that define the
  # alpha-cuts of the fuzzy mean and variance are built from the class sample
  n_class <- as.numeric(table(factor(M, levels = get_classes(M))))
  # -------------------------------
  #  Average Parameters
  # ------------------
  Parameters_media <- lapply(1:length(medias), function(i) { # loop to groups
    N <- n_class[i]
    lapply(1:length(medias[[1]]), function(k) { # loop to dimensions
      round(
        t(sapply(1:length(alpha), function(j) {
          c(
            medias[[i]][k] - (qnorm(1 - alpha[j] / 2) * (sqrt(varian[[i]][k, k] / N))),
            medias[[i]][k] + (qnorm(1 - alpha[j] / 2) * (sqrt(varian[[i]][k, k] / N)))
          )
        })),
        5
      )
    })
  })
  # -------------------------------
  # Variance Parameters
  # ------------------
  Parameters_varian <- lapply(1:length(medias), function(i) { # loop to groups
    N <- n_class[i]
    lapply(1:length(medias[[1]]), function(k) { # loop to dimensions
      round(
        t(sapply(1:length(alpha), function(j) {
          beta <- 0.05 # previously fixed
          lambda <- alpha[j]
          # ------
          L <- (1 - lambda) * qchisq(p = 1 - (beta / 2), N - 1) + (lambda * (N - 1))
          R <- (1 - lambda) * qchisq(p = beta / 2, N - 1) + (lambda * (N - 1))
          # ------
          c(
            ((N - 1) * varian[[i]][k, k]) / L,
            ((N - 1) * varian[[i]][k, k]) / R
          )
          # ------
        })),
        5
      )
    })
  })

  # -------------------------------------------------------
  structure(list(
    Parameters_varian = Parameters_varian,
    Parameters_media = Parameters_media,
    medias = medias,
    varian = varian,
    cols = cols,
    M = M,
    alpha = alpha,
    metd = metd,
    cores = cores,
    w = w
  ),
  class = "GauNBFuzzyParam"
  )
}
# -------------------------


#' @export
print.GauNBFuzzyParam <- function(x, ...) {
  # -----------------
  cat("\nFuzzy Gaussian Naive Bayes Classifier for Discrete Predictors\n\n")
  # -----------------
  cat("Variables:\n")
  print(names(x$medias[[1]]))
  cat("Class:\n")
  print(levels(x$M))
  # -----------------
}

#' @export
predict.GauNBFuzzyParam <- function(object,
                                    newdata,
                                    type = "class",
                                    ...) {
  # --------------------------------------------------------
  test <- as.data.frame(newdata)
  # --------------------------------------------------------
  Parameters_varian <- object$Parameters_varian
  Parameters_media <- object$Parameters_media
  medias <- object$medias
  varian <- object$varian
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
  n_classes <- length(medias)
  classes <- get_classes(M)

  dens <- function(x, i, k, j) {
    list(dnorm(x, mean = Parameters_media[[i]][[k]][j, 1], sd = sqrt(Parameters_varian[[i]][[k]][j, 1])),
         dnorm(x, mean = Parameters_media[[i]][[k]][j, 2], sd = sqrt(Parameters_varian[[i]][[k]][j, 2])))
  }
  R_M_obs <- fuzzy_param_scores(test_mat, n_classes, cols, alpha, metd, w, dens)

  scores_to_output(R_M_obs, classes, type)
}
