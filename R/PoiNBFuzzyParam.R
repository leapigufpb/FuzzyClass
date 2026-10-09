#' Fuzzy Poisson Naive Bayes Classifier with Fuzzy parameters
#'
#' \code{PoiNBFuzzyParam} Fuzzy Poisson Naive Bayes Classifier with Fuzzy parameters
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
#' \insertRef{soares2016assessment}{FuzzyClass}
#'
#' @examples
#'
#' set.seed(1) # determining a seed
#' class1 <- data.frame(vari1 = rpois(100,lambda = 2),
#'                      vari2 = rpois(100,lambda = 2),
#'                      vari3 = rpois(100,lambda = 2), class = 1)
#' class2 <- data.frame(vari1 = rpois(100,lambda = 1),
#'                      vari2 = rpois(100,lambda = 1),
#'                      vari3 = rpois(100,lambda = 1), class = 2)
#' class3 <- data.frame(vari1 = rpois(100,lambda = 5),
#'                      vari2 = rpois(100,lambda = 5),
#'                      vari3 = rpois(100,lambda = 5), class = 3)
#' data <- rbind(class1,class2,class3)
#'
#' # Splitting into Training and Testing
#' split <- caTools::sample.split(t(data[, 1]), SplitRatio = 0.7)
#' Train <- subset(data, split == "TRUE")
#' Test <- subset(data, split == "FALSE")
#' # ----------------
#' # matrix or data frame of test set cases.
#' # A vector will be interpreted as a row vector for a single case.
#' test <- Test[, -4]
#' fit_FPoiNB <- PoiNBFuzzyParam(
#'   train = Train[, -4],
#'   cl = Train[, 4], metd = 1, cores = 2
#' )
#'
#' pred_FPoiNB <- predict(fit_FPoiNB, test)
#'
#' head(pred_FPoiNB)
#' head(Test[, 4])
#' @importFrom stats cov dpois qnorm
#' @importFrom Rdpack reprompt
#' @importFrom MASS fitdistr
#'
#' @export
PoiNBFuzzyParam <- function(train, cl, alphacut = 0.0001, metd = 2, alp = c(0.35, 0.7, 0.86), w = c(0.1,0.3,0.6), cores = 2) {
  UseMethod("PoiNBFuzzyParam")
}

#' @export
PoiNBFuzzyParam.default <- function(train, cl, alphacut = 0.0001, metd = 2, alp = c(0.35, 0.7, 0.86), w = c(0.1,0.3,0.6), cores = 2) {

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
  # Verify data types
  verifyNumbers <- verifyNumbersFunction(dados, cols)

  # --------------------------------------------------------
  if(sum(verifyNumbers) != cols){ stop("All variables must be discrete values.") }
  # --------------------------------------------------------

  # --------------------------------------------------------
  # Finding lambda estimated
  lambda_estimated <- lapply(1:length(get_classes(M)), function(i) {
    Sub <- subset(dados, M == get_classes(M)[i])
    sapply(1:cols, function(j) {
      saida <- MASS::fitdistr(Sub[, j], "poisson")[1:2]
      return(saida)
    })
  })
  # --------------------------------------------------------
  # --------------------------------------------------------
  # Estimating Triangular Parameters
  alpha = alp
  if (metd != 4) {
    alpha <- seq(alphacut, 1.1, 0.1)
    alpha <- ifelse(alpha > 1, 1, alpha)
  }
  # -------------------------------
  # -------------------------------
  #  Alphas Parameters
  # ------------------
  Parameters_lambdas <- lapply(1:length(lambda_estimated), function(i) { # loop to groups
    lapply(1:ncol(lambda_estimated[[1]]), function(k) { # loop to dimensions
      round(
        t(sapply(1:length(alpha), function(j) {
          c(
            lambda_estimated[[i]][,k]$estimate - (1 - alpha[j])*lambda_estimated[[i]][,k]$sd,
            lambda_estimated[[i]][,k]$estimate + (1 - alpha[j])*lambda_estimated[[i]][,k]$sd
          )
        })),
        3
      )
    })
  })
  # -------------------------------------------------------
  structure(list(
    Parameters_lambdas = Parameters_lambdas,
    lambda_estimated = lambda_estimated,
    cols = cols,
    M = M,
    alpha = alpha,
    metd = metd,
    cores = cores,
    w = w
  ),
  class = "PoiNBFuzzyParam"
  )
}
# -------------------------


#' @export
print.PoiNBFuzzyParam <- function(x, ...) {
  # -----------------
  cat("\nFuzzy Poisson Naive Bayes Classifier for Discrete Predictors\n\n")
  # -----------------
  cat("Class:\n")
  print(levels(x$M))
  # -----------------
}

#' @export
predict.PoiNBFuzzyParam <- function(object,
                                    newdata,
                                    type = "class",
                                    ...) {
  # --------------------------------------------------------
  test <- as.data.frame(newdata)
  # --------------------------------------------------------
  Parameters_lambdas <- object$Parameters_lambdas
  lambda_estimated <- object$lambda_estimated
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
  classes <- get_classes(M)
  n_classes <- length(classes)

  dens <- function(x, i, k, j) {
    x <- round(x)
    list(dpois(x, lambda = as.numeric(Parameters_lambdas[[i]][[k]][j, 1])),
         dpois(x, lambda = as.numeric(Parameters_lambdas[[i]][[k]][j, 2])))
  }
  R_M_obs <- fuzzy_param_scores(test_mat, n_classes, cols, alpha, metd, w, dens)

  scores_to_output(R_M_obs, classes, type)
}
