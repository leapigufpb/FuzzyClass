#' Fuzzy Gaussian Naive Bayes Classifier Zadeh-based
#'
#' \code{FuzzyGaussianNaiveBayes} Fuzzy Gaussian Naive Bayes Classifier Zadeh-based
#'
#'
#' @param train matrix or data frame of training set cases.
#' @param cl factor of true classifications of training set
#' @param cores kept for backward compatibility; prediction is vectorized and does not start parallel workers (default = 2)
#' @param fuzzy boolean variable to use the membership function
#'
#' @return A vector of classifications
#'
#' @references
#' \insertRef{marcos2012online}{FuzzyClass}
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
#' fit_GNB <- FuzzyGaussianNaiveBayes(
#'   train = Train[, -5],
#'   cl = Train[, 5], cores = 2
#' )
#'
#' pred_GNB <- predict(fit_GNB, test)
#'
#' head(pred_GNB)
#' head(Test[, 5])
#' @importFrom stats cov dnorm qchisq qnorm
#' @importFrom Rdpack reprompt
#'
#' @export
FuzzyGaussianNaiveBayes <- function(train, cl, cores = 2, fuzzy = TRUE) {
  UseMethod("FuzzyGaussianNaiveBayes")
}

#' @export
FuzzyGaussianNaiveBayes.default <- function(train, cl, cores = 2, fuzzy = TRUE) {

  # --------------------------------------------------------
  # Estimating class parameters
  p_data <- predata(train,cl)
  # --
  train <-  p_data$train
  cols <- p_data$cols
  dados <- p_data$dados
  M <- p_data$M
  intervalos <- p_data$intervalos
  # --------------------------------------------------------
  N <- nrow(dados) # Number of observations
  # -------------------------------
  # --------------------------------------------------------
  names_class <- get_classes(M)
  Sturges <- Sturges(dados, M);
  # --------------------------------------------------------
  Comprim_Intervalo <- Comprim_Intervalo(dados, M, Sturges);
  minimos <- minimos(dados, M, cols);
  maximos <- maximos(dados, M, cols);
  # --------------------------------------------------------
  MinimosDataFrame <- minomosdt_function(minimos, M, Comprim_Intervalo, Sturges, cols)
  Frequencia <- Freq_esparsa(dados = dados,M = M, minomosdt = MinimosDataFrame, cols = cols)
  Pertinencias <- Pertinencia_esparsa(M = M, Frequencia, cols = cols)
  # --------------------------------------------------------
  # Finding Mu and Sigma for each class
  medias <- lapply(1:length(get_classes(M)), function(i) colMeans(subset(dados, M == get_classes(M)[i])))
  varian <- lapply(1:length(get_classes(M)), function(i) diag(diag(cov(subset(dados, M == get_classes(M)[i]))), (cols), (cols)))
  # --------------------------------------------------------
  # A priori probability of classes - considered equal
  pk <- rep(1 / length(get_classes(M)), length(get_classes(M)))
  # -----------------------
  logaritmo <- log(pk)
  log_determinante <- lapply(1:length(get_classes(M)), function(i) 0.5 * log(det(varian[[i]])))
  inversa_covar <- lapply(1:length(get_classes(M)), function(i) MASS::ginv(varian[[i]]))
  # -----------------------

  # -------------------------------------------------------
  structure(list(
    minimo = minimos,
    MinimosDataFrame = MinimosDataFrame,
    maximo = maximos,
    Comprim_Intervalo = Comprim_Intervalo,
    fuzzy = fuzzy,
    Sturges = Sturges,
    Pertinencias = Pertinencias,
    log_determinante = log_determinante,
    logaritmo = logaritmo,
    inversa_covar = inversa_covar,
    medias = medias,
    varian = varian,
    cols = cols,
    M = M,
    cores = cores
  ),
  class = "FuzzyGaussianNaiveBayes"
  )
}
# -------------------------


#' @export
print.FuzzyGaussianNaiveBayes <- function(x, ...) {
  if (x$fuzzy == T) {
    # -----------------
    cat("\nFuzzy Gaussian Naive Bayes Classifier for Continuous Predictors Zadeh-based\n\n")
    # -----------------
  } else {
    # -----------------
    cat("\nGaussian Naive Bayes Classifier for Continuous Predictors\n\n")
    # -----------------
  }
  cat("Variables:\n")
  print(names(x$medias[[1]]))
  cat("Class:\n")
  print(levels(x$M))
  # -----------------
}

#' @export
predict.FuzzyGaussianNaiveBayes <- function(object,
                                            newdata,
                                            type = "class",
                                            ...) {
  # --------------------------------------------------------
  test <- as.data.frame(newdata)
  # --------------------------------------------------------
  minimo <- object$minimo
  maximo <- object$maximo
  MinimosDataFrame <-  object$MinimosDataFrame
  Comprim_Intervalo <- object$Comprim_Intervalo
  Sturges <- object$Sturges
  log_determinante <- object$log_determinante
  logaritmo <- object$logaritmo
  Pertinencias <- object$Pertinencias
  inversa_covar <- object$inversa_covar
  medias <- object$medias
  cols <- object$cols
  M <- object$M
  cores <- object$cores
  fuzzy <- object$fuzzy
  # --------------------------------------------------------

  # --------------------------------------------------------
  # Calculation for each test observation
  # --------------
  test_mat <- as.matrix(test[, 1:cols, drop = FALSE])
  N_test <- nrow(test_mat)
  classes <- get_classes(M)
  n_classes <- length(classes)

  if (fuzzy == TRUE) {
    Pert_list <- function_new_membership_predict(test_mat, M = M, MinimosDataFrame = MinimosDataFrame, Pertinencia = Pertinencias, cols = cols)
  }

  R_M_obs <- matrix(0, nrow = N_test, ncol = n_classes)
  for (i in 1:n_classes) {
    D_i <- sweep(test_mat, 2, medias[[i]], "-")
    quad <- rowSums((D_i %*% inversa_covar[[i]]) * D_i)
    if (fuzzy == TRUE) {
      log_Pert <- log(pmax(Pert_list[[i]], 1e-15))
      R_M_obs[, i] <- log_Pert + logaritmo[i] - log_determinante[[i]] - 0.5 * quad
    } else {
      R_M_obs[, i] <- logaritmo[i] - log_determinante[[i]] - 0.5 * quad
    }
  }

  if (type == "class") {
    # -------------------------
    res_idx <- max.col(R_M_obs, ties.method = "first")
    resultado <- classes[res_idx]
    return(as.factor(resultado))
    # -------------------------
  } else {
    # -------------------------
    # Softmax conversion to probabilities
    max_scores <- R_M_obs[cbind(seq_len(N_test), max.col(R_M_obs))]
    exp_scores <- exp(R_M_obs - max_scores)
    probs <- exp_scores / rowSums(exp_scores, na.rm = TRUE)
    colnames(probs) <- classes
    return(probs)
    # -------------------------
  }
}
