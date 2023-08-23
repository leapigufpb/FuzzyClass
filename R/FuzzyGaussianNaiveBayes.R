#' Fuzzy Gaussian Naive Bayes Classifier Zadeh-based
#'
#' \code{FuzzyGaussianNaiveBayes} Fuzzy Gaussian Naive Bayes Classifier Zadeh-based
#'
#'
#' @param train matrix or data frame of training set cases.
#' @param cl factor of true classifications of training set
#' @param cores  how many cores of the computer do you want to use to use for prediction (default = 2)
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
#' @importFrom foreach '%dopar%'
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
  names_class <- unique(M)
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
  medias <- lapply(1:length(unique(M)), function(i) colMeans(subset(dados, M == unique(M)[i])))
  varian <- lapply(1:length(unique(M)), function(i) diag(diag(cov(subset(dados, M == unique(M)[i]))), (cols), (cols)))
  # --------------------------------------------------------
  # A priori probability of classes - considered equal
  pk <- rep(1 / length(unique(M)), length(unique(M)))
  # -----------------------
  logaritmo <- log(pk)
  log_determinante <- lapply(1:length(unique(M)), function(i) 0.5 * log(det(varian[[i]])))
  inversa_covar <- lapply(1:length(unique(M)), function(i) MASS::ginv(varian[[i]]))
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
    cat("\nFuzzy Gaussian Naive Bayes Classifier for Discrete Predictors Zadeh-based\n\n")
    # -----------------
  } else {
    # -----------------
    cat("\nGaussian Naive Bayes Classifier for Discrete Predictors\n\n")
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
  # Calculation of triangles for each test observation
  # --------------
  N_test <- nrow(test)
  # --------------
  # Defining how many CPU cores to use
  core <- parallel::makePSOCKcluster(cores)
  doParallel::registerDoParallel(core)
  # --------------
  # loop start
  R_M_obs <- foreach::foreach(h = 1:N_test, .combine = rbind) %dopar% {
    # ------------
    x <- test[h, ]
    # ------------
    R_M <- lapply(1:length(unique(M)), function(i) {

      Pertinencia_r <- pertinencia_predict_esparsa(minomosdt = MinimosDataFrame, x, Pert_joda = Pertinencias, cols,i)
      log_Pertinencia <- log(Pertinencia_r)

      if (fuzzy == T) {
        # --------------
        f <- log_Pertinencia + logaritmo[i] - log_determinante[[i]] - 0.5 * as.numeric(x - medias[[i]]) %*% inversa_covar[[i]] %*% as.numeric(x - medias[[i]])
        # --------------
      } else {
        # --------------
        f <- logaritmo[i] - log_determinante[[i]] - 0.5 * as.numeric(x - medias[[i]]) %*% inversa_covar[[i]] %*% as.numeric(x - medias[[i]])
        # --------------
      }

      return(f)
    })
    # --------------------------------------------------------
    R_M_class <- R_M
    # --------------------------------------------------------
    return(R_M_class)
  }
  # ------------
  # -------------------------
  parallel::stopCluster(core)
  # ---------
  if (type == "class") {
    # -------------------------
    R_M_obs <- matrix(R_M_obs,nrow = N_test)
    R_M_obs <- sapply(1:nrow(R_M_obs), function(i) which.max(R_M_obs[i, ]))
    resultado <- unique(M)[R_M_obs]
    return(as.factor(c(resultado)))
    # -------------------------
  } else {
    # -------------------------
    Infpos <- which(R_M_obs==Inf)
    R_M_obs[Infpos] <- .Machine$integer.max;
    R_M_obs <- matrix(unlist(R_M_obs),ncol = length(unique(M)), nrow = N_test)
    R_M_obs <- R_M_obs/rowSums(R_M_obs,na.rm = T)
    # ----------
    colnames(R_M_obs) <- unique(M)
    return(R_M_obs)
    # -------------------------
  }
}
