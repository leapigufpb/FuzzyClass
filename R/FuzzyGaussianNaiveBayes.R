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
  class <- length(unique(M))
  names_class <- unique(M)
  sizes <- table(factor(M))
  Sturges <- round(1 + 3.3 * log10(sizes)) # Sturges
  # -----------------------------
  # Minimum of each class for each variable
  # Class rows, variable columns
  minimo <- lapply(1:class, function(j) {
    sapply(1:cols, function(i) {
      min(dados[M == names_class[j], i])
    })
  })
  # -----------------------------
  # Maximum of each class for each variable
  # Row classes, variable columns
  maximo <- lapply(1:class, function(j) {
    sapply(1:cols, function(i) {
      max(dados[M == names_class[j], i])
    })
  })
  # -----------------------------
  AT_classe <- lapply(1:class, function(i) maximo[[i]] - minimo[[i]])
  # -----------------------------
  Comprim_Intervalo <- lapply(1:class, function(i) AT_classe[[i]] / Sturges[i])
  # --------------------------------------------------------
  # List within List, dimensions
  Freq <- lapply(1:class, function(i) {
    array(0, dim = Sturges)
  })
  # ----------------------

  # Looping in data by class [CRIAR]

  Pertinencias <- lapply(1:length(unique(M)), function(i) {
    dados2 <- dados[M == names_class[i], ]
    # loop in the observations of each group
    for (t in 1:nrow(dados2)) {
      x <- dados2[t, ]

      saida <- floor((x - minimo[[i]]) / Comprim_Intervalo[[i]]) + 1
      # ---
      aux <- (saida > Sturges[i])
      aux <- which(aux == T)
      if (length(aux) > 0) saida[aux] <- saida[aux] - 1
      # ---
      # Finding a position to increase a frequency
      res <- 0
      tamanho_saida <- length(saida)
      if (tamanho_saida > 1) {
        for (j in tamanho_saida:2) res <- res + (saida[j] - 1) * (Sturges[i]^(j - 1))
      }
      res <- res + saida[1]
      # ---
      # ---
      Freq[[i]][as.numeric(res)] <- Freq[[i]][as.numeric(res)] + 1
      # ---
      Freq[[i]][is.na(Freq[[i]])] <- 0
    }
    # Fim do for


    Pertinencia <- Freq[[i]] / sizes[i]

    return(Pertinencia)
  })
  # --------------------------------------------------------
  # Finding Mu and Sigma for each class
  medias <- lapply(1:length(unique(M)), function(i) colMeans(subset(dados, M == unique(M)[i])))
  varian <- lapply(1:length(unique(M)), function(i) diag(diag(cov(subset(dados, M == unique(M)[i]))), (cols), (cols)))
  # --------------------------------------------------------
  # A priori probability of classes - considered equal
  pk <- rep(1 / class, class)
  # -----------------------
  logaritmo <- log(pk)
  log_determinante <- lapply(1:length(unique(M)), function(i) 0.5 * log(det(varian[[i]])))
  inversa_covar <- lapply(1:length(unique(M)), function(i) MASS::ginv(varian[[i]]))
  # -----------------------

  # -------------------------------------------------------
  structure(list(
    minimo = minimo,
    maximo = maximo,
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
      saida <- abs(floor((x - minimo[[i]]) / Comprim_Intervalo[[i]]) + 1)
      # ---
      aux1 <- x < minimo[[i]]
      aux2 <- x > maximo[[i]]
      # ---
      log_Pertinencia <- ifelse((T %in% aux1) | (T %in% aux2), -50, 0)
      # ---
      # ---
      # Finding a position to increase a frequency
      res <- 0
      tamanho_saida <- length(saida)
      if (tamanho_saida > 1) {
        for (j in tamanho_saida:2) res <- res + (saida[j] - 1) * (Sturges[i]^(j - 1))
      }
      res <- abs(res + saida[1])
      # ---
      # ---
      if (log_Pertinencia == -50) {
        pert <- ifelse(is.na(Pertinencias[[i]][as.numeric(res)]) == T, 0, Pertinencias[[i]][as.numeric(res)])
        log_Pertinencia <- ifelse(pert <= 0, -50, log(Pertinencias[[i]][as.numeric(res)]))
      }

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
