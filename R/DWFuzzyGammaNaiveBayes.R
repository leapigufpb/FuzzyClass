#' Double Weighted Fuzzy Gamma Naive Bayes
#'
#' \code{DWFuzzyGammaNaiveBayes} Double Weighted Fuzzy Gamma Naive Bayes
#'
#'
#' @param train matrix or data frame of training set cases.
#' @param cl factor of true classifications of training set
#' @param cores how many cores of the computer do you want to use to use for prediction (default = 2)
#' @param fuzzy boolean variable to use the membership function
#' @param wdelta vector weight each class
#' @param weta vector  weight each feature
#'
#' @return A vector of classifications
#'
#' @references
#' \insertRef{marcos2020double}{FuzzyClass}
#'
#' @examples
#'
#' set.seed(1) # determining a seed
#' data(GamWeightData)
#'
#' # Splitting into Training and Testing
#' split <- caTools::sample.split(t(GamWeightData[, 1]), SplitRatio = 0.7)
#' Train <- subset(GamWeightData, split == "TRUE")
#' Test <- subset(GamWeightData, split == "FALSE")
#' # ----------------
#' # matrix or data frame of test set cases.
#' # A vector will be interpreted as a row vector for a single case.
#' test <- Test[, -4]
#' fit_NBT <- DWFuzzyGammaNaiveBayes(
#'   train = Train[, -4],
#'   cl = Train[, 4], cores = 2,
#'   wdelta = c(2.002/6,1.998/6,2.000/6),
#'   weta = c(3/10,2/10, 5/10)
#' )
#'
#' pred_NBT <- predict(fit_NBT, test)
#'
#' head(pred_NBT)
#' head(Test[, 4])
#' @importFrom stats dgamma
#'
#' @export
DWFuzzyGammaNaiveBayes <- function(train, cl, cores = 2, fuzzy = TRUE, wdelta, weta) {
  UseMethod("DWFuzzyGammaNaiveBayes")
}

#' @export
DWFuzzyGammaNaiveBayes.default <- function(train, cl, cores = 2, fuzzy = T, wdelta, weta) {

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

  # --------------------------------------------------------
  # Estimating Gamma Parameters
  parametersC <- estimation_parameters_gamma(M, cols, dados)

  # --------------------------------------------------------
  Sturges <- Sturges(dados, M);
  Comprim_Intervalo <- Comprim_Intervalo(dados, M, Sturges);
  minimos <- minimos(dados, M, cols);
  Freq <- Freq(dados, M, Comprim_Intervalo, Sturges, minimos, cols);
  Pertinencia <- Pertinencia(Freq, dados, M);
  # ------
  # A priori probability of classes - considered equal
  pk <- rep(1 / length(unique(M)), length(unique(M)))
  # -------------------------------------------------------


  # -------------------------------------------------------
  structure(list(
    parametersC = parametersC,
    minimos = minimos,
    cols = cols,
    M = M,
    cores = cores,
    Comprim_Intervalo = Comprim_Intervalo,
    Pertinencia = Pertinencia,
    Sturges = Sturges,
    pk = pk,
    fuzzy = fuzzy,
    wdelta = wdelta,
    weta = weta
  ),
  class = "DWFuzzyGammaNaiveBayes"
  )
}
# -------------------------


#' @export
print.DWFuzzyGammaNaiveBayes <- function(x, ...) {
  if (x$fuzzy == T) {
    # -----------------
    cat("\nDouble Weighted Fuzzy Gamma Naive Bayes Classifier for Discrete Predictors\n\n")
    # -----------------
  } else {
    # -----------------
    cat("\nDouble Weighted Naive Gamma  Bayes Classifier for Discrete Predictors\n\n")
    # -----------------
  }
  cat("Class:\n")
  print(levels(x$M))
  # -----------------
}

#' @export
predict.DWFuzzyGammaNaiveBayes <- function(object,
                                         newdata,
                                         type = "class",
                                         ...) {
  # --------------------------------------------------------
  test <- as.data.frame(newdata)
  # --------------------------------------------------------
  parametersC <- object$parametersC
  minimos <- object$minimos
  cols <- object$cols
  M <- object$M
  cores <- object$cores
  Comprim_Intervalo <- object$Comprim_Intervalo
  Pertinencia <- object$Pertinencia
  Sturges <- object$Sturges
  pk <- object$pk
  fuzzy <- object$fuzzy
  wdelta <- object$wdelta
  weta <- object$weta
  # --------------------------------------------------------

  # --------------------------------------------------------
  # Classification
  # --------------
  P <- density_values_gamma_dw(M, cols, test, parametersC, pk, wdelta, weta)

  # ---------
  N_test <- nrow(test)
  # --
  test <- split(test, seq(nrow(test)))
  # --
  if(fuzzy == T){
    retorno <- purrr::map(test, function_membership_predict_dw, M, Sturges, minimos, Comprim_Intervalo, Pertinencia, cols, weta)
    R_M_obs <- function_fuzzy_predict(retorno, P, M)
  }else{
    R_M_obs <- t(data.frame(matrix(unlist(P), nrow=length(P), byrow=TRUE)))
  }
  # ---------
  if (type == "class") {
    # -------------------------
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

# -------------------------------------------------------
# Functions

# ----------------
density_values_gamma_dw <- function(M, cols, test, parametersC, pk, wdelta, weta){
  lapply(1:length(unique(M)), function(i) {
    densidades <- sapply(1:cols, function(j) {
      stats::dgamma(test[, j], shape = parametersC[[i]][[j]][1], scale = parametersC[[i]][[j]][2])^(weta[j])
    })
    densidades <- apply(densidades, 1, prod)
    # Calcula a P(w_i) * P(X_k | w_i)
    p <- (pk[[i]]^(wdelta[i])) * densidades
    # ---
    return(p)
  })

}
# ----------------

# ----------------
estimation_parameters_gamma <- function(M, cols, dados){
  lapply(1:length(unique(M)), function(i) {
    lapply(1:cols, function(j) {
      SubSet <- dados[M == unique(M)[i], j]
      # --
      param <- stats::optim(c(.5,.5), log_ver_Gamma, method = "L-BFGS-B", y = SubSet, lower = 0.1, upper = max(SubSet))$par
      # --
      return(param)
    })
  })

}
# ----------------
