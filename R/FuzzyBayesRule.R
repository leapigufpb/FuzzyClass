#' Fuzzy Bayes Rule
#'
#' \code{FuzzyBayesRule} Fuzzy Bayes Rule
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
#' \insertRef{de2006fuzzy}{FuzzyClass}
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
#' fit_NBT <- FuzzyBayesRule(
#'   train = Train[, -5],
#'   cl = Train[, 5], cores = 2
#' )
#'
#' pred_NBT <- predict(fit_NBT, test)
#'
#' head(pred_NBT)
#' head(Test[, 5])
#' @importFrom stats dgamma
#'
#' @export
FuzzyBayesRule <- function(train, cl, cores = 2, fuzzy = TRUE) {
  UseMethod("FuzzyBayesRule")
}

#' @export
FuzzyBayesRule.default <- function(train, cl, cores = 2, fuzzy = T) {

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
  # Estimating Parameters
  parametersC <- estimation_parameters_fuzzy_bayes_rule(M, cols, dados)

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
    fuzzy = fuzzy
  ),
  class = "FuzzyBayesRule"
  )
}
# -------------------------


#' @export
print.FuzzyBayesRule <- function(x, ...) {
  if (x$fuzzy == T) {
    # -----------------
    cat("\nFuzzy Bayes Rule Classifier for Discrete Predictors\n\n")
    # -----------------
  } else {
    # -----------------
    cat("\nNaive Bayes Rule Classifier for Discrete Predictors\n\n")
    # -----------------
  }
  cat("Class:\n")
  print(levels(x$M))
  # -----------------
}

#' @export
predict.FuzzyBayesRule <- function(object,
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
  # --------------------------------------------------------

  # --------------------------------------------------------
  # Classification
  # --------------
  P <- density_values_multivariate_normal(M, cols, test, parametersC, pk)

  # ---------
  N_test <- nrow(test)
  # --
  test <- split(test, seq(nrow(test)))
  # --
  if(fuzzy == T){
    retorno <- purrr::map(test, function_membership_predict, M, Sturges, minimos, Comprim_Intervalo, Pertinencia, cols)
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
density_values_multivariate_normal <- function(M, cols, test, parametersC, pk){
  lapply(1:length(unique(M)), function(i) {
    densidades <- mvtnorm::dmvnorm(test,mean = parametersC[[i]][[1]], sigma = parametersC[[i]][[2]])
    # Calcula a P(w_i) * P(X_k | w_i)
    p <- pk[[i]] * densidades
    # ---
    return(p)
  })

}
# ----------------

# ----------------
estimation_parameters_fuzzy_bayes_rule <- function(M, cols, dados){
  lapply(1:length(unique(M)), function(i) {
      SubSet <- dados[M == unique(M)[i],]
      # --
      Mean <- colMeans(SubSet)
      VarMatrix <- var(SubSet)
      # --
      param <- list(Mean,VarMatrix)
      # --
      return(param)
  })

}
# ----------------
