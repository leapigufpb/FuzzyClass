#' Fuzzy Beta Naive Bayes
#'
#' \code{FuzzyBetaNaiveBayes} Fuzzy Beta Naive Bayes
#'
#'
#' @param train matrix or data frame of training set cases.
#' @param cl factor of true classifications of training set
#' @param cores  how many cores of the computer do you want to use (default = 2)
#' @param fuzzy boolean variable to use the membership function
#'
#' @return A vector of classifications
#'
#' @references
#' \insertRef{de2020new}{FuzzyClass}
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
#' #----------------
#' # matrix or data frame of test set cases.
#' # A vector will be interpreted as a row vector for a single case.
#' test <- Test[, -5]
#' fit_NBT <- FuzzyBetaNaiveBayes(
#'   train = Train[, -5],
#'   cl = Train[, 5], cores = 2
#' )
#'
#' pred_NBT <- predict(fit_NBT, test)
#'
#' head(pred_NBT)
#' head(Test[, 5])
#' @importFrom stats dbeta
#'
#' @export
FuzzyBetaNaiveBayes <- function(train, cl, cores = 2, fuzzy = TRUE) {
  UseMethod("FuzzyBetaNaiveBayes")
}

#' @export
FuzzyBetaNaiveBayes.default <- function(train, cl, cores = 2, fuzzy = TRUE) {

  #--------------------------------------------------------
  # Estimating class parameters
  train <- as.data.frame(train)
  cols <- ncol(train) # Number of variables
  if(is.null(cols)){
    cols <- 1
  }
  dados <- train # training data matrix
  M <- c(unlist(cl)) # true classes
  M <- factor(M, labels = sort(unique(M)))
  #--------------------------------------------------------
  for(j in 1:cols){
    dados[, j] <- dados[, j] / (max(dados[, j])+1e-2)
  }
  #--------------------------------------------------------
  # Estimating Beta Parameters
  parametersC <- estimation_parameters_beta(M, cols, dados)
  #--------------------------------------------------------

  #--------------------------------------------------------
  Sturges <- Sturges(dados, M);
  Comprim_Intervalo <- Comprim_Intervalo(dados, M, Sturges);
  minimos <- minimos(dados, M, cols);
  Freq <- Freq(dados, M, Comprim_Intervalo, Sturges, minimos, cols);
  Pertinencia <- Pertinencia(Freq, dados, M);
  #------
  # A priori probability of classes - considered equal
  pk <- rep(1 / length(unique(M)), length(unique(M)))
  #-------------------------------------------------------


  #-------------------------------------------------------
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
  class = "FuzzyBetaNaiveBayes"
  )
}
#-------------------------


#' @export
print.FuzzyBetaNaiveBayes <- function(x, ...) {
  if (x$fuzzy == T) {
    #-----------------
    cat("\nFuzzy Beta Naive Bayes Classifier for Discrete Predictors\n\n")
    #-----------------
  } else {
    #-----------------
    cat("\nNaive Beta  Bayes Classifier for Discrete Predictors\n\n")
    #-----------------
  }
  cat("Class:\n")
  print(levels(x$M))
  #-----------------
}

#' @export
predict.FuzzyBetaNaiveBayes <- function(object,
                                         newdata,
                                         type = "class",
                                         ...) {
  #--------------------------------------------------------
  test <- as.data.frame(newdata)
  #--------------------------------------------------------
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
  #--------------------------------------------------------
  for(j in 1:cols){
    test[, j] <- test[, j] / (max(test[, j])+1e-2)
  }
  #--------------------------------------------------------
  # Classification
  #--------------
  P <- density_values_beta(M, cols, test, parametersC, pk)
  #---------
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
  #---------
  if (type == "class") {
    #-------------------------
    R_M_obs <- sapply(1:nrow(R_M_obs), function(i) which.max(R_M_obs[i, ]))
    resultado <- unique(M)[R_M_obs]
    return(as.factor(c(resultado)))
    #-------------------------
  } else {
    #-------------------------
    Infpos <- which(R_M_obs==Inf)
    R_M_obs[Infpos] <- .Machine$integer.max;
    R_M_obs <- matrix(unlist(R_M_obs),ncol = length(unique(M)), nrow = N_test)
    R_M_obs <- R_M_obs/rowSums(R_M_obs,na.rm = T)
    #----------
    colnames(R_M_obs) <- unique(M)
    return(R_M_obs)
    #-------------------------
  }
}

# ----------------
density_values_beta <- function(M, cols, test, parametersC, pk){
  lapply(1:length(unique(M)), function(i) {
    densidades <- sapply(1:cols, function(j) {
      stats::dbeta(test[, j], shape1 = parametersC[[i]][[j]][1], shape2 = parametersC[[i]][[j]][2])
    })
    densidades <- apply(densidades, 1, prod)
    # Calcula a P(w_i) * P(X_k | w_i)
    p <- pk[[i]] * densidades
    # ---
    return(p)
  })

}
# ----------------

# ----------------
estimation_parameters_beta <- function(M, cols, dados){
  lapply(1:length(unique(M)), function(i) {
    lapply(1:cols, function(j) {
      SubSet <- dados[M == unique(M)[i], j]
      param <- MASS::fitdistr(SubSet, "beta", start = list(shape1 = 1, shape2 = 1), lower = c(0.001,0.001), upper = c(10,10))$estimate#max(SubSet) + 1e-2)$estimate
      return(param)
    })
  })

}
# ----------------
