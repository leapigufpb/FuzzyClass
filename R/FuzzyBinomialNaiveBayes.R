#' Fuzzy Binomial Naive Bayes
#'
#' \code{FuzzyBinomialNaiveBayes} Fuzzy Binomial Naive Bayes
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
#' \insertRef{moraes2016fuzzybinom}{FuzzyClass}
#'
#' @examples
#'
#' set.seed(1) # determining a seed
#' class1 <- data.frame(vari1 = rbinom(100,size = 10, prob = 0.2),
#'                      vari2 = rbinom(100,size = 10, prob = 0.2),
#'                     vari3 = rbinom(100,size = 10, prob = 0.2), class = 1)
#' class2 <- data.frame(vari1 = rbinom(100,size = 10, prob = 0.5),
#'                      vari2 = rbinom(100,size = 10, prob = 0.5),
#'                     vari3 = rbinom(100,size = 10, prob = 0.5), class = 2)
#' class3 <- data.frame(vari1 = rbinom(100,size = 10, prob = 0.8),
#'                      vari2 = rbinom(100,size = 10, prob = 0.8),
#'                      vari3 = rbinom(100,size = 10, prob = 0.8), class = 3)
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
#' fit_NBT <- FuzzyBinomialNaiveBayes(
#'   train = Train[, -4],
#'   cl = Train[, 4], cores = 2
#' )
#'
#' pred_NBT <- predict(fit_NBT, test)
#'
#' head(pred_NBT)
#' head(Test[, 4])
#' @importFrom stats dbinom uniroot
#'
#' @export
FuzzyBinomialNaiveBayes <- function(train, cl, cores = 2, fuzzy = TRUE) {
  UseMethod("FuzzyBinomialNaiveBayes")
}

#' @noRd
funcao_estimation_N <- function(n, x){
  var(x)  - (mean(x)*(n-mean(x)))/n
}
#-------------------------------


#' @export
FuzzyBinomialNaiveBayes.default <- function(train, cl, cores = 2, fuzzy = T) {

  # --------------------------------------------------------
  # Estimating class parameters
  p_data <- predata(train,cl)
  # --
  dados <- train <-  p_data$train
  cols <- p_data$cols
  M <- p_data$M
  intervalos <- p_data$intervalos
  # --------------------------------------------------------
  # --------------------------------------------------------
  # Verify if all variables are discrete
  verifyNumbers <- verifyNumbersFunction(dados, cols)

  # --------------------------------------------------------
  if(sum(verifyNumbers) != cols){ stop("All variables must be discrete values.") }
  # --------------------------------------------------------


  #--------------------------------------------------------
  # Estimating Gamma Parameters
  parametersC <- estimation_parameters_binomial(M, cols, dados)
  #--------------------------------------------------------
  #--------------------------------------------------------
  Sturges <- Sturges_discrete(dados, M);
  Comprim_Intervalo <- Comprim_Intervalo_discrete(dados, M);
  minimos <- minimos(dados, M, cols);
  MinimosDataFrame <- minomosdt_function(minimos, M, Comprim_Intervalo, Sturges, cols)
  Frequencia <- Freq_esparsa(dados = dados,M = M, minomosdt = MinimosDataFrame, cols = cols)
  Pertinencia <- Pertinencia_esparsa(M = M, Frequencia, cols = cols)
  #------
  # A priori probability of classes - considered equal
  pk <- rep(1 / length(unique(M)), length(unique(M)))
  #-------------------------------------------------------


  #-------------------------------------------------------
  structure(list(
    parametersC = parametersC,
    minimos = minimos,
    MinimosDataFrame = MinimosDataFrame,
    cols = cols,
    M = M,
    cores = cores,
    Comprim_Intervalo = Comprim_Intervalo,
    Pertinencia = Pertinencia,
    Sturges = Sturges,
    pk = pk,
    fuzzy = fuzzy
  ),
  class = "FuzzyBinomialNaiveBayes"
  )
}
#-------------------------


#' @export
print.FuzzyBinomialNaiveBayes <- function(x, ...) {
  if (x$fuzzy == T) {
    #-----------------
    cat("\nFuzzy Binomial Naive Bayes Classifier for Discrete Predictors\n\n")
    #-----------------
  } else {
    #-----------------
    cat("\nNaive Binomial  Bayes Classifier for Discrete Predictors\n\n")
    #-----------------
  }
  cat("Class:\n")
  print(levels(x$M))
  #-----------------
}

#' @export
predict.FuzzyBinomialNaiveBayes <- function(object,
                                         newdata,
                                         type = "class",
                                         ...) {
  #--------------------------------------------------------
  test <- as.data.frame(newdata)
  #--------------------------------------------------------
  parametersC <- object$parametersC
  minimos <- object$minimos
  MinimosDataFrame <-  object$MinimosDataFrame
  cols <- object$cols
  M <- object$M
  cores <- object$cores
  Comprim_Intervalo <- object$Comprim_Intervalo
  Pertinencia <- object$Pertinencia
  Sturges <- object$Sturges
  pk <- object$pk
  fuzzy <- object$fuzzy
  #--------------------------------------------------------

  #--------------------------------------------------------
  # Classification
  #--------------
  P <- density_values_binomial(M, cols, test, parametersC, pk)
  # ---------
  N_test <- nrow(test)
  # --
  # --
  if(fuzzy == T){
    Pertinencia_r <- function_new_membership_predict(test, M = M, MinimosDataFrame, Pertinencia, cols = cols)
    R_M_obs <- function_new_fuzzy_predict(retorno = Pertinencia_r, P, M)
  }else{
    R_M_obs <- t(data.frame(matrix(unlist(P), nrow=length(P), byrow=TRUE)))
  }
  # ---------
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

# -------------------------------------------------------
# Functions

# ----------------
density_values_binomial <- function(M, cols, test, parametersC, pk){
  lapply(1:length(unique(M)), function(i) {
    densidades <- sapply(1:cols, function(j) {
      t <- round(test[, j]) # Necessario para Binomial
      stats::dbinom(t, size = parametersC[[i]][[j]][1], prob = parametersC[[i]][[j]][2])
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
estimation_parameters_binomial <- function(M, cols, dados){
  lapply(1:length(unique(M)), function(i) {
    lapply(1:cols, function(j) {
      #print(c(i,j))
      SubSet <- dados[M == unique(M)[i], j]
      # --
      n <- try(uniroot(funcao_estimation_N, interval = c(1,100), x = SubSet)$root, silent = TRUE)
      if(inherits(n, "try-error")){
        n <- summary(funcao_estimation_N(1:100, x = SubSet))[3]
        n <- ifelse(round(n)< mean(SubSet), mean(SubSet)+1, round(n))
      }
      p <- mean(SubSet)/n
      # --
      param <- c(n = round(n), p =p)
      if(param[1]==0){param[1] <- 1}
      # --
      return(param)
    })
  })

}
# ----------------
