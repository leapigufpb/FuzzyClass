#' Fuzzy Naive Bayes Trapezoidal Classifier
#'
#' \code{FuzzyTrapezoidalNaiveBayes} Fuzzy Naive Bayes Trapezoidal Classifier
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
#' \insertRef{de2022fuzzy}{FuzzyClass}
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
#' fit_NBT <- FuzzyTrapezoidalNaiveBayes(
#'   train = Train[, -5],
#'   cl = Train[, 5], cores = 2
#' )
#'
#' pred_NBT <- predict(fit_NBT, test)
#'
#' head(pred_NBT)
#' head(Test[, 5])
#' @importFrom caTools sample.split
#'
#' @export
FuzzyTrapezoidalNaiveBayes <- function(train, cl, cores = 2, fuzzy = T) {
  UseMethod("FuzzyTrapezoidalNaiveBayes")
}

#' @export
FuzzyTrapezoidalNaiveBayes.default <- function(train, cl, cores = 2, fuzzy = T) {

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
  # Estimating Parameters
  parametersC <- estimation_parameters_trape(M, cols, dados)
  # --------------------------------------------------------
  Sturges <- Sturges(dados, M);
  Comprim_Intervalo <- Comprim_Intervalo(dados, M, Sturges);
  minimos <- minimos(dados, M, cols);
  #Freq <- Freq(dados, M, Comprim_Intervalo, Sturges, minimos, cols);
  #Pertinencia <- Pertinencia(Freq, dados, M);
  MinimosDataFrame <- minomosdt_function(minimos, M, Comprim_Intervalo, Sturges, cols)
  Frequencia <- Freq_esparsa(dados = dados,M = M, minomosdt = MinimosDataFrame, cols = cols)
  Pertinencia <- Pertinencia_esparsa(M = M, Frequencia, cols = cols)
  # ------
  # A priori probability of classes - considered equal
  pk <- rep(1 / length(unique(M)), length(unique(M)))
  # --------------------------------------------------------

  # -------------------------------------------------------
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
  class = "FuzzyTrapezoidalNaiveBayes"
  )
}
# -------------------------


#' @export
print.FuzzyTrapezoidalNaiveBayes <- function(x, ...) {
  if (x$fuzzy == T) {
    # -----------------
    cat("\nFuzzy Naive Bayes Trapezoidal Classifier for Discrete Predictors\n\n")
    # -----------------
  } else {
    # -----------------
    cat("\nNaive Bayes Trapezoidal Classifier for Discrete Predictors\n\n")
    # -----------------
  }
  cat("Class:\n")
  print(levels(x$M))
  # -----------------
}

#' @export
predict.FuzzyTrapezoidalNaiveBayes <- function(object,
                                               newdata,
                                               type = "class",
                                               ...) {
  # --------------------------------------------------------
  test <- as.data.frame(newdata)
  # --------------------------------------------------------
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
  # --------------------------------------------------------

  # --------------------------------------------------------
  # Classification
  # --------------
  P <- density_values_trape(M, cols, test, parametersC, pk)
  # --------------
  N_test <- nrow(test)
  # --------------
  if(fuzzy == T){
    Pertinencia_r <- function_new_membership_predict(test, M = M, MinimosDataFrame, Pertinencia, cols = cols)
    R_M_obs <- function_new_fuzzy_predict(retorno = Pertinencia_r, P, M)
  }else{
    R_M_obs <- t(data.frame(matrix(unlist(P), nrow=length(P), byrow=TRUE)))
  }
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

# ----------------
estimation_parameters_trape <- function(M, cols, dados){
  lapply(1:length(unique(M)), function(i) {
    lapply(1:cols, function(j) {
      SubSet <- dados[M == unique(M)[i], j]
      param <- getParametersTrapezoidal(SubSet)
      return(param)
    })
  })
}
# ----------------

# ----------------
density_values_trape <- function(M, cols, test, parametersC, pk){
  lapply(1:length(unique(M)), function(i) {
    densidades <- sapply(1:cols, function(j) {
      trapezoid::dtrapezoid(test[, j], min = parametersC[[i]][[j]][1],
                            mode1 = parametersC[[i]][[j]][2],
                            mode2 = parametersC[[i]][[j]][3],
                            max = parametersC[[i]][[j]][4]+1e-2)
      })
    densidades[which(is.na(densidades))] <- 0
    densidades <- apply(densidades, 1, prod)
    # Calcula a P(w_i) * P(X_k | w_i)
    p <- pk[[i]] * densidades
    # ---
    return(p)
  })

}
# ----------------



# --------------------------------------------------
getParametersTrapezoidal <- function(sample) {
  # -------------------------
  # sample length
  n <- length(sample)
  # -------------------------
  # min value
  minimum <- min(sample)
  # -------------------------
  # max value
  maximum <- max(sample)
  variance <- var(sample)
  m <- expected <- median.default(sample)
  # -------------------------
  model <- function(x) {
    # ----------
    # Expected value
    F1 <- ((-minimum * x[1] - x[1]^2 + maximum * x[2] + x[2]^2 + maximum^2 - minimum^2) /
             (3 * x[2] - 3 * x[1] + 3 * maximum - 3 * minimum)) - expected
    # ----------
    # Variance Value
    F2 <- (((6 * (x[2] - x[1])^4 + 12 * ((x[1] - minimum) + (maximum - x[2])) *
               (x[2] - x[1])^3 + (12 * ((x[1] - minimum) + (maximum - x[2]))^2 - 6 * (x[1] - minimum) * (maximum - x[2])) * (x[2] - x[1])^2) / (18 * ((x[1] - minimum) + 2 * (x[2] - x[1]) + (maximum - x[2]))^2)) +
             ((6 * ((x[1] - minimum) + (maximum - x[2])) * ((x[1] - minimum)^2 + (x[1] - minimum) * (maximum - x[2]) + (maximum - x[2])^2) * (x[2] - x[1]) +
                 ((x[1] - minimum) + (maximum - x[2]))^2 * ((x[1] - minimum)^2 + (x[1] - minimum) * (maximum - x[2]) + (maximum - x[2])^2)) / (18 * ((x[1] - minimum) + 2 * (x[2] - x[1]) + (maximum - x[2]))^2))) - variance
    # -------------------------
    # Return
    return(c(F1 = F1, F2 = F2))
    # -------------------------
  }
  # -------------------------
  # midpoint
  pmedio <- minimum + ((maximum - minimum) / 2)
  # --------
  ss <- rootSolve::multiroot(f = model, start = c(pmedio, pmedio + .1))
  ss <- c(ss$root[1], ss$root[2])
  # --------
  if (ss[1] > ss[2]) {
    # --------
    topo1 <- ss[2]
    topo2 <- ss[1]
    # --------
  } else {
    # --------
    topo1 <- ss[1]
    topo2 <- ss[2]
    # --------
  }
  # -------------------------
  # Verificando limites
  topo1 <- ifelse(topo1 < minimum, minimum, topo1)
  # ---
  topo2 <- ifelse(topo2 <= topo1, topo1,
                  ifelse(topo2 >= maximum, maximum, topo2)
  )
  # -------------------------
  # Parameters Return
  return(c(minimum, topo1, topo2, maximum))
}
