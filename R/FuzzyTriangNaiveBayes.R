#' Fuzzy Naive Bayes Triangular Classifier
#'
#' \code{FuzzyTriangularNaiveBayes} Fuzzy Naive Bayes Triangular Classifier
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
#' \insertRef{de2020online}{FuzzyClass}
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
#' fit_NBT <- FuzzyTriangularNaiveBayes(
#'   train = Train[, -5],
#'   cl = Train[, 5], cores = 2
#' )
#'
#' pred_NBT <- predict(fit_NBT, test)
#'
#' head(pred_NBT)
#' head(Test[, 5])
#'
#' @importFrom caTools sample.split
#'
#'
#' @export
FuzzyTriangularNaiveBayes <- function(train, cl, cores = 2, fuzzy = TRUE) {
  UseMethod("FuzzyTriangularNaiveBayes")
}

#' @export
FuzzyTriangularNaiveBayes.default <- function(train, cl, cores = 2, fuzzy = T) {

  # --------------------------------------------------------
  # Estimating class parameters
  train <- as.data.frame(train)
  cols <- ncol(train) # Number of variables
  if(is.null(cols)){
    cols <- 1
  }
  dados <- train # training data matrix
  M <- c(unlist(cl)) # true classes
  M <- factor(M, labels = sort(unique(M)))

  # --------------------------------------------------------
  # Estimating Triangular Parameters
  parametersC <- estimation_parameters_triang(M, cols, dados)
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
  class = "FuzzyTriangularNaiveBayes"
  )
}
# -------------------------


#' @export
print.FuzzyTriangularNaiveBayes <- function(x, ...) {
  if (x$fuzzy == T) {
    # -----------------
    cat("\nFuzzy Naive Bayes Triangular Classifier for Discrete Predictors\n\n")
    # -----------------
  } else {
    # -----------------
    cat("\nNaive Bayes Triangular Classifier for Discrete Predictors\n\n")
    # -----------------
  }
  cat("Class:\n")
  print(levels(x$M))
  # -----------------
}

#' @export
predict.FuzzyTriangularNaiveBayes <- function(object,
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
  P <- density_values_triang(M, cols, test, parametersC, pk)
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
    #R_M_obs <- matrix(R_M_obs,nrow = N_test)
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

# --------------------------------------------------
getParametersTriangular <- function(sample) {

  # --------------------------------------------------
  # Estimativa dos parametros a partir do m??todo dado em Werner's Blog
  # https://wernerantweiler.ca/blog.php?item=2019-06-05
  # Verificar uma alternativa mais confiavel e melhor citada na literatura
  # --------------------------------------------------
  qc <- sapply(1:ncol(sample), function(i) stats::quantile(sample[, i], probs = c(0.0625, 0.25, 0.75, 0.9375)))
  uc <- (qc[2, ] - qc[1, ])^2
  vc <- (qc[4, ] - qc[3, ])^2
  ac <- 2 * qc[1, ] - qc[2, ]
  bc <- 2 * qc[4, ] - qc[3, ]
  mc <- (uc * bc + vc * ac) / (uc + vc)
  if( any(ac == bc)){
    mc[which(ac==bc)] = ac[which(ac==bc)]
  }
  # ----------
  names(ac) <- colnames(sample)
  names(bc) <- colnames(sample)
  names(mc) <- colnames(sample)
  # -------------------------
  # Parameters Return
  return(rbind(ac, bc, mc))
}


# ----------------
density_values_triang <- function(M, cols, test, parametersC, pk){
  lapply(1:length(unique(M)), function(i) {
    densidades <- sapply(1:cols, function(j) {
      EnvStats::dtri(test[, j], min = parametersC[[i]][1, j], max = parametersC[[i]][2, j]+ 1e-7, mode = parametersC[[i]][3, j] + 1e-8)
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
estimation_parameters_triang <- function(M, cols, dados){
  lapply(1:length(unique(M)), function(i) {
    SubSet <- dados[M == unique(M)[i], ]
    getParametersTriangular(SubSet)
  })
}
# ----------------
