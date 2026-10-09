#' Fuzzy Naive Bayes Triangular Classifier
#'
#' \code{FuzzyTriangularNaiveBayes} Fuzzy Naive Bayes Triangular Classifier
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
  p_data <- predata(train,cl)
  # --
  train <-  p_data$train
  cols <- p_data$cols
  dados <- p_data$dados
  M <- p_data$M
  intervalos <- p_data$intervalos
  #--------------------------------------------------------

  # --------------------------------------------------------
  # Estimating Triangular Parameters
  parametersC <- estimation_parameters_triang(M, cols, dados)
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
  pk <- rep(1 / length(get_classes(M)), length(get_classes(M)))
  # -------------------------------------------------------


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
  # --------------------------------------------------------
  # Classification
  # --------------
  P <- density_values_triang(M, cols, test, parametersC, pk)
  # ---------
  N_test <- nrow(test)
  if (fuzzy == TRUE) {
    Pertinencia_r <- function_new_membership_predict(test, M = M, MinimosDataFrame, Pertinencia, cols = cols)
    R_M_obs <- function_new_fuzzy_predict(retorno = Pertinencia_r, P, M)
  } else {
    R_M_obs <- do.call(cbind, P)
  }
  # ---------
  if (type == "class") {
    pred_idx <- max.col(R_M_obs, ties.method = "first")
    resultado <- levels(M)[pred_idx]
    return(as.factor(resultado))
  } else {
    Infpos <- which(R_M_obs == Inf)
    R_M_obs[Infpos] <- .Machine$integer.max
    R_M_obs <- as.matrix(R_M_obs)
    rs <- rowSums(R_M_obs, na.rm = TRUE)
    rs[rs == 0] <- 1
    R_M_obs <- R_M_obs / rs
    colnames(R_M_obs) <- levels(M)
    return(R_M_obs)
  }
}

# -------------------------------------------------------
# Functions

# ----------------
density_values_triang <- function(M, cols, test, parametersC, pk) {
  classes <- levels(M)
  lapply(seq_along(classes), function(i) {
    if (cols == 1) {
      densidades <- pmax(EnvStats::dtri(test[[1]], min = parametersC[[i]][1, 1], max = parametersC[[i]][2, 1] + 1e-7, mode = parametersC[[i]][3, 1] + 1e-8), DENS_FLOOR)
    } else {
      dens_mat <- matrix(0, nrow = nrow(test), ncol = cols)
      for (j in seq_len(cols)) {
        dens_mat[, j] <- pmax(EnvStats::dtri(test[[j]], min = parametersC[[i]][1, j], max = parametersC[[i]][2, j] + 1e-7, mode = parametersC[[i]][3, j] + 1e-8), DENS_FLOOR)
      }
      densidades <- dens_mat[, 1]
      for (j in 2:cols) densidades <- densidades * dens_mat[, j]
    }
    pk[i] * densidades
  })
}
# ----------------
