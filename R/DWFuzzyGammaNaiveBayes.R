#' Double Weighted Fuzzy Gamma Naive Bayes
#'
#' \code{DWFuzzyGammaNaiveBayes} Double Weighted Fuzzy Gamma Naive Bayes
#'
#'
#' @param train matrix or data frame of training set cases.
#' @param cl factor of true classifications of training set
#' @param cores kept for backward compatibility; prediction is vectorized and does not start parallel workers (default = 2)
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
  MinimosDataFrame <-  object$MinimosDataFrame
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
density_values_gamma_dw <- function(M, cols, test, parametersC, pk, wdelta, weta) {
  classes <- levels(M)
  lapply(seq_along(classes), function(i) {
    if (cols == 1) {
      densidades <- (stats::dgamma(test[[1]], shape = parametersC[[i]][[1]][1], scale = parametersC[[i]][[1]][2]))^(weta[1])
    } else {
      dens_mat <- matrix(0, nrow = nrow(test), ncol = cols)
      for (j in seq_len(cols)) {
        dens_mat[, j] <- (stats::dgamma(test[[j]], shape = parametersC[[i]][[j]][1], scale = parametersC[[i]][[j]][2]))^(weta[j])
      }
      densidades <- dens_mat[, 1]
      for (j in 2:cols) densidades <- densidades * dens_mat[, j]
    }
    (pk[i]^(wdelta[i])) * densidades
  })
}
# ----------------
