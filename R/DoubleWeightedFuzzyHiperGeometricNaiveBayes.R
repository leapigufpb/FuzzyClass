#' Double Weighted Fuzzy Hipergeometric Naive Bayes
#'
#' \code{DWFuzzyHipergeometricNaiveBayes} Double Weighted Fuzzy Hipergeometric Naive Bayes
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
#' \insertRef{ferreira2025new}{FuzzyClass}
#'
#' @importFrom stats dhyper
#'
#' @examples
#' 
#' set.seed(1)
#' 
#' substituir_zero <- function(x) {
#'   if (x == 0) {
#'     while(1){
#'      new_valor <- rhyper(1,3,30,10)
#'       if(new_valor != 0){
#'         return(new_valor)
#'       }
#'     }
#'   } else {
#'     return(x)
#'   }
#' }
#' 
#' #Building dataframe
#' class1 <- data.frame(rhyper(72,3,30,10), 
#'                      sample(seq(1,5),72,replace = TRUE), class = "Bem treinado")
#' colnames(class1)[1] <- "sucessos"
#' class1$sucessos <- sapply(class1$sucessos, substituir_zero)
#' colnames(class1)[2] <- "tentativas"
#' colnames(class1)[3] <- "avaliação"
#' class2 <- data.frame(rhyper(72,3,17,12), 
#'                      sample(seq(4,7),72,replace = TRUE), class = "Mediano")
#' colnames(class2)[1] <- "sucessos"
#' #class2$sucessos <- ifelse(class2$sucessos == 0, 2, class2$sucessos)
#' colnames(class2)[2] <- "tentativas"
#' colnames(class2)[3] <- "avaliação"
#' class3 <- data.frame(rhyper(72,3,9,11),
#'                      sample(seq(7,10),72,replace = TRUE), class = "Precisa treinar")
#' colnames(class3)[1] <- "sucessos"
#' #class3$sucessos <- ifelse(class3$sucessos == 0, 3, class3$sucessos)
#' colnames(class3)[2] <- "tentativas"
#' colnames(class3)[3] <- "avaliação"
#' data <- rbind(class1,class2,class3)
#'
#' # Weights
#' weta1 <- c(0.22, 0.33, 0.45)
#' wdelta1 <- c(0.33,0.33,0.33) 
#' 
#' #spliting dataframe
#' split <- caTools::sample.split(t(data[, 1]), SplitRatio = 0.7)
#' Train <- subset(data, split == "TRUE")
#' Test <- subset(data, split == "FALSE")
#' 
#' test <- Test[, -3]
#' fit_NHT <- DWFuzzyHipergeometricNaiveBayes(
#'  train = Train[, -3],
#'  cl = Train[, 3], cores = 2,
#'  fuzzy = TRUE,
#'  wdelta = wdelta1,
#'  weta = weta1)
#' 
#' pred_NHT <- predict(fit_NHT, test)
#'
#'
#'
#' @export
DWFuzzyHipergeometricNaiveBayes <- function(train, cl, cores = 2, fuzzy = TRUE, wdelta, weta) {
  UseMethod("DWFuzzyHipergeometricNaiveBayes")
}

#------------------------------

#' @export
DWFuzzyHipergeometricNaiveBayes.default <- function(train, cl, cores = 2, fuzzy = T, wdelta, weta) {
  
  #--------------------------------------------------------
  # Estimating class parameters
  
  train <- as.data.frame(train)
  cols <- ncol(train)
  if (is.null(cols) || cols < 2 || cols %% 2 != 0) {
    stop("Inconsistent number of columns: train must have an even number of columns (successes and attempts pairs)")
  }
  dados <- train
  M <- c(unlist(cl))
  M <- factor(M, labels = sort(unique(M)))

  if (sum(verifyNumbersFunction(dados, cols)) != cols) {
    stop("All variables must be discrete values.")
  }

  # Splitting attempts and successes
  tentativas <- as.data.frame(dados[, seq(2, cols, by = 2), drop = FALSE])
  dados <- as.data.frame(dados[, seq(1, cols, by = 2), drop = FALSE])
  cols <- cols / 2
  
  #--------------------------------------------------------
 
  # Estimating Parameters
  parametersC <- estimation_parameters_hipergeomeric(M, cols, dados, tentativas) 
 
  #--------------------------------------------------------
   
  # --------------------------------------------------------
  Sturges <- Sturges(dados, M);
  Comprim_Intervalo <- Comprim_Intervalo(dados, M, Sturges);
  minimos <- minimos(dados, M, cols);
  MinimosDataFrame <- minomosdt_function(minimos, M, Comprim_Intervalo, Sturges, cols)
  Frequencia <- Freq_esparsa(dados = dados,M = M, minomosdt = MinimosDataFrame, cols = cols)
  Pertinencia <- Pertinencia_esparsa(M = M, Frequencia, cols = cols)
  # ------
  # A priori probability of classes - considered equal
  pk <- rep(1 / length(get_classes(M)), length(get_classes(M)))
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
  class = "DWFuzzyHipergeometricNaiveBayes"
  )
}
#-------------------------


#' @export
print.DWFuzzyHipergeometricNaiveBayes <- function(x, ...) {
  if (x$fuzzy == T) {
    #-----------------
    cat("\nDouble Weighted Fuzzy Hipergeometric Naive Bayes Classifier for Discrete Predictors\n\n")
    #-----------------
  } else {
    #-----------------
    cat("\nDouble Weighted Hipergeometric Naive Bayes Classifier for Discrete Predictors\n\n")
    #-----------------
  }
  cat("Class:\n")
  print(levels(x$M))
  #-----------------
}

#' @export
predict.DWFuzzyHipergeometricNaiveBayes <- function(object,
                                                  newdata,
                                                  type = "class",
                                                  ...) {
  #--------------------------------------------------------
  test <- as.data.frame(newdata)
  if (ncol(test) == 2 * object$cols) {
    test <- as.data.frame(test[, seq(1, ncol(test), by = 2), drop = FALSE])
  } else {
    test <- as.data.frame(test[, seq_len(object$cols), drop = FALSE])
  }
  #--------------------------------------------------------
  parametersC <- object$parametersC
  minimos <- object$minimos
  MinimosDataFrame <- object$MinimosDataFrame
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
  #--------------------------------------------------------
  
  #--------------------------------------------------------
  # Classification
  N_test <- nrow(test)
  if (fuzzy == TRUE) {
    Pertinencia_r <- function_new_membership_predict(test, M = M, MinimosDataFrame, Pertinencia, cols = cols)
    R_M_obs <- fuzzy_density_values_hipergeometric_dw(M = M, cols = cols,
                                                      test = test, parametersC = parametersC,
                                                      pk = pk, wdelta = wdelta,
                                                      weta = weta,
                                                      Pertinencia_r = Pertinencia_r)
  } else {
    R_M_obs <- density_values_hipergeometric_dw(M = M, cols = cols,
                                                test = test, parametersC = parametersC,
                                                pk = pk, wdelta = wdelta,
                                                weta = weta)
  }
  # ---------
  if (type == "class") {
    pred_idx <- max.col(as.matrix(R_M_obs), ties.method = "first")
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
# -------------------------------------------------------
density_values_hipergeometric_dw <- function(M, cols, test, parametersC, pk, wdelta, weta) {
  classes <- levels(M)
  n_classes <- length(classes)
  N_test <- nrow(test)
  saida <- matrix(0, nrow = N_test, ncol = n_classes)
  
  for (i in seq_len(n_classes)) {
    dens_mat <- matrix(1, nrow = N_test, ncol = cols)
    for (j in seq_len(cols)) {
      t <- round(test[[j]])
      k_val <- parametersC[[i]][[j]][1]
      m_val <- parametersC[[i]][[j]][2]
      n_val <- parametersC[[i]][[j]][3]
      dens_mat[, j] <- (stats::dhyper(t, m = m_val, n = n_val, k = k_val))^(weta[j])
    }
    densidades <- dens_mat[, 1]
    if (cols > 1) {
      for (j in 2:cols) densidades <- densidades * dens_mat[, j]
    }
    saida[, i] <- (pk[i]^(wdelta[i])) * densidades
  }
  colnames(saida) <- classes
  return(as.data.frame(saida))
}
# ----------------

# ----------------
fuzzy_density_values_hipergeometric_dw <- function(M, cols, test, parametersC, pk, wdelta, weta, Pertinencia_r) {
  classes <- levels(M)
  n_classes <- length(classes)
  N_test <- nrow(test)
  saida <- matrix(0, nrow = N_test, ncol = n_classes)
  
  for (i in seq_len(n_classes)) {
    dens_mat <- matrix(1, nrow = N_test, ncol = cols)
    pert <- if (is.list(Pertinencia_r)) Pertinencia_r[[i]] else Pertinencia_r
    for (j in seq_len(cols)) {
      t <- round(test[[j]])
      k_val <- parametersC[[i]][[j]][1]
      m_val <- parametersC[[i]][[j]][2]
      n_val <- parametersC[[i]][[j]][3]
      dens_mat[, j] <- (stats::dhyper(t, m = m_val, n = n_val, k = k_val) * pert)^(weta[j])
    }
    densidades <- dens_mat[, 1]
    if (cols > 1) {
      for (j in 2:cols) densidades <- densidades * dens_mat[, j]
    }
    saida[, i] <- (pk[i]^(wdelta[i])) * densidades
  }
  colnames(saida) <- classes
  return(as.data.frame(saida))
}
# ----------------