#' Fuzzy Bayes Rule
#'
#' \code{FuzzyBayesRule} Fuzzy Bayes Rule
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
  P <- density_values_multivariate_normal(M, cols, test, parametersC, pk)

  # ---------
  N_test <- nrow(test)
  # --
  #test <- split(test, seq(nrow(test)))
  # --
  # ---------
  if(fuzzy == TRUE){
    Pertinencia_r <- function_new_membership_predict(test, M = M, MinimosDataFrame, Pertinencia, cols = cols)
    R_M_obs <- function_new_fuzzy_predict(retorno = Pertinencia_r, P, M)
  }else{
    R_M_obs <- do.call(cbind, P)
  }
  # ---------
  scores_to_output(R_M_obs, levels(M), type)
}

# -------------------------------------------------------
# Functions

# ----------------
density_values_multivariate_normal <- function(M, cols, test, parametersC, pk){
  classes <- levels(M)
  lapply(seq_along(classes), function(i) {
    densidades <- mvtnorm::dmvnorm(test, mean = parametersC[[i]][[1]], sigma = parametersC[[i]][[2]])
    pk[[i]] * densidades
  })
}
# ----------------
