#' Fuzzy Beta Naive Bayes
#'
#' \code{FuzzyBetaNaiveBayes} Fuzzy Beta Naive Bayes
#'
#'
#' @param train matrix or data frame of training set cases.
#' @param cl factor of true classifications of training set
#' @param cores kept for backward compatibility; prediction is vectorized and does not start parallel workers (default = 2)
#' @param fuzzy boolean variable to use the membership function
#'
#' @param verbose logical; if `TRUE` (default, see [fuzzy_verbose()]) a message reports the training progress.
#'
#' @return A trained [FuzzyBetaNaiveBayesModel] R6 object (which inherits from [FuzzyClassifier]). Use `predict(object, newdata, type = "class")` or `object$predict(newdata)` to classify new observations; `type = "prob"` (or `"matrix"`) returns the matrix of class probabilities.
#'
#' @seealso [FuzzyBetaNaiveBayesModel] for the R6 interface and [fuzzy_naive_bayes()] (engine `"beta"`) for the tidymodels interface.
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
FuzzyBetaNaiveBayes <- function(train, cl, cores = 2, fuzzy = TRUE, verbose = fuzzy_verbose()) {
  FuzzyBetaNaiveBayesModel$new(fuzzy = fuzzy)$fit(train, cl, verbose = verbose)
}

#' @noRd
fit_FuzzyBetaNaiveBayes <- function(train, cl, cores = 2, fuzzy = TRUE) {

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
  col_max <- sapply(1:cols, function(j) max(dados[, j]))
  for(j in 1:cols){
    dados[, j] <- dados[, j] / (col_max[j] + 1e-2)
  }
  #--------------------------------------------------------
  # Estimating Beta Parameters
  parametersC <- estimation_parameters_beta(M, cols, dados)
  #--------------------------------------------------------

  #--------------------------------------------------------
  Sturges <- Sturges(dados, M);
  Comprim_Intervalo <- Comprim_Intervalo(dados, M, Sturges);
  minimos <- minimos(dados, M, cols);
  MinimosDataFrame <- minomosdt_function(minimos, M, Comprim_Intervalo, Sturges, cols)
  Frequencia <- Freq_esparsa(dados = dados,M = M, minomosdt = MinimosDataFrame, cols = cols)
  Pertinencia <- Pertinencia_esparsa(M = M, Frequencia, cols = cols)
  #------
  # A priori probability of classes - considered equal
  pk <- rep(1 / length(get_classes(M)), length(get_classes(M)))
  #-------------------------------------------------------


  #-------------------------------------------------------
  list(
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
    col_max = col_max,
    fuzzy = fuzzy
  )
}
#-------------------------


#' @noRd
predict_FuzzyBetaNaiveBayes <- function(object,
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
  col_max <- object$col_max
  if (is.null(col_max)) {
    col_max <- sapply(1:cols, function(j) max(test[, j]))
  }
  for(j in 1:cols){
    denom <- col_max[j] + 1e-2
    test[, j] <- pmin(pmax(test[, j] / denom, 1e-5), 1 - 1e-5)
  }
  #--------------------------------------------------------
  # Classification
  #--------------
  P <- density_values_beta(M, cols, test, parametersC, pk)
  #---------
  N_test <- nrow(test)
  # --
  # --
  #---------
  if(fuzzy == TRUE){
    Pertinencia_r <- function_new_membership_predict(test, M = M, MinimosDataFrame, Pertinencia, cols = cols)
    R_M_obs <- function_new_fuzzy_predict(retorno = Pertinencia_r, P, M)
  }else{
    R_M_obs <- do.call(cbind, P)
  }
  #---------
  if (type == "class") {
    #-------------------------
    pred_idx <- max.col(R_M_obs, ties.method = "first")
    resultado <- levels(M)[pred_idx]
    return(as.factor(resultado))
    #-------------------------
  } else {
    #-------------------------
    Infpos <- which(R_M_obs==Inf)
    R_M_obs[Infpos] <- .Machine$integer.max;
    R_M_obs <- as.matrix(R_M_obs)
    R_M_obs <- R_M_obs/rowSums(R_M_obs,na.rm = T)
    #----------
    colnames(R_M_obs) <- levels(M)
    return(R_M_obs)
    #-------------------------
  }
}

# ----------------
density_values_beta <- function(M, cols, test, parametersC, pk){
  classes <- levels(M)
  lapply(seq_along(classes), function(i) {
    dens_mat <- matrix(0, nrow = nrow(test), ncol = cols)
    for (j in seq_len(cols)) {
      dens_mat[, j] <- stats::dbeta(test[, j], shape1 = parametersC[[i]][[j]][1], shape2 = parametersC[[i]][[j]][2])
    }
    densidades <- dens_mat[, 1]
    if (cols > 1) {
      for (j in 2:cols) densidades <- densidades * dens_mat[, j]
    }
    pk[[i]] * densidades
  })
}
# ----------------

#' Fuzzy Beta Naive Bayes (R6 class)
#'
#' @description
#' R6 implementation of the Fuzzy Beta Naive Bayes classifier. Create the model with
#' `FuzzyBetaNaiveBayesModel$new()`, train it with `$fit(train, cl)` and classify new
#' observations with `$predict(newdata)`. The functional interface
#' [FuzzyBetaNaiveBayes()] trains and returns an object of this class.
#'
#' @examples
#' model <- FuzzyBetaNaiveBayesModel$new()
#' model$fit(train = iris[, -5] / 10, cl = iris$Species)
#' head(model$predict(iris[, -5] / 10))
#' head(model$predict(iris[, -5] / 10, type = "prob"))
#' model
#' @export
FuzzyBetaNaiveBayesModel <- R6::R6Class(
  classname = "FuzzyBetaNaiveBayes",
  inherit = FuzzyClassifier,
  public = list(
    #' @description Create a new (untrained) classifier.
    #' @param fuzzy See [FuzzyBetaNaiveBayes()].
    initialize = function(fuzzy = TRUE) {
      super$initialize(fuzzy = fuzzy)
    }
  ),
  private = list(
    label_fuzzy = "Fuzzy Beta Naive Bayes",
    label_crisp = "Beta Naive Bayes",
    estimate = function(train, cl) {
      do.call(fit_FuzzyBetaNaiveBayes, c(list(train = train, cl = cl), self$params))
    },
    classify = function(newdata, type) {
      predict_FuzzyBetaNaiveBayes(self$estimates, newdata, type = type)
    }
  )
)

