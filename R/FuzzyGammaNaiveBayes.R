#' Fuzzy Gamma Naive Bayes
#'
#' \code{FuzzyGammaNaiveBayes} Fuzzy Gamma Naive Bayes
#'
#'
#' @param train matrix or data frame of training set cases.
#' @param cl factor of true classifications of training set
#' @param cores kept for backward compatibility; prediction is vectorized and does not start parallel workers (default = 2)
#' @param fuzzy boolean variable to use the membership function
#'
#' @param verbose logical; if `TRUE` (default, see [fuzzy_verbose()]) a message reports the training progress.
#'
#' @return A trained [FuzzyGammaNaiveBayesModel] R6 object (which inherits from [FuzzyClassifier]). Use `predict(object, newdata, type = "class")` or `object$predict(newdata)` to classify new observations; `type = "prob"` (or `"matrix"`) returns the matrix of class probabilities.
#'
#' @seealso [FuzzyGammaNaiveBayesModel] for the R6 interface and [fuzzy_naive_bayes()] (engine `"gamma"`) for the tidymodels interface.
#'
#' @references
#' \insertRef{de2018fuzzy}{FuzzyClass}
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
#' fit_NBT <- FuzzyGammaNaiveBayes(
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
FuzzyGammaNaiveBayes <- function(train, cl, cores = 2, fuzzy = TRUE, verbose = fuzzy_verbose()) {
  FuzzyGammaNaiveBayesModel$new(fuzzy = fuzzy)$fit(train, cl, verbose = verbose)
}

#' @noRd
fit_FuzzyGammaNaiveBayes <- function(train, cl, cores = 2, fuzzy = T) {

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
    fuzzy = fuzzy
  )
}
# -------------------------


#' @noRd
predict_FuzzyGammaNaiveBayes <- function(object,
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
  P <- density_values_gamma(M, cols, test, parametersC, pk)

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
density_values_gamma <- function(M, cols, test, parametersC, pk) {
  classes <- levels(M)
  lapply(seq_along(classes), function(i) {
    if (cols == 1) {
      densidades <- stats::dgamma(test[[1]], shape = parametersC[[i]][[1]][1], scale = parametersC[[i]][[1]][2])
    } else {
      dens_mat <- matrix(0, nrow = nrow(test), ncol = cols)
      for (j in seq_len(cols)) {
        dens_mat[, j] <- stats::dgamma(test[[j]], shape = parametersC[[i]][[j]][1], scale = parametersC[[i]][[j]][2])
      }
      densidades <- dens_mat[, 1]
      for (j in 2:cols) densidades <- densidades * dens_mat[, j]
    }
    pk[i] * densidades
  })
}
# ----------------

#' Fuzzy Gamma Naive Bayes (R6 class)
#'
#' @description
#' R6 implementation of the Fuzzy Gamma Naive Bayes classifier. Create the model with
#' `FuzzyGammaNaiveBayesModel$new()`, train it with `$fit(train, cl)` and classify new
#' observations with `$predict(newdata)`. The functional interface
#' [FuzzyGammaNaiveBayes()] trains and returns an object of this class.
#'
#' @examples
#' model <- FuzzyGammaNaiveBayesModel$new()
#' model$fit(train = iris[, -5], cl = iris$Species)
#' head(model$predict(iris[, -5]))
#' head(model$predict(iris[, -5], type = "prob"))
#' model
#' @export
FuzzyGammaNaiveBayesModel <- R6::R6Class(
  classname = "FuzzyGammaNaiveBayes",
  inherit = FuzzyClassifier,
  public = list(
    #' @description Create a new (untrained) classifier.
    #' @param fuzzy See [FuzzyGammaNaiveBayes()].
    initialize = function(fuzzy = TRUE) {
      super$initialize(fuzzy = fuzzy)
    }
  ),
  private = list(
    label_fuzzy = "Fuzzy Gamma Naive Bayes",
    label_crisp = "Gamma Naive Bayes",
    estimate = function(train, cl) {
      do.call(fit_FuzzyGammaNaiveBayes, c(list(train = train, cl = cl), self$params))
    },
    classify = function(newdata, type) {
      predict_FuzzyGammaNaiveBayes(self$estimates, newdata, type = type)
    }
  )
)

