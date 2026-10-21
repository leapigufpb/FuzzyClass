#' Fuzzy Poisson Naive Bayes
#'
#' \code{FuzzyPoissonNaiveBayes} Fuzzy Poisson Naive Bayes
#'
#'
#' @param train matrix or data frame of training set cases.
#' @param cl factor of true classifications of training set
#' @param cores kept for backward compatibility; prediction is vectorized and does not start parallel workers (default = 2)
#' @param fuzzy boolean variable to use the membership function
#'
#' @param verbose logical; if `TRUE` (default, see [fuzzy_verbose()]) a message reports the training progress.
#'
#' @return A trained [FuzzyPoissonNaiveBayesModel] R6 object (which inherits from [FuzzyClassifier]). Use `predict(object, newdata, type = "class")` or `object$predict(newdata)` to classify new observations; `type = "prob"` (or `"matrix"`) returns the matrix of class probabilities.
#'
#' @seealso [FuzzyPoissonNaiveBayesModel] for the R6 interface and [fuzzy_naive_bayes()] (engine `"poisson"`) for the tidymodels interface.
#'
#' @references
#' \insertRef{moraes2015fuzzy}{FuzzyClass}
#'
#' @examples
#'
#' set.seed(1) # determining a seed
#' class1 <- data.frame(vari1 = rpois(100,lambda = 2),
#'                      vari2 = rpois(100,lambda = 2),
#'                      vari3 = rpois(100,lambda = 2), class = 1)
#' class2 <- data.frame(vari1 = rpois(100,lambda = 1),
#'                      vari2 = rpois(100,lambda = 1),
#'                      vari3 = rpois(100,lambda = 1), class = 2)
#' class3 <- data.frame(vari1 = rpois(100,lambda = 5),
#'                      vari2 = rpois(100,lambda = 5),
#'                      vari3 = rpois(100,lambda = 5), class = 3)
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
#' fit_NBT <- FuzzyPoissonNaiveBayes(
#'   train = Train[, -4],
#'   cl = Train[, 4], cores = 2
#' )
#'
#' pred_NBT <- predict(fit_NBT, test)
#'
#' head(pred_NBT)
#' head(Test[, 4])
#' @importFrom stats dpois
#'
#' @export
FuzzyPoissonNaiveBayes <- function(train, cl, cores = 2, fuzzy = TRUE, verbose = fuzzy_verbose()) {
  FuzzyPoissonNaiveBayesModel$new(fuzzy = fuzzy)$fit(train, cl, verbose = verbose)
}

#' @noRd
fit_FuzzyPoissonNaiveBayes <- function(train, cl, cores = 2, fuzzy = T) {

  # --------------------------------------------------------
  # Estimating class parameters
  p_data <- predata(train,cl)
  # --
  dados <- train <-  p_data$train
  cols <- p_data$cols
  M <- p_data$M
  intervalos <- p_data$intervalos
  # --------------------------------------------------------
  # Verify if all variables are discrete
  if (sum(verifyNumbersFunction(dados, cols)) != cols) {
    stop("All variables must be discrete values.")
  }


  # --------------------------------------------------------
  # Estimating Parameters
  parametersC <- estimation_parameters_poi(M, cols, dados)
  # --------------------------------------------------------
  Sturges <- Sturges_discrete(dados, M);
  Comprim_Intervalo <- Comprim_Intervalo_discrete(dados, M);
  minimos <- minimos(dados, M, cols);
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
predict_FuzzyPoissonNaiveBayes <- function(object,
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
  P <- density_values_poi(M, cols, test, parametersC, pk)
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
density_values_poi <- function(M, cols, test, parametersC, pk) {
  classes <- levels(M)
  lapply(seq_along(classes), function(i) {
    if (cols == 1) {
      densidades <- stats::dpois(round(test[[1]]), lambda = parametersC[[i]][[1]][1])
    } else {
      dens_mat <- matrix(0, nrow = nrow(test), ncol = cols)
      for (j in seq_len(cols)) {
        dens_mat[, j] <- stats::dpois(round(test[[j]]), lambda = parametersC[[i]][[j]][1])
      }
      densidades <- dens_mat[, 1]
      for (j in 2:cols) densidades <- densidades * dens_mat[, j]
    }
    pk[i] * densidades
  })
}
# ----------------

#' Fuzzy Poisson Naive Bayes (R6 class)
#'
#' @description
#' R6 implementation of the Fuzzy Poisson Naive Bayes classifier. Create the model with
#' `FuzzyPoissonNaiveBayesModel$new()`, train it with `$fit(train, cl)` and classify new
#' observations with `$predict(newdata)`. The functional interface
#' [FuzzyPoissonNaiveBayes()] trains and returns an object of this class.
#'
#' @examples
#' model <- FuzzyPoissonNaiveBayesModel$new()
#' x <- round(iris[, -5])
#' model$fit(train = x, cl = iris$Species)
#' head(model$predict(x))
#' head(model$predict(x, type = "prob"))
#' model
#' @export
FuzzyPoissonNaiveBayesModel <- R6::R6Class(
  classname = "FuzzyPoissonNaiveBayes",
  inherit = FuzzyClassifier,
  public = list(
    #' @description Create a new (untrained) classifier.
    #' @param fuzzy See [FuzzyPoissonNaiveBayes()].
    initialize = function(fuzzy = TRUE) {
      super$initialize(fuzzy = fuzzy)
    }
  ),
  private = list(
    label_fuzzy = "Fuzzy Poisson Naive Bayes",
    label_crisp = "Poisson Naive Bayes",
    estimate = function(train, cl) {
      do.call(fit_FuzzyPoissonNaiveBayes, c(list(train = train, cl = cl), self$params))
    },
    classify = function(newdata, type) {
      predict_FuzzyPoissonNaiveBayes(self$estimates, newdata, type = type)
    }
  )
)

