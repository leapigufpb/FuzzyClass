#' Fuzzy Naive Bayes
#'
#' \code{FuzzyNaiveBayes} Fuzzy Naive Bayes
#'
#'
#' @param train matrix or data frame of training set cases
#' @param cl factor of true classifications of training set
#' @param fuzzy boolean variable to use the membership function
#' @param m is M/N, where M is the number of classes and N is the number of train lines
#' @param Pi is 1/M, where M is the number of classes
#'
#' @param verbose logical; if `TRUE` (default, see [fuzzy_verbose()]) a message reports the training progress.
#'
#' @return A trained [FuzzyNaiveBayesModel] R6 object (which inherits from [FuzzyClassifier]). Use `predict(object, newdata, type = "class")` or `object$predict(newdata)` to classify new observations; `type = "prob"` (or `"matrix"`) returns the matrix of class probabilities.
#'
#' @seealso [FuzzyNaiveBayesModel] for the R6 interface and [fuzzy_naive_bayes()] (engine `"naive"`) for the tidymodels interface.
#'
#' @references
#' \insertRef{moraes2009another}{FuzzyClass}
#'
#' @examples
#'
#' set.seed(1) # determining a seed
#' data(HouseVotes84)
#'
#' # Splitting into Training and Testing
#' split <- caTools::sample.split(t(HouseVotes84[, 1]), SplitRatio = 0.7)
#' Train <- subset(HouseVotes84, split == "TRUE")
#' Test <- subset(HouseVotes84, split == "FALSE")
#' # ----------------
#' # matrix or data frame of test set cases.
#' # A vector will be interpreted as a row vector for a single case.
#' test <- Test[, -1]
#' fit_FNB <- FuzzyNaiveBayes(
#'   train = Train[, -1],
#'   cl = Train[, 1]
#' )
#'
#' pred_FNB <- predict(fit_FNB, test)
#'
#' head(pred_FNB)
#' head(Test[, 1])
#'
#' @importFrom stats model.extract na.pass sd terms predict
#'
#' @export
FuzzyNaiveBayes <- function(train, cl, fuzzy = TRUE, m = NULL, Pi = NULL, verbose = fuzzy_verbose()) {
  FuzzyNaiveBayesModel$new(fuzzy = fuzzy, m = m, Pi = Pi)$fit(train, cl, verbose = verbose)
}

#' @noRd
fit_FuzzyNaiveBayes <- function(train, cl, fuzzy = TRUE, m = NULL, Pi = NULL) {

  if(fuzzy==F){
    modelo <- e1071::naiveBayes(as.data.frame(train), factor(cl))

    modelo
    #structure(modelo,
    #class = "naiveBayes"
    #)


  }else{
  

  # --------------------------------------------------------
  # Estimating class parameters
  train <- as.data.frame(train)
  cols <- ncol(train) # Number of variables
  if(is.null(cols) |  length(cols) == 0){
    cols <- 1
  }
  dados <- train # training data matrix
  M <- c(unlist(cl)) # true classes
  M <- droplevels(factor(M))
  # --

  # --
  # converting character to factor
  typescolumns <- sapply(1:cols, function(i) is.character(train[, i]))
  columnscharac <- which(typescolumns)
  if (length(columnscharac) > 0) {
    train[columnscharac] <- lapply(train[columnscharac, drop = FALSE], as.factor)
  }
  # --


  # --
  res <- sapply(1:cols, function(i) {is.factor(train[,i])})
  if(sum(res) == cols){
    resul <- fit_FuzzyNaiveBayes_categorical(train, cl, fuzzy = T, m = m, Pi = Pi)
  }
  # --
  res <- sapply(1:cols, function(i) {is.numeric(train[,i])})
  if(sum(res) == cols){
    resul <- fit_FuzzyNaiveBayes_continuous(train, cl, fuzzy = T, m = m, Pi = Pi)
  }
  if (!exists("resul", inherits = FALSE)) {
    cli::cli_abort("All predictors must be either categorical (factor/character) or numeric.")
  }

  # -------------------------------------------------------
  list(
    parametersC = resul$parametersC,
    cols = resul$cols,
    M = resul$M,
    Pertinencia = resul$Pertinencia,
    pk = resul$pk,
    fuzzy = resul$fuzzy,
    Intervalos_Valores = resul$Intervalos_Valores,
    model = resul$model
  )

  }

}
# -------------------------


#' @noRd
predict_FuzzyNaiveBayes <- function(object,
                                   newdata,
                                   type = "class",
                                   ...) {
  # --------------------------------------------------------
  test <- as.data.frame(newdata)
  # --------------------------------------------------------
  # fuzzy = FALSE: classic naive Bayes estimated by e1071
  if (inherits(object, "naiveBayes")) {
    return(predict(object, test, type = if (type == "class") "class" else "raw"))
  }
  # --------------------------------------------------------
  parametersC <- object$parametersC
  cols <- object$cols
  M <- object$M
  Pertinencia <- object$Pertinencia
  pk <- object$pk
  fuzzy <- object$fuzzy
  model = object$model
  Intervalo_Valores = object$Intervalos_Valores
  # --------------------------------------------------------

  # --------------------------------------------------------
  # Classification
  # --------------
  if ((fuzzy == F) & (type == "class") & is.factor(test[,1])) {
    class(model) <- "naiveBayes"
    R_M_obs <- predict(model, test)
  } else {
    if ((fuzzy == F) & is.factor(test[,1])) {
      class(model) <- "naiveBayes"
      R_M_obs <- predict(model, test, type = "raw")
    }
  }


  if ((fuzzy == T) & is.factor(test[,1]) ) {
    R_M_obs <- predict_categorical(test, M, cols, parametersC, pk, Pertinencia, type )
  } else {
    if( (fuzzy == T) & is.numeric(test[,1]) ){
      R_M_obs <- predict_continuo(test, M, cols, parametersC, pk, Pertinencia, Intervalo_Valores, type )
    } else {
      if( (fuzzy == F) & is.numeric(test[,1]) ){
        R_M_obs <- predict_continuo_nonfuzzy(test, M, cols, parametersC, pk, Pertinencia, Intervalo_Valores, type )
      }
    }
  }

  return(R_M_obs)

}


#' @noRd
fit_FuzzyNaiveBayes_categorical <- function(train, cl, fuzzy = T, m = NULL, Pi = NULL) {

  # --------------------------------------------------------
  # Estimating class parameters
  train <- as.data.frame(train)
  cols <- ncol(train) # Number of variables
  if(is.null(cols)){
    cols <- 1
  }
  dados <- train # training data matrix
  M <- c(unlist(cl)) # true classes
  M <- droplevels(factor(M))

  # --------------------------------------------------------
  res <- sapply(1:cols, function(i) {is.factor(train[,i])})
  if(sum(res) != cols){ stop("All variables must be categorical (factor).") }
  # --------------------------------------------------------

    N <- nrow(dados)
  # --------------------------------------------------------

  # --------------------------------------------------------
  # Estimating Parameters
  if(is.null(m) & is.null(Pi)){ m <- length(unique(cl))/nrow(dados); Pi <-  1/m}
  # ---
  # List: variable x target
  parametersC <- lapply(1:length(get_classes(M)), function(i) {
    lapply(1:cols, function(j) {
      SubSet <- dados[M == get_classes(M)[i], j]
      param <- (table(SubSet) + m*Pi)/(sum(table(SubSet)) + m)
      return(param)
    })
  })

  model <- e1071::naiveBayes(x = dados, y = cl)
  # --------------------------------------------------------

  #--------------------------------------------------------
  Pertinencia <- lapply(1:length(get_classes(M)), function(i) {
    lapply(1:cols, function(j) {
      SubSet <- dados[M == get_classes(M)[i], j]
      param <- (table(SubSet))/(sum(table(SubSet)))
      return(param)
    })
  })
  #------
  # A priori probability of classes - considered equal
  pk <- rep(1 / length(get_classes(M)), length(get_classes(M)))
  # -------------------------------------------------------


  # -------------------------------------------------------
  return(list(
    parametersC = parametersC,
    cols = cols,
    M = M,
    Pertinencia = Pertinencia,
    pk = pk,
    fuzzy = fuzzy,
    Intervalos_Valores = NULL,
    model = model
  ))

}
# -------------------------

# -------------------------
#' @noRd
fit_FuzzyNaiveBayes_continuous <- function(train, cl, fuzzy = T, m = NULL, Pi = NULL) {

  # --------------------------------------------------------
  # Estimating class parameters
  train <- as.data.frame(train)
  cols <- ncol(train) # Number of variables
  if(is.null(cols) | length(cols) == 0){
    cols <- 1
  }
  dados <- train
  M <- c(unlist(cl)) # true classes
  M <- droplevels(factor(M))
  # --
  res <- sapply(1:cols, function(i) {is.numeric(train[,i])})
  if(sum(res) != cols){ stop("All variables must be numerical.") }
  # --
  N <- nrow(dados)
  # --------------------------------------------------------

  # --------------------------------------------------------
  # Estimating Parameters
  if(is.null(m) & is.null(Pi)){ m <- length(unique(cl))/nrow(dados); Pi <-  1/m}
  # --------------------------------------------------------

  #--------------------------------------------------------
  Sturges <- Sturges(dados, M);
  Comprim_Intervalo <- Comprim_Intervalo(dados, M, Sturges);
  minimos <- minimos(dados, M, cols);
  Freq <- Freq(dados, M, Comprim_Intervalo, Sturges, minimos, cols);
  Intervalos_Valores <- Intervalos_Valores(dados, M, Comprim_Intervalo, Sturges, minimos, cols);
  #------
  parametersC <- c()
  for(i in 1:length(get_classes(M))){
    parametersC[[i]] <- apply(Freq[[i]], 2, function(x) ( (x + m*Pi) / ( sum(x) +m ) ) )
  }

  #------
  Pertinencia <- Pertinencia(Freq, dados, M);
  #------
  # A priori probability of classes - considered equal
  pk <- rep(1 / length(get_classes(M)), length(get_classes(M)))
  # -------------------------------------------------------

  model <- parametersC

  # -------------------------------------------------------
  return(list(
    parametersC = parametersC,
    cols = cols,
    M = M,
    Pertinencia = Pertinencia,
    pk = pk,
    fuzzy = fuzzy,
    Intervalos_Valores = Intervalos_Valores,
    model = model
  ))

}
# -------------------------

# -------------------------
predict_categorical <- function(test, M, cols, parametersC, pk, Pertinencia, type) {

  N <- nrow(test)
  classes <- get_classes(M)
  n_classes <- length(classes)

  P <- lapply(seq_len(n_classes), function(i) {
    dens_mat <- matrix(1, nrow = N, ncol = cols)
    for (j in seq_len(cols)) {
      probs_j <- parametersC[[i]][[j]][as.character(test[[j]])]
      probs_j[is.na(probs_j)] <- 1e-5
      dens_mat[, j] <- as.numeric(probs_j)
    }
    densidades <- dens_mat[, 1]
    if (cols > 1) {
      for (j in 2:cols) densidades <- densidades * dens_mat[, j]
    }
    pk[[i]] * densidades
  })

  P_fuzzy <- lapply(seq_len(n_classes), function(i) {
    dens_mat <- matrix(1, nrow = N, ncol = cols)
    for (j in seq_len(cols)) {
      probs_j <- Pertinencia[[i]][[j]][as.character(test[[j]])]
      probs_j[is.na(probs_j)] <- 1e-5
      dens_mat[, j] <- as.numeric(probs_j)
    }
    densidades <- dens_mat[, 1]
    if (cols > 1) {
      for (j in 2:cols) densidades <- densidades * dens_mat[, j]
    }
    pk[[i]] * densidades
  })

  R_M_obs <- matrix(0, nrow = N, ncol = n_classes)
  for (i in seq_len(n_classes)) {
    R_M_obs[, i] <- P[[i]] * P_fuzzy[[i]]
  }

  if (type == "class") {
    pred_idx <- max.col(R_M_obs, ties.method = "first")
    resultado <- classes[pred_idx]
    return(as.factor(resultado))
  } else {
    Infpos <- which(R_M_obs == Inf)
    R_M_obs[Infpos] <- .Machine$integer.max
    rs <- rowSums(R_M_obs, na.rm = TRUE)
    rs[rs == 0] <- 1
    R_M_obs <- R_M_obs / rs
    colnames(R_M_obs) <- classes
    return(R_M_obs)
  }
}
# -------------------------


# -------------------------
predict_continuo <- function(test, M, cols, parametersC, pk, Pertinencia, Intervalo_Valores, type  ) {

  N <- nrow(test)
  N_test <- N
  classes <- get_classes(M)
  # --
  P <- lapply(1:length(classes), function(i) {
    densidades <- sapply(1:cols, function(j) {
      cuts <- as.numeric(sapply(strsplit(Intervalo_Valores[[i]][, j], ","), `[`, 2))
      pos <- pmin(pmax(findInterval(test[, j], cuts, left.open = FALSE) + 1L, 1L), length(cuts))
      vals <- parametersC[[i]][, j][pos]
      vals[is.na(vals) | is.na(test[, j])] <- 1e-5
      return(vals)
    })
    if (cols > 1) {
      res <- densidades[, 1]
      for (c_idx in 2:cols) res <- res * densidades[, c_idx]
      res
    } else {
      as.numeric(densidades[, 1])
    } * pk[[i]]
  })
  # --

  # --
  P_fuzzy <- lapply(1:length(classes), function(i) {
    densidades <- sapply(1:cols, function(j) {
      cuts <- as.numeric(sapply(strsplit(Intervalo_Valores[[i]][, j], ","), `[`, 2))
      pos <- pmin(pmax(findInterval(test[, j], cuts, left.open = FALSE) + 1L, 1L), length(cuts))
      vals <- Pertinencia[[i]][, j][pos]
      vals[is.na(vals) | is.na(test[, j])] <- 1e-5
      return(vals)
    })
    if (cols > 1) {
      res <- densidades[, 1]
      for (c_idx in 2:cols) res <- res * densidades[, c_idx]
      res
    } else {
      as.numeric(densidades[, 1])
    } * pk[[i]]
  })
  # --

  # --
  R_M_obs <- matrix(0, nrow = N_test, ncol = length(classes))
  for (i in 1:length(P)) {
    R_M_obs[, i] <- P[[i]] * P_fuzzy[[i]]
  }
  # --
  # ---------
  if (type == "class") {
    # -------------------------
    pred_idx <- max.col(R_M_obs, ties.method = "first")
    resultado <- classes[pred_idx]
    return(as.factor(resultado))
    # -------------------------
  } else {
    # -------------------------
    Infpos <- which(R_M_obs == Inf)
    R_M_obs[Infpos] <- .Machine$integer.max
    R_M_obs <- matrix(unlist(R_M_obs), ncol = length(classes), nrow = N_test)
    R_M_obs <- R_M_obs / rowSums(R_M_obs, na.rm = TRUE)
    # ----------
    colnames(R_M_obs) <- classes
    return(R_M_obs)
    # -------------------------
  }
}
# -------------------------

# -------------------------
predict_continuo_nonfuzzy <- function(test, M, cols, parametersC, pk, Pertinencia, Intervalo_Valores, type  ) {

  N <- nrow(test)
  N_test <- N
  classes <- get_classes(M)
  # --
  P <- lapply(1:length(classes), function(i) {
    densidades <- sapply(1:cols, function(j) {
      cuts <- as.numeric(sapply(strsplit(Intervalo_Valores[[i]][, j], ","), `[`, 2))
      pos <- pmin(pmax(findInterval(test[, j], cuts, left.open = FALSE) + 1L, 1L), length(cuts))
      vals <- parametersC[[i]][, j][pos]
      vals[is.na(vals) | is.na(test[, j])] <- 1e-5
      return(vals)
    })
    if (cols > 1) {
      res <- densidades[, 1]
      for (c_idx in 2:cols) res <- res * densidades[, c_idx]
      res
    } else {
      as.numeric(densidades[, 1])
    } * pk[[i]]
  })
  # --

  # --
  R_M_obs <- matrix(0, nrow = N_test, ncol = length(classes))
  for (i in 1:length(P)) {
    R_M_obs[, i] <- P[[i]]
  }
  # --
  # ---------
  if (type == "class") {
    # -------------------------
    pred_idx <- max.col(R_M_obs, ties.method = "first")
    resultado <- classes[pred_idx]
    return(as.factor(resultado))
    # -------------------------
  } else {
    # -------------------------
    Infpos <- which(R_M_obs == Inf)
    R_M_obs[Infpos] <- .Machine$integer.max
    R_M_obs <- matrix(unlist(R_M_obs), ncol = length(classes), nrow = N_test)
    R_M_obs <- R_M_obs / rowSums(R_M_obs, na.rm = TRUE)
    # ----------
    colnames(R_M_obs) <- classes
    return(R_M_obs)
    # -------------------------
  }
}
# -------------------------
# -------------------------

#' Fuzzy Naive Bayes (R6 class)
#'
#' @description
#' R6 implementation of the Fuzzy Naive Bayes classifier. Create the model with
#' `FuzzyNaiveBayesModel$new()`, train it with `$fit(train, cl)` and classify new
#' observations with `$predict(newdata)`. The functional interface
#' [FuzzyNaiveBayes()] trains and returns an object of this class.
#'
#' @examples
#' model <- FuzzyNaiveBayesModel$new()
#' model$fit(train = iris[, -5], cl = iris$Species)
#' head(model$predict(iris[, -5]))
#' head(model$predict(iris[, -5], type = "prob"))
#' model
#' @export
FuzzyNaiveBayesModel <- R6::R6Class(
  classname = "FuzzyNaiveBayes",
  inherit = FuzzyClassifier,
  public = list(
    #' @description Create a new (untrained) classifier.
    #' @param fuzzy See [FuzzyNaiveBayes()].
    #' @param m See [FuzzyNaiveBayes()].
    #' @param Pi See [FuzzyNaiveBayes()].
    initialize = function(fuzzy = TRUE, m = NULL, Pi = NULL) {
      super$initialize(fuzzy = fuzzy, m = m, Pi = Pi)
    }
  ),
  private = list(
    label_fuzzy = "Fuzzy Naive Bayes",
    label_crisp = "Naive Bayes",
    estimate = function(train, cl) {
      do.call(fit_FuzzyNaiveBayes, c(list(train = train, cl = cl), self$params))
    },
    classify = function(newdata, type) {
      predict_FuzzyNaiveBayes(self$estimates, newdata, type = type)
    }
  )
)

