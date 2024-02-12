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
#' @return A vector of classifications
#'
#' @references
#' \insertRef{moraes2009another}{FuzzyClass}
#'
#' @examples
#'
#' # Example Fuzzy with Discrete Features
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
#'
#' # Example Fuzzy with Continuous Features
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
#' fit_FNB <- FuzzyNaiveBayes(
#'   train = Train[, -5],
#'   cl = Train[, 5]
#' )
#'
#' pred_FNB <- predict(fit_FNB, test)
#'
#' head(pred_FNB)
#' head(Test[, 5])
#'
#'
#' @importFrom stats model.extract na.pass sd terms predict
#'
#' @export
FuzzyNaiveBayes <- function(train, cl, fuzzy = TRUE, m = NULL, Pi = NULL) {
  UseMethod("FuzzyNaiveBayes")
}

#' @export
FuzzyNaiveBayes.default <- function(train, cl, fuzzy = T, m = NULL, Pi = NULL) {

  # --------------------------------------------------------
  # Estimating class parameters
  train <- as.data.frame(train)
  cols <- ncol(train) # Number of variables
  if(is.null(cols)){
    cols <- 1
  }
  dados <- train # training data matrix
  M <- c(unlist(cl)) # true classes
  if(!is.factor(M))  M <- factor(M, labels = sort(unique(M)))
  # --

  # --
  # converting character to factor
  typescolumns <- sapply(1:cols, function(i) is.character(train[,i]))
  columnscharac <- which(typescolumns)
  trainTrans <- train[,columnscharac]
  trainTrans <- data.frame(purrr::map_dfr(trainTrans,factor))
  train[,columnscharac] <- trainTrans
  # --


  # --
  res <- sapply(1:cols, function(i) {is.factor(train[,i])})
  if(sum(res) == cols){
    resul <- FuzzyNaiveBayes.categorical(train, cl, fuzzy = T, m = NULL, Pi = NULL)
  }
  # --
  res <- sapply(1:cols, function(i) {is.numeric(train[,i])})
  if(sum(res) == cols){
    resul <- FuzzyNaiveBayes.continuo(train, cl, fuzzy = T, m = NULL, Pi = NULL)
  }




  # -------------------------------------------------------
  structure(list(
    parametersC = resul$parametersC,
    cols = resul$cols,
    M = resul$M,
    Pertinencia = resul$Pertinencia,
    pk = resul$pk,
    fuzzy = resul$fuzzy,
    Intervalos_Valores = resul$Intervalos_Valores,
    model = resul$model
  ),
  class = "FuzzyNaiveBayes"
  )
}
# -------------------------


#' @export
print.FuzzyNaiveBayes <- function(x, ...) {
  if (x$fuzzy == T) {
    # -----------------
    cat("\nFuzzy Naive Bayes Classifier for Discrete Predictors\n\n")
    # -----------------
  } else {
    # -----------------
    cat("\nNaive Bayes Classifier for Discrete Predictors\n\n")
    # -----------------
  }
  cat("Class:\n")
  print(levels(x$M))
  # -----------------
}

#' @export
predict.FuzzyNaiveBayes <- function(object,
                                   newdata,
                                   type = "class",
                                   ...) {
  # --------------------------------------------------------
  test <- as.data.frame(newdata)
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


#' @export
FuzzyNaiveBayes.categorical <- function(train, cl, fuzzy = T, m = NULL, Pi = NULL) {

  # --------------------------------------------------------
  # Estimating class parameters
  train <- as.data.frame(train)
  cols <- ncol(train) # Number of variables
  if(is.null(cols)){
    cols <- 1
  }
  dados <- train # training data matrix
  M <- c(unlist(cl)) # true classes
  if(!is.factor(M))  M <- factor(M, levels = unique(M))

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
  parametersC <- lapply(1:length(unique(M)), function(i) {
    lapply(1:cols, function(j) {
      SubSet <- dados[M == unique(M)[i], j]
      param <- (table(SubSet) + m*pi)/(sum(table(SubSet)) + m)
      return(param)
    })
  })

  model <- e1071::naiveBayes(x = dados, y = cl)
  # --------------------------------------------------------

  #--------------------------------------------------------
  Pertinencia <- lapply(1:length(unique(M)), function(i) {
    lapply(1:cols, function(j) {
      SubSet <- dados[M == unique(M)[i], j]
      param <- (table(SubSet))/(sum(table(SubSet)))
      return(param)
    })
  })
  #------
  # A priori probability of classes - considered equal
  pk <- rep(1 / length(unique(M)), length(unique(M)))
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
#' @export
FuzzyNaiveBayes.continuo <- function(train, cl, fuzzy = T, m = NULL, Pi = NULL) {

  # --------------------------------------------------------
  # Estimating class parameters
  train <- as.data.frame(train)
  cols <- ncol(train) # Number of variables
  if(is.null(cols)){
    cols <- 1
  }
  dados <- train
  M <- c(unlist(cl)) # true classes
  if(!is.factor(M))  M <- factor(M, levels = unique(M))
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
  for(i in 1:length(unique(M))){
    parametersC[[i]] <- apply(Freq[[i]], 2, function(x) ( (x + m*Pi) / ( sum(x) +m ) ) )
  }

  #------
  Pertinencia <- Pertinencia(Freq, dados, M);
  #------
  # A priori probability of classes - considered equal
  pk <- rep(1 / length(unique(M)), length(unique(M)))
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
predict_categorical <- function(test, M, cols, parametersC, pk, Pertinencia, type  ) {

  N <- nrow(test)
  # --
  P <- lapply(1:length(unique(M)), function(i) {
    densidades <- sapply(1:cols, function(j) {
      # --
      sapply(1:N, function(v) {
        if (is.na(test[v, j])) {
          return(1e-5)
        } else {
          pos <- which(names(parametersC[[i]][[j]]) == (test[v, j]))
          return(parametersC[[i]][[j]][pos])
        }
      })
      # --
    })
    densidades <- apply(densidades, 1, prod)
    # --
    p <- pk[[i]] * densidades
    # ---
    return(p)
  })
  # --

  # --
  P_fuzzy <- lapply(1:length(unique(M)), function(i) {
    densidades <- sapply(1:cols, function(j) {
      # --
      sapply(1:N, function(v) {
        if (is.na(test[v, j])) {
          return(1e-5)
        } else {
          pos <- which(names(Pertinencia[[i]][[j]]) == (test[v, j]))
          return(Pertinencia[[i]][[j]][pos])
        }
      })
      # --
    })
    densidades <- apply(densidades, 1, prod)
    # --
    p <- pk[[i]] * densidades
    # ---
    return(p)
  })
  # --

  # --
  R_M_obs <- array(NA, dim = c(length(P_fuzzy[[1]]), length(unique(M))))
  for (i in 1:length(P)) {
    R_M_obs[, i] <- P[[i]] * P_fuzzy[[i]]
  }
  # --
  # ---------
  if (type == "class") {
    # -------------------------
    R_M_obs <- sapply(1:nrow(R_M_obs), function(i) which.max(R_M_obs[i, ]))
    resultado <- unique(M)[R_M_obs]
    return(as.factor(c(resultado)))
    # -------------------------
  } else {
    # -------------------------
    Infpos <- which(R_M_obs == Inf)
    R_M_obs[Infpos] <- .Machine$integer.max
    R_M_obs <- matrix(unlist(R_M_obs), ncol = length(unique(M)))
    R_M_obs <- R_M_obs / rowSums(R_M_obs, na.rm = T)
    # ----------
    colnames(R_M_obs) <- unique(M)
    return(R_M_obs)
    # -------------------------
  }

  return(R_M_obs)

}
# -------------------------


# -------------------------
predict_continuo <- function(test, M, cols, parametersC, pk, Pertinencia, Intervalo_Valores, type  ) {

  N <- nrow(test)
  # --
  P <- lapply(1:length(unique(M)), function(i) {
    densidades <- sapply(1:cols, function(j) {
      # --
      sapply(1:N, function(v) {
        if (is.na(test[v, j])) {
          return(1e-5)
        } else {
          numeros <- as.numeric(unlist(strsplit(Intervalo_Valores[[i]][,j],",")))
          tamanho <- length(numeros)
          pos <- which( numeros >= (test[v, j]))[1] / 2
          pos <- ifelse(is.na(pos), tamanho/2,round(pos))
          pos <- ifelse(pos < 1, 1, round(pos))
          return(parametersC[[i]][,j][pos])
        }
      })
      # --
    })
    densidades <- apply(densidades, 1, prod)
    # --
    p <- pk[[i]] * densidades
    # ---
    return(p)
  })
  # --

  # --
  P_fuzzy <- lapply(1:length(unique(M)), function(i) {
    densidades <- sapply(1:cols, function(j) {
      # --
      sapply(1:N, function(v) {
        if (is.na(test[v, j])) {
          return(1e-5)
        } else {
          numeros <- as.numeric(unlist(strsplit(Intervalo_Valores[[i]][,j],",")))
          tamanho <- length(numeros)
          pos <- which( numeros >= (test[v, j]))[1] / 2
          pos <- ifelse(is.na(pos), tamanho/2,round(pos))
          pos <- ifelse(pos < 1, 1, round(pos))
          return(Pertinencia[[i]][,j][pos])
        }
      })
      # --
    })
    densidades <- apply(densidades, 1, prod)
    # --
    p <- pk[[i]] * densidades
    # ---
    return(p)
  })
  # --

  # --
  R_M_obs <- array(NA, dim = c(length(P_fuzzy[[1]]), length(unique(M))))
  for (i in 1:length(P)) {
    R_M_obs[, i] <- P[[i]] * P_fuzzy[[i]]
  }
  # --
  # ---------
  if (type == "class") {
    # -------------------------
    R_M_obs <- sapply(1:nrow(R_M_obs), function(i) which.max(R_M_obs[i, ]))
    resultado <- unique(M)[R_M_obs]
    return(as.factor(c(resultado)))
    # -------------------------
  } else {
    # -------------------------
    Infpos <- which(R_M_obs == Inf)
    R_M_obs[Infpos] <- .Machine$integer.max
    R_M_obs <- matrix(unlist(R_M_obs), ncol = length(unique(M)))
    R_M_obs <- R_M_obs / rowSums(R_M_obs, na.rm = T)
    # ----------
    colnames(R_M_obs) <- unique(M)
    return(R_M_obs)
    # -------------------------
  }

  return(R_M_obs)

}
# -------------------------

# -------------------------
# -------------------------
predict_continuo_nonfuzzy <- function(test, M, cols, parametersC, pk, Pertinencia, Intervalo_Valores, type  ) {

  N <- nrow(test)
  # --
  P <- lapply(1:length(unique(M)), function(i) {
    densidades <- sapply(1:cols, function(j) {
      # --
      sapply(1:N, function(v) {
        if (is.na(test[v, j])) {
          return(1e-5)
        } else {
          numeros <- as.numeric(unlist(strsplit(Intervalo_Valores[[i]][,j],",")))
          tamanho <- length(numeros)
          pos <- which( numeros >= (test[v, j]))[1] / 2
          pos <- ifelse(is.na(pos), tamanho/2,round(pos))
          pos <- ifelse(pos < 1, 1, round(pos))
          return(parametersC[[i]][,j][pos])
        }
      })
      # --
    })
    densidades <- apply(densidades, 1, prod)
    # --
    p <- pk[[i]] * densidades
    # ---
    return(p)
  })
  # --

  # --
  R_M_obs <- array(NA, dim = c(length(P[[1]]), length(unique(M))))
  for (i in 1:length(P)) {
    R_M_obs[, i] <- P[[i]]
  }
  # --
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
    Infpos <- which(R_M_obs == Inf)
    R_M_obs[Infpos] <- .Machine$integer.max
    R_M_obs <- matrix(unlist(R_M_obs),ncol = length(unique(M)), nrow = N_test)
    R_M_obs <- R_M_obs / rowSums(R_M_obs, na.rm = T)
    # ----------
    colnames(R_M_obs) <- unique(M)
    return(R_M_obs)
    # -------------------------
  }

  return(R_M_obs)

}
# -------------------------
# -------------------------

