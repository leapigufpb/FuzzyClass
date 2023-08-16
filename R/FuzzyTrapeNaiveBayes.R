#' Fuzzy Naive Bayes Trapezoidal Classifier
#'
#' \code{FuzzyTrapezoidalNaiveBayes} Fuzzy Naive Bayes Trapezoidal Classifier
#'
#'
#' @param train matrix or data frame of training set cases.
#' @param cl factor of true classifications of training set
#' @param cores  how many cores of the computer do you want to use to use for prediction (default = 2)
#' @param fuzzy boolean variable to use the membership function
#'
#' @return A vector of classifications
#'
#' @references
#' \insertRef{de2022fuzzy}{FuzzyClass}
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
#' fit_NBT <- FuzzyTrapezoidalNaiveBayes(
#'   train = Train[, -5],
#'   cl = Train[, 5], cores = 2
#' )
#'
#' pred_NBT <- predict(fit_NBT, test)
#'
#' head(pred_NBT)
#' head(Test[, 5])
#' @importFrom caTools sample.split
#'
#' @export
FuzzyTrapezoidalNaiveBayes <- function(train, cl, cores = 2, fuzzy = T) {
  UseMethod("FuzzyTrapezoidalNaiveBayes")
}

#' @export
FuzzyTrapezoidalNaiveBayes.default <- function(train, cl, cores = 2, fuzzy = T) {

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
  # Estimating class memberships
  pertinicesC <- lapply(1:length(unique(M)), function(i) {
    lapply(1:cols, function(j) {
      SubSet <- dados[M == unique(M)[i], j]
      getMembershipsTrapezoidal(SubSet, intervalos)
    })
  })
  # --------------------------------------------------------

  # --------------------------------------------------------
  # Estimating Trapezoidal Parameters
  parametersC <- lapply(1:length(unique(M)), function(i) {
    t(sapply(1:cols, function(j) {
      SubSet <- dados[M == unique(M)[i], j]
      getParametersTrapezoidal(SubSet)
    }))
  })
  # --------------------------------------------------------

  # -------------------------------------------------------
  structure(list(
    pertinicesC = pertinicesC,
    parametersC = parametersC,
    cols = cols,
    M = M,
    cores = cores,
    intervalos = intervalos,
    fuzzy = fuzzy
  ),
  class = "FuzzyTrapezoidalNaiveBayes"
  )
}
# -------------------------


#' @export
print.FuzzyTrapezoidalNaiveBayes <- function(x, ...) {
  if (x$fuzzy == T) {
    # -----------------
    cat("\nFuzzy Naive Bayes Trapezoidal Classifier for Discrete Predictors\n\n")
    # -----------------
  } else {
    # -----------------
    cat("\nNaive Bayes Trapezoidal Classifier for Discrete Predictors\n\n")
    # -----------------
  }
  cat("Class:\n")
  print(levels(x$M))
  # -----------------
}

#' @export
predict.FuzzyTrapezoidalNaiveBayes <- function(object,
                                               newdata,
                                               type = "class",
                                               ...) {
  # --------------------------------------------------------
  test <- as.data.frame(newdata)
  # --------------------------------------------------------
  pertinicesC <- object$pertinicesC
  parametersC <- object$parametersC
  cols <- object$cols
  M <- object$M
  cores <- object$cores
  intervalos <- object$intervalos
  fuzzy <- object$fuzzy
  # --------------------------------------------------------

  # --------------------------------------------------------
  # Classification
  # --------------
  N_test <- nrow(test)
  # --------------
  # Defining how many CPU cores to use
  core <- parallel::makePSOCKcluster(cores)
  doParallel::registerDoParallel(core)
  # --------------
  # loop start
  R_M_obs <- foreach::foreach(h = 1:N_test, .combine = rbind) %dopar% {

    # ------------
    x <- test[h, ]
    # ------------
    res <- sapply(1:length(unique(M)), function(i) {

      # -------------
      resultadoPerClass <- 1
      # ------------

      sapply(1:cols, function(j) {

        # x <= a
        if (x[j] <= parametersC[[i]][j, 1]) {
          # --------------
          resultadoPerClass <- resultadoPerClass * 0
          # --------------
        }
        # --------------
        # a < x < c
        if ((x[j] > parametersC[[i]][j, 1]) & (x[j] < parametersC[[i]][j, 2])) {
          # --------------
          resultadoPerClass <- resultadoPerClass *
            (((x[j] - parametersC[[i]][j, 1]) / (parametersC[[i]][j, 2] - parametersC[[i]][j, 1])) *
              (2 / ((parametersC[[i]][j, 4] - parametersC[[i]][j, 1]) + (parametersC[[i]][j, 3] - parametersC[[i]][j, 2]))))
          resultadoPerClass <- unlist(resultadoPerClass)
          # --------------
        }
        # --------------
        # c <= x <= d
        if ((x[j] >= parametersC[[i]][j, 2]) & (x[j] >= parametersC[[i]][j, 3])) {
          # --------------
          resultadoPerClass <- resultadoPerClass *
            (2 / ((parametersC[[i]][j, 4] - parametersC[[i]][j, 1]) +
              (parametersC[[i]][j, 3] - parametersC[[i]][j, 2])))
          # --------------
        }
        # --------------
        # d< x < b
        if ((x[j] > parametersC[[i]][j, 3]) & (x[j] < parametersC[[i]][j, 4])) {
          # --------------
          resultadoPerClass <- resultadoPerClass *
            (((parametersC[[i]][j, 4] - x[j]) / (parametersC[[i]][j, 4] - parametersC[[i]][j, 3])) * (2 / ((parametersC[[i]][j, 4] - parametersC[[i]][j, 1]) + (parametersC[[i]][j, 3] - parametersC[[i]][j, 2]))))
          # --------------
        }
        # --------------
        # b <= x
        if (parametersC[[i]][j, 4] <= x[j]) {
          # --------------
          resultadoPerClass <- resultadoPerClass * 0
          # --------------
        }

        # -----------------------------------------------------------------------
        # -----------------------------------------------------------------------
        if (fuzzy == T) {
          # --------------
          # Mcl(Xi)
          for (st in 1:intervalos) {
            if (st == intervalos) {
              if ((x[j] >= pertinicesC[[i]][[j]][st, 1]) & (x[j] <= pertinicesC[[i]][[j]][st, 2])) {
                resultadoPerClass <- resultadoPerClass * pertinicesC[[i]][[j]][st, 3]
              }
            } else {
              if ((x[j] > pertinicesC[[i]][[j]][st, 1]) & (x[j] < pertinicesC[[i]][[j]][st, 2])) {
                resultadoPerClass <- resultadoPerClass * pertinicesC[[i]][[j]][st, 3]
              }
            }
          }
        }
        # -----------------------------------------------------------------------
        # -----------------------------------------------------------------------

        # --------------
        # P(Wcl)
        resultadoPerClass <- resultadoPerClass * 1 / length(unique(M))
        # --------------
        return(resultadoPerClass)
      })
      # --------------------------------------------------------
    })
    # --------------------------------------------------------
    res[res==0] <- 1e-5
    produto <- matrix(as.numeric(res), ncol = length(unique(M)))
    produto <- apply(produto, 2, prod)
    # --------------------------------------------------------
    R_M_class <- produto
    # --------------------------------------------------------
    return(R_M_class)
  }
  # ------------
  # -------------------------
  parallel::stopCluster(core)
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

# --------------------------------------------------
getParametersTrapezoidal <- function(sample) {
  # -------------------------
  # sample length
  n <- length(sample)
  # -------------------------
  # min value
  minimum <- min(sample)
  # -------------------------
  # max value
  maximum <- max(sample)
  variance <- var(sample)
  m <- expected <- median.default(sample)
  # -------------------------
  model <- function(x) {
    # ----------
    # Expected value
    F1 <- ((-minimum * x[1] - x[1]^2 + maximum * x[2] + x[2]^2 + maximum^2 - minimum^2) /
             (3 * x[2] - 3 * x[1] + 3 * maximum - 3 * minimum)) - expected
    # ----------
    # Variance Value
    F2 <- (((6 * (x[2] - x[1])^4 + 12 * ((x[1] - minimum) + (maximum - x[2])) *
               (x[2] - x[1])^3 + (12 * ((x[1] - minimum) + (maximum - x[2]))^2 - 6 * (x[1] - minimum) * (maximum - x[2])) * (x[2] - x[1])^2) / (18 * ((x[1] - minimum) + 2 * (x[2] - x[1]) + (maximum - x[2]))^2)) +
             ((6 * ((x[1] - minimum) + (maximum - x[2])) * ((x[1] - minimum)^2 + (x[1] - minimum) * (maximum - x[2]) + (maximum - x[2])^2) * (x[2] - x[1]) +
                 ((x[1] - minimum) + (maximum - x[2]))^2 * ((x[1] - minimum)^2 + (x[1] - minimum) * (maximum - x[2]) + (maximum - x[2])^2)) / (18 * ((x[1] - minimum) + 2 * (x[2] - x[1]) + (maximum - x[2]))^2))) - variance
    # -------------------------
    # Return
    return(c(F1 = F1, F2 = F2))
    # -------------------------
  }
  # -------------------------
  # midpoint
  pmedio <- minimum + ((maximum - minimum) / 2)
  # --------
  ss <- rootSolve::multiroot(f = model, start = c(pmedio, pmedio + .1))
  ss <- c(ss$root[1], ss$root[2])
  # --------
  if (ss[1] > ss[2]) {
    # --------
    topo1 <- ss[2]
    topo2 <- ss[1]
    # --------
  } else {
    # --------
    topo1 <- ss[1]
    topo2 <- ss[2]
    # --------
  }
  # -------------------------
  # Verificando limites
  topo1 <- ifelse(topo1 < minimum, minimum, topo1)
  # ---
  topo2 <- ifelse(topo2 <= topo1, topo1,
                  ifelse(topo2 >= maximum, maximum, topo2)
  )
  # -------------------------
  # Parameters Return
  return(c(minimum, topo1, topo2, maximum))
}
