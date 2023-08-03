#' Fuzzy Naive Bayes Geometric Classifier
#'
#' \code{FuzzyGeoNaiveBayes} Naive Bayes Geometric Classifier
#'
#'
#' @param train matrix or data frame of training set cases.
#' @param cl factor of true classifications of training set
#' @param cores  how many cores of the computer do you want to use (default = 2)
#' @param fuzzy boolean variable to use the membership function
#'
#' @return A vector of classifications
#'
#' @references
#' \insertRef{moraes2021new}{FuzzyClass}
#'
#' @examples
#'
#' set.seed(1) # determining a seed
#' class1 <- data.frame(vari1 = rgeom(100,prob = 0.2),
#'                      vari2 = rgeom(100,prob = 0.2),
#'                      vari3 = rgeom(100,prob = 0.2), class = 1)
#' class2 <- data.frame(vari1 = rgeom(100,prob = 0.5),
#'                      vari2 = rgeom(100,prob = 0.5),
#'                      vari3 = rgeom(100,prob = 0.5), class = 2)
#' class3 <- data.frame(vari1 = rgeom(100,prob = 0.9),
#'                      vari2 = rgeom(100,prob = 0.9),
#'                      vari3 = rgeom(100,prob = 0.9), class = 3)
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
#' fit_NBT <- FuzzyGeoNaiveBayes(
#'   train = Train[, -4],
#'   cl = Train[, 4], cores = 2
#' )
#'
#' pred_NBT <- predict(fit_NBT, test)
#'
#' head(pred_NBT)
#' head(Test[, 4])
#' @export
FuzzyGeoNaiveBayes <- function(train, cl, cores = 2, fuzzy = T) {
  UseMethod("FuzzyGeoNaiveBayes")
}

#' @export
FuzzyGeoNaiveBayes.default <- function(train, cl, cores = 2, fuzzy = T) {

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


  #------------------------------------------------------------
  # Verify data types
  verifyNumbers <- sapply(1:cols, function(i){
    set.seed(3)
    n = 3
    subset <- sample(dados[,i],size = n, replace = F)
    result <- subset == floor(subset)
    if(sum(result) == n){
      result <- 1
    }else{
      result <- 0
    }
    return(result)
  })

  # --------------------------------------------------------
  if(sum(verifyNumbers) != cols){ stop("All variables must be discrete values.") }

  # --------------------------------------------------------
  # --------------------------------------------------------
  # Estimating class memberships
  pertinicesC <- lapply(1:length(unique(M)), function(i) {
    lapply(1:cols, function(j) {
      SubSet <- dados[M == unique(M)[i], j]
      getMemberships(SubSet, intervalos)
    })
  })
  # --------------------------------------------------------
  # --------------------------------------------------------
  # Estimating Geometric Parameters
  parametersC <- lapply(1:length(unique(M)), function(i) {
    lapply(1:cols, function(j) {
      SubSet <- dados[M == unique(M)[i], j]
      param <- MASS::fitdistr(SubSet, "geometric")$estimate
      return(param)
    })
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
  class = "FuzzyGeoNaiveBayes"
  )
}
# -------------------------


#' @export
print.FuzzyGeoNaiveBayes <- function(x, ...) {
  if (x$fuzzy == T) {
    # -----------------
    cat("\nFuzzy Naive Bayes Geometric Classifier for Discrete Predictors\n\n")
    # -----------------
  } else {
    # -----------------
    cat("\nNaive Bayes Geometric Classifier for Discrete Predictors\n\n")
    # -----------------
  }
  cat("Class:\n")
  print(levels(x$M))
  # -----------------
}

#' @export
predict.FuzzyGeoNaiveBayes <- function(object,
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

        resultadoPerClass <- ((1 - parametersC[[i]][[j]])^(x[j] - 1)) * parametersC[[i]][[j]]

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

#' @importFrom stats median.default var
getMemberships <- function(sample, breaks) {
  # -------------------------
  # sample length
  n <- length(sample)
  # -------------------------
  # min value
  Min <- min(sample)
  # -------------------------
  # max value
  Max <- max(sample)
  # -------------------------
  intervalos <- breaks
  pertinences <- matrix(nrow = intervalos, ncol = 3)

  # -------------------------

  passo <- (Max - Min) / intervalos
  # -------------------------
  freq <- matrix(nrow = intervalos, ncol = 3)
  # -------------------------
  for (i in 1:intervalos) {
    freq[i, 1] <- Min + passo * (i - 1)
    freq[i, 2] <- freq[i, 1] + passo
    freq[i, 3] <- 0
  }
  # -------------------------
  for (i in 1:n) {
    for (j in 1:intervalos) {
      # -------------------------
      if (j == intervalos) {
        if ((sample[i] >= freq[j, 1]) & (sample[i] <= freq[j, 2])) {
          freq[j, 3] <- freq[j, 3] + 1
        }
      } else {
        # -------------------------
        if ((sample[i] >= freq[j, 1]) & (sample[i] < freq[j, 2])) {
          freq[j, 3] <- freq[j, 3] + 1
        }
      }
    }
  }
  # -------------------------
  maxFreq <- max(freq[1:intervalos, 3])
  # -------------------------
  for (i in 1:intervalos) {
    pertinences[i, 1] <- freq[i, 1]
    pertinences[i, 2] <- freq[i, 2]
    # -------------------------
    if (freq[i, 1] == 0) {
      pertinences[i, 3] <- 0.001
    } else {
      pertinences[i, 3] <- freq[i, 3] / maxFreq
    }
  }

  # -------------------------
  return(pertinences)
  # -------------------------
}
