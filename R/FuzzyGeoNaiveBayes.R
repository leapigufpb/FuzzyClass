#' Fuzzy Naive Bayes Geometric Classifier
#'
#' \code{FuzzyGeoNaiveBayes} Naive Bayes Geometric Classifier
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
#' \insertRef{ferreira2023}{FuzzyClass}
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
  dados <- train <-  p_data$train
  cols <- p_data$cols
  M <- p_data$M
  intervalos <- p_data$intervalos
  # --------------------------------------------------------
  # --------------------------------------------------------
  # Verify if all variables are discrete
  verifyNumbers <- verifyNumbersFunction(dados, cols)

  # --------------------------------------------------------
  if(sum(verifyNumbers) != cols){ stop("All variables must be discrete values.") }

  # --------------------------------------------------------
  # --------------------------------------------------------
  # Estimating class memberships
  pertinicesC <- lapply(1:length(get_classes(M)), function(i) {
    lapply(1:cols, function(j) {
      SubSet <- dados[M == get_classes(M)[i], j]
      getMemberships(SubSet, intervalos)
    })
  })
  # --------------------------------------------------------
  # --------------------------------------------------------
  # Estimating Geometric Parameters
  parametersC <- lapply(1:length(get_classes(M)), function(i) {
    lapply(1:cols, function(j) {
      SubSet <- dados[M == get_classes(M)[i], j]
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
  classes <- levels(M)
  n_classes <- length(classes)
  pk <- rep(1 / n_classes, n_classes)

  R_M_obs <- matrix(0, nrow = N_test, ncol = n_classes)
  for (i in seq_len(n_classes)) {
    densidades <- rep(pk[i], N_test)
    for (j in seq_len(cols)) {
      x_vals <- test[[j]]
      # MASS::fitdistr(, "geometric") estimates p for the support {0, 1, 2, ...},
      # so the matching probability mass function is p * (1 - p)^x (dgeom)
      res_j <- stats::dgeom(round(x_vals), prob = parametersC[[i]][[j]])
      if (fuzzy == TRUE) {
        res_j <- res_j * membership_lookup(x_vals, pertinicesC[[i]][[j]])
      }
      densidades <- densidades * res_j
    }
    R_M_obs[, i] <- densidades
  }

  scores_to_output(R_M_obs, classes, type)
}

#' Histogram based membership function: the range of the sample is split in
#' `breaks` intervals [a, b) (the last one closed), the membership of each
#' interval is its frequency divided by the largest frequency, and empty
#' intervals receive 0.001.
#' @noRd
#' @importFrom stats median.default var
getMemberships <- function(sample, breaks) {
  sample <- sample[!is.na(sample)]
  Min <- min(sample)
  Max <- max(sample)
  intervalos <- breaks
  passo <- (Max - Min) / intervalos
  lower <- Min + passo * (seq_len(intervalos) - 1)
  pertinences <- cbind(lower, lower + passo, 0)
  freq <- tabulate(membership_bin(sample, pertinences), nbins = intervalos)
  maxFreq <- max(max(freq), 1)
  pertinences[, 3] <- ifelse(freq == 0, 0.001, freq / maxFreq)
  return(pertinences)
}

#' Index of the interval of `getMemberships` that contains each value
#' (NA outside the range of the training sample).
#' @noRd
membership_bin <- function(x, pertinences) {
  k <- nrow(pertinences)
  lo <- pertinences[1, 1]
  hi <- pertinences[k, 2]
  idx <- if (hi > lo) findInterval(x, pertinences[-1, 1]) + 1L else rep(k, length(x))
  idx[is.na(x) | x < lo | x > hi] <- NA
  idx
}

#' Membership of each value; values outside the training range behave as an
#' empty interval (0.001), as in `getMemberships`.
#' @noRd
membership_lookup <- function(x, pertinences) {
  mu <- pertinences[membership_bin(x, pertinences), 3]
  mu[is.na(mu)] <- 0.001
  mu
}
