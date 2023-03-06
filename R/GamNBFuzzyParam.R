#' Fuzzy Gamma Naive Bayes Classifier with Fuzzy parameters
#'
#' \code{GamNBFuzzyParam} Fuzzy Gamma Naive Bayes Classifier with Fuzzy parameters
#'
#'
#' @param train matrix or data frame of training set cases.
#' @param cl factor of true classifications of training set
#' @param alphacut value of the alpha-cut parameter, this value is between 0 and 1.
#' @param metd Method of transforming the triangle into scalar, It is the type of data entry for the test sample, use metd 1 if you want to use the Yager technique, metd 2 if you want to use the Q technique of the uniformity test (article: Directional Statistics and Shape analysis), and metd 3 if you want to use the Thorani technique
#' @param alp When metd for 4, it is necessary to have alp which are alpha-cut defined
#' @param w When metd for 4, it is necessary to have w which are alpha-cut weights defined
#' @param cores  how many cores of the computer do you want to use (default = 2)
#'
#' @return A vector of classifications
#'
#' @references
#' \insertRef{moraes2021new}{FuzzyClass}
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
#' fit_FGNB <- GamNBFuzzyParam(
#'   train = Train[, -5],
#'   cl = Train[, 5], metd = 1, cores = 2
#' )
#'
#' pred_FGNB <- predict(fit_FGNB, test)
#'
#' head(pred_FGNB)
#' head(Test[, 5])
#' @importFrom stats cov dgamma qnorm
#' @importFrom foreach '%dopar%'
#' @importFrom Rdpack reprompt
#'
#' @export
GamNBFuzzyParam <- function(train, cl, alphacut = 0.0001, metd = 2, alp = c(0.35, 0.7, 0.86), w = c(0.1,0.3,0.6), cores = 2) {
  UseMethod("GamNBFuzzyParam")
}

#' @export
GamNBFuzzyParam.default <- function(train, cl, alphacut = 0.0001, metd = 2, alp = c(0.35, 0.7, 0.86), w = c(0.1,0.3,0.6), cores = 2) {

  # --------------------------------------------------------
  # Estimating class parameters
  train <- as.data.frame(train)
  cols <- ncol(train) # Number of variables
  if (is.null(cols)) {
    cols <- 1
  }
  dados <- train # training data matrix
  M <- c(unlist(cl)) # true classes
  M <- factor(M, labels = sort(unique(M)))
  # --------------------------------------------------------
  # Finding Alpha and Beta for each class
  means <- lapply(1:length(unique(M)), function(i) {
    Sub <- subset(dados, M == unique(M)[i])
    sapply(1:cols, function(j) {
      saida <-mean(Sub[, j])
      return(saida)
    })
  })
  alphas <- lapply(1:length(unique(M)), function(i) {
    Sub <- subset(dados, M == unique(M)[i])
    sapply(1:cols, function(j) {
      saida <- 0.5 / ((log(mean(Sub[, j])) - mean(log(Sub[, j]))))
      return(saida)
    })
  })
  betas <- lapply(1:length(unique(M)), function(i) {
    Sub <- subset(dados, M == unique(M)[i])
    sapply(1:cols, function(j) {
      saida <- mean(Sub[, j]) / alphas[[i]][j]
      return(saida)
    })
  })
  ys <- lapply(1:length(unique(M)), function(i) {
    Sub <- subset(dados, M == unique(M)[i])
    sapply(1:cols, function(j) {
        saida <- sum(log(Sub[,j])) - length(Sub[,j])*log(mean(Sub[,j]))
      return(saida)
    })
  })
  ns <- lapply(1:length(unique(M)), function(i) {
    Sub <- subset(dados, M == unique(M)[i])
    sapply(1:cols, function(j) {
      saida <- length(Sub[,j])
      return(saida)
    })
  })
  # --------------------------------------------------------
  # --------------------------------------------------------
  # Estimating Triangular Parameters
  alpha = alp
  if (metd != 4) {
    alpha <- seq(alphacut, 1.1, 0.1)
    alpha <- ifelse(alpha > 1, 1, alpha)
  }
  # -------------------------------
  N <- nrow(dados) # Number of observations
  # -------------------------------
  #  Alphas Parameters
  # ------------------
  Parameters_alphas <- lapply(1:length(alphas), function(i) { # loop to groups
    lapply(1:length(alphas[[1]]), function(k) { # loop to dimensions
      round(
        t(sapply(1:length(alpha), function(j) {
          c(
            1 / ( (2/ns[[i]][k]) * ((-qnorm(alpha[j]/2) * (sqrt(ns[[i]][k]/(2*(alphas[[i]][k]^2)))) ) - ys[[i]][k]) ),
            1 / ( (2/ns[[i]][k]) * ((qnorm(alpha[j]/2) * (sqrt(ns[[i]][k]/(2*(alphas[[i]][k]^2)))) ) - ys[[i]][k]) )
          )
        })),
        5
      )
    })
  })
  # -------------------------------
  # Betas Parameters
  # ------------------
  Parameters_betas <- lapply(1:length(betas), function(i) { # loop to groups
    lapply(1:length(betas[[1]]), function(k) { # loop to dimensions
      round(
        t(sapply(1:length(alpha), function(j) {
          c(
            ( (2*means[[i]][k]/ns[[i]][k]) * ((qnorm(alpha[j]/2) * (sqrt(ns[[i]][k]/(2*(alphas[[i]][k]^2)))) ) - ys[[i]][k]) ),
            ( (2*means[[i]][k]/ns[[i]][k]) * ((-qnorm(alpha[j]/2) * (sqrt(ns[[i]][k]/(2*(alphas[[i]][k]^2)))) ) - ys[[i]][k]) )
          )
        })),
        5
      )
    })
  })
  # -------------------------------------------------------
  structure(list(
    Parameters_betas = Parameters_betas,
    Parameters_alphas = Parameters_alphas,
    alphas = alphas,
    betas = betas,
    cols = cols,
    M = M,
    alpha = alpha,
    metd = metd,
    cores = cores,
    w = w
  ),
  class = "GamNBFuzzyParam"
  )
}
# -------------------------


#' @export
print.GamNBFuzzyParam <- function(x, ...) {
  # -----------------
  cat("\nFuzzy Gamma Naive Bayes Classifier for Discrete Predictors\n\n")
  # -----------------
  cat("Variables:\n")
  print(names(x$Parameters_alphas[[1]]))
  cat("Class:\n")
  print(levels(x$M))
  # -----------------
}

#' @export
predict.GamNBFuzzyParam <- function(object,
                                    newdata,
                                    type = "class",
                                    ...) {
  # --------------------------------------------------------
  test <- as.data.frame(newdata)
  # --------------------------------------------------------
  Parameters_betas <- object$Parameters_betas
  Parameters_alphas <- object$Parameters_alphas
  alphas <- object$alphas
  betas <- object$betas
  cols <- object$cols
  M <- object$M
  alpha <- object$alpha
  metd <- object$metd
  cores <- object$cores
  w <- object$w
  # --------------------------------------------------------

  # --------------------------------------------------------
  # Calculation of triangles for each test observation
  # sum of Logs and calculation of Barycenter
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
    triangulos_obs <-
      lapply(1:length(alphas), function(i) { # loop to groups
        trian <- lapply(1:length(alphas[[1]]), function(k) { # loop to dimensions
          nn <- length(alpha)
          if(metd != 4){ nn <- 2 }
          t(sapply(1:nn, function(j) {
            if((j == 2) && (metd != 4) ){ j = length(alpha) }
            # ------------
            a <- dgamma(x = as.numeric(x[k]), shape = as.numeric(Parameters_alphas[[i]][[k]][j, 1]), scale = as.numeric(Parameters_betas[[i]][[k]][j, 1]))
            b <- dgamma(x = as.numeric(x[k]), shape = as.numeric(Parameters_alphas[[i]][[k]][j, 2]), scale = as.numeric(Parameters_betas[[i]][[k]][j, 2]))
            # ------------
            c(min(a, b), max(a, b))
            # ------------
          }))
        })
        if (length(trian) > 1) {
          return(Reduce("+", trian))
        } else {
          return(trian)
        }
      })
    # ------------
    # Center of Mass Calculation
    vec_trian <- triangulos_obs
    if(metd != 4) vec_trian <- lapply(1:length(unique(M)), function(i) c(triangulos_obs[[i]][1, 1], triangulos_obs[[i]][2, 1], triangulos_obs[[i]][1, 2]))
    # --------------------------------------------------------
    # Transforming Vector to Scalar
    # ------------
    R_M <- switch(metd,
                  # ------------
                  # Barycenter
                  # yager Distance
                  "1" = {
                    # ------------
                    Yagerdistance(vec_trian, M)
                    # ------------
                  },
                  "2" = {
                    # ------------
                    # Using distance Q
                    Qdistance(vec_trian, M)
                    # ------------
                  },
                  "3" = {
                    # ------------
                    # Thorani Distance
                    Thoranidistance(vec_trian, M)
                    # ------------
                  },
                  "4" = {
                    # ------------
                    # Alpha-Order for a class of fuzzy sets
                    AlphaOrderFuzzy(vec_trian, w, M)
                  }
    )
    # --------------------------------------------------------
    R_M_class <- R_M
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
    Infpos <- which(R_M_obs == Inf)
    R_M_obs[Infpos] <- .Machine$integer.max
    R_M_obs <- matrix(unlist(R_M_obs),ncol = length(unique(M)), nrow = N_test)
    R_M_obs <- R_M_obs/rowSums(R_M_obs,na.rm = T)
    # ----------
    colnames(R_M_obs) <- unique(M)
    return(R_M_obs)
    # -------------------------
  }
}
