#' Fuzzy Gaussian Naive Bayes Classifier with Fuzzy parameters
#'
#' \code{GauNBFuzzyParam} Fuzzy Gaussian Naive Bayes Classifier with Fuzzy parameters
#'
#'
#' @param train matrix or data frame of training set cases.
#' @param cl factor of true classifications of training set
#' @param alphacut value of the alpha-cut parameter, this value is between 0 and 1.
#' @param metd Method of transforming the triangle into scalar, It is the type of data entry for the test sample, use metd 1 if you want to use the Yager technique, metd 2 if you want to use the Q technique of the uniformity test (article: Directional Statistics and Shape analysis), and metd 3 if you want to use the Thorani technique
#' @param cores  how many cores of the computer do you want to use to use for prediction (default = 2)
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
#' fit_FGNB <- GauNBFuzzyParam(
#'   train = Train[, -5],
#'   cl = Train[, 5], metd = 1, cores = 2
#' )
#'
#' pred_FGNB <- predict(fit_FGNB, test)
#'
#' head(pred_FGNB)
#' head(Test[, 5])
#' @importFrom stats cov dnorm qchisq qnorm
#' @importFrom foreach '%dopar%'
#' @importFrom Rdpack reprompt
#'
#' @export
GauNBFuzzyParam <- function(train, cl, alphacut = 0.0001, metd = 2, cores = 2) {
  UseMethod("GauNBFuzzyParam")
}

#' @export
GauNBFuzzyParam.default <- function(train, cl, alphacut = 0.0001, metd = 2, cores = 2) {

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
  # Finding Mu and Sigma for each class
  medias <- lapply(1:length(unique(M)), function(i) colMeans(subset(dados, M == unique(M)[i])))
  varian <- lapply(1:length(unique(M)), function(i) diag(diag(cov(subset(dados, M == unique(M)[i]))), (cols), (cols)))
  # --------------------------------------------------------
  # --------------------------------------------------------
  # Estimating Triangular Parameters
  # --------------------------------------------------------
  if(metd > 3){ stop("metd argument must be <= 3.") }
  # --------------------------------------------------------
  alp=1
  w=1
  # --------------------------------------------------------
  alpha = alp
  if (metd != 4) {
    alpha <- seq(alphacut, 1.1, 0.1)
    alpha <- ifelse(alpha > 1, 1, alpha)
  }
  # -------------------------------
  N <- nrow(dados) # Number of observations
  # -------------------------------
  #  Average Parameters
  # ------------------
  Parameters_media <- lapply(1:length(medias), function(i) { # loop to groups
    lapply(1:length(medias[[1]]), function(k) { # loop to dimensions
      round(
        t(sapply(1:length(alpha), function(j) {
          c(
            medias[[i]][k] - (qnorm(1 - alpha[j] / 2) * (sqrt(varian[[i]][k, k] / N))),
            medias[[i]][k] + (qnorm(1 - alpha[j] / 2) * (sqrt(varian[[i]][k, k] / N)))
          )
        })),
        5
      )
    })
  })
  # -------------------------------
  # Variance Parameters
  # ------------------
  Parameters_varian <- lapply(1:length(medias), function(i) { # loop to groups
    lapply(1:length(medias[[1]]), function(k) { # loop to dimensions
      round(
        t(sapply(1:length(alpha), function(j) {
          beta <- 0.05 # previously fixed
          lambda <- alpha[j]
          # ------
          L <- (1 - lambda) * qchisq(p = 1 - (beta / 2), N - 1) + (lambda * (N - 1))
          R <- (1 - lambda) * qchisq(p = beta / 2, N - 1) + (lambda * (N - 1))
          # ------
          c(
            ((N - 1) * varian[[i]][k, k]) / L,
            ((N - 1) * varian[[i]][k, k]) / R
          )
          # ------
        })),
        5
      )
    })
  })

  # -------------------------------------------------------
  structure(list(
    Parameters_varian = Parameters_varian,
    Parameters_media = Parameters_media,
    medias = medias,
    varian = varian,
    cols = cols,
    M = M,
    alpha = alpha,
    metd = metd,
    cores = cores,
    w = w
  ),
  class = "GauNBFuzzyParam"
  )
}
# -------------------------


#' @export
print.GauNBFuzzyParam <- function(x, ...) {
  # -----------------
  cat("\nFuzzy Gaussian Naive Bayes Classifier for Discrete Predictors\n\n")
  # -----------------
  cat("Variables:\n")
  print(names(x$medias[[1]]))
  cat("Class:\n")
  print(levels(x$M))
  # -----------------
}

#' @export
predict.GauNBFuzzyParam <- function(object,
                                    newdata,
                                    type = "class",
                                    ...) {
  # --------------------------------------------------------
  test <- as.data.frame(newdata)
  # --------------------------------------------------------
  Parameters_varian <- object$Parameters_varian
  Parameters_media <- object$Parameters_media
  medias <- object$medias
  varian <- object$varian
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
      lapply(1:length(medias), function(i) { # loop to groups
        trian <- lapply(1:length(medias[[1]]), function(k) { # loop to dimensions
          nn <- length(alpha)
          if(metd != 4){ nn <- 2 }
          t(sapply(1:nn, function(j) {
            if((j == 2) && (metd != 4) ){ j = length(alpha) }
            # ------------
            a <- dnorm(x = as.numeric(x[k]), mean = as.numeric(Parameters_media[[i]][[k]][j, 1]), sd = sqrt(as.numeric(Parameters_varian[[i]][[k]][j, 1])))
            b <- dnorm(x = as.numeric(x[k]), mean = as.numeric(Parameters_media[[i]][[k]][j, 2]), sd = sqrt(as.numeric(Parameters_varian[[i]][[k]][j, 2])))
            # ------------
            c(min(a, b), max(a, b))
            # ------------
          }))
        })
        if (length(trian) > 1) {
          return(Reduce("+", trian))
        } else {
          return(Reduce("+", trian))
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
