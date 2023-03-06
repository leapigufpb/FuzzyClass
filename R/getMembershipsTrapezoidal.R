#' @importFrom stats median.default var
getMembershipsTrapezoidal <- function(sample, breaks) {
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
