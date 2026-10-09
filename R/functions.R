# Verify data types
#' @noRd
verifyNumbersFunction <- function(dados, cols = NULL) {
  dados <- as.data.frame(dados)
  if (is.null(cols) || cols == 0) cols <- ncol(dados)
  if (is.null(cols) || cols == 0) cols <- 1
  resul <- vapply(seq_len(cols), function(i) {
    vals <- dados[[i]]
    if (is.numeric(vals) && all(vals == floor(vals), na.rm = TRUE)) 1L else 0L
  }, integer(1))
  return(resul)
}

# Class labels in a single, consistent order (factor levels).
# All estimation and prediction steps must index classes through this helper,
# otherwise parameters (estimated per class) and labels may be misaligned.
#' @noRd
get_classes <- function(M) {
  if (is.factor(M)) levels(M) else sort(unique(M))
}

# Smallest density used for distributions with bounded support (triangular,
# trapezoidal): a test value outside the support of one variable would make the
# whole naive Bayes product zero for every class, and the prediction would be
# decided by the order of the classes instead of by the remaining variables.
#' @noRd
DENS_FLOOR <- 1e-10

# Verify Columns
#' @noRd
verifyColumns <- function(dados){
  cols <- ncol(dados)
  if(is.null(cols) || cols == 0){
    cols <- 1
  }
  return(cols)
}

# PreData
#' @noRd
predata <- function(dados, cl){
  train <- as.data.frame(dados)
  cols <- verifyColumns(train)
  M <- c(unlist(cl))
  M <- factor(M, levels = sort(unique(M)))
  intervalos <- 10

  lista <- list(train = train,
                dados = train,
                cols = cols,
                M = M,
                intervalos = intervalos)

  return(lista)
}

#---- Sturges -----#

#' @noRd
Sturges <- function(dados, M) {
  dados <- as.data.frame(dados)
  classes <- get_classes(M)
  Output <- lapply(1:length(classes), function(i) {
    SubSet <- dados[M == classes[i], , drop = FALSE]
    max(1L, as.integer(round(sqrt(nrow(SubSet)))))
  })
  return(Output)
}

#' @noRd
Sturges_discrete <- function(dados, M) {
  dados <- as.data.frame(dados)
  classes <- get_classes(M)
  Output <- lapply(1:length(classes), function(i) {
    SubSet <- dados[M == classes[i], 1]
    max(1L, as.integer(length(unique(SubSet))))
  })
  return(Output)
}

#---- Range Length ----#

#' @noRd
Comprim_Intervalo <- function(dados, M, Sturges) {
  dados <- as.data.frame(dados)
  classes <- get_classes(M)
  Output <- lapply(1:length(classes), function(i) {
    SubSet <- dados[M == classes[i], , drop = FALSE]
    k_c <- max(1L, Sturges[[i]])
    if(ncol(SubSet) == 1){
      comp <- (max(SubSet[[1]]) - min(SubSet[[1]])) / k_c
    } else {
      comp <- (apply(SubSet, 2, max) - apply(SubSet, 2, min)) / k_c
    }
    comp[comp <= 0] <- 1
    return(comp)
  })
  return(Output)
}

#' @noRd
Comprim_Intervalo_discrete <- function(dados, M){
  cols <- verifyColumns(dados)
  Output <- lapply(1:length(get_classes(M)), function(i){
    rep(1, cols)
  })
  return(Output)
}
#--------------------------------------------------------

#' @noRd
minimos <- function(dados, M, cols) {
  dados <- as.data.frame(dados)
  classes <- get_classes(M)
  Output <- lapply(1:length(classes), function(i) {
    SubSet <- dados[M == classes[i], , drop = FALSE]
    sapply(1:cols, function(j) min(SubSet[, j]))
  })
  return(Output)
}

#---------------

#---------------
#' @noRd
maximos <- function(dados, M, cols) {
  dados <- as.data.frame(dados)
  classes <- get_classes(M)
  Output <- lapply(1:length(classes), function(i) {
    SubSet <- dados[M == classes[i], , drop = FALSE]
    sapply(1:cols, function(j) max(SubSet[, j]))
  })
  return(Output)
}

#---------------

#' @noRd
Freq <- function(dados, M, Comprim_Intervalo, Sturges, minimos, cols) {
  dados <- as.data.frame(dados)
  classes <- get_classes(M)
  Freq <- lapply(1:length(classes), function(i) {
    SubSet <- dados[M == classes[i], , drop = FALSE]
    k_c <- max(1L, Sturges[[i]])
    ara <- matrix(0, nrow = k_c, ncol = cols)
    for (coluna in 1:cols) {
      m <- minimos[[i]][coluna]
      w <- Comprim_Intervalo[[i]][coluna]
      if (w <= 0) w <- 1
      faixas <- m + (1:k_c) * w
      idx <- findInterval(SubSet[, coluna], faixas, left.open = FALSE) + 1L
      idx <- pmin(pmax(idx, 1L), k_c)
      ara[, coluna] <- tabulate(idx, nbins = k_c)
    }
    return(ara)
  })
  return(Freq)
}

#' @noRd
Intervalos_Valores <- function(dados, M, Comprim_Intervalo, Sturges, minimos, cols) {
  classes <- get_classes(M)
  Freq <- lapply(1:length(classes), function(classe) {
    k_c <- max(1L, Sturges[[classe]])
    res_mat <- matrix("", nrow = k_c, ncol = cols)
    for (coluna in 1:cols) {
      faixaant <- minimos[[classe]][coluna]
      w <- Comprim_Intervalo[[classe]][coluna]
      for (linhaFreq in 1:k_c) {
        faixa <- faixaant + w
        res_mat[linhaFreq, coluna] <- paste0(round(faixaant, 3), ",", round(faixa, 3))
        faixaant <- faixa
      }
    }
    return(res_mat)
  })
  return(Freq)
}

#--------------------------------------------------------
#---- Pertinencia ----#

#' @noRd
Pertinencia <- function(Freq, dados, M) {
  dados <- as.data.frame(dados)
  classes <- get_classes(M)
  Output <- lapply(1:length(classes), function(i) {
    SubSet <- dados[M == classes[i], , drop = FALSE]
    NN <- max(nrow(SubSet), 1)
    Freq[[i]] / NN
  })
  return(Output)
}



#' @noRd
pertinencia_predict <- function(M, Sturges, minimos, Comprim_Intervalo, Pertinencia, cols, x){

  #---------------
  ACHOU_t <- c()
  ACHOU <- 0
  #---------------
  for (classe in 1:length(get_classes(M))) {
    # --
    # --
    for (coluna in 1:cols) { # class column
      for (linhaF in 1:Sturges[[classe]]) { # class row
        faixa <- minimos[[classe]][coluna] + Comprim_Intervalo[[classe]][coluna] # initial frequency band
        if (x[[coluna]] < faixa) { # checks if the class value belongs to that range
          ACHOU[coluna] <- Pertinencia[[classe]][linhaF, coluna] # accumulates value in the frequency range and interrupts this last 'for'
          break
        }
        if (linhaF == Sturges[[classe]]) {
          ACHOU[coluna] <- Pertinencia[[classe]][linhaF, coluna]
          break
        }
        faixa <- faixa + Comprim_Intervalo[[classe]][coluna] # track change -> next
      }
    }
    # ---
    ACHOU_t <- rbind(ACHOU_t, ACHOU) # Classes are the lines
    # ---
  }
  #-----
  row.names(ACHOU_t) <- get_classes(M)
  #--------------------------------------------------------
  ACHOU_t <- apply(ACHOU_t, 1, prod)

  return(ACHOU_t)

}



#' @noRd
pertinencia_predictDW <- function(M, Sturges, minimos, Comprim_Intervalo, Pertinencia, cols, x, weta){

  #---------------
  ACHOU_t <- c()
  ACHOU <- 0
  #---------------
  for (classe in 1:length(get_classes(M))) {
    # --
    # --
    for (coluna in 1:cols) { # class column
      for (linhaF in 1:Sturges[[classe]]) { # class row
        faixa <- minimos[[classe]][coluna] + Comprim_Intervalo[[classe]][coluna] # initial frequency band
        if (x[coluna] < faixa) { # checks if the class value belongs to that range
          ACHOU[coluna] <- weta[coluna]*Pertinencia[[classe]][linhaF, coluna] # accumulates value in the frequency range and interrupts this last 'for'
          break
        }
        if (linhaF == Sturges[[classe]]) {
          ACHOU[coluna] <- weta[coluna]*Pertinencia[[classe]][linhaF, coluna]
          break
        }
        faixa <- faixa + Comprim_Intervalo[[classe]][coluna] # track change -> next
      }
    }
    # ---
    ACHOU_t <- rbind(ACHOU_t, ACHOU) # Classes são as linhas
    # ---
  }
  #-----
  row.names(ACHOU_t) <- get_classes(M)
  #--------------------------------------------------------
  ACHOU_t <- apply(ACHOU_t, 1, prod)

  return(ACHOU_t)

}


# ----------------
#' @noRd
function_membership_predict <- function(x, M = M, Sturges = Sturges, minimos = minimos, Comprim_Intervalo = Comprim_Intervalo, Pertinencia = Pertinencia, cols = cols){

  ACHOU_t <- pertinencia_predict(M, Sturges, minimos, Comprim_Intervalo, Pertinencia, cols, x);
  return(ACHOU_t)

}
# ----------------

# ----------------
#' @noRd
function_membership_predict_dw <- function(x, M = M, Sturges = Sturges, minimos = minimos, Comprim_Intervalo = Comprim_Intervalo, Pertinencia = Pertinencia, cols = cols, weta = weta){

  ACHOU_t <- pertinencia_predictDW(M, Sturges, minimos, Comprim_Intervalo, Pertinencia, cols, x, weta);
  return(ACHOU_t)

}
# ----------------


# ----------------
#' @noRd
function_fuzzy_predict <- function(retorno, P, M){

  retorno2 <- data.frame(matrix(unlist(retorno), nrow=length(retorno), byrow=TRUE))
  f <- sapply(1:length(get_classes(M)), function(i) {
    P[[i]] * retorno2[,i]
  })

  return(f)

}
# ----------------

# ----------------
#' @noRd
function_fuzzy_predict_log <- function(retorno, P, M){

  retorno2 <- data.frame(matrix(unlist(retorno), nrow=length(retorno), byrow=TRUE))
  f <- sapply(1:length(get_classes(M)), function(i) {
    P[[i]] + log(retorno2[,i])
  })

  return(f)

}
# ----------------




# --------------------------------------------------------------------------
# Ranking of triangular fuzzy numbers (used by GauNBFuzzyParam, ExpNBFuzzyParam
# and PoiNBFuzzyParam). Each fuzzy number is given by its three vertices
# (p1 = left, p2 = core, p3 = right); p1, p2 and p3 are N x K matrices
# (N observations, K classes), so every observation is ranked at once.

#' Yager (1980) - barycenter
#' @noRd
Yagerdistance <- function(p1, p2, p3) {
  (p1 + 2 * p2 + p3) / 4
}

#' Using distance Q (not published).
#' Q = 3 * (l1 - l2)^2, where l1 >= l2 are the eigenvalues of y %*% t(y),
#' y = (p1, p2, p3). That matrix has rank one, so l1 = sum(y^2) and l2 = 0;
#' the closed form below avoids one eigen decomposition per observation/class.
#' @noRd
Qdistance <- function(p1, p2, p3) {
  3 * (p1^2 + p2^2 + p3^2)^2
}

#' Thorani - 2012
#' Article: Ordering Generalized Trapezoidal Fuzzy Numbers Using Orthocentre of Centroids
#' "changes were made"
#' @noRd
Thoranidistance <- function(p1, p2, p3) {
  (1 / ncol(p2)) * p2 * ((p2 - p1) * (p3 - p2) + 1)
}

#' 2024
#' Article: A novel approach for arithmetic operations and ranking of generalized fuzzy numbers with application
#' @noRd
GTFNdistance <- function(p1, p2, p3) {
  m <- pmax(abs(p1), abs(p2), abs(p3))
  d <- 10 * ceiling(m / 10)
  q1 <- p1 / d; q2 <- p2 / d; q3 <- p3 / d
  m_q <- pmax(abs(q1), abs(q2), abs(q3))
  ceiling(m_q / 10) * ((2 * q1 - q1^2) + (2 * q2 - q2^2) + (2 * q3 - q3^2))
}

#' Fuzzy-parameter naive Bayes scores (vectorized over observations).
#' `dens(x, i, k, j)` returns list(lower, upper): the densities of column `k`
#' for class `i` evaluated with the lower and upper end points of the
#' alpha-cut `j` of the fuzzy parameter. Densities are summed over the
#' variables and the resulting triangular fuzzy numbers are ranked by `metd`.
#'
#' metd = 4: alpha-ordering (2023) - "The alpha-ordering for a wide class of
#' fuzzy sets of the real line: the particular case of fuzzy numbers":
#' weighted sum of the midpoints of the alpha-cuts.
#' @noRd
fuzzy_param_scores <- function(test_mat, n_classes, cols, alpha, metd, w, dens) {
  N <- nrow(test_mat)
  if (!metd %in% 1:5) stop("'metd' must be 1, 2, 3, 4 or 5.")
  if (metd == 4) {
    R <- matrix(0, nrow = N, ncol = n_classes)
    w <- rep_len(w, length(alpha))
    for (i in seq_len(n_classes)) {
      for (j in seq_along(alpha)) {
        mid <- 0
        for (k in seq_len(cols)) {
          ab <- dens(test_mat[, k], i, k, j)
          mid <- mid + (pmin(ab[[1]], ab[[2]]) + pmax(ab[[1]], ab[[2]])) / 2
        }
        term <- w[j] * mid
        term[is.na(term)] <- 0
        R[, i] <- R[, i] + term
      }
    }
    return(R)
  }
  j_last <- length(alpha)
  p1 <- p2 <- p3 <- matrix(0, nrow = N, ncol = n_classes)
  for (i in seq_len(n_classes)) {
    for (k in seq_len(cols)) {
      x <- test_mat[, k]
      ab1 <- dens(x, i, k, 1)
      ab2 <- dens(x, i, k, j_last)
      p1[, i] <- p1[, i] + pmin(ab1[[1]], ab1[[2]])
      p2[, i] <- p2[, i] + pmin(ab2[[1]], ab2[[2]])
      p3[, i] <- p3[, i] + pmax(ab1[[1]], ab1[[2]])
    }
  }
  switch(metd,
         Yagerdistance(p1, p2, p3),
         Qdistance(p1, p2, p3),
         Thoranidistance(p1, p2, p3),
         NULL,
         GTFNdistance(p1, p2, p3))
}

#' Turns a matrix of non-negative class scores into predicted classes or
#' row-normalized probabilities.
#' @noRd
scores_to_output <- function(R_M_obs, classes, type) {
  R_M_obs <- as.matrix(R_M_obs)
  if (type == "class") {
    return(factor(classes[max.col(R_M_obs, ties.method = "first")], levels = classes))
  }
  R_M_obs[R_M_obs == Inf] <- .Machine$integer.max
  rs <- rowSums(R_M_obs, na.rm = TRUE)
  rs[rs == 0] <- 1
  R_M_obs <- R_M_obs / rs
  colnames(R_M_obs) <- classes
  R_M_obs
}


# --------------------------------------------------------------------------
# New Pertinence Function
# --------------------------------------------
# Matriz Intervalos de Minimos
#' @noRd
minomosdt_function <- function(minimos, M, Comprim_Intervalo, Sturges, cols){
  classes <- get_classes(M)
  minomosdt <- lapply(seq_along(classes), function(i){
    k_c <- max(1L, Sturges[[i]])
    m_i <- minimos[[i]]
    w_i <- Comprim_Intervalo[[i]]
    outer(0:k_c, w_i, "*") + rep(m_i, each = k_c + 1L)
  })
  return(minomosdt)
}
# --------------------------------------------

# --------------------------------------------
# Frequencia - Matriz Esparsa
#' @noRd
Freq_esparsa <- function(dados, M, minomosdt, cols){
  dados <- as.data.frame(dados)
  classes <- get_classes(M)
  Freq_joda <- lapply(1:length(classes), function(i){
    SubSet <- dados[M == classes[i], , drop = FALSE]
    n_lin <- nrow(minomosdt[[i]])
    k_c <- max(1L, n_lin - 1)
    ara <- matrix(0, nrow = k_c, ncol = cols)
    for (j in 1:cols) {
      faixas <- minomosdt[[i]][-1, j]
      idx <- findInterval(SubSet[, j], faixas, left.open = FALSE) + 1L
      idx <- pmin(pmax(idx, 1L), k_c)
      ara[, j] <- tabulate(idx, nbins = k_c)
    }
    result <- as.data.frame(ara)
    colnames(result) <- colnames(dados)[1:cols]
    return(result)
  })
  return(Freq_joda)
}
# --------------------------------------------

# --------------------------------------------
# Soma - Matriz Esparsa
#' @noRd
soma_freq <- function(M, Freq_joda, cols){
  classes <- get_classes(M)
  Soma_joda <- lapply(1:length(classes), function(i){
    result <- Freq_joda[[i]]
    result[,(cols+1)] <- sum(Freq_joda[[i]][,(cols+1)])
    return(result)
  })
  return(Soma_joda)
}
# --------------------------------------------

# --------------------------------------------
# Pertinencia - Matriz Esparsa
#' @noRd
Pertinencia_esparsa <- function(M, Freq_joda, cols){
  classes <- get_classes(M)
  Pert_joda <- lapply(1:length(classes), function(i){
    result <- Freq_joda[[i]]
    for (j in 1:cols){
      s <- sum(result[, j])
      if (s > 0) {
        result[, j] <- result[, j] / s
      } else {
        result[, j] <- 1 / max(1, nrow(result))
      }
    }
    return(result)
  })
  return(Pert_joda)
}
# --------------------------------------------------------------------------

# --------------------------------------------------------------------------
# Pertinencia Predict - Matriz Esparsa
#' @noRd
pertinencia_predict_esparsa <- function(minomosdt, x, Pert_joda, cols, i){
  k_c <- max(1L, nrow(minomosdt[[i]]) - 1)
  p_vals <- numeric(cols)
  for (j in 1:cols) {
    faixas <- minomosdt[[i]][-1, j]
    b_idx <- pmin(pmax(findInterval(as.numeric(x[j]), faixas, left.open = FALSE) + 1L, 1L), k_c)
    val <- Pert_joda[[i]][b_idx, j]
    if (is.na(val) || val <= 0) {
      val <- 1e-5
    }
    p_vals[j] <- val
  }
  return(prod(p_vals))
}
# --------------------------------------------------------------------------

# ----------------
#' @noRd
function_new_membership_predict <- function(test, M = M, MinimosDataFrame, Pertinencia, cols = cols){
  test <- as.matrix(test[, 1:cols, drop = FALSE])
  N <- nrow(test)
  classes <- get_classes(M)
  Pertinencia_r <- lapply(seq_along(classes), function(i){
    dt_i <- MinimosDataFrame[[i]]
    k_c <- max(1L, nrow(dt_i) - 1L)
    p_mat <- matrix(0, nrow = N, ncol = cols)
    pert_i <- Pertinencia[[i]]
    for (j in seq_len(cols)) {
      faixas <- dt_i[-1L, j]
      b_idx <- pmin(pmax(findInterval(test[, j], faixas, left.open = FALSE) + 1L, 1L), k_c)
      vals <- pert_i[[j]][b_idx]
      vals[is.na(vals) | vals <= 0] <- 1e-5
      p_mat[, j] <- vals
    }
    if (cols > 1) {
      res <- p_mat[, 1]
      for (c_idx in 2:cols) res <- res * p_mat[, c_idx]
      res
    } else {
      as.numeric(p_mat[, 1])
    }
  })
  return(Pertinencia_r)
}
# ----------------

# ----------------
#' @noRd
function_new_membership_predict_dw <- function(test, M = M, MinimosDataFrame, Pertinencia, cols = cols, weta = weta){
  test <- as.matrix(test[, 1:cols, drop = FALSE])
  N <- nrow(test)
  classes <- get_classes(M)
  Pertinencia_r <- lapply(seq_along(classes), function(i){
    dt_i <- MinimosDataFrame[[i]]
    k_c <- max(1L, nrow(dt_i) - 1L)
    p_mat <- matrix(0, nrow = N, ncol = cols)
    pert_i <- Pertinencia[[i]]
    for (j in seq_len(cols)) {
      faixas <- dt_i[-1L, j]
      b_idx <- pmin(pmax(findInterval(test[, j], faixas, left.open = FALSE) + 1L, 1L), k_c)
      vals <- pert_i[[j]][b_idx]
      vals[is.na(vals) | vals <= 0] <- 1e-5
      p_mat[, j] <- (weta[j] * vals)
    }
    if (cols > 1) {
      res <- p_mat[, 1]
      for (c_idx in 2:cols) res <- res * p_mat[, c_idx]
      res
    } else {
      as.numeric(p_mat[, 1])
    }
  })
  return(Pertinencia_r)
}
# ----------------

# ----------------
#' @noRd
function_new_fuzzy_predict <- function(retorno, P, M){
  classes <- get_classes(M)
  n_classes <- length(classes)
  N <- length(P[[1]])
  f <- matrix(0, nrow = N, ncol = n_classes)
  for (i in seq_len(n_classes)) {
    f[, i] <- P[[i]] * retorno[[i]]
  }
  return(f)
}
# ----------------

##################################################################################
# FUNÇÕES DA G0I

#Definie a função de log-verossimilhança
#' @noRd
log_likelihood <- function(params, Xobs, L){
  
  #Parâmetro alfa da g0i
  alpha <- params[1]
  
  #Parâmetro gama da g0i
  gama <- params[2]
  
  #Tamanho da amostra
  n <- length(Xobs)
  
  #Parcela constante da log-verossimilhança
  const <- n * L * log(L) + n * log(gamma(L - alpha)) - alpha * n * log(gama) -
    n * log(gamma(-alpha)) - n * log(gamma(L))
  
  #Parcela variável da log-verossimilhança
  var <- (L - 1) * sum(log(Xobs)) - (L - alpha) * sum(log(gama + L * Xobs))
  
  #Resultado (somas das duas parcelas)
  res <- (const + var)
  
  #Retorno do resultado
  return(res)
  
}

#---------------------------------------------------------------------------------
#GRADIENTE

#Define o gradiente da log-verossimilhança
#' @noRd
grad0 = function(Theta, Xobs, L){
  
  #Parâmetro alfa da g0i
  alpha=Theta[1] 
  
  #Parâmetro gama da g0i
  gama=Theta[2]
  
  #Tamanho da amostra
  n=length(Xobs) 
  
  #Vetor gradiente
  c(
    #Derivada da log-verossimilhança em relação a alfa
    - n * log(gama)  - n * digamma(L-alpha) + n * digamma(-alpha) + sum( log(gama + L*Xobs) ),
    
    #Derivada da log-verossimilhança em relação a gama
    - n * alpha / gama + (alpha-L) * sum( 1 / (gama + L * Xobs) )
    
  )
  
}

#---------------------------------------------------------------------------------
#GERADOR DA G0I

#Gera uma amostra de tamanho n da distribuição g0i
#' @noRd
#' @importFrom stats rgamma
rG0I = function(n,alpha,gama,L){
  
  #Gerando a g0i pela definição como a razão de duas gamas
  res = rgamma(n,L,L) / rgamma(n,alpha,gama);
  
  return(res)
  
}

#---------------------------------------------------------------------------------
#DENSIDADE DA G0I
#Calcula o valor da densidade da distribuição g0i
#' @noRd
dG0I <- function(x, alfa, gama, L){
  
  #Numerados da g0i
  numerador <- L^L * gamma(L - alfa) * x^(L - 1)
  
  #Denominador da g0i
  denominador <- (gama ^ alfa) * gamma(-alfa) * gamma(L) * (gama + L * x)^(L - alfa)
  
  #Densidade final (razão dos dois anteriores)
  densidade <- numerador / denominador
  
  return(densidade)
  
}

# Fim das funções da G0I
##################################################################################

##################################################################################
# SHARED PARAMETER ESTIMATION FUNCTIONS
#' @noRd
estimation_parameters_gamma <- function(M, cols, dados){
  classes <- get_classes(M)
  dados <- as.data.frame(dados)
  lapply(seq_along(classes), function(i) {
    lapply(seq_len(cols), function(j) {
      fit_gamma(dados[M == classes[i], j])
    })
  })
}

#' Maximum likelihood estimates (shape, scale) of the Gamma distribution.
#' The shape solves log(a) - digamma(a) = log(mean(y)) - mean(log(y)) and
#' scale = mean(y) / shape (e.g. Johnson, Kotz & Balakrishnan, 1994).
#' Falls back to the method of moments when the sample has non-positive
#' values (the log-likelihood is not defined there).
#' @noRd
fit_gamma <- function(y) {
  y <- y[is.finite(y)]
  m <- mean(y)
  v <- if (length(y) > 1) stats::var(y) else NA
  if (!is.finite(m) || m <= 0) return(c(1, 1))
  if (!is.finite(v) || v <= 0) return(c(1e6, m / 1e6))
  if (any(y <= 0)) return(c(m^2 / v, v / m))
  s <- log(m) - mean(log(y))
  if (!is.finite(s) || s <= 0) return(c(m^2 / v, v / m))
  shape <- stats::uniroot(function(a) log(a) - digamma(a) - s,
                          lower = 1e-8, upper = 1e8, tol = 1e-12)$root
  c(shape, m / shape)
}

#' @noRd
estimation_parameters_hipergeomeric <- function(M, cols, dados, tentativas){
  classes <- get_classes(M)
  lapply(1:length(classes), function(i) {
    lapply(1:cols, function(j) {
      subTent <- tentativas[M == classes[i], j]
      N_aux <- max(subTent)
      SubSet <- dados[M == classes[i], j]
      poly_coeffs <- c(-mean(SubSet)^2 * N_aux,
                       mean(SubSet) * N_aux - (mean(SubSet)^2) - var(SubSet) * N_aux + var(SubSet),
                       -mean(SubSet))
      k <- try(Mod(polyroot(poly_coeffs))[1], silent = TRUE)
      if (inherits(k, "try-error") || is.na(k)) {
        k <- mean(SubSet)
      }
      if (k < 0) {
        k <- abs(k)
      }
      if (k == 0) {
        k <- 1
      }
      m <- mean(SubSet) * (N_aux + 1) / k
      m_round <- round(m)
      param <- c(k = round(k), m = m_round, n = max(0, N_aux - m_round), N = N_aux)
      if (param[1] == 0) { param[1] <- 1 }
      return(param)
    })
  })
}

#' @noRd
estimation_parameters_beta <- function(M, cols, dados){
  classes <- get_classes(M)
  lapply(1:length(classes), function(i) {
    lapply(1:cols, function(j) {
      SubSet <- dados[M == classes[i], j]
      param <- try(suppressWarnings(MASS::fitdistr(SubSet, "beta", start = beta_moments(SubSet), lower = c(0.001, 0.001))$estimate), silent = TRUE)
      if (inherits(param, "try-error")) {
        # Method of moments fallback for beta
        param <- unlist(beta_moments(SubSet))
      }
      return(param)
    })
  })
}

#' Method of moments estimates of the Beta distribution (also used as
#' starting values of the maximum likelihood fit).
#' @noRd
beta_moments <- function(y) {
  m <- mean(y)
  v <- stats::var(y)
  if (!is.finite(v) || v <= 0 || v >= m * (1 - m)) v <- m * (1 - m) * 0.5
  a <- m * (m * (1 - m) / v - 1)
  b <- (1 - m) * (m * (1 - m) / v - 1)
  list(shape1 = max(0.01, a), shape2 = max(0.01, b))
}

#' @noRd
estimation_parameters_exp <- function(M, cols, dados){
  classes <- get_classes(M)
  lapply(1:length(classes), function(i) {
    lapply(1:cols, function(j) {
      SubSet <- dados[M == classes[i], j]
      param <- 1 / max(1e-5, mean(SubSet, na.rm = TRUE))
      return(param)
    })
  })
}

#' @noRd
estimation_parameters_poi <- function(M, cols, dados){
  classes <- get_classes(M)
  lapply(1:length(classes), function(i) {
    lapply(1:cols, function(j) {
      SubSet <- dados[M == classes[i], j]
      param <- max(1e-5, mean(SubSet, na.rm = TRUE))
      return(param)
    })
  })
}

#' @noRd
getParametersTriangular <- function(sample) {
  sample <- as.matrix(sample)
  qc <- sapply(1:ncol(sample), function(i) stats::quantile(sample[, i], probs = c(0.0625, 0.25, 0.75, 0.9375)))
  uc <- (qc[2, ] - qc[1, ])^2
  vc <- (qc[4, ] - qc[3, ])^2
  ac <- 2 * qc[1, ] - qc[2, ]
  bc <- 2 * qc[4, ] - qc[3, ]
  denom <- uc + vc
  denom[denom == 0] <- 1
  mc <- (uc * bc + vc * ac) / denom
  if (any(ac == bc)) {
    mc[which(ac == bc)] <- ac[which(ac == bc)]
  }
  names(ac) <- colnames(sample)
  names(bc) <- colnames(sample)
  names(mc) <- colnames(sample)
  return(rbind(ac, bc, mc))
}

#' @noRd
estimation_parameters_triang <- function(M, cols, dados){
  classes <- get_classes(M)
  lapply(1:length(classes), function(i) {
    SubSet <- dados[M == classes[i], , drop = FALSE]
    getParametersTriangular(SubSet)
  })
}

#' @noRd
getParametersTrapezoidal <- function(sample) {
  sample <- as.numeric(sample)
  n <- length(sample)
  minimum <- min(sample)
  maximum <- max(sample)
  if (minimum == maximum) {
    return(c(minimum, minimum, maximum, maximum))
  }
  variance <- var(sample)
  expected <- stats::median.default(sample)
  model <- function(x) {
    denom <- 3 * x[2] - 3 * x[1] + 3 * maximum - 3 * minimum
    if (abs(denom) < 1e-10) denom <- 1e-10
    F1 <- ((-minimum * x[1] - x[1]^2 + maximum * x[2] + x[2]^2 + maximum^2 - minimum^2) / denom) - expected
    denom2 <- 18 * ((x[1] - minimum) + 2 * (x[2] - x[1]) + (maximum - x[2]))^2
    if (abs(denom2) < 1e-10) denom2 <- 1e-10
    F2 <- (((6 * (x[2] - x[1])^4 + 12 * ((x[1] - minimum) + (maximum - x[2])) *
               (x[2] - x[1])^3 + (12 * ((x[1] - minimum) + (maximum - x[2]))^2 - 6 * (x[1] - minimum) * (maximum - x[2])) * (x[2] - x[1])^2) / denom2) +
             ((6 * ((x[1] - minimum) + (maximum - x[2])) * ((x[1] - minimum)^2 + (x[1] - minimum) * (maximum - x[2]) + (maximum - x[2])^2) * (x[2] - x[1]) +
                 ((x[1] - minimum) + (maximum - x[2]))^2 * ((x[1] - minimum)^2 + (x[1] - minimum) * (maximum - x[2]) + (maximum - x[2])^2)) / denom2)) - variance
    return(c(F1 = F1, F2 = F2))
  }
  pmedio <- minimum + ((maximum - minimum) / 2)
  ss_sol <- try(suppressWarnings(rootSolve::multiroot(f = model, start = c(pmedio, pmedio + .1))), silent = TRUE)
  if (!inherits(ss_sol, "try-error")) {
    ss <- c(ss_sol$root[1], ss_sol$root[2])
    if (ss[1] > ss[2]) {
      topo1 <- ss[2]; topo2 <- ss[1]
    } else {
      topo1 <- ss[1]; topo2 <- ss[2]
    }
  } else {
    topo1 <- minimum + (maximum - minimum) / 3
    topo2 <- minimum + 2 * (maximum - minimum) / 3
  }
  topo1 <- ifelse(topo1 < minimum, minimum, topo1)
  topo2 <- ifelse(topo2 <= topo1, topo1, ifelse(topo2 >= maximum, maximum, topo2))
  return(c(minimum, topo1, topo2, maximum))
}

#' @noRd
estimation_parameters_trape <- function(M, cols, dados){
  classes <- get_classes(M)
  lapply(1:length(classes), function(i) {
    lapply(1:cols, function(j) {
      SubSet <- dados[M == classes[i], j]
      getParametersTrapezoidal(SubSet)
    })
  })
}

#' @noRd
estimation_parameters_fuzzy_bayes_rule <- function(M, cols, dados){
  classes <- get_classes(M)
  lapply(1:length(classes), function(i) {
    SubSet <- dados[M == classes[i], , drop = FALSE]
    Mean <- colMeans(SubSet)
    VarMatrix <- stats::var(SubSet)
    list(Mean, VarMatrix)
  })
}
