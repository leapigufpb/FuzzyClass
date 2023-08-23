
# Verify Columns
#' @noRd
verifyColumns <- function(dados){
  cols <- ncol(dados)# Number of variables
  if(is.null(cols) | cols == 0){
    cols <- 1
  }
  return(cols)

}

# PreData
#' @noRd
predata <- function(dados,cl){
  train <- as.data.frame(dados)
  cols <- verifyColumns(dados)
  M <- c(unlist(cl)) # true classes
  M <- factor(M, labels = sort(unique(M)))
  intervalos <- 10 # Division to memberships

  lista = list(train = train,
               dados = dados,
               cols = cols,
               M = M,
               intervalos = intervalos)

  return(lista)

}



#---- Sturges -----#

#' @noRd
Sturges <- function(dados, M) {

  Output <- lapply(1:length(unique(M)), function(i) {
    SubSet <- dados[M == unique(M)[i], ]
    if(ncol(dados) == 1){
      return(round(sqrt(length(SubSet))))
    }else{
      return(round(sqrt(nrow(SubSet))))
    }

  })

  return(Output)
}


#---- Range Length ----#

#' @noRd
Comprim_Intervalo <- function(dados, M, Sturges) {

  Output <- lapply(1:length(unique(M)), function(i) {
    SubSet <- dados[M == unique(M)[i], ]
    if(ncol(dados) == 1){
      comp <- (max(SubSet) - min(SubSet)) / Sturges[[i]]
    }else{
      comp <- (apply(SubSet, 2, max) - apply(SubSet, 2, min)) / Sturges[[i]]
    }
  })

  return(Output)
}
#--------------------------------------------------------

#' @noRd
minimos <- function(dados, M, cols) {

  Output <- lapply(1:length(unique(M)), function(i) {
    sapply(1:cols, function(j) {
      SubSet <- dados[M == unique(M)[i], ]
      if (ncol(dados) == 1){
        return(min(SubSet))
      }else{
        return(min(SubSet[, j]))
      }
    })
  })

  return(Output)
}

#---------------

#---------------
#' @noRd
maximos <- function(dados, M, cols) {

  Output <- lapply(1:length(unique(M)), function(i) {
    sapply(1:cols, function(j) {
      SubSet <- dados[M == unique(M)[i], ]
      if (ncol(dados) == 1){
        return(max(SubSet))
      }else{
        return(max(SubSet[, j]))
      }
    })
  })

  return(Output)
}

#---------------


#' @noRd
Freq <- function(dados, M, Comprim_Intervalo, Sturges, minimos, cols) {

  #--------------------------------------------------------
  Freq <- lapply(1:length(unique(M)), function(i) {
    ara <- array(0, dim = c(Sturges[[i]], cols))
    return(ara)
  })
  for (classe in 1:length(unique(M))) {
    # --
    SubSet <- dados[M == unique(M)[classe], ]
    SubSet <- as.data.frame(SubSet)
    NN = nrow(SubSet)
    # --
    for (coluna in 1:cols) { # class column
      for (linhaClasse in 1:NN) { # class row
        faixa <- minimos[[classe]][coluna] + Comprim_Intervalo[[classe]][coluna] # initial frequency band
        for (linhaFreq in 1:Sturges[[classe]]) { # frequency line
          # --
          if (SubSet[linhaClasse, coluna] < faixa) { # checks if the class value belongs to that range
            Freq[[classe]][linhaFreq, coluna] <- Freq[[classe]][linhaFreq, coluna] + 1 # accumulates value in the frequency range and interrupts this last 'for'
            break
          }
          if (linhaFreq == Sturges[[classe]] && SubSet[linhaClasse, coluna] >= faixa) {
            Freq[[classe]][linhaFreq, coluna] <- Freq[[classe]][linhaFreq, coluna] + 1
            break
          }
          faixa <- faixa + Comprim_Intervalo[[classe]][coluna] # track change -> next
        }
      }
    }
  }
  #----------
  return(Freq)
  #----------
}


#' @noRd
Intervalos_Valores <- function(dados, M, Comprim_Intervalo, Sturges, minimos, cols) {

  #--------------------------------------------------------
  Freq <- lapply(1:length(unique(M)), function(i) {
    ara <- array(0, dim = c(Sturges[[i]], cols))
    return(ara)
  })
  for (classe in 1:length(unique(M))) {
    # --
    SubSet <- dados[M == unique(M)[classe], ]
    # --
    for (coluna in 1:cols) { # class column
      for (linhaClasse in 1:nrow(SubSet)) { # class row
        faixaant <- minimos[[classe]][coluna]
        faixa <- minimos[[classe]][coluna] + Comprim_Intervalo[[classe]][coluna] # initial frequency band
        for (linhaFreq in 1:Sturges[[classe]]) { # frequency line
         # --
           Freq[[classe]][linhaFreq, coluna] <- paste0(round(faixaant,3),",",round(faixa,3));
           # --
          faixaant <- faixa
          faixa <- faixa + Comprim_Intervalo[[classe]][coluna] # track change -> next
        }
      }
    }
  }
  #----------
  return(Freq)
  #----------
}


#--------------------------------------------------------
#---- Pertinencia ----#

#' @noRd
Pertinencia <- function(Freq, dados, M) {

  dados <- as.data.frame(dados)
  Output <- lapply(1:length(unique(M)), function(i) {
    if(ncol(dados) == 1){
      NN = length(dados[M == unique(M)[i], ])
    }else{
      NN = nrow(dados[M == unique(M)[i], ])
    }
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
  for (classe in 1:length(unique(M))) {
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
  row.names(ACHOU_t) <- unique(M)
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
  for (classe in 1:length(unique(M))) {
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
  row.names(ACHOU_t) <- unique(M)
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
  f <- sapply(1:length(unique(M)), function(i) {
    P[[i]] * retorno2[,i]
  })

  return(f)

}
# ----------------

# ----------------
#' @noRd
function_fuzzy_predict_log <- function(retorno, P, M){

  retorno2 <- data.frame(matrix(unlist(retorno), nrow=length(retorno), byrow=TRUE))
  f <- sapply(1:length(unique(M)), function(i) {
    P[[i]] + log(retorno2[,i])
  })

  return(f)

}
# ----------------




#' @noRd
log_ver_Gamma <- function(theta,y){

  # Gamma Likelihood Function

  beta = theta[2]
  alpha = theta[1]

  saida <- - sum(dgamma(y,shape = alpha, scale = beta, log = T),na.rm=T)

  return(saida)
}


#' Barycenter
#' Yager method
#' @noRd
Yagerdistance <- function(vec_trian, M){

  value <- sapply(1:length(unique(M)), function(i) (vec_trian[[i]][1] + 2*vec_trian[[i]][2] + vec_trian[[i]][3]) / 4)
  return(value)

}

#' Using distance Q
#' @noRd
Qdistance <- function(vec_trian, M){

  # ------------
  # Using distance Q
  value <- sapply(1:length(unique(M)), function(i) {
      # ------------
      # Start 3 values
      y <- vec_trian[[i]]
      # ------------
      # getting the product zz*
      S <- y %*% t(Conj(y)) # matrix k x k
      # ------------
      # getting the eigenvalues
      l <- eigen(S)$values
      # Calculating Q
      Q <- 3 * (l[1] - l[2])^2
      # ------------
      return(Q)
    })
    # ------------

  return(value)

}


#' Thorani
#' Article: Ordering Generalized Trapezoidal Fuzzy Numbers Using Orthocentre of Centroids
#' "changes were made"
#' @noRd
Thoranidistance <- function(vec_trian, M){

  # ------------
  # Using distance Q
  value <- sapply(1:length(unique(M)), function(i) {
    (1/length(unique(M)))*
      vec_trian[[i]][2] * ((vec_trian[[i]][2] - vec_trian[[i]][1])*(vec_trian[[i]][3] - vec_trian[[i]][2]) + 1)


  })

  return(value)
}


#' Alpha-Order for a class of fuzzy sets
#' Article: Alpha-Order for a class of fuzzy sets
#' "changes were made"
#' @noRd
AlphaOrderFuzzy <- function(vec_trian, w,  M){

  vec_mean <- lapply(1:length(vec_trian), function(i) rowMeans(vec_trian[[i]]))
  # ------------
  value <- sapply(1:length(unique(M)), function(i) {
    sum(w*vec_mean[[i]],na.rm = T)
  })

  return(value)
}

# --------------------------------------------------------------------------
# New Pertinence Function
# --------------------------------------------
# Matriz Intervalos de Minimos
#' @noRd
minomosdt_function <- function(minimos, M, Comprim_Intervalo, Sturges, cols){
  # --------------------------------------------
  minomosdt <- lapply(1:length(unique(M)),function(i){
    minim <- sapply(1:cols, function(j){
      minimiosx <- c(minimos[[i]][j])
      for(h in 2:(Sturges[[i]]+1)){
        minimiosx[h] <- minimiosx[h-1] + Comprim_Intervalo[[i]][j]
      }
      return(minimiosx)
    })
    return(minim)
  })
  # --------------------------------------------
  return(minomosdt)
}
# --------------------------------------------



`%>%` <- magrittr::`%>%`

# --------------------------------------------
# Frequencia - Matriz Esparsa
#' @noRd
Freq_esparsa <- function(dados, M, minomosdt, cols){

  Freq_joda <- lapply(1:length(unique(M)), function(i){
    Frequencia <- data.frame()
    XX <- subset(dados, M == unique(M)[i])
    for (t in 1:nrow(XX)){

      x <- XX[t,]

      vetor <- sapply(1:cols, function(j){
        max(which(as.numeric(x[j]) >= minomosdt[[i]][,j]))
      })

      Frequencia <- rbind(Frequencia, c(vetor,1))
    }

    # --------------------------------------------
    result <- Frequencia %>%
      dplyr::group_by(Frequencia[,-(cols+1)]) %>%
      dplyr::summarize(Freq = dplyr::n())
    # --------------------------------------------
    result <- data.frame(result)
    return(result)
  })
  # --------------------------------------------
  return(Freq_joda)

}
# --------------------------------------------

# --------------------------------------------
# Soma - Matriz Esparsa
#' @noRd
soma_freq <- function(M, Freq_joda, cols){
  Soma_joda <- lapply(1:length(unique(M)), function(i){
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
  # --------------------------------------------
  Pert_joda <- lapply(1:length(unique(M)), function(i){
    result <- Freq_joda[[i]]
    result[,(cols+1)] <- Freq_joda[[i]][,(cols+1)]/sum(Freq_joda[[i]][,(cols+1)])
    return(result)
  })
  # --------------------------------------------
  return(Pert_joda)
}
# --------------------------------------------------------------------------


# --------------------------------------------------------------------------
# Pertinencia Predict - Matriz Esparsa
#' @noRd
pertinencia_predict_esparsa <- function(minomosdt, x, Pert_joda, cols,i){

  vetor <- try(sapply(1:cols, function(j){
    maximo <- which(as.numeric(x[j]) >= minomosdt[[i]][,j])
    if(length(maximo)!=0){
      return(max(maximo))
    }else{
      return(0)
    }


  }),silent = T)

  if(any(0 %in% vetor)){
    Pertinencia_r <- 2.2e-16
    return(Pertinencia_r)
  }else{
    Perts <- Pert_joda[[i]]
    posicao <- which(apply(Perts[,-(cols+1)], 1, function(row) all(row == vetor)))

    if(length(posicao) == 0){
      Pertinencia_r <- 2.2e-16
      return(Pertinencia_r)
    }else{
      Pertinencia_r <- Perts[posicao,(cols+1)]
      return(Pertinencia_r)
    }
  }

}
# --------------------------------------------------------------------------

# ----------------
#' @noRd
function_new_membership_predict <- function(test, M = M, MinimosDataFrame, Pertinencia, cols = cols){
  Pertinencia_r <- lapply(1:length(unique(M)), function(i){
    sapply(1:nrow(test), function(j){
      x <- test[j,]
      pertinencia_predict_esparsa(minomosdt = MinimosDataFrame, x, Pert_joda = Pertinencia, cols,i)
    })
  })

  return(Pertinencia_r)
}
# ----------------

# ----------------
#' @noRd
function_new_fuzzy_predict <- function(retorno, P, M){

  f <- sapply(1:length(unique(M)), function(i) {
    P[[i]] * retorno[[i]]
  })

  return(f)

}
# ----------------
