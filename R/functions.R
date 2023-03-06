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



# ----------------
#' @noRd
function_membership_predict <- function(x, M = M, Sturges = Sturges, minimos = minimos, Comprim_Intervalo = Comprim_Intervalo, Pertinencia = Pertinencia, cols = cols){

  ACHOU_t <- pertinencia_predict(M, Sturges, minimos, Comprim_Intervalo, Pertinencia, cols, x);
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



#' @noRd
log_ver_Gamma <- function(theta,y){

  # Gamma Likelihood Function

  beta = theta[2]
  alpha = theta[1]

  saida <- - sum(dgamma(y,shape = alpha, scale = beta, log = T),na.rm=T)

  return(saida)
}


#' @noRd
#' Barycenter
#' Yager method
Yagerdistance <- function(vec_trian, M){

  value <- sapply(1:length(unique(M)), function(i) (vec_trian[[i]][1] + 2*vec_trian[[i]][2] + vec_trian[[i]][3]) / 4)
  return(value)

}

#' @noRd
#' Using distance Q
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


#' @noRd
#' Yagger

#' @noRd
#' Thorani
#' Article: Ordering Generalized Trapezoidal Fuzzy Numbers Using Orthocentre of Centroids
#' "changes were made"
Thoranidistance <- function(vec_trian, M){

  # ------------
  # Using distance Q
  value <- sapply(1:length(unique(M)), function(i) {
    (1/length(unique(M)))*
      vec_trian[[i]][2] * ((vec_trian[[i]][2] - vec_trian[[i]][1])*(vec_trian[[i]][3] - vec_trian[[i]][2]) + 1)


  })

  return(value)
}


#' @noRd
#' Alpha-Order for a class of fuzzy sets
#' Article: Alpha-Order for a class of fuzzy sets
#' "changes were made"
AlphaOrderFuzzy <- function(vec_trian, w,  M){

  vec_mean <- lapply(1:length(vec_trian), function(i) rowMeans(vec_trian[[i]]))
  # ------------
  value <- sapply(1:length(unique(M)), function(i) {
    sum(w*vec_mean[[i]],na.rm = T)
  })

  return(value)
}
