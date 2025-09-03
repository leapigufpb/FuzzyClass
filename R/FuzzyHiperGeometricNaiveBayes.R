#' Fuzzy Hipergeometric Naive Bayes
#'
#' \code{FuzzyHipergeometricNaiveBayes} Fuzzy Hipergeometric Naive Bayes
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
#' \insertRef{ferreira2025new}{FuzzyClass}
#'
#' @importFrom stats dhyper
#'
#' @examples
#' 
#' set.seed(1) # determining a seed
#' 
#' substituir_zero <- function(x) {
#'   if (x == 0) {
#'     while(1){
#'      new_valor <- rhyper(1,3,30,10)
#'       if(new_valor != 0){
#'         return(new_valor)
#'       }
#'     }
#'   } else {
#'     return(x)
#'   }
#' }
#' 
#' #Building dataframe
#' class1 <- data.frame(rhyper(72,3,30,10), 
#'                      sample(seq(1,5),72,replace = TRUE), class = "Bem treinado")
#' colnames(class1)[1] <- "sucessos"
#' class1$sucessos <- sapply(class1$sucessos, substituir_zero)
#' colnames(class1)[2] <- "tentativas"
#' colnames(class1)[3] <- "avaliação"
#' class2 <- data.frame(rhyper(72,3,17,12), 
#'                      sample(seq(4,7),72,replace = TRUE), class = "Mediano")
#' colnames(class2)[1] <- "sucessos"
#' colnames(class2)[2] <- "tentativas"
#' colnames(class2)[3] <- "avaliação"
#' class3 <- data.frame(rhyper(72,3,9,11),
#'                      sample(seq(7,10),72,replace = TRUE), class = "Precisa treinar")
#' colnames(class3)[1] <- "sucessos"
#' colnames(class3)[2] <- "tentativas"
#' colnames(class3)[3] <- "avaliação"
#' data <- rbind(class1,class2,class3)
#' 
#' 
#' #spliting dataframe
#' split <- caTools::sample.split(t(data[, 1]), SplitRatio = 0.7)
#' Train <- subset(data, split == "TRUE")
#' Test <- subset(data, split == "FALSE")
#' 
#' test <- Test[, -3]
#' fit_NHT <- FuzzyHipergeometricNaiveBayes(
#'  train = Train[, -3],
#'  cl = Train[, 3], cores = 2
#' )
#' 
#' pred_NHT <- predict(fit_NHT, test)
#'
#'
#'
#' @export
FuzzyHipergeometricNaiveBayes <- function(train, cl, cores = 2, fuzzy = TRUE) {
  UseMethod("FuzzyHipergeometricNaiveBayes")
}

#------------------------------

#' @export
FuzzyHipergeometricNaiveBayes.default <- function(train, cl, cores = 2, fuzzy = T) {
  
  #--------------------------------------------------------
  # Estimating class parameters
  
  train <- as.data.frame(train)
  cols <- ncol(train)
  if(is.null(cols)){
    cols <- 1
  }
  if(cols %% 2 != 0){
    stop("Inconsistent number of columns")
  }
   if(is.null(cols)){
    cols <- 2
  }
  dados <- train # training data matrix
  M <- c(unlist(cl)) 
  M <- factor(M, labels = sort(unique(M))) 
  #--------------------------------------------------------
  # --------------------------------------------------------
  # Verify data typesR_M_obs
  verifyNumbers <- sapply(1:cols, function(i){
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
  
  #Spliting attempts and successes
  tentativas <- as.data.frame(dados[, seq(2, cols, by = 2)])
  dados <- as.data.frame(dados[, seq(1, cols, by = 2)])
  cols <- cols / 2
  
  #--------------------------------------------------------
 
  # Estimating Parameters
  parametersC <- estimation_parameters_hipergeomeric(M, cols, dados, tentativas) 
 
  #--------------------------------------------------------
  
  # --------------------------------------------------------
  Sturges <- Sturges(dados, M);
  Comprim_Intervalo <- Comprim_Intervalo(dados, M, Sturges);
  minimos <- minimos(dados, M, cols);
  MinimosDataFrame <- minomosdt_function(minimos, M, Comprim_Intervalo, Sturges, cols)
  Frequencia <- Freq_esparsa(dados = dados,M = M, minomosdt = MinimosDataFrame, cols = cols)
  Pertinencia <- Pertinencia_esparsa(M = M, Frequencia, cols = cols)
  # ------
  # A priori probability of classes - considered equal
  pk <- rep(1 / length(unique(M)), length(unique(M)))
  #--------------------------------------------------------

  #-------------------------------------------------------
  
  structure(list(
    parametersC = parametersC,
    minimos = minimos,
    MinimosDataFrame = MinimosDataFrame,
    cols = cols,
    M = M,
    cores = cores,
    Comprim_Intervalo = Comprim_Intervalo,
    Pertinencia = Pertinencia,
    Sturges = Sturges,
    pk = pk,
    fuzzy = fuzzy
  ),
  class = "FuzzyHipergeometricNaiveBayes"
  )
}

#-------------------------


#' @export
print.FuzzyHipergeometricNaiveBayes <- function(x, ...) {
  if (x$fuzzy == T) {
    #-----------------
    cat("\nFuzzy Hipergeometric Naive Bayes Classifier for Discrete Predictors\n\n")
    #-----------------
  } else {
    #-----------------
    cat("\nNaive Hipergeometric  Bayes Classifier for Discrete Predictors\n\n")
    #-----------------
  }
  cat("Class:\n")
  print(levels(x$M))
  #-----------------
}

#' @export
predict.FuzzyHipergeometricNaiveBayes <- function(object,
                                                  newdata,
                                                  type = "class",
                                                  ...) {
  #--------------------------------------------------------
  test <- as.data.frame(newdata)
  #--------------------------------------------------------
  parametersC <- object$parametersC
  minimos <- object$minimos
  MinimosDataFrame <-  object$MinimosDataFrame
  cols <- object$cols
  M <- object$M
  cores <- object$cores
  Comprim_Intervalo <- object$Comprim_Intervalo
  Pertinencia <- object$Pertinencia
  Sturges <- object$Sturges
  pk <- object$pk
  fuzzy <- object$fuzzy
  #--------------------------------------------------------
  
  #--------------------------------------------------------
  # Classification
  #--------------
  # ---------
  N_test <- nrow(test)
  # --
  # --
  if(fuzzy == T){
    Pertinencia_r <- function_new_membership_predict(test, M = M, MinimosDataFrame, Pertinencia, cols = cols)
    R_M_obs <- fuzzy_density_values_hipergeometric(M = M, cols = cols,
                                                      test = test, parametersC = parametersC,
                                                      pk = pk,
                                                      Pertinencia_r = Pertinencia_r)
    
  }else{
    R_M_obs <- density_values_hipergeometric(M = M, cols = cols,
                                                test = test, parametersC = parametersC,
                                                pk = pk)
  }
  # ---------
  if (type == "class") {
    #-------------------------
    R_M_obs <- sapply(1:nrow(R_M_obs), function(i) which.max(R_M_obs[i, ]))
    resultado <- unique(M)[R_M_obs]
    return(as.factor(c(resultado)))
    #-------------------------
  } else {
    #-------------------------
    Infpos <- which(R_M_obs==Inf)
    R_M_obs[Infpos] <- .Machine$integer.max;
    R_M_obs <- matrix(unlist(R_M_obs),ncol = length(unique(M)), nrow = N_test)
    R_M_obs <- R_M_obs/rowSums(R_M_obs,na.rm = T)
    #----------
    colnames(R_M_obs) <- unique(M)
    return(R_M_obs)
    #-------------------------
  }
}

# ----------------
density_values_hipergeometric <- function(M, cols, test, parametersC, pk){
  saida_temp <- lapply(1:length(unique(M)), function(i) {
    densidades <- sapply(1:cols, function(j) {
      t <- round(test[, j])
      #casos_de_sucesso estimado   #fracassos da amostra = Total - sucessos   #número de tentativas estimado
      (stats::dhyper(t, m = parametersC[[i]][[j]][2],
                     n = parametersC[[i]][[j]][3],
                     k = parametersC[[i]][[j]][1])) #calcula a probabilidade de p(x=k) de cada uma das variáveis contidas nas colunas dos dados de teste
    })
    densidades <- apply(densidades, 1, prod)
    # Calcula a P(w_i) * P(X_k | w_i)
    p <- pk[[i]] * densidades
    # ---
    return(p)
  })
  
  saida <- data.frame(matrix(unlist(saida_temp), ncol=length(saida_temp), byrow=F))
  return(saida)
}
# ----------------

# ----------------
fuzzy_density_values_hipergeometric <- function(M, cols, test, parametersC, pk, Pertinencia_r){
  saida_temp <- lapply(1:length(unique(M)), function(i) {
    densidades <- sapply(1:cols, function(j) {
      t <- round(test[, j])
      #casos_de_sucesso estimado   #fracassos da amostra = Total - sucessos   #número de tentativas estimado
      (stats::dhyper(t, m = parametersC[[i]][[j]][2],
                     n = parametersC[[i]][[j]][3],
                     k = parametersC[[i]][[j]][1])*
          Pertinencia_r[[i]]) #calcula a probabilidade de p(x=k) de cada uma das variáveis contidas nas colunas dos dados de teste
    })
    densidades <- apply(densidades, 1, prod)
    # Calcula a P(w_i) * P(X_k | w_i)
    p <- pk[[i]] * densidades
    # ---
    return(p)
  })
  
  saida <- data.frame(matrix(unlist(saida_temp), ncol=length(saida_temp), byrow=F))
  return(saida)
}
# ----------------


# ----------------
estimation_parameters_hipergeomeric <- function(M, cols, dados, tentativas){
  lapply(1:length(unique(M)), function(i) {
    lapply(1:cols, function(j) { 
      #print(c(i,j))
      subTent <- tentativas[M == unique(M)[i], j]
      N_aux <- max(subTent)
      SubSet <- dados[M == unique(M)[i], j]
      k <- try(Mod(polyroot(c(-mean(SubSet)^2*N_aux, mean(SubSet)*N_aux - (mean(SubSet)^2) - var(SubSet)*N_aux + var(SubSet),-mean(SubSet))))[1], silent = TRUE) #Recebe o valor que zera a função de estimação_N
      cat(k)
      cat("\n")
      if(k < 0){
        k <- k * -1
      }
      #ADICIONAR TRATAMENTO PARA RAIZES COMPLEXAS
      m <- mean(SubSet) * (N_aux+1) / k
      # --
      param <- c(k = round(k), m = round(m), n = (N_aux - round(m)), N = N_aux)

      if(param[1]==0){param[1] <- 1}
      # --
      return(param)
    })
  })
}

# ----------------