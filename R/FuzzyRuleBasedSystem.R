#' Fuzzy Rule-based System
#'
#' \code{FuzzyRuleBasedSystem} Fuzzy Rule-based System
#'
#'
#' @references
#' \insertRef{de2020new}{FuzzyClass}
#'
#' @examples
#'
#' ## Creating Fuzzy Variables and Fuzzy Partitions
#' ## Example 1 - It is the same of the R-Package 'Sets'
#'
#' ## Creating Fuzzy Variables and Fuzzy Partitions
#' ## Example is the same of the R-Package 'Sets'
#' ## One Domain was used for Evaluation and Tip
#'
#' ## Universe X - evaluation by the consumer in [0,10]
#' minX = 0
#' maxX = 25
#' X <- Set.Universe(minX, maxX)
#' FRBS = "Mandani"
#'
#' ## Service
#' service.poor <- GauFS(X, 0.0, 1.5)
#' #plotFS(X,service.poor)
#'
#' service.good <- GauFS(X, 5.0, 1.5)
#' #plotFS(X,service.good)
#'
#' service.excelent <- GauFS(X, 10, 1.5)
#' #plotFS(X,service.excelent)
#'
#' ## Food
#' food.rancid <- TraFS(X, 0, 0, 2, 4)
#' #plotFS(X,food.rancid)
#'
#' food.delicious <- TraFS(X, 7, 9, 10, 10)
#' #plotFS(X,food.delicious)
#'
#' ## Tip
#' tip.cheap <- TriFS(X, 0, 5, 10)
#' #plotFS(X,tip.cheap)
#'
#' tip.average <- TriFS(X, 7.5, 12.5, 17.5)
#' #plotFS(X,tip.average)
#'
#' tip.generous <- TriFS(X, 15, 20, 25)
#' #plotFS(X,tip.generous)
#'
#' ## Set the input
#' ## Evaluation by the consumer (Facts)
#'
#' service = 3
#' food = 8
#'
#' ## Rules -- Specify rules and add outputs in a list
#' ## A list is created in order to store the output of each rule
#'
#' ## Out_Rules_Lst <- list()
#'
#'
#' ## 1) IF service is poor OR food is rancid THEN tip is cheap
#' ## antecedent of the rule
#' ## temp <- NOT(FRBS, dmFS(service,X,service.poor))
#' temp <- OR(FRBS, dmFS(service,X,service.poor), dmFS(food,X,food.rancid))
#'
#' # consequent of the rule
#' fr1 <- Implication(FRBS, temp, X, tip.cheap)
#' plotFS(X,fr1)
#'
#'
#' ## 2) IF service is good THEN tip is average
#'
#' ## temp <- NOT(FRBS, dmFS(service,X,service.good))
#' temp <- dmFS(service,X,service.good)
#' fr2 <- Implication(FRBS, temp, X, tip.average)
#' plotFS(X,fr2)
#'
#' ## Cria lista com as saidas. Precisa de duas para dar certo
#' Out_Rules_Lst <- list(fr1,fr2)
#'
#'
#' ## 3) IF service is excelent OR food is delicious THEN tip is generous
#'
#' ## temp <- NOT(OR(FRBS, dmFS(service,X,service.excelent), dmFS(food,X,food.delicious)))
#' temp <- OR(FRBS, dmFS(service,X,service.excelent), dmFS(food,X,food.delicious))
#' fr3 <- Implication(FRBS, temp, X, tip.generous)
#' plotFS(X,fr3)
#'
#' ## Cria uma nova lista com a saída. Agora é possível concatenar as listas
#' lst_aux <- list(fr3)
#'
#' ## Compute the agregation of all previous Rules
#' ## Concatenando as listas
#' Out_Rules_Lst <- c(Out_Rules_Lst, lst_aux)
#'
#' ## Numero de regras eh o comprimento da lista
#' length(Out_Rules_Lst)
#'
#' OutFS <-Aggregation(FRBS, X, Out_Rules_Lst)
#' plotFS(X,OutFS)
#'
#'
#' ## Compute centroid in order provide final decision
#'
#' FinalDecision <- centroidFS(X,OutFS)
#' FinalDecision
#'

###################################################################
## Define the Universe X (a limited set) of Work (Set.Universe): ##
## from min(X) to max(X) with fixed resolution = 0.05            ##
###################################################################
## Bug: trash in the last decimals of Universe was fixed with [X = round(X,6)]

#' @export
#' @rdname hidden_functions
#' @keywords internal
Set.Universe <- function(minX, maxX) {
  ## Check if the membership function can be created for those parameters and the Universe U
  flag = 1
  if (maxX < minX) {
    print("Error: The Universe can not be created: min < max")
    flag = 0
  }

  if (flag == 1) {
    resolution = 0.05
    h <- round((maxX - minX)/resolution) + 1
    X <- rep(0, h)
    X[1] <- minX
    aux <- minX
    i = 2
    while ( i <= h ) {
      X[i] <- X[i-1] + resolution
      i=i+1
    }

    # Clean the vector w.r.t. some trashes in the last decimals
    X = round(X,6)
    # return Universe by resolution
    return(X)
  }
  ## Senão, o que retorna ???
}
######################################################
## The sets XXXFS must be suppourt in the Universe, ##
## it means: min(X) <= support(xxxFS) <= max(X)     ##
######################################################
##################################
## Triangular fuzzy set (TriFS) ##
## Parameters:                  ##
## a - the lower boundary       ##
## b - max value of MF - center ##
## c - the upper boundary       ##
##################################
##  Bug shift MF was fixed. Changed code for copying MF_temp to the MF vector
#' @export
#' @rdname hidden_functions
TriFS <- function(U,a,b,c) {
  ## Check if the membership function can be created for those parameters and the Universe U
  flag = 1
  if (a < min(U) || a > max(U) || c < min(U) || c > max(U)) {
    print("Support of the Fuzzy Sets must be include in the Universe")
    flag = 0
  }
  if (a > b || b > c || c < a) {
    print("a <= b <= c for a triangular membership function")
    flag = 0
  }
  if (flag == 1) {
    ## Auxiliar variables
    aa <- 0.0
    bb <- 1.0
    T1 <- c(a, b)
    T2 <- c(aa, bb)
    T3 <- c(b, c)
    T4 <- c(bb, aa)

    supportL <- seq(a, b, 0.05)
    supportR <- seq(b, c, 0.05)

    ##linear regresssions to estimate fuzzy set
    res=stats::lm(formula = T2 ~ T1)
    intercept1 <- res$coefficients[1]
    coeff1 <- res$coefficients[2]
    res=stats::lm(formula = T4 ~ T3)
    intercept2 <- res$coefficients[1]
    coeff2 <- res$coefficients[2]

    ## Left-side of a triangular fuzzy set (a,b)
    if(length(supportL) == 1) {
      left_interval = 1
    } else {
      left_interval <- supportL*coeff1 + intercept1
    }

    ## Right-side of a triangular fuzzy set (c,d)
    if (length(supportR) == 1) {
      right_interval = 1
    } else {
      right_interval <-  supportR*coeff2 + intercept2
    }

    ## Unifying Left and Right
    si <- supportR
    si <- si[2:length(si)]
    support = c(supportL, si)
    ri <- right_interval
    ri<- ri[2:length(ri)]
    MF_temp=c(left_interval, ri)

    pos_min_support <- max(which(U<=a))
    pos_max_support <- min(which(U>=c))

    MF <- rep(0, length(U))

    ## Copia MF_temp para o vetor MF substituindo os zeros nas posicoes adequadas

    j=1
    for (i in pos_min_support: pos_max_support) {
      MF[i] <- MF_temp[j]
      j = j + 1
    }

    ## Build the matrix MF_Tri with Universe and its Triangular Membership Function
    MF_Tri <- cbind(U, MF)


    ## Compute the alpha-cuts for the Triangular Fuzzy Set
    alpha <- seq(0, 1.0, 0.05)

    ##linear regresssions to estimate fuzzy set
    res=stats::lm(formula = T1 ~ T2)
    intercept1 <- res$coefficients[1]
    coeff1 <- res$coefficients[2]
    res=stats::lm(formula = T3 ~ T4)
    intercept2 <- res$coefficients[1]
    coeff2 <- res$coefficients[2]

    ## Left-side of a triangular fuzzy set (a,b)
    left_interval <- alpha*coeff1 + intercept1
    ## Right-side of a triangular fuzzy set (c,d)
    right_interval <-  alpha*coeff2 + intercept2

    ri <- sort(right_interval)
    ri<- ri[2:length(ri)]
    support=c(left_interval, ri)
    compl <- (1-alpha)
    compl <- compl[2:length(compl)]
    alpha_cut=c(alpha, compl)
    alpha_cuts <- cbind(support, alpha_cut)

    ## Output is a list with the name of type of FS, its parameters,
    ## the Membership Function created and its alpha cuts

    Lst <- list(name="Triangular", parameters=c(a,b,c), Membership.Function=MF_Tri, alphacuts=alpha_cuts)

    # return(MF_Tri)
    return(Lst)
  }
  ## Senão, o que retorna ??? return(NULL) ???
}
###################################################
## Trapezoidal fuzzy set (TraFS)                 ##
## Parameters:                                   ##
## a - the lower boundary                        ##
## b and c - flat or interval of max value of MF ##
## d - the upper boundary                        ##
###################################################
#' @export
#' @rdname hidden_functions
TraFS <- function(U,a,b,c,d) {

  ## Verifica se o valor de 'a' é maior a 'minX'. Se for, então
  ## {
  ##   Verifica se 'b' é maior a 'minX'. Se for, então
  ##   {
  ##      Verifica se 'c' é maior a 'minX' E 'c' é menor que 'maxX. Se for, então
  ##      {

  ## Check if the membership function can be created for those parameters and the Universe U
  flag = 1
  if (a < min(U) || a > max(U) || d < min(U) || d > max(U)) {
    print("Support of the Fuzzy Sets must be include in the Universe")
    flag = 0
  }
  if (a > b || a > c || a > d || b > c || b > d || c > d) {
    print("a <= b <= c <= d for a trapezoidal membership function")
    flag = 0
  }
  if (flag == 1) {
    ## Auxiliar variables
    aa <- 0.0
    bb <- 1.0
    T1 <- c(a, b)
    T2 <- c(aa, bb)
    T3 <- c(c, d)
    T4 <- c(bb, aa)
    supportL <- seq(a, b, 0.05)
    supportC <- seq(b, c, 0.05)
    supportR <- seq(c, d, 0.05)

    ##linear regresssion to estimate fuzzy set
    res=stats::lm(formula = T2 ~ T1)
    intercept1 <- res$coefficients[1]
    coeff1 <- res$coefficients[2]
    res=stats::lm(formula = T4 ~ T3)
    intercept2 <- res$coefficients[1]
    coeff2 <- res$coefficients[2]

    ## Left-side of a trapezoidal fuzzy set (a,b)
    if(length(supportL) == 1) {
      left_interval = 1
    } else {
      left_interval <- supportL*coeff1 + intercept1
    }

    ## Flat of a trapezoidal fuzzy set (b,c)
    center_interval <- rep(1, length(supportC))

    ## Right-side of a trapezoidal fuzzy set (c,d)
    if (length(supportR) == 1) {
      right_interval = 1
    } else {
      right_interval <-  supportR*coeff2 + intercept2
    }

    ## Unifying Left, Center and Right
    ui <- supportC
    ui <- ui[2:length(ui)]
    si <- supportR
    si <- si[2:length(si)]
    support = c(supportL, ui, si)
    ti <- center_interval
    ti<- ti[2:length(ti)]
    ri <- right_interval
    ri<- ri[2:length(ri)]
    MF_temp=c(left_interval, ti, ri)

    pos_min_support <- max(which(U<=a))
    pos_max_support <- min(which(U>=d))

    MF <- rep(0, length(U))

    ## Copia MF_temp para o vetor MF substituindo os zeros nas posicoes adequadas

    j=1
    for (i in pos_min_support: pos_max_support) {
      MF[i] <- MF_temp[j]
      j = j + 1
    }

    MF <- round(MF, 7)
    MF_Tra <- cbind(U, MF)

    ## Compute the alpha-cuts for the Triangular Fuzzy Set
    alpha <- seq(0, 1.0, 0.05)

    ##linear regresssions to estimate fuzzy set
    res=stats::lm(formula = T1 ~ T2)
    intercept1 <- res$coefficients[1]
    coeff1 <- res$coefficients[2]
    res=stats::lm(formula = T3 ~ T4)
    intercept2 <- res$coefficients[1]
    coeff2 <- res$coefficients[2]

    ## Left-side of a trapezoidal fuzzy set (a,b)
    left_interval <- alpha*coeff1 + intercept1
    ## Flat of a trapezoidal fuzzy set (b,c)
    center_interval <- seq(b, c, 0.05)
    ## Right-side of a trapezoidal fuzzy set (c,d)
    right_interval <-  alpha*coeff2 + intercept2

    ri <- sort(right_interval)
    ri<- ri[2:length(ri)]
    ti <- center_interval
    ti<- ti[2:length(ti)]
    support=c(left_interval, ti, ri)
    compl <- (1-alpha)
    compl <- compl[2:length(compl)]
    compl2 <- rep(1, length(ti))
    ## compl2 <- compl2[2:length(compl2)]
    alpha_cut=c(alpha, compl2, compl)
    alpha_cuts <- cbind(support, alpha_cut)

    ## Output is a list with the name of type of FS, its parameters,
    ## the Membership Function created and its alpha cuts

    Lst <- list(name="Trapezoidal", parameters=c(a,b,c,d), Membership.Function=MF_Tra, alphacuts=alpha_cuts)

    # return(MF_Tra)
    return(Lst)
  }
  ## Senão, o que retorna ??? return(NULL) ???
}
##################################
## Constant fuzzy set (ConstFS) ##
## Parameters:                  ##
## x is the MF constant value   ##
## for all elements in U        ##
##################################
ConstFS <- function(U,x) {
  ## Check if the membership function can be created for those parameters and the Universe U
  flag = 1
  if (x < 0 || x > 1) {
    print("Value x for Constant Fuzzy Set must be include in the [0,1]")
    flag = 0
  }
  if (flag == 1) {
    MF <- rep(x, length(U))
  }
  MF_Const <- cbind(U, MF)

  ## Compute the alpha-cuts for the Constant Fuzzy Set - REVER - [min(U),max(U)] VALE AtEH 0.7
  alpha <- seq(0, 1.0, 0.05)
  left_interval <- rep(0, length(alpha))
  right_interval <- rep(0, length(alpha))
  interval <- seq(0, x, 0.05)

  ## Left-side of a constant fuzzy set
  for (i in 1 : length(interval)) {
    left_interval[i] <- min(U)
  }
  ## Right-side of a constant fuzzy set
  for (i in 1 : length(interval)) {
    right_interval[i] <- max(U)
  }

  ri <- sort(right_interval)
  ri<- ri[2:length(ri)]
  support=c(left_interval, ri)
  compl <- (1-alpha)
  compl <- compl[2:length(compl)]
  alpha_cut=c(alpha, compl)
  alpha_cuts <- cbind(support, alpha_cut)

  ## Output is a list with the name of type of FS, its parameters,
  ## the Membership Function created and its alpha cuts

  Lst <- list(name="Constant", parameters=c(x), Membership.Function=MF_Const, alphacuts=alpha_cuts)

  # return(MF_Const)
  return(Lst)

}
###################################
## Singleton fuzzy set (SingFS): ##
## Parameters:                   ##
## x only one value for          ##
## MF = 1, if U = x; 0, U <> x.  ##
###################################
## Singleton fuzzy set (SingFS): MF = 1, if U = x; 0, U <> x.
SingFS <- function(U,x) {
  ## Check if the membership function can be created for those parameters and the Universe U
  flag = 1
  if (x < min(U) || x > max(U)) {
    print("Value x for Constant Fuzzy Set must be include in the Universe")
    flag = 0
  }
  if (flag == 1) {
    MF <- rep(0, length(U))
    resolution <- U[2] - U[1]
    if ( (x / resolution - round(x/resolution)) == 0 ) {
      position <- which(U == x)
      MF[position] = 1
    } else {
      position <- round(x/resolution)+1
      MF[position] = 1    ## approximate value of x position in U
    }
  }
  MF_Sing <- cbind(U, MF)

  ## Compute the alpha-cuts for the Constant Fuzzy Set - Only position U=x has all alpha-cuts

  alpha <- seq(0, 1.0, 0.05)
  ## Left-side of a singleton fuzzy set
  left_interval <- rep(U[position], length(alpha))
  ## Right-side of a singleton fuzzy set
  right_interval <-  rep(U[position], length(alpha))

  ri <- sort(right_interval)
  ri<- ri[2:length(ri)]
  support=c(left_interval, ri)
  compl <- (1-alpha)
  compl <- compl[2:length(compl)]
  alpha_cut=c(alpha, compl)
  alpha_cuts <- cbind(support, alpha_cut)

  ## Output is a list with the name of type of FS, its parameters,
  ## the Membership Function created and its alpha cuts

  Lst <- list(name="Singleton", parameters=c(x), Membership.Function=MF_Sing, alphacuts=alpha_cuts)

  # return(MF_Sing)
  return(Lst)

}
##################################
## Gaussian fuzzy set (GauFS)   ##
## Parameters:                  ##
## m – Mean of FS               ##
## s – Standasd Deviation of FS ##
##################################
## Non-zero values in the MF - fixed
## Outliers in the alpha-cuts - fixed
#' @export
#' @rdname hidden_functions
GauFS <- function(U,m,s) {
  ## Check if the membership function can be created for those parameters and the Universe U
  flag = 1
  if (m < min(U) || m > max(U)) {
    print("Mean of the Gaussian Fuzzy Set must be include in the Universe")
    flag = 0
  }
  if (flag == 1) {
    ## Auxiliar variables
    ## Left-side of a Gaussian fuzzy set (m,s)
    left_interval <- rep(0, length(U))
    right_interval <- rep(0, length(U))
    MF <- rep(0, length(U))
    i = 1
    left_interval[1] <- exp(-0.5*((U[1]-m)/s)^2)
    while (left_interval[i] < 1  & i < length(U)) {
      i = i + 1
      left_interval[i] <- exp(-0.5*((U[i]-m)/s)^2)
    }
    for (j in (i+1):length(U)) {
      right_interval[j] <- exp(-0.5*((U[j]-m)/s)^2)
    }
    ## Unifying Left and Right
    for ( j in 1:length(U) ) {
      MF[j] <- max(left_interval[j], right_interval[j])
    }

    MF <- round(MF, 7)
    MF_Gau <- cbind(U, MF)

    ## Compute the alpha-cuts for the Gaussian Fuzzy Set
    alpha <- seq(0, 1.0, 0.05)

    left_interval <- rep(0, length(alpha))
    right_interval <- rep(0, length(alpha))

    for(j in 1 : length(alpha)) {
      left_interval[j] <- m + s*sqrt(-2*log(alpha[j]))
    }
    for(j in 1 : length(alpha)) {
      right_interval[j] <- m - s*sqrt(-2*log(alpha[j]))
    }

    #########################################################
    ## Check if the extreme values are +/- Infinity
    ##
    if (left_interval[1] == Inf) {
      left_interval[1] <- U[max(which(MF>0))]
      ##   Procura os valores > U_max de left_interval e substitui pelo valor de U_max
      check <- left_interval > max(U)
      for (i in 1:length(left_interval)) {
        if (check[i] == TRUE) {
          left_interval[i] <- max(U)
        }
      }
    }

    if (right_interval[1] == -Inf) {
      right_interval[1] <- U[1]
      ##   Procura os valores < U_min de right_interval e substitui pelo valor de U_min
      check <- right_interval < min(U)
      for (i in 1:length(right_interval)) {
        if (check[i] == TRUE) {
          right_interval[i] <- min(U)
        }
      }
    }
    #########################################################

    ## Unifying Left and Right
    ri <- sort(left_interval)
    ri<- ri[2:length(ri)]
    support=c(right_interval, ri)
    compl <- (1-alpha)
    compl <- compl[2:length(compl)]
    alpha_cut=c(alpha, compl)
    alpha_cuts <- cbind(support, alpha_cut)

    ## Output is a list with the name of type of FS, its parameters,
    ## the Membership Function created and its alpha cuts

    Lst <- list(name="Gaussian", parameters=c(m,s), Membership.Function=MF_Gau, alphacuts=alpha_cuts)

    # return(MF_Gau)
    return(Lst)
  }
  ## Senão, o que retorna ??? return(NULL) ???
}
#########################################################
## Intersection of two fuzzy sets on the same Universe ##
## Intersection is given by the Min (interFS)          ##
#########################################################
interFS  <- function(U,FS1, FS2) {
  fs1 <- FS1[[3]][,2]
  fs2 <- FS2[[3]][,2]
  InterFS <- rep(0, length(U))
  for ( i in 1:length(U) ) {
    InterFS[i] <- min(fs1[i],fs2[i])
  }
  return(InterFS)
}
##################################################
## Union of two fuzzy sets on the same Universe ##
## Union is given by the Max (uniFS)            ##
##################################################
#' @export
#' @rdname hidden_functions
uniFS  <- function(U,FS, FS1, FS2) {
  MaxFS <- rep(0, length(U))
  if (FS[[1]] == "Triangular" || FS[[1]] == "Trapezoidal") {
    fs1 <- FS1[[3]][,2]
    fs2 <- FS2[[3]][,2]
    for ( i in 1:length(U) ) {
      UniFS[i] <- max(fs1[i],fs2[i])
    }
  }
  else {
    for ( i in 1:length(U) ) {
      UniFS[i] <- max(FS1[i],FS2[i])
    }
  }
  return(UniFS)
}
###########################################################
## Complement of a fuzzy set w.r.t the Universe (compFS) ##
###########################################################
#' @export
#' @rdname hidden_functions
compFS  <- function(U,FS) {
  fs <- FS[[3]][,2]
  CompFS <- rep(0, length(U))
  for ( i in 1:length(U) ) {
    CompFS[i] <- (1-fs[i])
  }
  return(CompFS)
}
################################################################
## Degree of membership of an element x to a fuzzy set (dmFS) ##
################################################################
#' @export
#' @rdname hidden_functions
dmFS <- function(x,U,FS) {
  if (FS[[1]] == "Triangular") {
    parameters <- FS[[2]]
    aa <- 0.0
    bb <- 1.0
    T1 <- c(parameters[1], parameters[2])
    T2 <- c(aa, bb)
    T3 <- c(parameters[2], parameters[3])
    T4 <- c(bb, aa)
    ##linear regresssions to estimate Triangular fuzzy set
    res=stats::lm(formula = T2 ~ T1)
    intercept1 <- res$coefficients[1]
    coeff1 <- res$coefficients[2]
    res=stats::lm(formula = T4 ~ T3)
    intercept2 <- res$coefficients[1]
    coeff2 <- res$coefficients[2]
    yL <- x * coeff1 + intercept1
    if (yL < 0 || yL > 1 || is.na(yL)) { yL = 0}
    yR <- x * coeff2 + intercept2
    if (yR < 0 || yR > 1 || is.na(yR)) { yR = 0}
    y = max(yL, yR)
    return(y)
  }
  if (FS[[1]] == "Trapezoidal") {
    parameters <- FS[[2]]
    if ( x >= parameters[2] & x <= parameters[3] ) {
      y = 1
      return(y)
    }
    else {
      aa <- 0.0
      bb <- 1.0
      T1 <- c(parameters[1], parameters[2])
      T2 <- c(aa, bb)
      T3 <- c(parameters[3], parameters[4])
      T4 <- c(bb, aa)
      ##linear regresssions to estimate Triangular fuzzy set
      res=stats::lm(formula = T2 ~ T1)
      intercept1 <- res$coefficients[1]
      coeff1 <- res$coefficients[2]
      res=stats::lm(formula = T4 ~ T3)
      intercept2 <- res$coefficients[1]
      coeff2 <- res$coefficients[2]
      yL <- x * coeff1 + intercept1
      if (yL < 0 || yL > 1 || is.na(yL)) { yL = 0}
      yR <- x * coeff2 + intercept2
      if (yR < 0 || yR > 1 || is.na(yR)) { yR = 0}
      y = max(yL, yR)
      return(y)
    }
  }
  if (FS[[1]] == "Gaussian") {
    parameters <- FS[[2]]
    y = exp(-0.5*((x-parameters[1])/parameters[2])^2)
    if ( y <= 10^(-4) ) {
      y = 0                ## Valor menor do que 10^-4 => 0
    }
    return(y)
  }
  if (FS[[1]] == "Singleton") {
    parameters <- FS[[2]]
    if ( x == parameters[1] ) {
      y = 1
    } else {
      y = 0
    }
    return(y)
  }
}
#########################################
## Centroid method for defuzzification ##
#########################################
#' @export
#' @rdname hidden_functions
centroidFS <- function(U,FS) {
  centroid = 0
  sum_memberhip = 0
  for (i in 1: length(U)) {
    centroid = centroid + (U[i] * FS[i])
    sum_memberhip = sum_memberhip + FS[i]
  }
  centroid = centroid/sum_memberhip
  return(centroid)
}
#######################################
## Method for computing OR on two FS ##
## Mamdani type using MAX            ##
#######################################
#' @export
#' @rdname hidden_functions
OR <- function(FRBS,x,y) {
  if (FRBS == "Mandani") {
    out <- max(x,y)
  }
  return(out)
}
########################################
## Method for computing AND on two FS ##
## Mamdani type using MIN             ##
########################################
#' @export
#' @rdname hidden_functions
AND <- function(FRBS,x,y) {
  if (FRBS == "Mandani") {
    out <- min(x,y)
  }
  return(out)
}
######################################
## Method for computing NOT on a FS ##
## Mamdani type using (1 - MF(FS))  ##
######################################
#' @export
#' @rdname hidden_functions
NOT <- function(FRBS,x) {
  if (FRBS == "Mandani") {
    out <- (1 - x)
  }
  return(out)
}
################################################
## Method for computing Implication on two FS ##
## Mamdani type using MIN                     ##
################################################
#' @export
#' @rdname hidden_functions
Implication <- function(FRBS,x,U,FS) {
  if (FRBS == "Mandani") {
    FS1 <- ConstFS(U,x)
    MF <- interFS(U,FS1, FS)
  }
  return(MF)
}
#############################################
## Method for computing Aggregation on FSs ##
## Mamdani type using MAX                  ##
##############################################
#' @export
#' @rdname hidden_functions
Aggregation <- function(FRBS,U,Lst) {
  if (FRBS == "Mandani") {
    MF <- Lst[[1]]
    for (i in 2:length(Lst)) {
      for ( j in 1:length(U) ) {
        MF[j] <- max(MF[j], Lst[[i]][j])
      }
    }
  }
  return(MF)
}
##################################################
## Plot a fuzzy set w.r.t the Universe (plotFS) ##
##################################################
#' @export
#' @rdname hidden_functions
plotFS <- function(Universe,FS) {
  if (FS[[1]] == "Triangular" || FS[[1]] == "Trapezoidal" || FS[[1]] == "Gaussian" || FS[[1]] == "Singleton" || is.na(FS[[1]])) {
    FuzzySet <- FS[[3]][,2]
    plot(Universe, FuzzySet, pch=".", lty=1, ylim=c(0,1))
    graphics::lines (Universe, FuzzySet, col='blue', lty=1, ylim=c(0,1))
  }
  else {
    FuzzySet <- FS
    plot(Universe, FuzzySet, pch=".", lty=1,ylim=c(0,1))
    graphics::lines (Universe, FuzzySet, col='blue', lty=1,ylim=c(0,1))
  }
}

