% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/FuzzyGammaNaiveBayes.R
\name{FuzzyGammaNaiveBayes}
\alias{FuzzyGammaNaiveBayes}
\title{Fuzzy Gamma Naive Bayes}
\usage{
FuzzyGammaNaiveBayes(train, cl, cores = 2, fuzzy = TRUE)
}
\arguments{
\item{train}{matrix or data frame of training set cases.}

\item{cl}{factor of true classifications of training set}

\item{cores}{how many cores of the computer do you want to use to use for prediction (default = 2)}

\item{fuzzy}{boolean variable to use the membership function}
}
\value{
A vector of classifications
}
\description{
\code{FuzzyGammaNaiveBayes} Fuzzy Gamma Naive Bayes
}
\examples{

set.seed(1) # determining a seed
data(iris)

# Splitting into Training and Testing
split <- caTools::sample.split(t(iris[, 1]), SplitRatio = 0.7)
Train <- subset(iris, split == "TRUE")
Test <- subset(iris, split == "FALSE")
# ----------------
# matrix or data frame of test set cases.
# A vector will be interpreted as a row vector for a single case.
test <- Test[, -5]
fit_NBT <- FuzzyGammaNaiveBayes(
  train = Train[, -5],
  cl = Train[, 5], cores = 2
)

pred_NBT <- predict(fit_NBT, test)

head(pred_NBT)
head(Test[, 5])
}
\references{
\insertRef{de2018fuzzy}{FuzzyClass}
}
