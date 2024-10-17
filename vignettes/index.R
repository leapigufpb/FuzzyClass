## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)


## -----------------------------------------------------------------------------
# Load the iris dataset
data(iris)

# Display the structure of the dataset
str(iris)


## -----------------------------------------------------------------------------
# Calculate summary statistics by species
summary_by_species <- by(iris[, -5], iris$Species, summary)
summary_by_species



## -----------------------------------------------------------------------------
# Scatter plot of sepal length vs. sepal width
plot(iris$Sepal.Length, iris$Sepal.Width, col = iris$Species, pch = 19,
     xlab = "Sepal Length", ylab = "Sepal Width", main = "Sepal Length vs. Sepal Width")
legend("topright", legend = levels(iris$Species), col = 1:3, pch = 19)



## -----------------------------------------------------------------------------
# Scatter plot of petal length vs. petal width
plot(iris$Petal.Length, iris$Petal.Width, col = iris$Species, pch = 19,
     xlab = "Petal Length", ylab = "Petal Width", main = "Petal Length vs. Petal Width")
legend("topright", legend = levels(iris$Species), col = 1:3, pch = 19)



## ----message=FALSE, warning=FALSE---------------------------------------------
library(FuzzyClass)

# Load the iris dataset
data(iris)

# Splitting the dataset into training and testing sets
set.seed(123)
train_index <- sample(nrow(iris), nrow(iris) * 0.7)
train_data <- iris[train_index, ]
test_data <- iris[-train_index, ]



## -----------------------------------------------------------------------------

# Build the Fuzzy Gaussian Naive Bayes classifier
fit_FGNB <- GauNBFuzzyParam(train = train_data[, -5],
                            cl = train_data[, 5], metd = 2, cores = 1)




## -----------------------------------------------------------------------------

# Make predictions on the testing data
predictions <- predict(fit_FGNB, test_data[, -5])

head(predictions)

# Calculate the accuracy
correct_predictions <- sum(predictions == test_data[, 5])
total_predictions <- nrow(test_data)
accuracy <- correct_predictions / total_predictions

accuracy


