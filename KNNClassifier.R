# KNN classifier
# A classification strategy, which uses non-parametric approach to estimate the probabilities
# directly from data.
# No explicit model is derived.

# Frequentist probability estimate - The observation is assigned to the class for which 
# P(Y = y| X = x) = Ny / k is maximum.
# Nearby observations - similarity/ dissimilarity measure - pearson, cosine, Euclidean dist

# User-defined parameter -k
# k is small - few observations in the vicinity of x, boundaries are non-linear, 
# the classifier is very flexible. As a result, it is more prone to overfitting.
# k is large - less flexible, variance is reduced, whereas bias is increased.

# Deal with ties in distance
# When there are ties, for instance, between k =7 and k=8, then K will be increased to
# include all the tied points that are in the same distance from x.

# kNN() from the package class is a kNN classifier with Euclidean distance.

new_iris_predictors <- iris[, c("Sepal.Width", "Petal.Width")]
new_iris_class <- iris[, "Species"]

# Remove the target observation from the training and move to the test dataset
( target <- which((iris$Sepal.Width == 2.5) & (iris$Petal.Width==1.5)))

new_iris_test <- new_iris_predictors[target,]
new_iris_predictors <- new_iris_predictors[-target,]
new_iris_class <- new_iris_class[-target]

library(class)
(knn(train = new_iris_predictors, test = new_iris_test, 
     cl = new_iris_class, k = 1, prob = TRUE))
(knn(train = new_iris_predictors, test = new_iris_test, 
     cl = new_iris_class, k = 3, prob = TRUE))
(knn(train = new_iris_predictors, test = new_iris_test, 
     cl = new_iris_class, k = 5, prob = TRUE))
(knn(train = new_iris_predictors, test = new_iris_test, 
     cl = new_iris_class, k = 7, prob = TRUE))

# Practice exercise 1
distmatr <- as.matrix(dist(iris[, c("Sepal.Width", "Petal.Width")]))
targetrow <- distmatr[target,]
targetrow[order(targetrow)[1:10]]

# Practice exercise 2
set.seed(0)
no_obs <- dim(iris)[1] # No. of observations (150)

accuracy <- rep(0, 10)

for(i in 1:10)
{
   test_index <- sample(no_obs, size = as.integer(no_obs*0.2), replace = FALSE) # 20% data records for test
   test_predictors <- iris[test_index, c("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width")]
   test_class <- iris[test_index, "Species"]
   training_index <- -test_index # 80% data records for training
   training_predictors <- iris[training_index, c("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width")]
   training_class <- iris[training_index, "Species"]
   
   Pred_class <- knn(train = training_predictors, test = test_predictors, 
                     cl = training_class, k = 1)
   (cont_tab <- table(Pred_class, test_class))
   
   (accuracy[i] <- sum(diag(cont_tab))/ sum(cont_tab))
}
mean(accuracy)
sd(accuracy)

accuracy

# Selecting k experimentally
# K is usually selected through experimenting. 
set.seed(0)
meanaccr <- rep(0, 20)
for(k in 1:20)
{
   accuracy <- rep(0, 10)
   for(i in 1:10)
   {
      test_index <- sample(no_obs, size = as.integer(no_obs*0.2), replace = FALSE) # 20% data records for test
      test_predictors <- iris[test_index, c("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width")]
      test_class <- iris[test_index, "Species"]
      training_index <- -test_index # 80% data records for training
      training_predictors <- iris[training_index, c("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width")]
      training_class <- iris[training_index, "Species"]
      
      Pred_class <- knn(train = training_predictors, test = test_predictors, 
                        cl = training_class, k = k)
      (cont_tab <- table(Pred_class, test_class))
      
      (accuracy[i] <- sum(diag(cont_tab))/ sum(cont_tab))
   }
   meanaccr[k] <- mean(accuracy)
}

plot(1:20, meanaccr, xlab = "k", ylab = "Mean accuracy")

# 1 - For low to moderate dimensionality datasets, with proper (dis)similarity measure chosen, 
# with K properly set, knn can provide very competitive results. However, it will perform
# lowly for high dimensional data.
# 2 - knn doesn't provide information about relevant predictors unlike LDA, QDA, and logistic 
# regression.
# 3 - Numerical predictors need to be rescaled within [0, 1] if they've wider intervals.
# 4 - KNN can be used for regression to model a numeric dependent variable, Y.


