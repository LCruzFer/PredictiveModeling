library('ggplot2')
library('nycflights13')

#Problem 1 
euclid_dist = function(a, b){
  #calculate euclidean distance between two vectors 
  dist = sqrt((a-b)^2)
  return(dist)
}

knn_prediction = function(x, y, k, x0){
  #need a distance matrix, i.e. for each point in x0 have a column with distances to points in x (training set)
  #then rank the values in each column and get the ones from 1 to k
  #use the respective observations in the y vector to calculate the mean over them as the prediction 
  #set up empty distance matrix
  distmat = matrix(NA, nrow = length(x), ncol = length(x))
  #set up empty predictions vector
  predictions = rep(NA, length(x0))
  #loop over each point in test sample
  for (i in 1:length(x0)){
    #get distances to points in training sample
    distmat[, i] = euclid_dist(x0[i], x)
    #get rank of distances 
    ranksmat = apply(distmat, 2, rank, ties.method = "random")
  }
  #now get the values with rank 1 to k in each column 
  for (i in 1:length(x0)){
    #get a vector containing true/false depending on whether point is one of k nearest neigbors (rank btw 1 and k)
    neighbors = ranksmat[, i]%in%seq(1, k, 1)
    #now get the corresponding values in y (y[neighbors]) and average them
    avg = mean(y[neighbors])
    #this average is the prediction for point x0[i]
    predictions[i] = avg
  }
  return(predictions)
}


x0 = rnorm(100, 2, 10)
x = rnorm(100, 2, 10)
y = 2*x + rnorm(100, 0, 1)
k = 3

testing = knn_prediction(x=x, y=y, k=k, x0=x0)

#Problem 2 
#load the flights data 
data(flights)
dim(flights)
#a) only keep the data for which arr_delay and dep_delay are not NA 
flights = flights[is.na(flights$arr_delay) == FALSE, ]
flights = flights[is.na(flights$dep_delay) == FALSE, ]
#b) 
x0 = seq(-10, 50, length.out = 100)
y = flights$arr_delay 
x = flights$dep_delay 
#prediction matrix where columns contain predictions for different k
predictions = seq(1, length(x0))
for (k in c(1, 20, 200)){
  predictions = cbind(predictions, knn_prediction(x, y, k, x0))
}