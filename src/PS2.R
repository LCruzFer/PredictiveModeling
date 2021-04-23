library('ggplot2')
library('nycflights13')
library('stringr')
#Problem 1 
euclid_dist = function(a, b){
  #calculate euclidean distance between two vectors 
  dist = sqrt((a-b)^2)
  return(dist)
}

knn_prediction = function(x, y, k, x0){
  #need a distance matrix, i.e. for each point in x0 have a column with 
  #distances to points in x (training set)
  #then rank the values in each column and get the ones from 1 to k
  #use the respective observations in the y vector to calculate the mean over 
  #them as the prediction 
  #set up empty distance matrix, nrows must be length of our training set x 
  #(element i,j shows distance of training point i to test point j)
  distmat = matrix(NA, nrow = length(x), ncol = length(x0))
  #set up empty predictions vector
  predictions = rep(NA, length(x0))
  #loop over each point in test sample
  for (i in 1:length(x0)){
    #get distances to points in training sample
    distmat[, i] = euclid_dist(x0[i], x)
  }
  #get rank of distances 
  ranksmat = apply(distmat, 2, rank, ties.method = "random")
  #now get the values with rank 1 to k in each column 
  for (i in 1:length(x0)){
    #get a vector containing true/false depending on whether point is one of k nearest neigbors 
    #(rank btw 1 and k)
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
  print(k)
}
predictions = predictions[, -1]
#bind together in dataframe 
pred_df = data.frame(cbind(x0, predictions))
names(pred_df) = c('x0', 'k1', 'k20', 'k200')

#c) 
plot = ggplot(pred_df, aes(x=x0))
plot + geom_line(aes(y=k1, color='darkred')) +
       geom_line(aes(y=k20, color='blue')) +
       geom_line(aes(y=k200, color='green')) + 
       xlab('Test Data') + 
       ylab('Predicted Arrival Delay') 

#Problem 3 
#first only keep observations that are not NA 
subflights = na.omit(flights[, c('arr_delay', 'dep_delay', 'distance', 'day')])
#sample 2000 random observations from the df w/0 NA by drawing 2000 random 
#numbers from the range 1 to nrow(df)
flights_train = subflights[sample(nrow(subflights), 2000), ]

#functions for evaluation of fit
mse = function(res){
  n = length(res)
  mse = mean(res**2)
  return(mse)
}
aic = function(res, k){
  n = length(res)
  aic = n*log(sum(res**2)/n)+2*k
  return(aic)
}
bic = function(res, k){
  n = length(res)
  bic = n*log(sum(res**2)/n)+k*log(n)
  return(bic)
}
loo_mse = function(res, mod){
  res_loo = res/(1-hatvalues(mod))
  loo_mse = mean(res_loo**2)
  return(loo_mse)
}
subsets = function(data, dependent){
  #create all possible regression subsets of independent variables given the dataset 
  #and the dependent variable 
  all_independents = names(data)[names(data) != dependent]
  #get all possible subsets with m-1 variables, where m is number of elements 
  subsets = combn(independents, length(independents)-1)
  #now bind subsets into single vectors 
  subset_vecs = list(all_independents)
  #add all single variables
  for (i in all_independents){
    subset_vecs = c(subset_vecs, list(i))
  }
  #now add all possible combinations of n-1 variables
  for (i in seq(dim(subsets)[2])){
   subset_vecs = c(subset_vecs, list(subsets[, i]))
  }
  return(subset_vecs)
}

eval_subsets = function(data, dependent){
  #first get all possible subsets 
  subset_vecs = subsets(data, dependent)
  #then for each list in subset_vecs: 
    #1: crate a formula 
    #2: run linear regression 
    #3: calculate all 4 criteria and save them as columns of matrix, where each row is one
    #criterion of fit and the rows are models 
    #or better us a data.frame as saving option 
  evaluation = matrix(NA, nrow = 4, ncol = length(subset_vecs))
  for (i in 1:length(subset_vecs)){
    #get the formula
    dependents = unlist(subset_vecs[i])
    #get number of regressors for aic and bic later on 
    k = length(dependents)
    form = paste(dependents, collapse = '+')
    form = paste(independent, form, sep = '~')
    #perform regression and get residuals(need model for calculating loo mse)
    mod = lm(form, data)
    res = mod$res
    #calculate the mse and save it as first element in column i
    evaluation[1, i] = mse(res)
    #calculate aic 
    evaluation[2, i] = aic(res, k)
    #calculate bic 
    evaluation[3, i] = bic(res, k)
    #calculate cross validated mse 
    evaluation[4, i] = loo_mse(res, mod)
  }
  evals = c('mse', 'aic', 'bic', 'loo_mse')
  for (i in 1:dim(evaluation)[1]){
     where_min = which.min(evaluation[i, ])
     best_mod = paste(unlist(subset_vecs[where_min]), collapse = ' + ')
     phrase = paste('Best model according to', evals[i], 'is: ', sep = ' ')
     print(paste(phrase, best_mod, 'with', evaluation[i, where_min], sep = ' '))
  }
  return(evaluation)
}

evaluation = eval_subsets(flights_train, 'arr_delay')
