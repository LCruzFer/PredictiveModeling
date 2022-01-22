rm(list=ls())

library(ggplot2)
library(randomForest)
library(tree)
library(dplyr)
library(glmnet)
library(reshape2)
library(plotROC)
library(reliabilitydiag)
library(murphydiagram)
library(quantreg)
library(lmtest)
library(sandwich)

#set working directory to data path 
setwd("/Users/llccf/OneDrive/Dokumente/4. Semester/PredictiveModeling/exam/")
#also set seed so replications of this code generate same results 
set.seed(2021)

############
#Functions
############
#This section contains all functions defined for the problems solved in their respective section.
#Comments within the functions explain what is going on in there and whenever applied referenced
#in the comments.

test_train_sample=function(dat, test_n=1000){
  #split a sample in dat into a test and training sample 
  #test_n defines size of test sample created
  #rownames are reset to avoid confusion between them and indices
  #create slicing 
  slices=sample.int(nrow(dat), size=test_n) 
  #create subsets
  train=dat[-slices, ]
  rownames(train)=NULL
  test=dat[slices, ]
  rownames(test)=NULL
  #return them in a named list
  dat_list=list('train'=train, 'test'=test)
  return (dat_list)
}

#reuse functions from PS2 for KNN predictions
euclid_dist=function(a, b){
  #calculate euclidean distance between vectors/matrices/data.frames
  dist=sqrt(sum((a-b)^2))
  return(dist)
}

knn_prediction=function(x, y, k, x0){
  #need a distance matrix, i.e. for each point in x0 have a column with 
  #distances to points in x (training set)
  #then rank the values in each column and get the ones from 1 to k
  #use the respective observations in the y vector to calculate the mean over 
  #them as the prediction 
  #set up vector in which predictions will be saved
  predictions=rep(NA, length(y))
  #then loop over each observation in test data
  for(i in 1:length(y)){
    #get euclidean distances between single row in test data and all rows 
    #in training data
    euclid=apply(x, 1, euclid_dist, x0[i, ])
    #sort them by size and get first k values -> indices that are closest ones
    ranks=rank(euclid, ties.method='random')<=k
    #get corresponding y values in training data and get mean as prediction
    predictions[[i]]=mean(y[ranks])
  }
  return(predictions)
}

cv_knn_prediction=function(x, y, k_range, x0, y0){
  #this function makes KNN prediction using each value in k_range as number 
  #of neighbors, then calculates MSE for each of them and returns predictions that 
  #yield smallest MSE 
  #create empty matrix that will be filled column wise with predictions for y
  #for different k
  predictions_k=matrix(NA, nrow=length(y), ncol=length(k_range))
  #and empty vector in which mse will be saved
  mses=rep(NA, length(k_range))
  for(i in 1:length(k_range)){
    #make prediction for given k
    predictions_k[, i]=knn_prediction(x=x, y=y, k=k_range[[i]], x0=x0)
    mses[i]=mean((y0-predictions_k[, i])^2)
  }
  #find index of predictions with smallest mse 
  lowest=which(mses==min(mses))
  #get the predictions of the corresponding k
  predictions_fin=predictions_k[, lowest]
  #return these predictions, the min mse and the corresponding k
  return(list(predictions=predictions_fin, mse=mses[lowest], k_min=k_range[lowest]))
}

visual_eval=function(data, true, preds, title){
  #this function plots true values against predictions of the variable as a visual way
  #of evaluation
  plot=ggplot(test_preds, aes(x=true, y=preds))+
    geom_point(size=0.2)+
    geom_abline(color='red', size=0.1)+
    labs(x='Charges', y='Prediction', title=title)
  return(plot)
}

mz_test=function(true, preds){
  #This function runs the Mincer-Zarnowitz Test for prediction evaluation
  #first run a simple OLS regression of true values on predictions 
  reg=lm(true~preds)
  #make a Wald test on whether the coefficients are jointly significant
  coefci=coefci(reg, vcov.=NeweyWest)%>%round(3)
  W=t(coef(reg)-c(0,1))%*%solve(NeweyWest(reg))%*%(coef(reg)-c(0,1))
  pval=1-pchisq(W,2)%>%round(3)
  return(list(ci=coefci, wald=W, pval=pval))
}

#Bergman loss function with phi(x)=x^2
sq_loss=function(true, pred){(true-pred)^2}
#Bergman loss function with phi(x)=-log(x)
log_loss=function(true, pred){loss=log(true) - log(true) + true/pred - 1}
#Bergman loss function with phi(x)=exp(-x)
exp_loss=function(true, pred){exp(-true) - exp(-pred) + exp(-pred) *(true-pred)}

dm_test=function(true, pred1, pred2, loss='squared'){
  #this function runs a Diebold-Mariano Test using the loss function specified in 
  #loss argument
  if(loss=='squared'){
    d=sq_loss(true, pred1)-sq_loss(true, pred2)
    reg=lm(d~1)
  }
  #if loss=exp use loss function with exp(-x)
  else if(loss=='exp'){
    d=exp_loss(true, pred1)-exp_loss(true, pred2)
    reg=lm(d~1)
  }
  #if loss=log use QLIKE loss function which arises from using phi(x)=-log(x)
  else if(loss=='log'){
    d=log_loss(true, pred1)-log_loss(true, pred2)
    reg=lm(d~1)
  }
  else{
    print('Specify loss to be squared, log, or exp!')
  }
  #then run significane test with heteroskedastiticy robust std errors 
  sig_test=coefci(reg, vcov.=NeweyWest)
  return(list(mean_d=mean(d), sigtest=sig_test))
}

make_compare_frame=function(comp1, comp2, comp3){
  lowerbounds=c(comp1$sigtest[1], comp2$sigtest[1], comp3$sigtest[1])
  upperbounds=c(comp1$sigtest[2], comp2$sigtest[2], comp3$sigtest[2])
  comparison=c('KNN vs RF', 'Lasso vs RF', 'Lasso vs KNN')
  return(data.frame(Methods=comparison, Lower=lowerbounds, Upper=upperbounds))
}

############
#Problem 2
############
#read in data 
insurance=read.csv('insurance.csv')

#a) split data into test and training set 
#same procedure as in Problem 1 
test_train_data=test_train_sample(insurance, test_n=dim(insurance)[1]/2)
test_dat=test_train_data$test
train_dat=test_train_data$train
#also prepare a dataframe in which predictions and true values are saved 
test_preds=test_dat%>%dplyr::select(charges)

#b) Predict mean of y=charges with 3 different prediction methods 
#ASSUME THAT WE ARE TALKING ABOUT THE CONDITIONAL MEAN

#1 LASSO
#Lasso 
#prepare training data: get x and interactions as well as y values as single dataframes/vectors 
x_train=model.matrix(charges~.^2, data=train_dat)
y_train=train_dat$charges
#fit model using cross validation
lasso=cv.glmnet(x=x_train, y=y_train, alpha=1)
#prepare test data 
x_test=model.matrix(charges~.^2, data=test_dat)
#then fit model to test data
#turn lasso predictions into vector as they are otherwise saved as a dataframe inside
#the dataframe with predictions 
test_preds$lasso_preds=as.vector(predict(lasso, newx=x_test, s='lambda.1se'))
#get variables that are not removed by lasso for knn 
#first get matrix of coefficients and remove intercepts
lasso_coefs=as.matrix(coef(lasso, s='lambda.1se'))[-1:-2, ]
#then extract names where coefficient is not 0
lasso_vars=names(lasso_coefs)[lasso_coefs!=0]

#2 Random Forest 
#fit a random forest to training data 
rf=randomForest(charges~., data=train_dat)
#then make predictions 
test_preds$rf_preds=predict(rf, newdata=test_dat)

#3 KNN 
#fit KNN regression 
knn_x_train=model.matrix(charges~.^2, data=train_dat)[, lasso_vars]
knn_x_test=model.matrix(charges~.^2, data=test_dat)[, lasso_vars]
knn_y_train=train_dat$charges
test_preds$knn_preds=knn_prediction(knn_x_train, knn_y_train, 5, knn_x_test)

#c) Evaluation 
#need a long df again
test_preds_long=melt(test_preds, measure.vars=c('knn_preds', 'rf_preds', 'lasso_preds'), variable.name='pred_method', value.name='pred_vals')

####
#Absolute Evaluation 

#Visual 
#Lasso 
plot_lasso=visual_eval(test_preds, test_preds$charges, test_preds$lasso_preds, title='Lasso Predictions')
#Random Forest 
plot_rf=visual_eval(test_preds, test_preds$charges, test_preds$rf_preds, title='Random Forest Predictions')
#KNN 
plot_knn=visual_eval(test_preds, test_preds$charges, test_preds$knn_preds, title='KNN Predictions')
#Facet of all predictions 
facet=ggplot(test_preds_long, aes(x=charges, y=pred_vals))+geom_point(size=0.2)+
  geom_abline(size=0.1, color='red')+
  facet_wrap(vars(pred_method))

#MSE 
#calculate the MSE for each prediction method 
mse=test_preds_long%>%group_by(pred_method)%>%summarize(mse=mean((charges-pred_vals)^2, na.rm=TRUE))
print(mse)
#MZ test 
#run Mincer-Zarnowitz test for all predictions 
#Lasso
MZ_lasso=mz_test(test_preds$charges, test_preds$lasso_preds)
#Random Forest 
MZ_rf=mz_test(test_preds$charges, test_preds$rf_preds)
#KNN
MZ_knn=mz_test(test_preds$charges, test_preds$knn_preds)

names=c('Lasso', 'KNN', 'RF')
ci_int=list(MZ_lasso$ci[1, ], MZ_rf$ci[1, ], MZ_knn$ci[1, ])
ci_pred=list(MZ_lasso$ci[2, ], MZ_rf$ci[2, ], MZ_knn$ci[2, ])
wald=list(MZ_lasso$wald, MZ_rf$wald, MZ_knn$wald)
pvals=list(MZ_lasso$pval, MZ_rf$pval, MZ_knn$pval)
mz_table=as.data.frame(cbind(Method=names, 'Intercept CI'=ci_int, 'Predictions CI'=ci_pred, 'Wald Stat'=wald, 'p value'=pvals))

#only for Lasso the intercept isn't significant, for all three the coefficient on
#predictions is significant 

###
#Relative Evaluation 

#Diebold-Mariano Tests with squared loss function
#start with Lasso vs RF 
dm_lasso_rf=dm_test(test_preds$charges, test_preds$lasso_preds, test_preds$rf_preds)
#Lasso vs KNN
dm_lasso_knn=dm_test(test_preds$charges, test_preds$lasso_preds, test_preds$knn_preds)
#KNN vs RF 
dm_knn_rf=dm_test(test_preds$charges, test_preds$knn_preds, test_preds$rf_preds)

#Diebold-Mariano Tests with QLIKE loss function 
#start with Lasso vs RF 
dm_lasso_rf=dm_test(test_preds$charges, test_preds$lasso_preds, test_preds$rf_preds, loss='log')
#Lasso vs KNN
dm_lasso_knn=dm_test(test_preds$charges, test_preds$lasso_preds, test_preds$knn_preds, loss='log')
#KNN vs RF 
dm_knn_rf=dm_test(test_preds$charges, test_preds$knn_preds, test_preds$rf_preds, loss='log')

#Diebold-Mariano Tests with exponential loss function 
#start with Lasso vs RF 
dm_lasso_rf=dm_test(test_preds$charges, test_preds$lasso_preds, test_preds$rf_preds, loss='exp')
#Lasso vs KNN
dm_lasso_knn=dm_test(test_preds$charges, test_preds$lasso_preds, test_preds$knn_preds, loss='exp')
#KNN vs RF 
dm_knn_rf=dm_test(test_preds$charges, test_preds$knn_preds, test_preds$rf_preds, loss='exp')

#Murphydiagrams
#Lasso vs RF 
murphydiagram(test_preds$lasso_preds, test_preds$rf_preds, test_preds$charges)
#Lasso vs KNN
murphydiagram(test_preds$lasso_preds, test_preds$knn_preds, y=test_preds$charges)
#RF vs KNN
murphydiagram(test_preds$rf_preds, test_preds$knn_preds, y=test_preds$charges)